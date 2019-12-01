#!/bin/python3

import json
import os
import rdflib as r
from rdflib import Graph, Namespace, BNode
from rdflib.namespace import RDF
from rdflib.term import Literal
import click

###
## constants

# [] RDF configuration -> customize IRIs: the IRIs for each WN should
# be read from the wns.tsv config file, then placed in a dict like
# WN30_LANG
## [] remove hardcoded stuff
# [] read configuration using labels, not harcoded field indices

WN30 = Namespace("https://w3id.org/own-pt/wn30/schema/")
WN30EN = Namespace("https://w3id.org/own-pt/wn30-en/instances/")
WN30PT = Namespace("https://w3id.org/own-pt/wn30-pt/instances/")

WN30_LANG = {"en": WN30EN, "pt": WN30PT}

POS1TOLONG = {"A": "adj", "N": "noun", "R": "adv", "S": "adjs", "V": "verb"}
SYNSETTYPE = {"A": Literal("AdjectiveSynset"), "N":
              Literal("NounSynset"), "R": Literal("AdverbSynset"),
              "S": Literal("AdjectiveSatelliteSynset"), "V":
              Literal("VerbSynset")}

COMMENT = "comment"
COMMENTS = "_comments"
DEFINITION = "definition"
EXAMPLE = "example"
LANG = "lang"
LEXICOGRAPHER_FILE = "lexicographerFile"
ID = "id"
EXAMPLES = "examples"
WORDSENSES = "wordsenses"
CONTAINS_WORDSENSE = "containsWordSense"
LEXICAL_FORM = "lexicalForm"
LEXICAL_ID = "lexicalId"
POINTERS = "pointers"
POSITION = "_position"
NAME = "name"
SENSEKEY = "senseKey"
FRAME = "frame"
FRAMES = "frames"
WSENSE = "wordsense"
SYNSET = "synset"
RELATIONS = "relations"
SOURCE_BEGIN = "sourceBegin"
SOURCE_END = "sourceEnd"
SYNTACTIC_MARKER = "syntacticMarker"
TARGET_LEXICAL_FORM = "_targetLexicalForm"
TARGET = "_target"

CURRENT_LANG = None  # initialized during runtime

SYNSET_RELATIONS, WORD_RELATIONS = {}, {}

WN_TARGET = WN30[TARGET]
WN_TARGET_LEXICAL_FORM = WN30[TARGET_LEXICAL_FORM]
WN_FRAME = WN30[FRAME]
WN_LEXICOGRAPHER_FILE = WN30[LEXICOGRAPHER_FILE]
WN_LANG = WN30[LANG]
WN_CONTAINS_WORDSENSE = WN30[CONTAINS_WORDSENSE]
WN_SOURCE_BEGIN = WN30[SOURCE_BEGIN]
WN_LEXICAL_ID = WN30[LEXICAL_ID]
WN_LEXICAL_FORM = WN30[LEXICAL_FORM]
WN_DEFINITION = WN30[DEFINITION]
WN_COMMENT = WN30[COMMENT]
WN_SYNTACTIC_MARKER = WN30[SYNTACTIC_MARKER]
WN_EXAMPLE = WN30[EXAMPLE]
RDF_TYPE = RDF.type


###
## rdf -> text
def read_config(config_dir):
    def read_tsv(file_path, line_function, initial_value):
        def read_line(line):
            return list(map(str.strip, line.split(sep='\t')))

        res = initial_value
        with open(file_path, 'r') as input_stream:
            field_names = read_line(next(input_stream))
            number_fields = len(field_names)
            for line_number, line in enumerate(input_stream):
                line_fields = read_line(line)
                number_line_fields = len(line_fields)
                if number_line_fields == 1 & (line_fields[0].lstrip()[:2] == '--'):
                    pass
                elif number_line_fields == number_fields:
                    res = line_function(res, line_fields)
                else:
                    print("Error: while reading TSV file {}, uneven number of fields; should have {} fields, but has {} fields in line {}".format(
                        file_path, number_fields, number_line_fields, 1 + line_number))
                    break
            return res

    def read_relations(res, fields):
        (synset_relations, word_relations) = res
        relation_name = fields[3]
        relation_text = fields[2]
        if relation_text != "_":
            if fields[5] == "word":
                word_relations[relation_name] = relation_text
            elif fields[5] == "synset":
                synset_relations[relation_name] = relation_text
            else:
                word_relations[relation_name] = relation_text
                synset_relations[relation_name] = relation_text
        return (synset_relations, word_relations)
    #
    synset_relations, word_relations = read_tsv(os.path.join(
        config_dir, "relations.tsv"), read_relations, ({}, {}))
    return (synset_relations, word_relations)


def summarize(graph, node):
    # for debugging
    print(node)
    print("  subject")
    for pred, obj in graph.predicate_objects(node):
        print("    {} {}".format(r.namespace.split_uri(pred)[1], obj))
    print("  object")
    for subj, pred in graph.subject_predicates(node):
        print("    {} {}".format(subj, r.namespace.split_uri(pred)[1]))
    return None


def print_graph(graph, output_dir):
    global CURRENT_LANG
    lex_files = set(graph.objects(predicate=WN_LEXICOGRAPHER_FILE))
    langs = set(graph.objects(predicate=WN_LANG))
    for lang in langs:
        CURRENT_LANG = lang
        lang_output_dir = os.path.join(output_dir, lang)
        os.makedirs(lang_output_dir, exist_ok=True)
        print(lang)
        for lexicographer_file in lex_files:
            print_lexfile(graph, lexicographer_file, lang_output_dir)
            print("  {}".format(lexicographer_file))


def sort_word_senses(graph, synset):
    def word_sense_form(ws):
        return graph.value(ws, WN_LEXICAL_FORM, any=False)
    #
    wordsenses = list(graph.objects(synset, WN_CONTAINS_WORDSENSE))
    assert wordsenses, synset
    return sorted(wordsenses, key=word_sense_form)


def sort_synsets(graph, synsets):
    result = sorted(synsets, key=lambda s: graph.value(s, WN_SOURCE_BEGIN))
    return result


def print_lexfile(graph, lexicographer_file, output_dir):
    lexfile_synsets = graph.subjects(predicate=WN_LEXICOGRAPHER_FILE, object=lexicographer_file)
    lang_synsets = list(filter(lambda s: graph.value(s, WN_LANG) == CURRENT_LANG,
                               lexfile_synsets))
    if lang_synsets:
        with open(os.path.join(output_dir, lexicographer_file), 'w+') as output_stream:
            write = lambda data, *args, **kwargs: print(data, file=output_stream, *args, **kwargs)
            pos, lexname = lexicographer_file.split(".")
            write("{}.{}".format(pos, lexname), end="\n\n")
            for synset in sort_synsets(graph, lang_synsets):
                print_synset(graph, synset, lexicographer_file, write)


def word_sense_id(graph, lexicographer_file, word_sense, synset=None):
    synset = synset or graph.value(predicate=WN_CONTAINS_WORDSENSE, object=word_sense)
    lexical_form = graph.value(word_sense, WN_LEXICAL_FORM, any=False)
    lexical_id = graph.value(word_sense, WN_LEXICAL_ID, any=False)
    in_lang = graph.value(synset, WN_LANG, any=False)
    assert in_lang, word_sense
    assert lexical_form, word_sense
    assert lexical_id is not None, word_sense
    return (in_lang, lexicographer_file, lexical_form, int(lexical_id))


def synset_id(graph, lexicographer_file, synset, target_lexical_form):
    for sense in graph.objects(synset, WN_CONTAINS_WORDSENSE):
        if graph.value(sense, WN_LEXICAL_FORM, any=False).eq(target_lexical_form):
            return word_sense_id(graph, lexicographer_file, sense, synset)
    # shouldn't get here
    print(target_lexical_form, synset)
    return None


def print_word_sense_id(wordsense_id, lexicographer_file=None):
    (in_lang, in_lexfile, lexical_form, lexical_id) = wordsense_id
    lexfile_str = "{}:".format(in_lexfile) if in_lexfile.neq(lexicographer_file) else ""
    prefix = "{}".format(lexfile_str) if in_lang == CURRENT_LANG else "@{}:{}:".format(in_lang, in_lexfile)
    maybe_lexical_id = "" if lexical_id == 0 else "[{}]".format(lexical_id)
    return "{}{}{}".format(prefix, lexical_form, maybe_lexical_id)


def print_synset(graph, synset, lexicographer_file, write):
    def print_relations():
        def print_relation(name, wordsense_id):
            return "{}: {}".format(name,
                                   print_word_sense_id(wordsense_id, lexicographer_file))
        rels = []
        for predicate, obj in graph.predicate_objects(synset):
            _, predicate_name = r.namespace.split_uri(predicate)
            predicate_txt_name = SYNSET_RELATIONS.get(predicate_name, None)
            target_synset = graph.value(obj, WN_TARGET, any=False)
            if predicate_txt_name:
                if target_synset:
                    target_lexfile = graph.value(target_synset, WN_LEXICOGRAPHER_FILE)
                    target_lexical_form = graph.value(obj, WN_TARGET_LEXICAL_FORM, any=False)
                    rels.append((predicate_txt_name,
                                 synset_id(graph, target_lexfile, target_synset,
                                           target_lexical_form)))
            elif target_synset:
                raise LookupError("{} relation not found".format(predicate_name))
        for (relation_name, target_id) in sorted(rels):
            write(print_relation(relation_name, target_id))
    #
    comment = graph.value(synset, WN_COMMENT, any=False)
    if comment:
        write("# " + comment.replace("\n", "\n# "))
    sorted_word_senses = sort_word_senses(graph, synset)
    for word_sense in sorted_word_senses:
        print_word_sense(graph, word_sense, lexicographer_file, synset, write)
    # definition
    definition = graph.value(synset, WN_DEFINITION, any=False)
    if definition:
        write("{}: {}".format(SYNSET_RELATIONS[DEFINITION], definition))
    else:
        raise LookupError("No definition for synset")
    # examples
    examples = graph.objects(synset, WN_EXAMPLE)
    sorted_examples = sorted(examples)
    for example in sorted_examples:
        write("{}: {}".format(SYNSET_RELATIONS[EXAMPLE], example))
    # frames
    frame_ids = list(graph.objects(synset, WN_FRAME))
    sorted_frame_ids = sorted(frame_ids)
    if frame_ids:
        write("{}: {}".format(SYNSET_RELATIONS[FRAME], " ".join(sorted_frame_ids)))
    print_relations()
    write("")

def print_word_sense(graph, word_sense, lexicographer_file, synset, write):
    def print_word_relations():
        def print_word_relation(name, wordsense_id):
            return " {} {}".format(name,
                                   print_word_sense_id(wordsense_id, lexicographer_file))
        frames = []
        relations = []
        markers = []
        for predicate, obj in graph.predicate_objects(word_sense):
            _, predicate_name = r.namespace.split_uri(predicate)
            predicate_txt_name = WORD_RELATIONS.get(predicate_name, None)
            if predicate_name == FRAME:
                frames.append(obj)
            elif predicate_name == SYNTACTIC_MARKER:
                markers.append(obj)
            elif predicate_txt_name:
                synset = graph.value(predicate=WN_CONTAINS_WORDSENSE, object=obj,
                                     any=False)
                if synset:
                    # obj is wordsense too
                    target_lexfile = graph.value(synset, WN_LEXICOGRAPHER_FILE)
                    relations.append((predicate_txt_name,
                                      word_sense_id(graph, target_lexfile, obj)))
                # else:
                #     raise LookupError("relation {} target {} not wordsense".format(predicate_name, obj))
        if frames:
            write(" {} {}".format(
                WORD_RELATIONS[FRAME], " ".join(sorted(frames))), end="")
        if markers:
            [marker] = markers  # check that there is only one marker
            write(" {} {}".format(
                WORD_RELATIONS[SYNTACTIC_MARKER], marker), end="")
        for relation_name, target in sorted(relations):
            write(print_word_relation(relation_name, target), end="")
        write("")
    #
    sense_id = word_sense_id(graph, lexicographer_file, word_sense, synset)
    lexicographer_file = sense_id[1]
    lexical_form_id = print_word_sense_id(sense_id, lexicographer_file)
    write("{}: {}".format(SYNSET_RELATIONS[CONTAINS_WORDSENSE], lexical_form_id),
          end="")
    print_word_relations()


def rdf2text_go(graph, config_dir, output_dir):
    global SYNSET_RELATIONS, WORD_RELATIONS
    (SYNSET_RELATIONS, WORD_RELATIONS) = read_config(config_dir)
    print_graph(graph, output_dir)
    return None


###
## json -> rdf
def from_json(json_input):
    for line in json_input:
        yield json.loads(line)


def to_graph(synsets_gen, release=False):
    from urllib.parse import quote

    def make_id(wn_name, id_str, obj=SYNSET):
        return WN30_LANG[wn_name][quote("{}-{}".format(obj, id_str))]

    def parse_id_wn_name(id_str):
        # [] this is fragile
        components = id_str.split("-")
        return components[0]

    def add_relation(head, relation, obj=SYNSET):
        target_id_str = relation["targetId"]
        target_wn = parse_id_wn_name(target_id_str)
        target_id = make_id(target_wn, target_id_str, obj)
        relation_name = WN30[relation[NAME]]
        if release or obj == WSENSE:
            g.add((head, relation_name, target_id))
        else:
            bnode = BNode()
            g.add((head, relation_name, bnode))
            g.add((bnode, WN_TARGET, target_id))
            target_lexical_form = Literal(relation[TARGET_LEXICAL_FORM])
            g.add((bnode, WN_TARGET_LEXICAL_FORM, target_lexical_form))

    def add_frame(head, frame):
        g.add((head, WN_FRAME, Literal(frame)))

    def add_word_sense(wn_name, wordsense, lexicographer_file, synset_id):
        wordsense_id = make_id(wn_name, wordsense[ID], WSENSE)
        g.add((synset_id, WN30[CONTAINS_WORDSENSE], wordsense_id))
        g.add((wordsense_id, WN30[LEXICAL_ID], Literal(wordsense[LEXICAL_ID])))
        g.add((wordsense_id, WN30[LEXICAL_FORM], Literal(wordsense[LEXICAL_FORM])))
        syntacticMarker = wordsense.get(SYNTACTIC_MARKER)
        if syntacticMarker:
            g.add((wordsense_id, WN_SYNTACTIC_MARKER, Literal(syntacticMarker)))
        for relation in wordsense.get(POINTERS, []):
            add_relation(wordsense_id, relation, WSENSE)
        frames = wordsense.get(FRAMES, [])
        for frame in frames:
            add_frame(wordsense_id, frame)

    def add_synset(synset):
        wn_name = synset["wn"]
        synset_id = make_id(wn_name, synset[ID], SYNSET)
        lexicographer_file = Literal(synset[LEXICOGRAPHER_FILE])
        synset_type = SYNSETTYPE[synset["pos"]]
        g.add((synset_id, WN30[LANG], Literal(wn_name)))
        g.add((synset_id, RDF_TYPE, synset_type))
        g.add((synset_id, WN30[LEXICOGRAPHER_FILE], lexicographer_file))
        g.add((synset_id, WN30[DEFINITION], Literal(synset[DEFINITION])))
        if not release:
            [begin, end] = synset[POSITION]
            g.add((synset_id, WN30[SOURCE_BEGIN], Literal(begin)))
            g.add((synset_id, WN30[SOURCE_END], Literal(end)))
            comments = synset.get(COMMENTS)
            if comments:
                comment = "\n".join(comments)
                g.add((synset_id, WN30[COMMENT], Literal(comment)))
        for example in synset.get(EXAMPLES, []):
            g.add((synset_id, WN30[EXAMPLE], Literal(example)))
        for wordsense in synset[WORDSENSES]:
            add_word_sense(wn_name, wordsense, lexicographer_file, synset_id)
        for relation in synset.get(RELATIONS, []):
            add_relation(synset_id, relation, SYNSET)
        frames = synset.get(FRAMES, [])
        for frame in frames:
            add_frame(synset_id, frame)
    #
    g = Graph()
    for synset in synsets_gen:
        add_synset(synset)
    return g


###
## CLI
@click.group()
def cli():
    return None

@cli.command()
@click.argument('json_input', type=click.File(mode="r"), required=True)
@click.argument('rdf_output', type=click.File(mode="wb"), required=True)
@click.option('-f', '--rdf-format', 'rdf_format',
              type=click.STRING, default='nt', show_default=True,
              help="RDF output format. Must be accepted by RDFlib.")
@click.option('-r', '--release', 'release',
              is_flag=True, default=False, show_default=True,
              help="output release RDF, which is cleaner but does not allow re-serialization to text")
def json2rdf(json_input, rdf_output, rdf_format, release):
    """Convert JSON_INPUT to RDF_OUTPUT."""
    graph = to_graph(from_json(json_input), release)
    graph.serialize(destination=rdf_output, format=rdf_format)


@cli.command()
@click.argument('rdf_input',
                type=click.File(mode="rb"), required=True)
@click.argument('config_dir',
                type=click.Path(exists=True, file_okay=False,
                                resolve_path=True), required=True)
@click.argument('output_dir',
                type=click.Path(file_okay=False, resolve_path=True,
                                writable=True), required=True)
@click.option('-f', '--rdf-file-format', 'rdf_file_format',
              type=click.STRING, default='nt', show_default=True,
              help="RDF input format. Must be accepted by RDFlib.")
def rdf2text(rdf_input, config_dir, output_dir, rdf_file_format="nt"):
    """Convert RDF_INPUT to lexicographer files placed at OUTPUT_DIR,
according to the configuration files in CONFIG_DIR."""
    graph = Graph()
    graph.parse(rdf_input, format=rdf_file_format)
    rdf2text_go(graph, config_dir, output_dir)


@cli.command()
@click.argument('json_input',
                type=click.File(mode="rb"), required=True)
@click.argument('config_dir',
                type=click.Path(exists=True, file_okay=False, resolve_path=True),
                required=True)
@click.argument('output_dir',
                type=click.Path(file_okay=False, resolve_path=True, writable=True),
                required=True)
def json2text(json_input, config_dir, output_dir):
    """Convert JSON_INPUT to lexicographer files placed at OUTPUT_DIR,
according to the configuration files in CONFIG_DIR."""
    graph = to_graph(from_json(json_input))
    rdf2text_go(graph, config_dir, output_dir)


if __name__ == '__main__':
    cli()
