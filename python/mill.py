#!/bin/python3

import json
import os
import rdflib as r
from rdflib import Graph, Namespace
from rdflib.namespace import RDF
from rdflib.term import Literal
import click

###
## constants

# [] RDF configuration -> customize IRIs
## [] remove hardcoded stuff
# [] read configuration using labels, not harcoded field indices

WN30 = Namespace("https://w3id.org/own-pt/wn30/schema/")
WN30EN = Namespace("https://w3id.org/own-pt/wn30-en/instances/")
WN30PT = Namespace("https://w3id.org/own-pt/wn30-pt/instances/") # not used (yet)

WN30_LANG = {"en": WN30EN, "pt": WN30PT}

POS1TOLONG = {"A" : "adj", "N" : "noun", "R" : "adv", "S" : "adjs", "V" : "verb"}
SYNSETTYPE = {"A" : Literal("AdjectiveSynset"), "N" :
              Literal("NounSynset"), "R" : Literal("AdverbSynset"),
              "S" : Literal("AdjectiveSatelliteSynset"), "V" :
              Literal("VerbSynset")}

COMMENT            = "comment"
COMMENTS           = "comments"
DEFINITION         = "definition"
EXAMPLE            = "example"
LANG               = "lang"
LEXICOGRAPHER_FILE = "lexicographerFile"
ID                 = "id"
EXAMPLES           = "examples"
WORDSENSES         = "wordsenses"
CONTAINS_WORDSENSE = "containsWordSense"
LEXICAL_FORM       = "lexicalForm"
LEXICAL_ID         = "lexicalId"
POINTERS           = "pointers"
POSITION           = "position"
NAME               = "name"
SENSEKEY           = "senseKey"
FRAME              = "frame"
FRAMES             = "frames"
WORDSENSE          = "wordsense"
SYNSET             = "synset"
RELATIONS          = "relations"
SOURCE_BEGIN       = "sourceBegin"
SOURCE_END         = "sourceEnd"
SAME_AS            = "sameAs"
SYNTACTIC_MARKER   = "syntacticMarker"

CURRENT_LANG=None # initialized during runtime

SYNSET_RELATIONS, WORD_RELATIONS = {}, {}

WN_FRAME = WN30[FRAME]
WN_LEXICOGRAPHER_FILE = WN30[LEXICOGRAPHER_FILE]
WN_LANG = WN30[LANG]
WN_CONTAINS_WORDSENSE = WN30[CONTAINS_WORDSENSE]
WN_SAME_AS = WN30[SAME_AS]
WN_SOURCE_BEGIN = WN30[SOURCE_BEGIN]
WN_LEXICAL_ID = WN30[LEXICAL_ID]
WN_LEXICAL_FORM = WN30[LEXICAL_FORM]
WN_DEFINITION = WN30[DEFINITION]
WN_COMMENT = WN30[COMMENT]
WN_PERTAINS_TO = WN30["pertainsTo"]
WN_HYPONYM_OF = WN30["hyponymOf"]
WN_INSTANCE_OF = WN30["instanceOf"]
WN_ENTAILS = WN30["entails"]
WN_ANTONYM_OF = WN30["antonymOf"]
WN_SIMILAR_TO = WN30["similarTo"]
WN_SYNTACTIC_MARKER = WN30[SYNTACTIC_MARKER]
WN_EXAMPLE = WN30[EXAMPLE]

PREFERENTIAL_RELATION_MAP = {"AdjectiveSatelliteSynset": [WN_SIMILAR_TO],
                             "AdjectiveSynset": [WN_ANTONYM_OF,
                                                 WN_PERTAINS_TO,
                                                 WN_SIMILAR_TO],
                             "NounSynset": [WN_INSTANCE_OF, WN_HYPONYM_OF],
                             "VerbSynset": [WN_HYPONYM_OF, WN_ENTAILS],
                             "AdverbSynset": [WN_PERTAINS_TO]}

INDISTINGUISHABLES = 0

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


def print_graph(graph, output_dir):
    global CURRENT_LANG
    lex_files = set(graph.objects(predicate=WN_LEXICOGRAPHER_FILE))
    langs     = set(graph.objects(predicate=WN_LANG))
    for lang in langs:
        CURRENT_LANG = lang
        output_dir = os.path.join(output_dir, lang)
        os.makedirs(output_dir, exist_ok=True)
        for lexicographer_file in lex_files:
            print_lexfile(graph, lexicographer_file, output_dir)


def sort_word_senses(graph, synset):
    def word_sense_form(ws):
        return graph.value(ws, WN_LEXICAL_FORM, any=False)
    #
    wordsenses = list(graph.objects(synset, WN_CONTAINS_WORDSENSE))
    if wordsenses:
        return sorted(wordsenses, key=word_sense_form)
    else:
        print("missing ws for {}".format(synset))


def sort_synsets(graph, synsets):
    result = sorted(synsets, key=lambda s: graph.value(s, WN_SOURCE_BEGIN))
    return result


def print_lexfile(graph, lexicographer_file, output_dir):
    lexfile_synsets = graph.subjects(predicate=WN_LEXICOGRAPHER_FILE, object=lexicographer_file)
    lang_synsets = list(filter(lambda s: graph.value(s, WN_LANG) == CURRENT_LANG,
                               lexfile_synsets))
    if lang_synsets:
        with open(os.path.join(output_dir, lexicographer_file), 'w+') as output_stream:
            write = lambda data, *args, **kwargs: print(data, file=output_stream,
                                                        *args, **kwargs)
            pos, lexname = lexicographer_file.split(".")
            write("{}.{}".format(pos, lexname), end="\n\n")
            for synset in sort_synsets(graph, lang_synsets):
                print_synset(graph, synset, lexicographer_file, write)


###
## new id scheme
def get_clashes(graph, lexicographer_file, synset, word_sense):
    # returns synsets in same lexicographer file with a word sense
    # with the same lexical form
    clashes = []
    lexical_form = graph.value(word_sense, WN_LEXICAL_FORM)
    for other_word_sense in graph.subjects(WN_LEXICAL_FORM, lexical_form):
        other_synset = graph.value(predicate=WN_CONTAINS_WORDSENSE,
                                   object=other_word_sense)
        if synset != other_synset:
            other_lexfile = graph.value(other_synset, WN_LEXICOGRAPHER_FILE)
            if other_lexfile == lexicographer_file:
                clashes.append(other_synset)
    return clashes

WN_ID_RELATION = WN30['idRelation']
WN_ID_TARGET = WN30['idTarget']
WN_ID_WORDSENSE = WN30['idWordSense']

def pick_word_sense_relation_id(graph, lexicographer_file, word_sense, synset):
    def is_clash(relation, target, objtype):
        clashed = False
        if objtype == "synset":
            for other_synset in clashes:
                for other_target in graph.objects(other_synset, relation):
                    if target == other_target:
                        clashed = True
                        break
        elif objtype == "wordsense":
            for other_synset in clashes:
                for other_word_sense in graph.objects(other_synset, WN_CONTAINS_WORDSENSE):
                    for other_target in graph.objects(other_word_sense, relation):
                        if target == other_target:
                            clashed = True
                            break
        else:
            raise Exception("objtype must be 'synset' or 'wordsense'")
        return clashed
    #
    def find_any_relation():
        global INDISTINGUISHABLES
        id_relation = None
        for relation, obj in graph.predicate_objects(synset):
            if graph.value(obj, WN_CONTAINS_WORDSENSE): # obj is synset too
                if not is_clash(relation, obj, "synset"):
                    id_relation = (relation, obj)
                    break
        if id_relation and None not in id_relation:
            return id_relation
        else:
            for relation, obj in graph.predicate_objects(word_sense):
                if graph.value(predicate=WN_CONTAINS_WORDSENSE, object=obj):
                    # obj is wordsense too
                    if not is_clash(relation, obj, "wordsense"):
                        id_relation = (relation, obj)
                        break
            if id_relation and None not in id_relation:
                return id_relation
            else:
                # [] return something here
                INDISTINGUISHABLES += 1
                #print("no identifying relation found for {}".format(word_sense))
    #
    def find_id_relation():
        id_relation = None
        if clashes:
            synset_type = graph.value(synset, RDF["type"])
            preferential_relations = PREFERENTIAL_RELATION_MAP[str(synset_type)]
            for relation in preferential_relations:
                target = graph.value(synset, relation)
                if target and not is_clash(relation, target, "synset"):
                    id_relation = (relation, target)
                    break
            if id_relation and None not in id_relation:
                return id_relation
            else:
                return find_any_relation()
        else:
            return None
    #
    clashes = get_clashes(graph, lexicographer_file, synset, word_sense)
    return find_id_relation()

def pick_synset_ids(graph, lexicographer_file, synset):
    wordsense_as_id = None
    for word_sense in graph.objects(synset, WN_CONTAINS_WORDSENSE):
        word_sense_id = pick_word_sense_relation_id(graph, lexicographer_file, word_sense, synset)
        if word_sense_id:
            (id_relation, id_target) = word_sense_id
            graph.set((word_sense, WN_ID_RELATION, id_relation))
            graph.set((word_sense, WN_ID_TARGET, id_target))
        else:
            wordsense_as_id = word_sense
    if not wordsense_as_id:
        wordsense_as_id = graph.value(synset, WN_CONTAINS_WORDSENSE)
    graph.set((synset, WN_ID_WORDSENSE, wordsense_as_id))
    return None

def pick_wn_ids(graph):
    lex_files = set(graph.objects(predicate=WN_LEXICOGRAPHER_FILE))
    for lex_file in lex_files:
        for synset in graph.subjects(WN_LEXICOGRAPHER_FILE, lex_file):
            pick_synset_ids(graph, lex_file, synset)
    
###

def word_sense_id(graph, lexicographer_file, word_sense, synset=None):
    def get_id_relation():
        id_relation = graph.value(word_sense, WN_ID_RELATION)
        id_target = graph.value(word_sense, WN_ID_TARGET)
        if id_relation and id_target:
            _, name = r.namespace.split_uri(id_relation)
            id_relation_txt_name = SYNSET_RELATIONS.get(name, False) or WORD_RELATIONS.get(name)
            target_id_wordsense = graph.value(id_target, WN_ID_WORDSENSE)
            if target_id_wordsense:
                id_target = target_id_wordsense
            target_lexical_form = graph.value(id_target, WN_LEXICAL_FORM)
            return (id_relation_txt_name, target_lexical_form)
        elif id_relation or id_target:
            assert False
        else:
            return None
    synset = synset or graph.value(predicate=WN_CONTAINS_WORDSENSE, object=word_sense)
    lexical_form = graph.value(word_sense, WN_LEXICAL_FORM)
    in_lang = graph.value(synset, WN_LANG)
    assert in_lang, word_sense
    assert lexical_form, word_sense
    return (in_lang, lexicographer_file, lexical_form, get_id_relation())

def synset_id(graph, lexicographer_file, synset):
    word_sense = graph.value(synset, WN_ID_WORDSENSE)
    return word_sense_id(graph, lexicographer_file, word_sense, synset)

def print_word_sense_id(wordsense_id, lexicographer_file=None):
    (in_lang, in_lexfile, lexical_form, id_relation) = wordsense_id
    lexfile_str = "{}:".format(in_lexfile) if in_lexfile.neq(lexicographer_file) else ""
    prefix = "{}".format(lexfile_str) if in_lang == CURRENT_LANG else "@{}:{}:".format(in_lang, in_lexfile)
    id_relation_txt = ""
    if id_relation:
        (predicate, target_lexical_form) = id_relation
        id_relation_txt = " ({} {})".format(predicate, target_lexical_form)
    return "{}{}{}".format(prefix, lexical_form, id_relation_txt)



def print_synset(graph, synset, lexicographer_file, write):
    def print_relations():
        def print_relation(name, wordsense_id):
            return "{}: {}".format(name,
                                   print_word_sense_id(wordsense_id, lexicographer_file))
        rels = []
        for predicate, obj in graph.predicate_objects(synset):
            _, predicate_name = r.namespace.split_uri(predicate)
            predicate_txt_name = SYNSET_RELATIONS.get(predicate_name, None)
            obj_is_synset = graph.value(obj, WN_CONTAINS_WORDSENSE)
            if predicate_txt_name:
                if obj_is_synset:
                    target_lexfile = graph.value(obj, WN_LEXICOGRAPHER_FILE)
                    rels.append((predicate_txt_name,
                                 synset_id(graph, target_lexfile, obj)))
            else:
                if obj_is_synset:
                    raise LookupError("{} relation not found".format(predicate_name))
        rels.sort(key=relation_sort_key)
        for (relation_name, target) in rels:
            write(print_relation(relation_name, target))
    #
    comment = graph.value(synset, WN_COMMENT, any=False)
    if comment:
        write("# " + comment.replace("\n", "\n# "))
    sorted_word_senses = sort_word_senses(graph, synset)
    for word_sense in sorted_word_senses:
        print_word_sense(graph, word_sense, lexicographer_file, write)
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

def relation_sort_key(wordsense_id):
    (relname, (wn_name, lexicographer_file, lexical_form, maybe_id_relation)) = wordsense_id
    id_relation = maybe_id_relation or ("", "")
    res = (relname, wn_name, lexicographer_file, lexical_form, id_relation)
    return res


def print_word_sense(graph, word_sense, lexicographer_file, write):
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
            if predicate_name == "frame":
                frames.append(obj)
            elif predicate_name == "syntacticMarker":
                markers.append(obj)
            elif predicate_txt_name:
                synset = graph.value(predicate=WN_CONTAINS_WORDSENSE, object=obj)
                if synset:
                    # obj is wordsense too
                    target_lexfile = graph.value(synset, WN_LEXICOGRAPHER_FILE)
                    relations.append((predicate_txt_name,
                                      word_sense_id(graph, target_lexfile, obj)))
                else:
                    raise LookupError("relation {} target {} not wordsense".format(predicate_name, obj))
        if frames:
            frames.sort()
            write(" {} {}".format(
                WORD_RELATIONS[FRAME], " ".join(frames)), end="")
        if markers:
            [marker] = markers  # check that there is only one marker
            write(" {} {}".format(
                WORD_RELATIONS[SYNTACTIC_MARKER], marker), end="")
        relations.sort(key=relation_sort_key)
        for relation_name, target in relations:
            write(print_word_relation(relation_name, target), end="")
        write("")
    #
    lexical_form = graph.value(word_sense, WN_LEXICAL_FORM)
    write("{}: {}".format(SYNSET_RELATIONS[CONTAINS_WORDSENSE], lexical_form),
          end="")
    print_word_relations()



###
## json -> rdf
def from_json(json_input):
    for line in json_input:
        yield json.loads(line)

def to_graph(synsets_gen):
    def make_id(lang, lexicographer_file, lexical_form, lexical_id, obj=SYNSET):
        return WN30_LANG[lang]["{}-{}-{}-{}".format(obj, lexicographer_file,
                                                    lexical_form, lexical_id)]

    def parse_id(id_array, obj=SYNSET):
        [lang, pos, lexname, lexical_form, lexical_id] = id_array
        lexicographer_file = "{}.{}".format(POS1TOLONG[pos], lexname)
        obj_id = make_id(lang, lexicographer_file, lexical_form, lexical_id, obj)
        return lang, obj_id, Literal(lexicographer_file), SYNSETTYPE[pos]

    def add_relation(head, relation, obj=SYNSET):
        _, obj_id, _, _ = parse_id(relation[ID], obj)
        g.add((head, WN30[relation[NAME]], obj_id))

    def add_frame(head, frame):
        g.add((head, WN_FRAME, Literal(frame)))

    def add_word_sense(lang, wordsense, lexicographer_file, synset_id):
        wordsense_id = make_id(lang, lexicographer_file, wordsense[LEXICAL_FORM]
                               , wordsense[LEXICAL_ID], WORDSENSE)
        g.add((synset_id, WN30[CONTAINS_WORDSENSE], wordsense_id))
        g.add((wordsense_id, WN30[LEXICAL_ID], Literal(wordsense[LEXICAL_ID])))
        g.add((wordsense_id, WN30[LEXICAL_FORM], Literal(wordsense[LEXICAL_FORM])))
        g.add((wordsense_id, WN30[SENSEKEY], Literal(wordsense[SENSEKEY])))
        syntacticMarker = wordsense.get(SYNTACTIC_MARKER)
        if syntacticMarker:
            g.add((wordsense_id, WN_SYNTACTIC_MARKER, Literal(syntacticMarker)))
        for relation in wordsense[POINTERS]:
            add_relation(wordsense_id, relation, WORDSENSE)
        frames = wordsense.get(FRAMES, [])
        for frame in frames:
            add_frame(wordsense_id, frame)

    def add_synset(synset):
        lang, synset_id, lexicographer_file, synset_type = parse_id(synset[ID])
        g.add((synset_id, WN30[LANG], Literal(lang)))
        g.add((synset_id, RDF.type, synset_type))
        g.add((synset_id, WN30[LEXICOGRAPHER_FILE], lexicographer_file))
        g.add((synset_id, WN30[DEFINITION], Literal(synset[DEFINITION])))
        [begin, end] = synset[POSITION]
        g.add((synset_id, WN30[SOURCE_BEGIN], Literal(begin)))
        g.add((synset_id, WN30[SOURCE_END], Literal(end)))
        comments = synset[COMMENTS]
        if comments:
            comment = "\n".join(comments)
            g.add((synset_id, WN30[COMMENT], Literal(comment)))
        for example in synset[EXAMPLES]:
            g.add((synset_id, WN30[EXAMPLE], Literal(example)))
        for wordsense in synset[WORDSENSES]:
            add_word_sense(lang, wordsense, lexicographer_file, synset_id),
        for relation in synset[RELATIONS]:
            add_relation(synset_id, relation)
        frames = wordsense.get(FRAMES, [])
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
@click.option('-f', '--rdf-format', 'rdf_format'
              , type=click.STRING, default='nt', show_default=True,
              help="RDF output format. Must be accepted by RDFlib.")
def json2rdf(json_input, rdf_output, rdf_format='nt'):
    """Convert JSON_INPUT to RDF_OUTPUT."""
    graph = to_graph(from_json(json_input))
    graph.serialize(destination=rdf_output, format=rdf_format)


@cli.command()
@click.argument('rdf_input',
                type=click.File(mode="rb"), required=True)
@click.argument('config_dir',
                type=click.Path(exists=True, file_okay=False, resolve_path=True), required=True)
@click.argument('output_dir'
                , type=click.Path(file_okay=False, resolve_path=True, writable=True), required=True)
@click.option('-f', '--rdf-file-format', 'rdf_file_format'
              , type=click.STRING, default='nt', show_default=True,
              help="RDF input format. Must be accepted by RDFlib.")
def rdf2text(rdf_input, config_dir, output_dir, rdf_file_format="nt"):
    """Convert RDF_INPUT to lexicographer files placed at OUTPUT_DIR,
according to the configuration files in CONFIG_DIR."""
    global SYNSET_RELATIONS, WORD_RELATIONS
    (SYNSET_RELATIONS, WORD_RELATIONS) = read_config(config_dir)
    graph = Graph()
    graph.parse(rdf_input, format=rdf_file_format)
    print_graph(graph, output_dir)

if __name__ == '__main__':
    cli()
