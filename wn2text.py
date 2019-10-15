#!/bin/python3

import json
import os
import rdflib as r
from rdflib import Graph, Namespace
from rdflib.namespace import RDF
from rdflib.term import Literal
import click

WN30 = Namespace("https://w3id.org/own-pt/wn30/schema/")
WN30EN = Namespace("https://w3id.org/own-pt/wn30-en/instances/")
WN30PT = Namespace("https://w3id.org/own-pt/wn30-pt/instances/") # not used (yet)

CURRENT_LANG=Literal("en")

SYNSET_RELATIONS, WORD_RELATIONS, FRAMES_TO_ID = {}, {}, {}

LEXICOGRAPHER_FILE = WN30['lexicographerFile']
LANG = WN30['lang']
CONTAINS_WORDSENSE = WN30['containsWordSense']
SAME_AS = WN30['sameAs']
LEXICAL_ID = WN30['lexicalId']
LEXICAL_FORM = WN30['lexicalForm']
WORD = WN30['word']

@click.group()
def cli():
    return None

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
def to_text(rdf_input, config_dir, output_dir, rdf_file_format="nt"):
    """Convert RDF_INPUT to lexicographer files placed at OUTPUT_DIR,
according to the configuration files in CONFIG_DIR."""
    global SYNSET_RELATIONS, WORD_RELATIONS, FRAMES_TO_ID
    (SYNSET_RELATIONS, WORD_RELATIONS, FRAMES_TO_ID) = read_config(config_dir)
    graph = Graph()
    graph.parse(rdf_input, format=rdf_file_format)
    print_graph(graph, output_dir)

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

    def read_frames(res, fields):
        res[fields[1]] = fields[0]
        return res

    synset_relations, word_relations = read_tsv(os.path.join(
        config_dir, "relations.tsv"), read_relations, ({}, {}))
    frames = read_tsv(os.path.join(config_dir, "frames.tsv"),
                      read_frames, {})
    return (synset_relations, word_relations, frames)


def print_graph(graph, output_dir):
    global CURRENT_LANG
    lex_files = set(graph.objects(predicate=LEXICOGRAPHER_FILE))
    langs     = set(graph.objects(predicate=LANG))
    if not langs:
        langs = [CURRENT_LANG]
        for synset in graph.subjects(CONTAINS_WORDSENSE):
            graph.add((synset, LANG, CURRENT_LANG))
    for lang in langs:
        CURRENT_LANG = lang
        for lexicographer_file in lex_files:
            print_lexfile(graph, lexicographer_file, output_dir)


def sort_word_senses(graph, synset):
    def word_sense_form(ws):
        word = graph.value(ws, WORD)
        lexical_form = graph.value(word, LEXICAL_FORM)
        return lexical_form
    #
    wordsenses = list(graph.objects(synset, CONTAINS_WORDSENSE))
    if wordsenses:
        return sorted(wordsenses,key=word_sense_form)
    else:
        print("missing ws for {}".format(synset))


def sort_synsets(graph, synsets):
    # careful when changing this, other scripts depend on this function
    synsets_word_senses = map(lambda ss: (ss, sort_word_senses(graph, ss)), synsets)
    result = sorted(synsets_word_senses, key=lambda i: word_sense_id(graph, "", i[1][0]))
    return result


def print_lexfile(graph, lexicographer_file, output_dir):
    lexfile_synsets = graph.subjects(predicate=LEXICOGRAPHER_FILE, object=lexicographer_file)
    lang_synsets = list(filter(lambda s: graph.value(s,LANG) == CURRENT_LANG,
                               lexfile_synsets))
    if lang_synsets:
        with open(os.path.join(output_dir, CURRENT_LANG, lexicographer_file), 'w+') as output_stream:
            write = lambda data, *args, **kwargs: print(data, file=output_stream,
                                                        *args, **kwargs)
            pos, lexname = lexicographer_file.split(".")
            write("{}.{}".format(pos, lexname), end="\n\n")
            for synset, sorted_word_senses in sort_synsets(graph, lang_synsets):
                print_synset(graph, synset, sorted_word_senses, lexicographer_file, write)


def word_sense_id(graph, lexicographer_file, word_sense):
    word = graph.value(word_sense, WORD)
    word_form = graph.value(word, LEXICAL_FORM)
    lexical_id = graph.value(word_sense, LEXICAL_ID)
    in_synset = graph.value(
        predicate=CONTAINS_WORDSENSE, object=word_sense)
    in_lang = graph.value(in_synset, LANG)
    in_lexfile = graph.value(
        subject=in_synset, predicate=LEXICOGRAPHER_FILE)
    if None not in (word, word_form, lexical_id, in_synset, in_lexfile, in_lang):
        return (in_lang, in_lexfile, word_form, int(lexical_id))
    else:
        raise LookupError("Error: missing wordsense information for wordsense {}.\n word_form: {}, lexical_id: {}, in_synset: {}, in_lexfile: {}".format(word_sense, word_form, lexical_id, in_synset, in_lexfile))


def print_word_sense_id(wordsense_id, lexicographer_file=None):
    (in_lang, in_lexfile, word_form, lexical_id) = wordsense_id
    lexfile_str = "{}:".format(in_lexfile) if in_lexfile.neq(lexicographer_file) else ""
    prefix = "{}".format(lexfile_str) if in_lang == CURRENT_LANG else "@{}:{}:".format(in_lang, in_lexfile)
    return "{}{}{}".format(prefix,
                             word_form,
                             " {}".format(lexical_id) if lexical_id != 0 else "")


def print_synset(graph, synset, sorted_word_senses, lexicographer_file, write):
    def print_relations():
        def print_relation(name, wordsense_id):
            return "{}: {}".format(name,
                                   print_word_sense_id(wordsense_id, lexicographer_file))
        rels = []
        for predicate, obj in graph.predicate_objects(synset):
            _, predicate_name = r.namespace.split_uri(predicate)
            predicate_txt_name = SYNSET_RELATIONS.get(predicate_name, None)
            if predicate_name in ["frame", "containsWordSense", "gloss",
                                  "example", "lexicographerFile", "lexicalForm"]:
                pass
            elif predicate_txt_name:
                rels.append((predicate_txt_name,
                             word_sense_id(graph, lexicographer_file,
                                           # first word sense is head
                                           sort_word_senses(graph, obj)[0])))
            # # we can't raise this error because we decided to ignore lots of relations
            # else:
            #     raise LookupError("{} relation not found".format(predicate_name))
        rels.sort()
        for (relation_name, target) in rels:
            write(print_relation(relation_name, target))

    def print_synset_gloss_splited_in_definition_and_examples(gloss):
        def remove_quotes(example):
            if example[-1] == "\"" and "\"" not in example[:-1]:
                return example.strip("\"")
            else:
                return "\"" + example

        def_examples = gloss.split("; \"")
        definition = def_examples[0].strip()
        examples = def_examples[1:]
        write("{}: {}".format(SYNSET_RELATIONS["definition"], definition))
        for example in examples:
            write("{}: {}".format(
                SYNSET_RELATIONS["example"], remove_quotes(example.strip())))

    for word_sense in sorted_word_senses:
        print_word_sense(graph, word_sense, lexicographer_file, write)
    # definition
    print_synset_gloss_splited_in_definition_and_examples(
        graph.value(synset, WN30["gloss"]))
    # examples
    for example in graph.objects(synset, WN30["example"]):
        write("{}: {}".format(SYNSET_RELATIONS["example"], example))
    # frames
    frames = graph.objects(synset, WN30["frame"])
    frame_ids = list(map(lambda frame: FRAMES_TO_ID[frame.n3()[1:-1]], frames))
    if frame_ids:
        write("{}: {}".format(SYNSET_RELATIONS['frame'], " ".join(frame_ids)))
    print_relations()
    write("")


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
                frames.append(FRAMES_TO_ID[str(obj)])
            elif predicate_name == "syntacticMarker":
                markers.append(obj)
            elif predicate_txt_name:
                relations.append((predicate_txt_name,
                                  word_sense_id(graph, lexicographer_file, obj)))
            # # we can't raise this error because we decided to ignore lots of relations
            # else:
            #     raise lookuperror("{} not found".format(predicate_name))
        if frames:
            frames.sort()
            write(" {} {}".format(
                WORD_RELATIONS["frame"], " ".join(frames)), end="")
        if markers:
            [marker] = markers  # check that there is only one marker
            write(" {} {}".format(
                WORD_RELATIONS["syntacticMarker"], marker), end="")
        relations.sort()
        for relation_name, target in relations:
            write(print_word_relation(relation_name, target), end="")
        write("")

    write("{}: {}".format(SYNSET_RELATIONS['containsWordSense'],
                          print_word_sense_id(word_sense_id(graph,
                                                            lexicographer_file,
                                                            word_sense),
                                              lexicographer_file)),
          end="")
    print_word_relations()

@cli.command()
@click.argument('original_file', type=click.File(mode="rb"), required=True)
@click.argument('new_file', type=click.File(mode="rb"), required=True)
@click.option('-f', '--rdf-format', 'rdf_format'
              , type=click.STRING, default='nt', show_default=True,
              help="RDF output format. Must be accepted by RDFlib.")
def check_conversion(original_file, new_file, rdf_format):
    # check relations between synsets and wordsenses are preserved
    ## not checking lexform and lexical ids and other literals because
    ## missing these would have caused syntactic problems (plus we
    ## don't export syntacticMarker yet, for instance)
    def new_uri(lexicographer_file, original_uri, wn_obj):
        if wn_obj == "synset":
            sorted_wordsenses = sort_word_senses(original_g, original_uri)
            wordsense = sorted_wordsenses[0]
        elif wn_obj == "wordsense":
            wordsense = original_uri
        else:
            raise Exception("argument wn_obj must be either synset or wordsense")
        word = original_g.value(wordsense, WORD)
        lexical_form = original_g.value(word, LEXICAL_FORM)
        lexical_id = original_g.value(wordsense, LEXICAL_ID)
        return WN30EN["{}-{}-{}-{}".format(wn_obj, lexicographer_file,
                                           lexical_form, lexical_id)]
    #
    original_g = Graph()
    new_g = Graph()
    original_g.parse(original_file, format=rdf_format)
    new_g.parse(new_file, format=rdf_format)
    for (original_en_synset, subj_lexfile) in original_g.subject_objects(LEXICOGRAPHER_FILE): # for every synset
        for (predicate, obj) in original_g.predicate_objects(original_en_synset): # for every relation
            obj_lexfile = original_g.value(obj, LEXICOGRAPHER_FILE, any=False)
            if predicate == CONTAINS_WORDSENSE:
                original_en_wordsense = obj
                for (predicate, obj) in original_g.predicate_objects(original_en_wordsense):
                    obj_synset = original_g.value(predicate=CONTAINS_WORDSENSE,object=obj)
                    if obj_synset: # if truthy obj is also a wordsense
                        obj_lexfile = original_g.value(obj_synset, LEXICOGRAPHER_FILE)
                        new_en_wordsense = new_uri(subj_lexfile, original_en_wordsense, "wordsense")
                        new_en_obj = new_uri(obj_lexfile, obj, "wordsense")
                        if (new_en_wordsense, predicate, new_en_obj) not in new_g:
                            print("wordsense relation {} missing between {} and {}".format(predicate, new_en_wordsense, new_en_obj))
            elif obj_lexfile: # truthy if object is synset too
                new_en_synset = new_uri(subj_lexfile, original_en_synset, "synset")
                new_en_obj = new_uri(obj_lexfile, obj, "synset")
                if (new_en_synset, predicate, new_en_obj) not in new_g:
                    print("synset relation {} missing between {} and {}".format(predicate, new_en_synset, new_en_obj))
                new_pt_synset = new_g.value(predicate=SAME_AS, object=new_en_synset)
                new_pt_obj = new_g.value(predicate=SAME_AS, object=new_en_obj)
                if (new_pt_synset, predicate, new_pt_obj) not in new_g:
                    print("synset relation {} missing between {} and {}".format(predicate, new_pt_synset, new_pt_obj))


if __name__ == '__main__':
    cli()
