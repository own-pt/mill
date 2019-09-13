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


@click.group()
def cli():
    return None


@cli.command()
@click.argument('rdf_input',
                type=click.File(mode="r"), required=True)
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
    (synset_relations, word_relations, frames_to_id) = read_config(config_dir)
    graph = Graph()
    graph.parse(rdf_input, format=rdf_file_format)
    print_graph(graph, synset_relations, word_relations,
                frames_to_id, output_dir)

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
        relation_name = fields[0]
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


def print_graph(graph, synset_relations, word_relations, frames_to_id, output_dir):
    query_results = graph.query(
        """SELECT DISTINCT ?lexFile
       WHERE {
          ?_ wn30:lexicographerFile ?lexFile .
       }""", initNs={'wn30': WN30})
    for (lexicographer_file,) in query_results:
        print_lexfile(graph, lexicographer_file, synset_relations,
                      word_relations, frames_to_id, output_dir)


def sort_word_senses(graph, synset):
    return sorted(graph.objects(synset, WN30["containsWordSense"]),
                  key=lambda ws: graph.value(graph.value(ws, WN30["word"]),
                                             WN30["lexicalForm"]))


def sort_synsets(graph, synsets):
    synsets_word_senses = map(lambda ss: (
        ss, sort_word_senses(graph, ss)), synsets)
    return sorted(synsets_word_senses, key=lambda i: word_sense_id(graph, "", i[1][0]))


def print_lexfile(graph, lexicographer_file, synset_relations,
                  word_relations, frames_to_id, output_dir):
    with open(os.path.join(output_dir, lexicographer_file), 'w') as output_stream:
        write = lambda data, *args, **kwargs: print(data, file=output_stream,
                                                    *args, **kwargs)
        pos, lexname = lexicographer_file.split(".")
        write("{}.{}".format(pos, lexname), end="\n\n")
        synsets = graph.subjects(WN30['lexicographerFile'], lexicographer_file)
        for synset, sorted_word_senses in sort_synsets(graph, synsets):
            print_synset(graph, synset, sorted_word_senses, lexicographer_file,
                         synset_relations, word_relations, frames_to_id, write)


def word_sense_id(graph, lexicographer_file, word_sense):
    word = graph.value(word_sense, WN30["word"])
    word_form = graph.value(word, WN30["lexicalForm"])
    lexical_id = graph.value(word_sense, WN30["lexicalId"])
    in_synset = graph.value(
        predicate=WN30["containsWordSense"], object=word_sense)
    in_lexfile = graph.value(
        subject=in_synset, predicate=WN30["lexicographerFile"])
    if None not in (word, word_form, lexical_id, in_synset, in_lexfile):
        return (in_lexfile, word_form, int(lexical_id))
    else:
        raise LookupError("Error: missing wordsense information for wordsense {}.\n word_form: {}, lexical_id: {}, in_synset: {}, in_lexfile: {}".format(word_sense, word_form, lexical_id, in_synset, in_lexfile))


def print_word_sense_id(wordsense_id, lexicographer_file=None):
    (in_lexfile, word_form, lexical_id) = wordsense_id
    return "{}{}{}".format("{}:".format(in_lexfile)
                           if in_lexfile.neq(lexicographer_file)
                           else "",
                           word_form,
                           " {}".format(lexical_id) if lexical_id != 0 else "")


def print_synset(graph, synset, sorted_word_senses, lexicographer_file, synset_relations,
                 word_relations, frames_to_id, write):
    def print_relations():
        def print_relation(name, wordsense_id):
            return "{}: {}".format(name,
                                   print_word_sense_id(wordsense_id, lexicographer_file))
        rels = []
        for predicate, obj in graph.predicate_objects(synset):
            _, predicate_name = r.namespace.split_uri(predicate)
            predicate_txt_name = synset_relations.get(predicate_name, None)
            if predicate_name in ["frame", "containsWordSense", "gloss",
                                  "example", "lexicographerFile", "lexicalForm"]:
                pass
            elif predicate_txt_name:
                rels.append((predicate_txt_name,
                             word_sense_id(graph, lexicographer_file,
                                           # first word sense is head
                                           sort_word_senses(graph, obj)[0])))
        rels.sort()
        for (relation_name, target) in rels:
            write(print_relation(relation_name, target))

    def print_synset_gloss_splited_in_defintion_and_examples(gloss):
        def remove_quotes(example):
            if example[-1] == "\"" and "\"" not in example[:-1]:
                return example.strip("\"")
            else:
                return "\"" + example

        def_examples = gloss.split("; \"")
        definition = def_examples[0].strip()
        examples = def_examples[1:]
        write("{}: {}".format(synset_relations["definition"], definition))
        for example in examples:
            write("{}: {}".format(
                synset_relations["example"], remove_quotes(example.strip())))

    for word_sense in sorted_word_senses:
        print_word_sense(graph, word_sense, lexicographer_file,
                         word_relations, synset_relations, frames_to_id, write)
    # definition
    print_synset_gloss_splited_in_defintion_and_examples(
        graph.value(synset, WN30["gloss"]))
    # examples
    for example in graph.objects(synset, WN30["example"]):
        write("{}: {}".format(synset_relations["example"], example))
    # frames
    frames = graph.objects(synset, WN30["frame"])
    frame_ids = list(map(lambda frame: frames_to_id[frame.n3()[1:-1]], frames))
    if frame_ids:
        write("{}: {}".format(synset_relations['frame'], " ".join(frame_ids)))
    print_relations()
    write("")


def print_word_sense(graph, word_sense, lexicographer_file,
                     word_relations, synset_relations, frames_to_id, write):
    def print_word_relations():
        def print_word_relation(name, wordsense_id):
            return " {} {}".format(name,
                                   print_word_sense_id(wordsense_id, lexicographer_file))
        frames = []
        relations = []
        markers = []
        for predicate, obj in graph.predicate_objects(word_sense):
            _, predicate_name = r.namespace.split_uri(predicate)
            predicate_txt_name = word_relations.get(predicate_name, None)
            if predicate_name == "frame":
                frames.append(frames_to_id[str(obj)])
            elif predicate_name == "syntacticMarker":
                markers.append(obj)
            elif predicate_txt_name:
                relations.append((predicate_txt_name,
                                  word_sense_id(graph, lexicographer_file, obj)))
        if frames:
            frames.sort()
            write(" {} {}".format(
                word_relations["frame"], " ".join(frames)), end="")
        if markers:
            [marker] = markers  # check that there is only one marker
            write(" {} {}".format(
                word_relations["syntacticMarker"], marker), end="")
        relations.sort()
        for relation_name, target in relations:
            write(print_word_relation(relation_name, target), end="")
        write("")

    write("{}: {}".format(synset_relations['containsWordSense'],
                          print_word_sense_id(word_sense_id(graph,
                                                            lexicographer_file,
                                                            word_sense),
                                              lexicographer_file)),
          end="")
    print_word_relations()


###
## json -> rdf
DEFINITION         = "definition"
EXAMPLE            = "example"
LEXICOGRAPHER_FILE = "lexicographerFile"
ID                 = "id"
EXAMPLES           = "examples"
WORDSENSES         = "wordsenses"
CONTAINS_WORDSENSE = "containsWordSense"
LEXICAL_FORM       = "lexicalForm"
LEXICAL_ID         = "lexicalId"
POINTERS           = "pointers"
NAME               = "name"
SENSEKEY           = "senseKey"
FRAME              = "frame"
FRAMES             = "frames"
WORDSENSE          = "wordsense"
SYNSET             = "synset"
RELATIONS          = "relations"

def from_json(json_input):
    for line in json_input:
        yield json.loads(line)

def to_graph(synsets_gen):
    def make_id(lexicographer_file, lexical_form, lexical_id, obj=SYNSET):
        return WN30EN["{}-{}-{}-{}".format(obj, lexicographer_file
                                           , lexical_form, lexical_id)]

    def parse_id(id_array, obj=SYNSET):
        [pos, lexname, lexical_form, lexical_id] = id_array
        lexicographer_file = "{}.{}".format(POS1TOLONG[pos], lexname)
        obj_id = make_id(lexicographer_file, lexical_form, lexical_id, obj)
        return obj_id, Literal(lexicographer_file), SYNSETTYPE[pos]

    def add_relation(head, relation, obj=SYNSET):
        g.add((head, WN30[relation[NAME]], parse_id(WN30EN[relation[ID]], obj)[0]))

    def add_frame(head, frame):
        g.add((head, WN30[FRAME], Literal(frame)))

    def add_word_sense(wordsense, lexicographer_file, synset_id):
        wordsense_id = make_id(lexicographer_file, wordsense[LEXICAL_FORM]
                               , wordsense[LEXICAL_ID], WORDSENSE)
        g.add((synset_id, WN30[CONTAINS_WORDSENSE], wordsense_id))
        g.add((wordsense_id, WN30[LEXICAL_FORM], Literal(wordsense[LEXICAL_FORM])))
        g.add((wordsense_id, WN30[SENSEKEY], Literal(wordsense[SENSEKEY])))
        map(lambda relation: add_relation(wordsense_id, relation, WORDSENSE)
            , wordsense[POINTERS])
        map(lambda frame: add_frame(wordsense_id, frame), wordsense[FRAMES])

    def add_synset(synset):
        synset_id, lexicographer_file, synset_type = parse_id(synset[ID])
        g.add((synset_id, RDF.type, synset_type))
        g.add((synset_id, WN30[LEXICOGRAPHER_FILE], lexicographer_file))
        g.add((synset_id, WN30[DEFINITION], Literal(synset[DEFINITION])))
        map(lambda example: g.add((synset_id, WN30[EXAMPLE], Literal(example)))
            , synset[EXAMPLES])
        map(lambda wordsense: add_word_sense(wordsense, lexicographer_file, synset_id),
            synset[WORDSENSES])
        map(lambda relation: add_relation(synset_id, relation), synset[RELATIONS])
        map(lambda frame: add_frame(synset_id, frame), synset[FRAMES])


    g = Graph()
    for synset in synsets_gen:
        add_synset(synset)
    return g

@cli.command()
@click.argument('json_input', type=click.File(mode="r"), required=True)
@click.argument('rdf_output', type=click.File(mode="wb"), required=True)
@click.option('-f', '--rdf-format', 'rdf_format'
              , type=click.STRING, default='nt', show_default=True,
              help="RDF output format. Must be accepted by RDFlib.")
def json_to_rdf(json_input, rdf_output, rdf_format='nt'):
    """Convert JSON_INPUT to RDF_OUTPUT."""
    graph = to_graph(from_json(json_input))
    graph.serialize(destination=rdf_output, format=rdf_format)

POS1TOLONG = {"A" : "adj", "N" : "noun", "R" : "adv", "S" : "adjs", "V" : "verb"}
SYNSETTYPE = {"A" : Literal("AdjectiveSynset"), "N" :
              Literal("NounSynset"), "R" : Literal("AdverbSynset"),
              "S" : Literal("AdjectiveSatelliteSynset"), "V" :
              Literal("VerbSynset")}

if __name__ == '__main__':
    cli()
