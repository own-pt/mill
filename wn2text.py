#!/bin/python3

import os
import rdflib as r
from rdflib import Graph, Namespace
import click

# missing:
# - adjective sattellites

wn30 = Namespace("https://w3id.org/own-pt/wn30/schema/")


@click.command()
@click.argument('rdf_file', type=click.Path(exists=True, dir_okay=False, resolve_path=True), required=True)
@click.argument('config_dir', type=click.Path(exists=True, file_okay=False, resolve_path=True), required=True)
@click.argument('output_dir', type=click.Path(file_okay=False, resolve_path=True, writable=True), required=True)
@click.option('-f', '--rdf-file-format', 'rdf_file_format', type=click.STRING, default='nt', show_default=True,
              help="Type of RDF input file. Must be accepted by RDFlib.")
def main(rdf_file, config_dir, output_dir, rdf_file_format="nt"):
    (synset_relations, word_relations, frames_to_id) = read_config(config_dir)
    graph = Graph()
    graph.parse(rdf_file, format=rdf_file_format)
    print_graph(graph, synset_relations, word_relations,
                frames_to_id, output_dir)


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
        relation_rdf = fields[2].split('/')[-1]
        relation_text = fields[1]
        if fields[4] == "word":
            word_relations[relation_rdf] = relation_text
        elif fields[4] == "synset":
            synset_relations[relation_rdf] = relation_text
        else:
            word_relations[relation_rdf] = relation_text
            synset_relations[relation_rdf] = relation_text
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
       }""", initNs={'wn30': wn30})
    for (lexicographerFile,) in query_results:
        print_lexfile(graph, lexicographerFile, synset_relations,
                      word_relations, frames_to_id, output_dir)


def sort_word_senses(graph, synset):
    return sorted(graph.objects(synset, wn30["containsWordSense"]),
                  key=lambda ws: graph.value(graph.value(ws, wn30["word"]),
                                             wn30["lexicalForm"]))


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
        synsets = graph.subjects(wn30['lexicographerFile'], lexicographer_file)
        for synset, sorted_word_senses in sort_synsets(graph, synsets):
            print_synset(graph, synset, sorted_word_senses, lexicographer_file,
                         synset_relations, word_relations, frames_to_id, write)


def word_sense_id(graph, lexicographer_file, word_sense):
    word = graph.value(word_sense, wn30["word"])
    word_form = graph.value(word, wn30["lexicalForm"])
    lexical_id = graph.value(word_sense, wn30["lexicalId"])
    in_synset = graph.value(
        predicate=wn30["containsWordSense"], object=word_sense)
    in_lexfile = graph.value(
        subject=in_synset, predicate=wn30["lexicographerFile"])
    if None not in (word, word_form, lexical_id, in_synset, in_lexfile):
        return (in_lexfile, word_form, int(lexical_id))
    else:
        raise "Error: missing wordsense information"


def print_word_sense_id(wordsense_id, lexicographer_file=None):
    (in_lexfile, word_form, lexical_id) = wordsense_id
    return "{}{}{}".format("{}:".format(in_lexfile)
                           if in_lexfile.neq(lexicographer_file)
                           else "",
                           word_form,
                           " {}".format(lexical_id) if lexical_id != 0 else "")


def print_synset(graph, synset, sorted_word_senses, lexicographerFile, synset_relations,
                 word_relations, frames_to_id, write):
    def print_relations():
        def print_relation(name, wordsense_id):
            return "{}: {}".format(name,
                                   print_word_sense_id(wordsense_id, lexicographerFile))
        rels = []
        for predicate, obj in graph.predicate_objects(synset):
            _, predicate_name = r.namespace.split_uri(predicate)
            # print(predicate_name)
            predicate_txt_name = synset_relations.get(predicate_name, None)
            if predicate_name in ["frame", "containsWordSense", "gloss",
                                  "example", "lexicographerFile", "lexicalForm"]:
                pass
            elif predicate_txt_name:
                rels.append((predicate_txt_name,
                             word_sense_id(graph, lexicographerFile,
                                           # first word sense is head
                                           sort_word_senses(graph, obj)[0])))
        rels.sort()
        for (relation_name, target) in rels:
            write(print_relation(relation_name, target))

    for word_sense in sorted_word_senses:
        print_word_sense(graph, word_sense, lexicographerFile,
                         word_relations, synset_relations, frames_to_id, write)
    # definition
    write("{}: {}".format(
        synset_relations.get("gloss", "d"), graph.value(synset, wn30["gloss"])))
    # examples
    for example in graph.objects(synset, wn30["example"]):
        write("{}: {}".format(synset_relations["example"], example))
    # frames
    frames = graph.objects(synset, wn30["frame"])
    frame_ids = list(map(lambda frame: frames_to_id[frame.n3()[1:-1]], frames))
    if frame_ids:
        write("{}: {}".format(synset_relations['frame'], " ".join(frame_ids)))
    print_relations()
    write("")


def print_word_sense(graph, word_sense, lexicographerFile,
                     word_relations, synset_relations, frames_to_id, write):
    def print_word_relations():
        def print_word_relation(name, wordsense_id):
            return " {} {}".format(name,
                                   print_word_sense_id(wordsense_id, lexicographerFile))
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
                                  word_sense_id(graph, lexicographerFile, obj)))
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
                                                            lexicographerFile,
                                                            word_sense),
                                              lexicographerFile)),
          end="")
    print_word_relations()


if __name__ == '__main__':
    main()
