import os
import rdflib as r
from rdflib import Graph
from rdflib import Namespace

# missing:
## - syntactic markers
# - ?

wn30 = Namespace("https://w3id.org/own-pt/wn30/schema/")

def main (rdf_file, config_dir, output_dir, rdf_file_format="nt"):
    (synset_relations, word_relations, frames_to_id) = read_config(config_dir)
    graph = Graph()
    graph.parse(rdf_file, format=rdf_file_format)
    print_graph(graph, synset_relations, word_relations, frames_to_id, output_dir)

def read_config(config_dir):
    def read_tsv(file_path, line_function, initial_value):
        def read_line(line):
            return list(map(str.strip, line.split(sep='\t')))

        res = initial_value
        with open(file_path, 'r') as input_stream:
            field_names = read_line(next(input_stream))
            number_fields = len(field_names)
            for line_number,line in enumerate(input_stream):
                line_fields = read_line(line)
                number_line_fields = len(line_fields)
                if number_line_fields == 1 & (line_fields[0].lstrip()[:2] == '--'):
                    pass
                elif number_line_fields == number_fields:
                    res = line_function(res, line_fields)
                else:
                    print("Error: while reading TSV file {}, uneven number of fields; should have {} fields, but has {} fields in line {}".format(file_path, number_fields, number_line_fields, 1 + line_number))
                    break
            return res

    def read_relations(res, fields):
        (synset_relations, word_relations) = res
        if fields[4] == "word":
            word_relations[fields[2]] = fields[1]
        elif fields[4] == "synset":
            synset_relations[fields[2]] = fields[1]
        else:
            word_relations[fields[2]] = fields[1]
            synset_relations[fields[2]] = fields[1]
        return (synset_relations, word_relations)

    def read_frames(res, fields):
        res[fields[1]] = fields[0]
        return res

    synset_relations, word_relations  = read_tsv(os.path.join(config_dir, "relations.tsv"),
                                                 read_relations, ({}, {}))
    frames = read_tsv(os.path.join(config_dir, "frames.tsv"),
                      read_frames, {})
    return (synset_relations, word_relations, frames)


def print_graph (graph, synset_relations, word_relations, frames_to_id, output_dir):
    query_results = graph.query(
    """SELECT DISTINCT ?lexFile
       WHERE {
          ?_ wn30:lexicographerFile ?lexFile .
       }""", initNs={ 'wn30' : wn30 })
    for (lexicographerFile,) in query_results:
        with open(os.path.join(output_dir, lexicographerFile), 'w') as output_stream:
            write = lambda data, *args, **kwargs: print(data, file=output_stream,
                                                        *args, **kwargs)
            for synset in graph.subjects(wn30['lexicographerFile'], lexicographerFile):
                print_synset(graph, synset, lexicographerFile,
                             synset_relations, word_relations, frames_to_id, write)


def word_sense_id(graph, lexicographer_file, word_sense):
    word = graph.value(word_sense, wn30["word"])
    word_form = graph.value(word, wn30["lexicalForm"])
    lexical_id = graph.value(word_sense, wn30["lexicalId"])
    in_synset = graph.value(predicate=wn30["containsWordSense"], object=word_sense)
    in_lexfile = graph.value(subject=in_synset, predicate=wn30["lexicographerFile"])
    if None not in (word, word_form, lexical_id, in_synset, in_lexfile):
        return "{}{}{}".format("{}:".format(in_lexfile)
                               if in_lexfile.neq(lexicographer_file)
                               else "",
                               word_form,
                               " {}".format(lexical_id) if lexical_id.neq("0") else "")
    else:
        #print("Missing {}".format(word_sense))
        pass

def print_synset(graph, synset, lexicographerFile, synset_relations,
                 word_relations, frames_to_id, write):
    def sorted_word_senses(synset):
        return sorted(graph.objects(synset, wn30["containsWordSense"]),
                      key=lambda ws: graph.value(ws, wn30["senseKey"]))

    def print_relations():
        rels = []
        for predicate, obj in graph.predicate_objects(synset):
            predicate_name = os.path.basename(os.path.basename(predicate.n3().strip("<>")))
            predicate_txt_name = synset_relations.get(predicate_name, None)
            # maybe not include frame as relation?
            if predicate_txt_name == "frames":
                pass
            elif predicate_txt_name:
                rels.append("{}: {}".format(predicate_txt_name,
                                            word_sense_id(graph, lexicographerFile,
                                                          # first word sense is head
                                                          sorted_word_senses(obj)[0])))
        rels.sort()
        for relation in rels:
            write(relation)

    for word_sense in sorted_word_senses(synset):
        print_word_sense(graph, word_sense, lexicographerFile,
                         word_relations, frames_to_id, write)
    # definition
    write("d: {}".format(graph.value(synset, wn30["gloss"])))
    # examples
    for example in graph.objects(synset, wn30["example"]):
        write("e: {}".format(example))
    # frames
    frames = graph.objects(synset, wn30["frame"])
    frame_ids = list(map(lambda frame: frames_to_id[frame.n3()[1:-1]], frames))
    if frame_ids:
        write("fs: {}".format(" ".join(frame_ids)))
    print_relations()
    write("")


def print_word_sense(graph, word_sense, lexicographerFile,
                     relations_map, frames_to_id, write):
    def print_word_relations():
        frames = []
        relations = []
        for predicate, obj in graph.predicate_objects(word_sense):
            predicate_name = os.path.basename(os.path.basename(predicate.n3().strip("<>")))
            predicate_txt_name = relations_map.get(predicate_name, None)
            if predicate_txt_name == "frames":
                frames.append(frames_to_id[str(obj)])
            elif predicate_txt_name:
                relations.append(" {} {}".format(predicate_txt_name,
                                                 word_sense_id(graph, lexicographerFile, obj)))
        if frames:
            frames.sort()
            write(" frames {}".format(" ".join(frames)), end="")
        relations.sort()
        write("".join(relations))

    write("w: {}".format(word_sense_id(graph, lexicographerFile, word_sense)), end="")
    print_word_relations()
