import os
import rdflib as r
from rdflib import Graph
from rdflib import Namespace

# missing:
## - syntactic markers
# - ?

wn30 = Namespace("https://w3id.org/own-pt/wn30/schema/")

def main (rdf_file, config_dir, output_dir, rdf_file_format="nt"):
    (relations_map, frames_to_id) = read_config(config_dir)
    graph = Graph()
    graph.parse(rdf_file, format=rdf_file_format)
    print_graph(graph, relations_map, frames_to_id, output_dir)

def read_config(config_dir):
    def read_tsv(file_path, key_index, value_index):
        # return dict whose elements correspond to a line in the
        # TSV. The element's key is the field corresponding to
        # key_index, and the value corresponds to value_index
        res = {}
        with open(file_path, 'r') as input_stream:
            field_names = next(input_stream).strip().split(sep='\t')
            number_fields = len(field_names)
            for line_number,line in enumerate(input_stream):
                line_fields = line.strip().split(sep="\t")
                number_line_fields = len(line_fields)
                if number_line_fields == 1 & (line_fields[0].lstrip()[:2] == '--'):
                    pass
                elif number_line_fields == number_fields:
                    res[line_fields[key_index]] = line_fields[value_index]
                else:
                    print("Error: while reading TSV file {}, uneven number of fields; should have {} fields, but has {} fields in line {}".format(file_path, number_fields, number_line_fields, 1 + line_number))
                    break
            return res
                        
    #lexnames   = read_tsv(os.path.join(config_dir, "lexnames.tsv"), 1, 0)
    relations  = read_tsv(os.path.join(config_dir, "relations.tsv"), 2, 1)
    frames     = read_tsv(os.path.join(config_dir, "frames.tsv"), 1, 0)
    return (relations, frames)
    

def print_graph (graph, relations_map, frames_to_id, output_dir):
    query_results = graph.query(
    """SELECT DISTINCT ?lexFile
       WHERE {
          ?_ wn30:lexicographerFile ?lexFile .
       }""", initNs={ 'wn30' : wn30 })
    for (lexicographerFile,) in query_results:
        with open(os.path.join(output_dir, lexicographerFile), 'w') as output_stream:
            write = lambda data, *args, **kwargs: print(data, file=output_stream,
                                                        *args, **kwargs)
            for synset in g.subjects(wn30['lexicographerFile'], lexicographerFile):
                print_synset(graph, synset, lexicographerFile, relations_map, frames_to_id, write)

def print_synset(graph, synset, lexicographerFile, relations_map, frames_to_id, write):
    for word_sense in g.objects(synset, wn30['containsWordSense']):
        print_word_sense(graph, word_sense, lexicographerFile, relations_map, frames_to_id, write)
    # definition
    write("d: {}".format(graph.value(synset, wn30["gloss"])))
    # examples
    for example in graph.objects(synset, wn30["example"]):
        write("e: {}".format(example))
    # frames
    frames = graph.objects(synset, wn30["frame"])
    frame_ids = map(lambda frame: frames_to_id[frame], frames)
    write("fs: {}".format(" ".join(frame_ids)))
    ## how to print relations?
    write("\n")


def print_word_sense(graph, word_sense, lexicographerFile,
                     relations_map, frames_to_id, write):
    def word_sense_id(word_sense):
        word = graph.value(word_sense, wn30["word"])
        word_form = graph.value(word, wn30["lexicalForm"])
        lexical_id = graph.value(word_sense, wn30["lexicalId"])
        in_synset = graph.value(predicate=wn30["containsWordSense"], object=word_sense)
        in_lexfile = graph.value(subject=in_synset, predicate=wn30["lexicographerFile"])
        return "{}{}{}".format("{}:".format(in_lexfile)
                               if in_lexfile.neq(lexicographerFile)
                               else "",
                               word_form,
                               " {}".format(lexical_id) if lexical_id.neq("0") else "")
        
    def print_word_relations():
        frames = []
        relations = []
        for predicate, obj in graph.predicate_objects(word_sense):
            predicate_name = os.path.basename(os.path.basename(predicate.n3().strip("<>")))
            predicate_txt_name = relations_map.get(predicate_name, None)
            if predicate_txt_name == "frames":
                frames.append(frames_to_id[str(obj)])
            elif predicate_txt_name:
                relations.append(" {} {}".format(predicate_txt_name, word_sense_id(obj)))
        if frames:
            frames.sort()
            write(" frames {}".format(" ".join(frames)), end="")
        relations.sort()
        write("".join(relations))

    write("w: {}".format(word_sense_id(word_sense)), end="")
    print_word_relations()


