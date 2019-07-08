import os
import rdflib as r
from rdflib import Graph
from rdflib import Namespace

# missing:
## - syntactic markers
# - ?

wn30 = Namespace("https://w3id.org/own-pt/wn30/schema/")

def main (rdf_file, output_dir, rdf_file_format="nt"):
    graph = Graph()
    graph.parse(rdf_file, format=rdf_file_format)
    print_graph(graph, output_dir)

def print_graph (graph, output_dir):
    query_results = graph.query(
    """SELECT DISTINCT ?lexFile
       WHERE {
          ?_ wn30:lexicographerFile ?lexFile .
       }""", initNs={ 'wn30': wn30 })
    for (lexicographerFile,) in query_results:
        with open(os.path.join(output_dir, lexicographerFile), 'w') as output_stream:
            write = lambda data: print(data, file=output_stream)
            write(lexicographerFile)
            for synset in g.subjects(wn30['lexicographerFile'], lexicographerFile):
                print_synset(synset, write)

def print_synset(synset, write):
    for word_sense in g.objects(synset, wn30['containsWordSense']):
        print_word_sense(word_sense, write)
    write(graph.value(synset, wn30["gloss"]))
    for example in graph.objects(synset, wn30["example"]):
        write(example)
    for frame in graph.objects(synset, wn30["frame"]):
        # have to map frame texts to ids
        pass
    # how to print relations?


def print_word_sense(word_sense, write):
    word = graph.value(word_sense, wn30["word"])
    word_form = graph.value(word, wn30["lexicalForm"])
    lexical_id = graph.value(word_sense, wn30["lexicalId"])
    write("w: {}{}".format(word_form,
                           "" if lexical_id.eq("0") else " {}".format(lexical_id)))
    # how to print word relations?
