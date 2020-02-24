#!/bin/python3

# This script is an example of how to modify the mill wordnet
# programatically using Python. The JSON exported by mill is read to
# an rdflib graph, the graph is modified by the `go` function, and
# then it is serialized back to the text format.
#     python <this-file>.py --help
# for options

from mill import WN30
from mill import rdf2text_go, to_graph, from_json
from bootstrap_legacy_rdf import (WN_LEXICAL_FORM, WN_SIMILAR_TO, WN_TARGET,
                                  WN_TARGET_LEXICAL_FORM)
from bootstrap_legacy_rdf import pick_head

import click
from rdflib import BNode

WN_SATELLITE_OF = WN30["satelliteOf"]


def go(graph):
    similar_tos = list(graph.subject_objects(WN_SIMILAR_TO))
    for head_synset, bnode in similar_tos:
        satellite_synset = graph.value(bnode, WN_TARGET, any=False)
        bnode2 = BNode()
        target_sense = pick_head(graph, head_synset)
        target_lexical_form = graph.value(target_sense, WN_LEXICAL_FORM,
                                          any=False)
        graph.add((satellite_synset, WN_SATELLITE_OF, bnode2))
        graph.add((bnode2, WN_TARGET, head_synset))
        graph.add((bnode2, WN_TARGET_LEXICAL_FORM, target_lexical_form))


@click.command()
@click.argument('json_input',
                type=click.File(mode="rb"), required=True)
@click.argument('config_dir',
                type=click.Path(exists=True, file_okay=False,
                                resolve_path=True), required=True)
@click.argument('output_dir',
                type=click.Path(file_okay=False, resolve_path=True,
                                writable=True), required=True)
def main(json_input, config_dir, output_dir):
    graph = to_graph(from_json(json_input))
    go(graph)
    rdf2text_go(graph, config_dir, output_dir)
    return None


if __name__ == '__main__':
    main()
