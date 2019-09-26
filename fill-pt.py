from rdflib import Graph, Namespace, Literal
from rdflib.namespace import RDFS, OWL, SKOS
import click

WN30 = Namespace("https://w3id.org/own-pt/wn30/schema/")
CONTAINS_WORDSENSE = WN30["containsWordSense"]
SAME_AS = OWL["sameAs"]
FORBIDDEN_PREDICATE_LIST = [CONTAINS_WORDSENSE,SAME_AS,SKOS["inScheme"],WN30["gloss"],WN30["synsetId"],RDFS["type"], WN30["example"]]
FORBIDDEN_PREDICATES = {pred: True for pred in FORBIDDEN_PREDICATE_LIST}

@click.command()
@click.argument('input_file', type=click.File(mode="rb"), required=True)
@click.argument('output_file', type=click.Path(dir_okay=False,resolve_path=True),
                required=True)
@click.option('-f', '--rdf-file-format', 'rdf_file_format', type=click.STRING, default='nt', show_default=True,
              help="Type of RDF input file. Must be accepted by RDFlib.")
def main(input_file, rdf_file_format, output_file):
    graph = Graph()
    graph.parse(rdf_file, format=rdf_file_format)
    go(graph) # modifies graph
    graph.serialize(rdf_final, format="nt")
    return None

def go(graph):
    def en_to_pt_synset(en_synset):
        return graph.value(en_synset,SAME_AS,default=False,any=False)
    #
    for (en_synset, _, pt_synset) in graph.triples((None, SAME_AS,None)):
        # fill up info from English synset to Portuguese one
        g.add((pt_synset, WN30["lang"], Literal("pt")))
        g.add((en_synset, WN30["lang"], Literal("en")))
        for (_, pred, obj) in graph.triples((en_synset, None, None)):
            if pred not in FORBIDDEN_PREDICATES:
                obj = en_to_pt_synset(obj) or obj
                graph.add((pt_synset,pred,obj))
        for (subj,pred,_) in graph.triples((None, None, en_synset)):
            if pred not in FORBIDDEN_PREDICATES:
                subj = en_to_pt_synset(subj) or subj
                graph.add((subj,pred,pt_synset))
        # add dummy stuff if missing in Portuguese
        if (pt_synset, WN30["gloss"], None) not in graph:
            graph.add((pt_synset, WN30["gloss"], Literal("#;Missing gloss")))
        if (pt_synset, CONTAINS_WORDSENSE, None) not in graph:
            # FIXME: remove frames and markers and relations from
            # word, but add relation to inexisting word that will mark
            # that the synset is missing a Portuguese word (or
            # something like that; using a marker would be even nicer,
            # but that might run into problems when we forbid markers
            # from non-adjectives)
            fill_word = graph.value(en_synset, CONTAINS_WORDSENSE)
            graph.add(pt_synset, CONTAINS_WORDSENSE, fill_word)
        # FIXME: if we remove lexicographerFile predicate we can use
        # wn2text.py to generate Portuguese text wordnet


if __name__ == '__main__':
    main()
