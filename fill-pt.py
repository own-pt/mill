from rdflib import Graph, Namespace, Literal, URIRef
from rdflib.term import Node
from rdflib.namespace import RDF, OWL, SKOS, RDFS, split_uri
from wn2text import sort_synsets  # wn2text is defined at https://github.com/own-pt/mill
import click

WN30PT = Namespace("https://w3id.org/own-pt/wn30-pt/instances/") # not used (yet)
WN30 = Namespace("https://w3id.org/own-pt/wn30/schema/")
CONTAINS_WORDSENSE = WN30["containsWordSense"]
SAME_AS = OWL["sameAs"]
SYNTACTIC_MARKER = WN30["syntacticMarker"]
LEXICAL_FORM = WN30["lexicalForm"]
FRAME = WN30["frame"]
WORD = WN30["word"]
LEXICAL_ID = WN30["lexicalId"]
LEXICOGRAPHER_FILE = WN30["lexicographerFile"]
FORBIDDEN_PREDICATE_LIST = [LEXICOGRAPHER_FILE, CONTAINS_WORDSENSE, SAME_AS, SKOS["inScheme"], WN30["gloss"], WN30["synsetId"], RDF["type"], WN30["example"]]
FORBIDDEN_PREDICATES = {pred: True for pred in FORBIDDEN_PREDICATE_LIST}


input_file = "own-all.nt"
rdf_file_format = 'nt'
output_file='wn30pt.nt'
@click.command()
@click.argument('input_file', type=click.File(mode="rb"), required=True)
@click.argument('output_file', type=click.Path(dir_okay=False,resolve_path=True),
                required=True)
@click.option('-f', '--rdf-file-format', 'rdf_file_format', type=click.STRING, default='nt', show_default=True,
              help="Type of RDF input file. Must be accepted by RDFlib.")
def main(input_file, rdf_file_format, output_file):
    graph = Graph()
    graph.parse(input_file, format=rdf_file_format)
    go(graph) # modifies graph
    graph.serialize(output_file, format="nt")
    return None

def go(graph):
    def en_to_pt_synset(en_synset):
        return graph.value(en_synset,SAME_AS,default=False,any=False)
    def synset_minimal_wordsense(synset):
        wordsenses = graph.objects(synset,CONTAINS_WORDSENSE)
        if not wordsenses:
            print(synset)
        word_forms = list(map(lambda ws: graph.value(graph.value(ws, WORD), LEXICAL_FORM), wordsenses))
        if None in word_forms or not word_forms:
            print(synset)
        min_word_form = min(word_forms)
        return min_word_form
    #
    pt_lexfiles = set()
    # fill up info from English synset to Portuguese one
    for (en_synset, _, pt_synset) in graph.triples((None, SAME_AS,None)):
        # not really needed:
        graph.add((pt_synset, WN30["lang"], Literal("pt")))
        graph.add((en_synset, WN30["lang"], Literal("en")))
        # lexfile
        english_lexfile = graph.value(en_synset, LEXICOGRAPHER_FILE)
        portuguese_lexfile = Literal("{}@pt".format(english_lexfile))
        pt_lexfiles.add(portuguese_lexfile)
        graph.add((pt_synset, LEXICOGRAPHER_FILE, portuguese_lexfile))
        # FIXME: add sameAs relation
        # everything else
        for (_, pred, obj) in graph.triples((en_synset, None, None)):
            if pred not in FORBIDDEN_PREDICATES:
                obj = en_to_pt_synset(obj) or obj
                graph.add((pt_synset,pred,obj))
        for (subj,pred,_) in graph.triples((None, None, en_synset)):
            if pred not in FORBIDDEN_PREDICATES:
                subj = en_to_pt_synset(subj) or subj
                graph.add((subj,pred,pt_synset))
        ## add dummy stuff if missing in Portuguese
        # gloss/definition
        if (pt_synset, WN30["gloss"], None) not in graph:
            graph.add((pt_synset, WN30["gloss"], Literal("#;Missing_gloss")))
        # wordsenses and words
        wordsenses = list(graph.objects(pt_synset, CONTAINS_WORDSENSE))
        # if we don't force the generator the test below is moot
        if wordsenses:
            for wordsense in wordsenses:
                word = graph.value(wordsense, WORD)
                if not word:
                    (_, wordsense_name) = split_uri(wordsense)
                    word = WN30PT["word-{}".format(wordsense_name)]
                    graph.add((wordsense, WORD, word))
                if not graph.value(word,LEXICAL_FORM):
                    lexical_form = graph.value(wordsense, RDFS.label)
                    graph.add((word, LEXICAL_FORM, lexical_form))
        else:
            (_, synset_uri) = split_uri(pt_synset)
            wordsense = WN30PT["wordsense-{}.".format(synset_uri)]
            graph.add((pt_synset, CONTAINS_WORDSENSE, wordsense))
            word = WN30PT["word-{}".format(synset_uri)]
            graph.add((wordsense, WORD, word))
            graph.add((word, LEXICAL_FORM, Literal("#;Missing_wordsense_{}".format(synset_uri))))
    # # this adds lexical ids ignoring synset order
    # for lexfile in pt_lexfiles:
    #     count = {}
    #     synsets = graph.subjects(LEXICOGRAPHER_FILE, lexfile)
    #     for synset in synsets:
    #         wordsenses = graph.objects(synset, CONTAINS_WORDSENSE)
    #         if not wordsenses:
    #             print(synset)
    #         for wordsense in wordsenses:
    #             if not isinstance(wordsense, Node):
    #                 print(wordsense)
    #                 print(synset)
    #             if (wordsense, LEXICAL_ID, None) not in graph:
    #                 lexical_form = graph.value(wordsense, LEXICAL_FORM)
    #                 lexical_id = count.get(lexical_form, 0)
    #                 graph.add((wordsense, LEXICAL_ID, Literal("{}".format(lexical_id))))
    #                 count[lexical_form] = lexical_id + 1
    # # add lexical ids and lexical forms if missing
    for lexfile in pt_lexfiles:
        count = {}
        synsets = graph.subjects(LEXICOGRAPHER_FILE, lexfile)
        for synset in sorted(synsets, key=synset_minimal_wordsense):
            for wordsense in graph.objects(synset,CONTAINS_WORDSENSE):
                if (wordsense, LEXICAL_ID, None) not in graph:
                    lexical_form = graph.value(wordsense, LEXICAL_FORM)
                    lexical_id = count.get(lexical_form, 0)
                    graph.add((wordsense, LEXICAL_ID, Literal("{}".format(lexical_id))))
                    count[lexical_form] = lexical_id + 1
    return None



if __name__ == '__main__':
    main()
