from rdflib import Graph, Namespace, Literal, URIRef
from rdflib.term import Node
from rdflib.namespace import RDF, OWL, SKOS, RDFS, split_uri
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
    def handle_spaces(string):
        return string.replace(" ", "_").strip()
    #
    pt_lexfiles = set()
    # use list so that it is safe to add sameAs relations while we
    # iterate over them
    same_as_relations = list(graph.triples((None, SAME_AS,None)))
    # fill up info from English synset to Portuguese one
    for (en_synset, _, pt_synset) in same_as_relations:
        # not really needed:
        graph.add((pt_synset, WN30["lang"], Literal("pt")))
        graph.add((en_synset, WN30["lang"], Literal("en")))
        # lexfile
        english_lexfile = graph.value(en_synset, LEXICOGRAPHER_FILE)
        portuguese_lexfile = Literal("{}@pt".format(english_lexfile))
        pt_lexfiles.add(portuguese_lexfile)
        graph.add((pt_synset, LEXICOGRAPHER_FILE, portuguese_lexfile))
        # add reverse sameAs relation
        graph.add((pt_synset, SAME_AS, en_synset))
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
            graph.add((pt_synset, WN30["gloss"], Literal("@_Missing_gloss")))
        # wordsenses and words
        wordsenses = list(graph.objects(pt_synset, CONTAINS_WORDSENSE))
        # if we don't force the generator the test below is moot
        if wordsenses:
            lexical_forms_seen = {} # to remove duplicate wordsenses
                                    # (usually one form with spaces
                                    # and the other with underscores)
            for wordsense in wordsenses:
                word = graph.value(wordsense, WORD)
                if not word:
                    (_, wordsense_name) = split_uri(wordsense)
                    word = WN30PT["word-{}".format(wordsense_name)]
                    graph.add((wordsense, WORD, word))
                new_lexical_form = graph.value(word, LEXICAL_FORM) or graph.value(wordsense, RDFS.label)
                lexical_form = Literal(handle_spaces(new_lexical_form))
                if lexical_forms_seen.get(lexical_form, None):
                    # remove duplicate wordsense
                    graph.remove((None, None, wordsense))
                else:
                    lexical_forms_seen[lexical_form] = True
                    # remove previous lexical_forms
                    graph.remove((word, LEXICAL_FORM, None))
                    # add new lexical_form (with underscores instead of spaces)
                    graph.add((word, LEXICAL_FORM, lexical_form))
        else:
            (_, synset_uri) = split_uri(pt_synset)
            wordsense = WN30PT["wordsense-{}.".format(synset_uri)]
            graph.add((pt_synset, CONTAINS_WORDSENSE, wordsense))
            word = WN30PT["word-{}".format(synset_uri)]
            graph.add((wordsense, WORD, word))
            graph.add((word, LEXICAL_FORM, Literal("@_Missing_wordsense_{}".format(synset_uri))))
            graph.add((wordsense, LEXICAL_ID, Literal("0")))
    # add lexical ids and lexical forms if missing
    for lexfile in pt_lexfiles:
        count = {}
        synsets = graph.subjects(LEXICOGRAPHER_FILE, lexfile)
        for synset in sorted(synsets, key=synset_minimal_wordsense):
            for wordsense in graph.objects(synset,CONTAINS_WORDSENSE):
                word = graph.value(wordsense, WORD)
                if (wordsense, LEXICAL_ID, None) not in graph:
                    lexical_form = graph.value(word, LEXICAL_FORM)
                    lexical_id = count.get(lexical_form, 0)
                    graph.add((wordsense, LEXICAL_ID, Literal("{}".format(lexical_id))))
                    count[lexical_form] = lexical_id + 1
    return None



if __name__ == '__main__':
    main()
