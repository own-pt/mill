from mill import WN30, WN30PT, WN_CONTAINS_WORDSENSE, WN_LEXICAL_FORM, WN_LANG, WN_LEXICAL_ID, WN_EXAMPLE, WN_LEXICOGRAPHER_FILE
from mill import rdf2text_go
from bootstrap_legacy_rdf import WN_WORD, WN_GLOSS, RDF_TYPE, EN
from bootstrap_legacy_rdf import fixes_to_legacy_rdf
from rdflib import Graph, Namespace, Literal, URIRef
from rdflib.term import Node
from rdflib.namespace import RDF, OWL, SKOS, RDFS, split_uri
import click

# This script is very ad hoc and has lots of hardcoded stuff; this is
# by design, since it is only meant to be run once as bootstrap for
# the text files. run
## python <this-file>.py --help
# for options

OWL_SAME_AS = OWL["sameAs"]

FORBIDDEN_PREDICATE_LIST = [WN_LANG, WN_CONTAINS_WORDSENSE, OWL_SAME_AS, SKOS["inScheme"], WN_GLOSS, WN30["synsetId"], RDF_TYPE, WN_EXAMPLE]
FORBIDDEN_PREDICATES = {pred: True for pred in FORBIDDEN_PREDICATE_LIST}

PT = Literal("pt")


def fix_pt_words(graph):
    # pt words have problems with spacing (spaces vs
    # hyphens/underlines), plus senses sometimes have no words,
    # sometimes a word has multiple lexical forms.. this function
    # creates words when missing, and fixes multiple lexical forms
    def fix_lexical_form(lexical_forms, label):
        lexical_form = None
        n = len(lexical_forms)
        if n == 0:
            lexical_form = label
        elif n == 1:
            lexical_form = lexical_forms[0]
        else :
            # whenever there are more than one lexical forms usually
            # one has underlines/hyphens and the other spaces; we want
            # to pick the one with hyphens
            for candidate in lexical_forms:
                if "-" in candidate:
                    lexical_form = candidate
                    break
            if not lexical_form :
                lexical_form = lexical_forms[0]
        return lexical_form.replace(" ", "_").strip()
    #
    for sense in graph.objects(None, WN_CONTAINS_WORDSENSE):
        word = graph.value(sense, WN_WORD, any=False)
        lexical_forms = list(graph.objects(word, WN_LEXICAL_FORM))
        label = graph.value(sense, RDFS.label, any=False)
        lexical_form = Literal(fix_lexical_form(lexical_forms, label))
        if not word:
            (_, wordsense_name) = split_uri(sense)
            word = WN30PT["word-{}".format(lexical_form)]
            graph.add((sense, WN_WORD, word))
        # remove previous lexical_forms
        graph.remove((word, WN_LEXICAL_FORM, None))
        # add new lexical_form (with underscores instead of spaces)
        graph.add((word, WN_LEXICAL_FORM, lexical_form))
    return None


def fill_pt(graph):
    def en_to_pt_synset(en_synset):
        return graph.value(en_synset, OWL_SAME_AS, default=False, any=False)
    def synset_minimal_wordsense(synset):
        wordsenses = graph.objects(synset, WN_CONTAINS_WORDSENSE)
        word_forms = list(map(lambda ws: graph.value(
            graph.value(ws, WN_WORD, any=False),
            WN_LEXICAL_FORM, any=False), wordsenses))
        assert word_forms and None not in word_forms, synset
        min_word_form = min(word_forms)
        return min_word_form
    #
    # use list so that it is safe to add sameAs relations while we
    # iterate over them
    same_as_relations = list(graph.triples((None, OWL_SAME_AS, None)))
    # fill up info from English synset to Portuguese one
    for (en_synset, _, pt_synset) in same_as_relations:
        graph.add((pt_synset, WN_LANG, PT))
        graph.add((en_synset, WN_LANG, EN))
        # everything else
        for (_, pred, obj) in graph.triples((en_synset, None, None)):
            if pred not in FORBIDDEN_PREDICATES:
                obj = en_to_pt_synset(obj) or obj
                graph.add((pt_synset, pred, obj))
        for (subj,pred,_) in graph.triples((None, None, en_synset)):
            if pred not in FORBIDDEN_PREDICATES:
                subj = en_to_pt_synset(subj) or subj
                graph.add((subj, pred, pt_synset))
        ## add dummy stuff if missing in Portuguese
        # gloss/definition
        if (pt_synset, WN_GLOSS, None) not in graph:
            # doing the split here or else the wn2text script will do
            # it for us and then we would have spurious English
            # examples in Portuguese
            english_definition = graph.value(en_synset, WN_GLOSS).split("; \"")[0].strip()
            graph.add((pt_synset, WN_GLOSS, Literal("@en_{}".format(english_definition))))
        # wordsenses and words
        wordsenses = list(graph.objects(pt_synset, WN_CONTAINS_WORDSENSE))
        # if we don't force the generator the test below is moot
        if wordsenses:
            lexical_forms_seen = {} # to remove duplicate wordsenses
                                    # (usually one form with spaces
                                    # and the other with underscores)
            for wordsense in wordsenses:
                word = graph.value(wordsense, WN_WORD, any=False)
                lexical_form = graph.value(word, WN_LEXICAL_FORM, any=False)
                assert lexical_form, wordsense
                if lexical_forms_seen.get(lexical_form, None):
                    # remove duplicate wordsense
                    graph.remove((None, None, wordsense))
                    graph.remove((wordsense, None, None))
                else:
                    lexical_forms_seen[lexical_form] = True
        else:
            (_, synset_uri) = split_uri(pt_synset)
            english_wordsenses = graph.objects(en_synset, WN_CONTAINS_WORDSENSE)
            for ix, english_wordsense in enumerate(english_wordsenses):
                wordsense = WN30PT["{}-{}.".format(
                    synset_uri.replace("synset", "wordsense", 1), ix)]
                graph.add((pt_synset, WN_CONTAINS_WORDSENSE, wordsense))
                word = WN30PT["{}-{}".format(
                    synset_uri.replace("synset", "word", 1), ix)]
                graph.add((wordsense, WN_WORD, word))
                english_lexical_form = graph.value(
                    graph.value(english_wordsense, WN_WORD, any=False),
                    WN_LEXICAL_FORM, any=False)
                graph.add((word, WN_LEXICAL_FORM,
                           Literal("@en_{}".format(english_lexical_form))))
    # add lexical ids and lexical forms if missing
    for lexfile in set(graph.objects(predicate=WN_LEXICOGRAPHER_FILE)):
        count = {}
        all_synsets = graph.subjects(predicate=WN_LEXICOGRAPHER_FILE, object=lexfile)
        synsets = filter(lambda s: graph.value(s, WN_LANG, any=False).eq(PT), all_synsets)
        for synset in sorted(synsets, key=synset_minimal_wordsense):
            for wordsense in graph.objects(synset,WN_CONTAINS_WORDSENSE):
                word = graph.value(wordsense, WN_WORD, any=False)
                if (wordsense, WN_LEXICAL_ID, None) not in graph:
                    lexical_form = graph.value(word, WN_LEXICAL_FORM, any=False)
                    assert lexical_form
                    lexical_id = count.get(lexical_form, 0)
                    graph.add((wordsense, WN_LEXICAL_ID, Literal("{}".format(lexical_id))))
                    count[lexical_form] = lexical_id + 1
    for en_synset, pt_synset in graph.subject_objects(OWL_SAME_AS):
        graph.remove((en_synset, OWL_SAME_AS, pt_synset))
        graph.add((pt_synset, OWL_SAME_AS, en_synset))
    return None


###
## fix multiple glosses: add one as definition and the other as
## examples
WN_DEFINITION = WN30['definition']
def fix_multiple_glosses(graph):
    for synset in graph.subjects(WN_LANG, PT):
        glosses = list(graph.objects(synset, WN_DEFINITION))
        if len(glosses) > 1:
            graph.remove((synset, WN_DEFINITION, None))
            graph.add((synset, WN_DEFINITION, Literal(glosses[0].strip())))
            for gloss in glosses[1:]:
                example = "GLOSS_: " + gloss.strip()
                graph.add((synset, WN_EXAMPLE, Literal(example)))
    return None


###
## fix wrong URIs
WN_DERIVATIONALLY_RELATED = WN30["derivationallyRelated"]

def fix_derivationally_related_wrong_uris(graph):
    for subj, obj in graph.subject_objects(WN_DERIVATIONALLY_RELATED):
        if (None, WN_CONTAINS_WORDSENSE, obj) not in graph:
            obj = URIRef(obj.replace("-a-", "-s-", 1))
            assert (None, WN_CONTAINS_WORDSENSE, obj) in graph, obj
            graph.add((subj, WN_DERIVATIONALLY_RELATED, obj))


# examples have trailing and leading whitespace
def fix_examples(graph):
    for synset, example in graph.subject_objects(WN_EXAMPLE):
        graph.remove((synset, WN_EXAMPLE, example))
        graph.add((synset, WN_EXAMPLE, Literal(example.strip())))
    return None


def fixes_to_pt_rdf(graph):
    fix_multiple_glosses(graph)
    fix_derivationally_related_wrong_uris(graph)
    fix_examples(graph)
    return None


@click.command()
@click.argument('rdf_input',
                type=click.File(mode="rb"), required=True)
@click.argument('config_dir',
                type=click.Path(exists=True, file_okay=False, resolve_path=True), required=True)
@click.argument('output_dir'
                , type=click.Path(file_okay=False, resolve_path=True, writable=True), required=True)
@click.argument('sense_map_file',
                type=click.File(mode="wb"), required=True)
@click.option('-f', '--rdf-file-format', 'rdf_file_format'
              , type=click.STRING, default='nt', show_default=True,
              help="RDF input format. Must be accepted by RDFlib.")
def main(rdf_input, config_dir, output_dir, sense_map_file, rdf_file_format):
    """Convert RDF_INPUT to lexicographer files placed at OUTPUT_DIR,
according to the configuration files in CONFIG_DIR."""
    graph = Graph()
    graph.parse(rdf_input, format=rdf_file_format)
    fix_pt_words(graph)
    fill_pt(graph)
    fixes_to_legacy_rdf(graph, config_dir, sense_map_file)
    fixes_to_pt_rdf(graph)
    rdf2text_go(graph, config_dir, output_dir)
    return None


if __name__ == '__main__':
    main()
