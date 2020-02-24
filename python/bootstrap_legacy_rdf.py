#!/bin/python3

# This script is very ad hoc and has lots of hardcoded stuff; this is
# by design, since it is only meant to be run once as bootstrap for
# the text files. run
## python <this-file>.py --help
# for options

from mill import WN30, WN_LEXICAL_ID, WN_LEXICOGRAPHER_FILE, WN_LEXICAL_FORM, WN_CONTAINS_WORDSENSE, SYNSET_RELATIONS, WN_TARGET, WN_TARGET_LEXICAL_FORM, WN_LANG, WN_DEFINITION, WN_EXAMPLE, WN_SOURCE_BEGIN, WN_FRAME, SYNSET_RELATIONS, RDF_TYPE
from mill import sort_synsets, sort_word_senses, rdf2text_go, read_config, summarize
import rdflib as r
from rdflib import Graph, Namespace
from rdflib.namespace import RDF
from rdflib.term import Literal, BNode, URIRef
import click

EN = Literal("en")

WN_SYNSET_ID = WN30["synsetId"]
WN_SENSEKEY = WN30["senseKey"]
WN_SIMILAR_TO = WN30["similarTo"]
WN_LEMMA = WN30["lemma"]
WN_WORD = WN30["word"]
WN_GLOSS = WN30["gloss"]

def pick_synset_relation_targets(graph, config_dir):
    global SYNSET_RELATIONS
    WN_HEAD_SENSE = WN30["headSense"]
    # in text, each synset relation points to a particular sense; in
    # bootstraping phase we pick preferentially senses that have
    # lexical id of zero, or the first in sort order
    def pick_synset_heads():
        def pick_head(synset):
            # try to pick sense with lexical id of zero (for easier to
            # read files)
            sorted_senses = sort_word_senses(graph, synset)
            for sense in sorted_senses:
                if graph.value(sense, WN_LEXICAL_ID).eq("0"):
                    return sense
            return sorted_senses[0]
        for synset in graph.subjects(WN_LEXICOGRAPHER_FILE):
            head_sense = pick_head(synset)
            graph.add((synset, WN_HEAD_SENSE, head_sense))
        return None
    #
    (SYNSET_RELATIONS, _) = read_config(config_dir)
    pick_synset_heads()
    for synset in graph.subjects(WN_LEXICOGRAPHER_FILE, None):
        for predicate, target in graph.predicate_objects(synset):
            _, predicate_name = r.namespace.split_uri(predicate)
            predicate_txt_name = SYNSET_RELATIONS.get(predicate_name, None)
            target_is_synset = graph.value(target, WN_CONTAINS_WORDSENSE, any=True)
            if predicate_txt_name:
                if target_is_synset:
                    bnode = BNode()
                    graph.remove((synset, predicate, target))
                    graph.add((synset, predicate, bnode))
                    graph.add((bnode, WN_TARGET, target))
                    target_sense = graph.value(target, WN_HEAD_SENSE, any=False)
                    target_lexical_form = graph.value(target_sense, WN_LEXICAL_FORM,
                                                      any=False)
                    graph.add((bnode, WN_TARGET_LEXICAL_FORM, target_lexical_form))
            elif target_is_synset:
                raise LookupError("{} relation not found".format(predicate_name))
    graph.remove((None, WN_HEAD_SENSE, None))
    return None

###
## fix some glosses
WN_GLOSS = WN30["gloss"]
WN_DEFINITION = WN30["definition"]
WN_EXAMPLE = WN30["example"]

def fix_glosses(graph):
    def split_gloss(gloss):
        def remove_quotes(example):
            example.strip()
            if example[-1] == "\"" and "\"" not in example[:-1]:
                return example.strip("\"")
            else:
                return "\"" + example

        def_examples = gloss.split("; \"")
        definition = def_examples[0].strip()
        examples = def_examples[1:]
        return (definition, map(remove_quotes, examples))
    #
    for synset, gloss in graph.subject_objects(WN_GLOSS):
        definition, examples = split_gloss(gloss)
        graph.add((synset, WN_DEFINITION, Literal(definition)))
        for example in examples:
            graph.add((synset, WN_EXAMPLE, Literal(example)))
    graph.remove((None, WN_GLOSS, None))
    return None

###
## add missing sense keys
SYNSET_TYPE = {
    WN30["AdjectiveSynset"]: "3",
    WN30["NounSynset"]: "1",
    WN30["AdverbSynset"]: "4",
    WN30["AdjectiveSatelliteSynset"]: "5",
    WN30["VerbSynset"]: "2"
}

LEXICOGRAPHER_FILE_NUM = {"adj.all": 0,
"adj.pert": 1,
"adv.all": 2,
"noun.Tops": 3,
"noun.act": 4,
"noun.animal": 5,
"noun.artifact": 6,
"noun.attribute": 7,
"noun.body": 8,
"noun.cognition": 9,
"noun.communication": 10,
"noun.event": 11,
"noun.feeling": 12,
"noun.food": 13,
"noun.group": 14,
"noun.location": 15,
"noun.motive": 16,
"noun.object": 17,
"noun.person": 18,
"noun.phenomenon": 19,
"noun.plant": 20,
"noun.possession": 21,
"noun.process": 22,
"noun.quantity": 23,
"noun.relation": 24,
"noun.shape": 25,
"noun.state": 26,
"noun.substance": 27,
"noun.time": 28,
"verb.body": 29,
"verb.change": 30,
"verb.cognition": 31,
"verb.communication": 32,
"verb.competition": 33,
"verb.consumption": 34,
"verb.contact": 35,
"verb.creation": 36,
"verb.emotion": 37,
"verb.motion": 38,
"verb.perception": 39,
"verb.possession": 40,
"verb.social": 41,
"verb.stative": 42,
"verb.weather": 43,
"adj.ppl": 44}

def get_sense_key(graph, sense, synset=None):
    synset = synset or graph.value(predicate=WN_CONTAINS_WORDSENSE, object=sense)
    if (synset, WN_LANG, EN) in graph:
        sense_key = graph.value(sense, WN_SENSEKEY, any=False)
        if sense_key:
            return sense_key
        word = graph.value(sense, WN_WORD)
        lemma = graph.value(word, WN_LEMMA)
        assert lemma, sense
        for synset_type_uri in graph.objects(synset, RDF_TYPE):
            # loop because RDF_TYPE object might be either a POS
            # indicator or a core concept indicator
            synset_type = SYNSET_TYPE.get(synset_type_uri, None)
            if synset_type:
                break
        lexicographer_file = graph.value(synset, WN_LEXICOGRAPHER_FILE)
        lexicographer_file_num = LEXICOGRAPHER_FILE_NUM[str(lexicographer_file)]
        assert synset_type and lexicographer_file, synset
        lexical_id = graph.value(sense, WN_LEXICAL_ID)
        assert lexical_id, sense
        head_lemma, head_lexical_id = "", ""
        head_sense = graph.value(sense, WN_SIMILAR_TO)
        if head_sense:
            head_word = graph.value(head_sense, WN_WORD)
            head_lemma = graph.value(head_word, WN_LEMMA)
            head_lexical_id = graph.value(head_sense, WN_LEXICAL_ID)
            assert head_lemma and head_lexical_id, head_sense
        sense_key = "{}%{}:{}:{}:{}:{}".format(lemma, synset_type,
                                               lexicographer_file_num,
                                               lexical_id, head_lemma,
                                               head_lexical_id)
        return sense_key

def add_missing_sensekeys(graph):
    for synset, sense in graph.subject_objects(WN_CONTAINS_WORDSENSE):
        if (synset, WN_LANG, EN) in graph:
            if not (sense, WN_SENSEKEY, None) in graph:
                sense_key = get_sense_key(graph, sense, synset)
                assert sense_key
                graph.add((sense, WN_SENSEKEY, Literal(sense_key)))

###
## make similarTo one-way
def make_similar_to_one_way(graph):
    similar_tos = list(graph.subject_objects(WN_SIMILAR_TO))
    for subj, obj in similar_tos:
        if any(map(lambda k: k == WN30["AdjectiveSynset"], graph.objects(obj, RDF_TYPE))):
            graph.remove((subj, WN_SIMILAR_TO, obj))
    return None

###
## making the old RDF model closer to the current one: remove words,
## since they don't really exist in the text format
WN_WORD = WN30["word"]
WN_CONTAINS_WORDSENSE = WN30["containsWordSense"]
WN_LEXICAL_FORM = WN30["lexicalForm"]

def add_lexical_form_to_senses(graph):
    for synset, sense in graph.subject_objects(WN_CONTAINS_WORDSENSE):
        word = graph.value(sense, WN_WORD, any=False)
        assert word, sense
        lexical_form = graph.value(word, WN_LEXICAL_FORM)
        assert lexical_form, sense
        graph.add((sense, WN_LEXICAL_FORM, lexical_form))
    ## no need to delete this info
    # for word in set(graph.objects(None, WN_WORD)):
    #     graph.remove((word, None, None))
    #     graph.remove((None, None, word))
    return None

###
## add fake source_begin info, for sorting
WN_SOURCE_BEGIN = WN30["sourceBegin"]

def add_source_begin(graph):
    def synset_minimal_wordsense(synset):
        wordsenses = graph.objects(synset, WN_CONTAINS_WORDSENSE)
        word_forms = list(map(lambda ws: graph.value(ws, WN_LEXICAL_FORM), wordsenses))
        if None in word_forms or not word_forms:
            print(synset)
        min_word_form = min(word_forms)
        return min_word_form
    #
    graph.remove((None, WN_SOURCE_BEGIN, None))
    lexicographer_files = set(graph.objects(predicate=WN_LEXICOGRAPHER_FILE))
    for lexicographer_file in lexicographer_files:
        sorted_synsets = sorted(graph.subjects(WN_LEXICOGRAPHER_FILE, lexicographer_file), key=synset_minimal_wordsense)
        for ix, synset in enumerate(sorted_synsets):
            graph.add((synset, WN_SOURCE_BEGIN, Literal(ix)))
    return None

###
## add frames
FRAMES_TO_ID = {
    "Something ----s": Literal(1),
    "Somebody ----s": Literal(2),
    "It is ----ing": Literal(3),
    "Something is ----ing PP": Literal(4),
    "Something ----s something Adjective/Noun": Literal(5),
    "Something ----s Adjective/Noun": Literal(6),
    "Somebody ----s Adjective": Literal(7),
    "Somebody ----s something": Literal(8),
    "Somebody ----s somebody": Literal(9),
    "Something ----s somebody": Literal(10),
    "Something ----s something": Literal(11),
    "Something ----s to somebody": Literal(12),
    "Somebody ----s on something": Literal(13),
    "Somebody ----s somebody something": Literal(14),
    "Somebody ----s something to somebody": Literal(15),
    "Somebody ----s something from somebody": Literal(16),
    "Somebody ----s somebody with something": Literal(17),
    "Somebody ----s somebody of something": Literal(18),
    "Somebody ----s something on somebody": Literal(19),
    "Somebody ----s somebody PP": Literal(20),
    "Somebody ----s something PP": Literal(21),
    "Somebody ----s PP": Literal(22),
    "Somebody's (body part) ----s": Literal(23),
    "Somebody ----s somebody to INFINITIVE": Literal(24),
    "Somebody ----s somebody INFINITIVE": Literal(25),
    "Somebody ----s that CLAUSE": Literal(26),
    "Somebody ----s to somebody": Literal(27),
    "Somebody ----s to INFINITIVE": Literal(28),
    "Somebody ----s whether INFINITIVE": Literal(29),
    "Somebody ----s somebody into V-ing something": Literal(30),
    "Somebody ----s something with something": Literal(31),
    "Somebody ----s INFINITIVE": Literal(32),
    "Somebody ----s VERB-ing": Literal(33),
    "It ----s that CLAUSE": Literal(34),
    "Something ----s INFINITIVE": Literal(35),
}

WN_FRAME = WN30["frame"]

def use_frame_numbers(graph):
    for subj, frame in graph.subject_objects(WN_FRAME):
        frame_id = FRAMES_TO_ID[frame.strip()]
        graph.remove((subj, WN_FRAME, frame))
        graph.add((subj, WN_FRAME, frame_id))
    return None


def fix_satellite_lexical_ids(graph):
    # fix lexical ids of satellite senses
    ## must be run after add_source_begin
    synsets = graph.subjects(WN_LEXICOGRAPHER_FILE, Literal("adj.all"))
    english_synsets = filter(lambda s: graph.value(s, WN_LANG).eq(EN), synsets)
    aux = {}
    for synset in sort_synsets(graph, english_synsets):
        for sense in sort_word_senses(graph, synset):
            lexical_form = graph.value(sense, WN_LEXICAL_FORM, any=False)
            new_lexid = aux[lexical_form] = aux.get(lexical_form, -1) + 1
            graph.remove((sense, WN_LEXICAL_ID, None))
            graph.add((sense, WN_LEXICAL_ID, Literal(str(new_lexid))))
    return None

def create_sense_map(graph, sense_map_file):
    with open(sense_map_file, "w") as f:
        for synset, sense in graph.subject_objects(WN_CONTAINS_WORDSENSE):
            if (synset, WN_LANG, EN) in graph:
                synset_id = graph.value(synset, WN_SYNSET_ID, any=False)
                sense_key = graph.value(sense, WN_SENSEKEY, any=False)
                lexicographer_file = graph.value(synset, WN_LEXICOGRAPHER_FILE, any=False)
                lexical_form = graph.value(sense, WN_LEXICAL_FORM, any=False)
                lexical_id = graph.value(sense, WN_LEXICAL_ID, any=False)
                new_sense_id = "{}-{}-{}[{}]".format(EN, lexicographer_file, lexical_form, lexical_id)
                assert all([synset_id, sense_key, lexicographer_file, lexical_form, lexical_id]), sense
                print("{}\t{}\t{}".format(sense_key, synset_id, new_sense_id), file=f)
    return None


def add_wn_name(graph, wn_name):
    # when running only for one language, must add WN_LANG predicate
    # to synsets
    for synset in graph.subjects(WN_LEXICOGRAPHER_FILE):
        graph.add((synset, WN_LANG, Literal(wn_name)))
    return None


def fixes_to_legacy_rdf(graph, config_dir, sense_map_file):
    add_lexical_form_to_senses(graph)
    add_source_begin(graph)
    use_frame_numbers(graph)
    make_similar_to_one_way(graph)
    fix_satellite_lexical_ids(graph)
    add_missing_sensekeys(graph)
    create_sense_map(graph, sense_map_file)
    pick_synset_relation_targets(graph, config_dir)
    fix_glosses(graph)
    return None


@click.command()
@click.argument('rdf_input',
                type=click.File(mode="rb"), required=True)
@click.argument('config_dir',
                type=click.Path(exists=True, file_okay=False, resolve_path=True), required=True)
@click.argument('output_dir'
                , type=click.Path(file_okay=False, resolve_path=True, writable=True), required=True)
@click.argument('sense_map_file',
                type=click.Path(file_okay=True, resolve_path=True, writable=True), required=True)
@click.option('-f', '--rdf-file-format', 'rdf_file_format'
              , type=click.STRING, default='nt', show_default=True,
              help="RDF input format. Must be accepted by RDFlib.")
def main(rdf_input, config_dir, output_dir, sense_map_file, rdf_file_format):
    """Convert RDF_INPUT to lexicographer files placed at OUTPUT_DIR,
according to the configuration files in CONFIG_DIR. Produces sense map
file that is saved to SENSE_MAP_FILE."""
    graph = Graph()
    graph.parse(rdf_input, format=rdf_file_format)
    add_wn_name(graph, EN)
    fixes_to_legacy_rdf(graph, config_dir, sense_map_file)
    rdf2text_go(graph, config_dir, output_dir)
    return None

if __name__ == '__main__':
    main()
