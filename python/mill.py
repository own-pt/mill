#!/bin/python3

import json
import os
import rdflib as r
from rdflib import Graph, Namespace
from rdflib.namespace import RDF
from rdflib.term import Literal
import click
from itertools import chain

###
## constants

# [] RDF configuration -> customize IRIs
## [] remove hardcoded stuff
# [] read configuration using labels, not harcoded field indices

WN30 = Namespace("https://w3id.org/own-pt/wn30/schema/")
WN30EN = Namespace("https://w3id.org/own-pt/wn30-en/instances/")
WN30PT = Namespace("https://w3id.org/own-pt/wn30-pt/instances/") # not used (yet)

WN30_LANG = {"en": WN30EN, "pt": WN30PT}

POS1TOLONG = {"A" : "adj", "N" : "noun", "R" : "adv", "S" : "adjs", "V" : "verb"}
SYNSETTYPE = {"A" : Literal("AdjectiveSynset"), "N" :
              Literal("NounSynset"), "R" : Literal("AdverbSynset"),
              "S" : Literal("AdjectiveSatelliteSynset"), "V" :
              Literal("VerbSynset")}

COMMENT            = "comment"
COMMENTS           = "comments"
DEFINITION         = "definition"
EXAMPLE            = "example"
LANG               = "lang"
LEXICOGRAPHER_FILE = "lexicographerFile"
ID                 = "id"
EXAMPLES           = "examples"
WORDSENSES         = "wordsenses"
CONTAINS_WORDSENSE = "containsWordSense"
LEXICAL_FORM       = "lexicalForm"
LEXICAL_ID         = "lexicalId"
POINTERS           = "pointers"
POSITION           = "position"
NAME               = "name"
SENSEKEY           = "senseKey"
FRAME              = "frame"
FRAMES             = "frames"
WSENSE             = "wordsense"
SYNSET             = "synset"
RELATIONS          = "relations"
SOURCE_BEGIN       = "sourceBegin"
SOURCE_END         = "sourceEnd"
SAME_AS            = "sameAs"
SYNTACTIC_MARKER   = "syntacticMarker"

CURRENT_LANG=None # initialized during runtime

SYNSET_RELATIONS, WORD_RELATIONS = {}, {}

WN_FRAME = WN30[FRAME]
WN_LEXICOGRAPHER_FILE = WN30[LEXICOGRAPHER_FILE]
WN_LANG = WN30[LANG]
WN_CONTAINS_WORDSENSE = WN30[CONTAINS_WORDSENSE]
WN_SAME_AS = WN30[SAME_AS]
WN_SOURCE_BEGIN = WN30[SOURCE_BEGIN]
WN_LEXICAL_ID = WN30[LEXICAL_ID]
WN_LEXICAL_FORM = WN30[LEXICAL_FORM]
WN_DEFINITION = WN30[DEFINITION]
WN_COMMENT = WN30[COMMENT]
WN_PERTAINS_TO = WN30["pertainsTo"]
WN_HYPONYM_OF = WN30["hyponymOf"]
WN_INSTANCE_OF = WN30["instanceOf"]
WN_ENTAILS = WN30["entails"]
WN_ANTONYM_OF = WN30["antonymOf"]
WN_SIMILAR_TO = WN30["similarTo"]
WN_SYNTACTIC_MARKER = WN30[SYNTACTIC_MARKER]
WN_EXAMPLE = WN30[EXAMPLE]

###
## rdf -> text
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
        relation_name = fields[3]
        relation_text = fields[2]
        if relation_text != "_":
            if fields[5] == "word":
                word_relations[relation_name] = relation_text
            elif fields[5] == "synset":
                synset_relations[relation_name] = relation_text
            else:
                word_relations[relation_name] = relation_text
                synset_relations[relation_name] = relation_text
        return (synset_relations, word_relations)
    #
    synset_relations, word_relations = read_tsv(os.path.join(
        config_dir, "relations.tsv"), read_relations, ({}, {}))
    return (synset_relations, word_relations)


def print_graph(graph, output_dir):
    global CURRENT_LANG
    lex_files = set(graph.objects(predicate=WN_LEXICOGRAPHER_FILE))
    langs     = set(graph.objects(predicate=WN_LANG))
    for lang in langs:
        CURRENT_LANG = lang
        lang_output_dir = os.path.join(output_dir, lang)
        os.makedirs(lang_output_dir, exist_ok=True)
        print(lang)
        for lexicographer_file in lex_files:
            print_lexfile(graph, lexicographer_file, lang_output_dir)
            print("  {}".format(lexicographer_file))

def summarize(graph, node):
    print(node)
    print("  subject")
    for pred, obj in graph.predicate_objects(node):
        print("    {} {}".format(r.namespace.split_uri(pred)[1], obj))
    print("  object")
    for subj, pred in graph.subject_predicates(node):
        print("    {} {}".format(subj, r.namespace.split_uri(pred)[1]))
    return None

# def find_inter(graph):
#     # for debugging: find relation between english sense and pt sense
#     for subj, obj in graph.subject_objects(WN_TEMP_ID):
#         subj_synset = graph.value(None, WN_CONTAINS_WORDSENSE, subj)
#         obj_synset = graph.value(None, WN_CONTAINS_WORDSENSE, obj)
#         subj_lang = graph.value(subj_synset, WN_LANG)
#         obj_lang = graph.value(obj_synset, WN_LANG)
#         assert all([subj_synset, subj_lang, obj_synset, obj_lang]), (subj, obj)
#         if subj_lang != Literal("pt") and subj_lang != obj_lang:
#             summarize(subj)
#             summarize(obj)
#             break
#     return None

def sort_word_senses(graph, synset):
    def word_sense_form(ws):
        return graph.value(ws, WN_LEXICAL_FORM, any=False)
    #
    wordsenses = list(graph.objects(synset, WN_CONTAINS_WORDSENSE))
    if wordsenses:
        return sorted(wordsenses, key=word_sense_form)
    else:
        print("missing ws for {}".format(synset))


def sort_synsets(graph, synsets):
    result = sorted(synsets, key=lambda s: graph.value(s, WN_SOURCE_BEGIN))
    return result


def print_lexfile(graph, lexicographer_file, output_dir):
    lexfile_synsets = graph.subjects(predicate=WN_LEXICOGRAPHER_FILE, object=lexicographer_file)
    lang_synsets = list(filter(lambda s: graph.value(s, WN_LANG) == CURRENT_LANG,
                               lexfile_synsets))
    if lang_synsets:
        with open(os.path.join(output_dir, lexicographer_file), 'w+') as output_stream:
            write = lambda data, *args, **kwargs: print(data, file=output_stream,
                                                        *args, **kwargs)
            pos, lexname = lexicographer_file.split(".")
            write("{}.{}".format(pos, lexname), end="\n\n")
            for synset in sort_synsets(graph, lang_synsets):
                print_synset(graph, synset, lexicographer_file, write)


###
## new id scheme
def get_clashes(graph, lexicographer_file, synset, word_sense):
    # returns synsets in same lexicographer file with a word sense
    # with the same lexical form
    clashes = []
    lexical_form = graph.value(word_sense, WN_LEXICAL_FORM)
    for other_word_sense in graph.subjects(WN_LEXICAL_FORM, lexical_form):
        other_synset = graph.value(predicate=WN_CONTAINS_WORDSENSE,
                                   object=other_word_sense)
        if synset != other_synset:
            other_lexfile = graph.value(other_synset, WN_LEXICOGRAPHER_FILE)
            if other_lexfile == lexicographer_file:
                clashes.append(other_synset)
    return clashes

WN_ID_RELATION = WN30['idRelation']
WN_ID_TARGET = WN30['idTarget']
WN_ID_WORDSENSE = WN30['idWordSense']
WN_TEMP_ID = WN30['tempId']
WN_SYNONYM_OF = WN30["synonymOf"]
WN_SENSEKEY = WN30[SENSEKEY]
WN_SYNSET_ID = WN30["synsetId"]

PREFERENTIAL_RELATION_MAP = {
    WN_SIMILAR_TO  : True,
    WN_ANTONYM_OF  : True,
    WN_PERTAINS_TO : True,
    WN_SIMILAR_TO  : True,
    WN_INSTANCE_OF : True,
    WN_HYPONYM_OF  : True,
    WN_HYPONYM_OF  : True,
    WN_ENTAILS     : True,
    WN_PERTAINS_TO : True
}

INDISTINGUISHABLES = 0

def relation_preference(relation_obj):
    return not PREFERENTIAL_RELATION_MAP.get(relation_obj[0], False)

def pick_word_sense_relation_id(graph, lexicographer_file, word_sense, synset):
    # relation_id is tuple of relation name and either synset or sense
    def is_clash(relation, target_lexical_form):
        for other_synset in clashes:
            # search in synset relations
            for other_target in graph.objects(other_synset, relation):
                for other_sense in graph.objects(other_target, WN_CONTAINS_WORDSENSE):
                    other_target_lexical_form = graph.value(other_sense, WN_LEXICAL_FORM)
                    if target_lexical_form == other_target_lexical_form:
                        return True # clashed
            # search in sense relations
            for other_word_sense in graph.objects(other_synset, WN_CONTAINS_WORDSENSE):
                for other_target in graph.objects(other_word_sense, relation):
                    other_target_lexical_form = graph.value(other_target, WN_LEXICAL_FORM)
                    if target_lexical_form == other_target_lexical_form:
                        return True # clashed
        return False
    #
    def pick_lexical_id_relation(index):
        lexical_id_form = Literal(str(index))
        lexical_id_senses = graph.subjects(WN_LEXICAL_FORM, lexical_id_form)
        for lexical_id_sense in lexical_id_senses:
            lexical_id_synset = graph.value(predicate=WN_CONTAINS_WORDSENSE,
                                            object=lexical_id_sense, any=False)
            lexical_id_lang = graph.value(lexical_id_synset, WN_LANG, any=False)
            synset_lang = graph.value(synset, WN_LANG, any=False)
            assert lexical_id_lang and synset_lang, lexical_id_sense
            if lexical_id_lang == synset_lang:
                if not is_clash(WN_TEMP_ID, lexical_id_form):
                    return lexical_id_sense
    #
    def find_any_relation():
        global INDISTINGUISHABLES
        id_relation = None
        # try synset relations
        synset_relations = filter(lambda r_o: r_o[0] != WN_CONTAINS_WORDSENSE, graph.predicate_objects(synset))
        sense_relations  = list(graph.predicate_objects(word_sense))
        for relation, obj in sorted(chain(synset_relations, sense_relations), key=relation_preference):
            target_senses = list(graph.objects(obj, WN_CONTAINS_WORDSENSE))
            # obj is synset
            if target_senses:
                # for each target synset relation
                clashed = False
                for sense in target_senses:
                    # for each sense of relation target synset
                    sense_lexical_form = graph.value(sense, WN_LEXICAL_FORM)
                    clashed = is_clash(relation, sense_lexical_form) or clashed
                if not clashed:
                    return (relation, obj)
            # obj is sense
            elif graph.value(predicate=WN_CONTAINS_WORDSENSE, object=obj):
                obj_lexical_form = graph.value(obj, WN_LEXICAL_FORM)
                if not is_clash(relation, obj_lexical_form):
                    return (relation, obj)
        # last resort: create artificial lexicalId relation
        INDISTINGUISHABLES += 1
        lexical_id = graph.value(word_sense, WN_LEXICAL_ID)
        lexical_id_sense = None
        for i in chain([int(lexical_id)], range(0, 30)):
            lexical_id_sense = pick_lexical_id_relation(i)
            if lexical_id_sense:
                graph.set((word_sense, WN_TEMP_ID, lexical_id_sense))
                return (WN_TEMP_ID, lexical_id_sense)
        assert False, word_sense
    #
    def synonym_clash(sibling_lexical_form):
        for other_synset in clashes:
            for other_sense in graph.objects(other_synset, WN_CONTAINS_WORDSENSE):
                other_lexical_form = graph.value(other_sense, WN_LEXICAL_FORM)
                if other_lexical_form == sibling_lexical_form:
                    # clash
                    return True
        return False
    def find_id_relation():
        if clashes:
            # try distinguishing by sibling senses
            for sibling_sense in graph.objects(synset, WN_CONTAINS_WORDSENSE):
                if sibling_sense != word_sense:
                    sibling_lexical_form = graph.value(sibling_sense, WN_LEXICAL_FORM)
                    assert sibling_lexical_form
                    clash = synonym_clash(sibling_lexical_form)
                    if not clash:
                        return (WN_SYNONYM_OF, sibling_sense)
            # try other discriminants
            return find_any_relation()
        else: # no clashes
            return None
    #
    clashes = get_clashes(graph, lexicographer_file, synset, word_sense)
    return find_id_relation()

def pick_synset_ids(graph, lexicographer_file, synset):
    wordsense_as_id = None
    for word_sense in graph.objects(synset, WN_CONTAINS_WORDSENSE):
        word_sense_id = pick_word_sense_relation_id(graph, lexicographer_file, word_sense, synset)
        if word_sense_id:
            assert None not in word_sense_id, word_sense
            (id_relation, id_target) = word_sense_id
            graph.set((word_sense, WN_ID_RELATION, id_relation))
            graph.set((word_sense, WN_ID_TARGET, id_target))
        else:
            # if a sense has no clashes make it the ID sense for that
            # synset
            wordsense_as_id = word_sense
    if not wordsense_as_id:
        wordsense_as_id = graph.value(synset, WN_CONTAINS_WORDSENSE)
    graph.set((synset, WN_ID_WORDSENSE, wordsense_as_id))
    return None

def pick_wn_ids(graph):
    graph.remove((None, WN_TEMP_ID, None))
    graph.remove((None, WN_ID_RELATION,  None))
    graph.remove((None, WN_ID_WORDSENSE, None))
    graph.remove((None, WN_ID_TARGET,    None))
    lex_files = set(graph.objects(predicate=WN_LEXICOGRAPHER_FILE))
    for lex_file in lex_files:
        for synset in graph.subjects(WN_LEXICOGRAPHER_FILE, lex_file):
            pick_synset_ids(graph, lex_file, synset)
    return None

def print_id_map(graph, output_file):
    # include PoS information? (as synset-id doesn't include PoS)
    with open(output_file, 'w+') as output_stream:
        for synset, lexicographer_file in graph.subject_objects(WN_LEXICOGRAPHER_FILE):
            if (synset, WN_LANG, Literal("en")):
                synset_id = graph.value(synset, WN_SYNSET_ID)
                assert synset_id, synset
                for sense in graph.objects(synset, WN_CONTAINS_WORDSENSE):
                    sense_key = graph.value(sense, WN_SENSEKEY)
                    assert sense_key, sense
                    sense_lexical_form = graph.value(sense, WN_LEXICAL_FORM)
                    assert sense_lexical_form, sense
                    sense_id_relation = graph.value(sense, WN_ID_RELATION)
                    if sense_id_relation:
                        _, sense_id_relation = r.namespace.split_uri(sense_id_relation)
                    sense_id_target = graph.value(sense, WN_ID_TARGET)
                    if (sense_id_target, WN_CONTAINS_WORDSENSE, None) in graph:
                        sense_id_target = graph.value(sense_id_target, WN_ID_WORDSENSE)
                    sense_id_target_lexical_form = graph.value(sense_id_target, WN_LEXICAL_FORM)
                    assert sense_id_relation and sense_id_target_lexical_form if sense_id_relation or sense_id_target_lexical_form else True, sense
                    print(sense_key, synset_id, lexicographer_file, sense_lexical_form,
                          sense_id_relation, sense_id_target_lexical_form,
                          sep='\t', file=output_stream)
    return None

###

def word_sense_id(graph, lexicographer_file, word_sense, synset=None):
    def get_id_relation():
        id_relation = graph.value(word_sense, WN_ID_RELATION)
        id_target = graph.value(word_sense, WN_ID_TARGET)
        if id_relation and id_target:
            _, name = r.namespace.split_uri(id_relation)
            id_relation_txt_name = SYNSET_RELATIONS.get(name, False) or WORD_RELATIONS.get(name)
            id_sense = graph.value(id_target, WN_ID_WORDSENSE)
            if id_sense:
                id_target = graph.value(id_sense, WN_LEXICAL_FORM)
                assert id_target, id_sense
            else:
                id_target = graph.value(id_target, WN_LEXICAL_FORM)
            return (id_relation_txt_name, id_target)
        elif id_relation or id_target:
            assert False
        else:
            return None
    synset = synset or graph.value(predicate=WN_CONTAINS_WORDSENSE, object=word_sense)
    lexical_form = graph.value(word_sense, WN_LEXICAL_FORM)
    in_lang = graph.value(synset, WN_LANG)
    assert in_lang, word_sense
    assert lexical_form, word_sense
    return (in_lang, lexicographer_file, lexical_form, get_id_relation())

def synset_id(graph, lexicographer_file, synset):
    word_sense = graph.value(synset, WN_ID_WORDSENSE)
    return word_sense_id(graph, lexicographer_file, word_sense, synset)

def print_word_sense_id(wordsense_id, lexicographer_file=None):
    (in_lang, in_lexfile, lexical_form, id_relation) = wordsense_id
    lexfile_str = "{}:".format(in_lexfile) if in_lexfile.neq(lexicographer_file) else ""
    prefix = "{}".format(lexfile_str) if in_lang == CURRENT_LANG else "@{}:{}:".format(in_lang, in_lexfile)
    id_relation_txt = ""
    if id_relation:
        (predicate, target_lexical_form) = id_relation
        id_relation_txt = "({} {})".format(predicate, target_lexical_form)
    return "{}{}{}".format(prefix, lexical_form, id_relation_txt)



def print_synset(graph, synset, lexicographer_file, write):
    def print_relations():
        def print_relation(name, wordsense_id):
            return "{}: {}".format(name,
                                   print_word_sense_id(wordsense_id, lexicographer_file))
        rels = []
        for predicate, obj in graph.predicate_objects(synset):
            _, predicate_name = r.namespace.split_uri(predicate)
            predicate_txt_name = SYNSET_RELATIONS.get(predicate_name, None)
            obj_is_synset = graph.value(obj, WN_CONTAINS_WORDSENSE)
            if predicate_txt_name:
                if obj_is_synset:
                    target_lexfile = graph.value(obj, WN_LEXICOGRAPHER_FILE)
                    rels.append((predicate_txt_name,
                                 synset_id(graph, target_lexfile, obj)))
            else:
                if obj_is_synset:
                    raise LookupError("{} relation not found".format(predicate_name))
        rels.sort(key=relation_sort_key)
        for (relation_name, target) in rels:
            write(print_relation(relation_name, target))
    #
    comment = graph.value(synset, WN_COMMENT, any=False)
    if comment:
        write("# " + comment.replace("\n", "\n# "))
    sorted_word_senses = sort_word_senses(graph, synset)
    for word_sense in sorted_word_senses:
        print_word_sense(graph, word_sense, lexicographer_file, write)
    # definition
    definition = graph.value(synset, WN_DEFINITION, any=False)
    if definition:
        write("{}: {}".format(SYNSET_RELATIONS[DEFINITION], definition))
    else:
        raise LookupError("No definition for synset")
    # examples
    examples = graph.objects(synset, WN_EXAMPLE)
    sorted_examples = sorted(examples)
    for example in sorted_examples:
        write("{}: {}".format(SYNSET_RELATIONS[EXAMPLE], example))
    # frames
    frame_ids = list(graph.objects(synset, WN_FRAME))
    sorted_frame_ids = sorted(frame_ids)
    if frame_ids:
        write("{}: {}".format(SYNSET_RELATIONS[FRAME], " ".join(sorted_frame_ids)))
    print_relations()
    write("")

def relation_sort_key(wordsense_id):
    (relname, (wn_name, lexicographer_file, lexical_form, maybe_id_relation)) = wordsense_id
    if maybe_id_relation and None in maybe_id_relation:
        print(wordsense_id)
    id_relation = maybe_id_relation or ("", "")
    res = (relname, wn_name, lexicographer_file, lexical_form, id_relation)
    return res


def print_word_sense(graph, word_sense, lexicographer_file, write):
    def print_word_relations():
        def print_word_relation(name, wordsense_id):
            return " {} {}".format(name,
                                   print_word_sense_id(wordsense_id, lexicographer_file))
        frames = []
        relations = []
        markers = []
        for predicate, obj in graph.predicate_objects(word_sense):
            _, predicate_name = r.namespace.split_uri(predicate)
            predicate_txt_name = WORD_RELATIONS.get(predicate_name, None)
            if predicate_name == "frame":
                frames.append(obj)
            elif predicate_name == "syntacticMarker":
                markers.append(obj)
            elif predicate_txt_name:
                synset = graph.value(predicate=WN_CONTAINS_WORDSENSE, object=obj)
                if synset:
                    # obj is wordsense too
                    target_lexfile = graph.value(synset, WN_LEXICOGRAPHER_FILE)
                    relations.append((predicate_txt_name,
                                      word_sense_id(graph, target_lexfile, obj)))
                # else:
                #     raise LookupError("relation {} target {} not wordsense".format(predicate_name, obj))
        if frames:
            frames.sort()
            write(" {} {}".format(
                WORD_RELATIONS[FRAME], " ".join(frames)), end="")
        if markers:
            [marker] = markers  # check that there is only one marker
            write(" {} {}".format(
                WORD_RELATIONS[SYNTACTIC_MARKER], marker), end="")
        relations.sort(key=relation_sort_key)
        for relation_name, target in relations:
            write(print_word_relation(relation_name, target), end="")
        write("")
    #
    lexical_form = graph.value(word_sense, WN_LEXICAL_FORM)
    write("{}: {}".format(SYNSET_RELATIONS[CONTAINS_WORDSENSE], lexical_form),
          end="")
    print_word_relations()



###
## json -> rdf
def from_json(json_input):
    for line in json_input:
        yield json.loads(line)

def to_graph(synsets_gen):
    def make_id(lang, lexicographer_file, lexical_form, lexical_id, obj=SYNSET):
        return WN30_LANG[lang]["{}-{}-{}-{}".format(obj, lexicographer_file,
                                                    lexical_form, lexical_id)]

    def parse_id(id_array, obj=SYNSET):
        [lang, pos, lexname, lexical_form, lexical_id] = id_array
        lexicographer_file = "{}.{}".format(POS1TOLONG[pos], lexname)
        obj_id = make_id(lang, lexicographer_file, lexical_form, lexical_id, obj)
        return lang, obj_id, Literal(lexicographer_file), SYNSETTYPE[pos]

    def add_relation(head, relation, obj=SYNSET):
        _, obj_id, _, _ = parse_id(relation[ID], obj)
        g.add((head, WN30[relation[NAME]], obj_id))

    def add_frame(head, frame):
        g.add((head, WN_FRAME, Literal(frame)))

    def add_word_sense(lang, wordsense, lexicographer_file, synset_id):
        wordsense_id = make_id(lang, lexicographer_file, wordsense[LEXICAL_FORM]
                               , wordsense[LEXICAL_ID], WSENSE)
        g.add((synset_id, WN30[CONTAINS_WORDSENSE], wordsense_id))
        g.add((wordsense_id, WN30[LEXICAL_ID], Literal(wordsense[LEXICAL_ID])))
        g.add((wordsense_id, WN30[LEXICAL_FORM], Literal(wordsense[LEXICAL_FORM])))
        g.add((wordsense_id, WN30[SENSEKEY], Literal(wordsense[SENSEKEY])))
        syntacticMarker = wordsense.get(SYNTACTIC_MARKER)
        if syntacticMarker:
            g.add((wordsense_id, WN_SYNTACTIC_MARKER, Literal(syntacticMarker)))
        for relation in wordsense[POINTERS]:
            add_relation(wordsense_id, relation, WSENSE)
        frames = wordsense.get(FRAMES, [])
        for frame in frames:
            add_frame(wordsense_id, frame)

    def add_synset(synset):
        lang, synset_id, lexicographer_file, synset_type = parse_id(synset[ID])
        g.add((synset_id, WN30[LANG], Literal(lang)))
        g.add((synset_id, RDF.type, synset_type))
        g.add((synset_id, WN30[LEXICOGRAPHER_FILE], lexicographer_file))
        g.add((synset_id, WN30[DEFINITION], Literal(synset[DEFINITION])))
        [begin, end] = synset[POSITION]
        g.add((synset_id, WN30[SOURCE_BEGIN], Literal(begin)))
        g.add((synset_id, WN30[SOURCE_END], Literal(end)))
        comments = synset[COMMENTS]
        if comments:
            comment = "\n".join(comments)
            g.add((synset_id, WN30[COMMENT], Literal(comment)))
        for example in synset[EXAMPLES]:
            g.add((synset_id, WN30[EXAMPLE], Literal(example)))
        for wordsense in synset[WORDSENSES]:
            add_word_sense(lang, wordsense, lexicographer_file, synset_id),
        for relation in synset[RELATIONS]:
            add_relation(synset_id, relation)
        frames = wordsense.get(FRAMES, [])
        for frame in frames:
            add_frame(synset_id, frame)
    #
    g = Graph()
    for synset in synsets_gen:
        add_synset(synset)
    return g

###
## CLI
@click.group()
def cli():
    return None

@cli.command()
@click.argument('json_input', type=click.File(mode="r"), required=True)
@click.argument('rdf_output', type=click.File(mode="wb"), required=True)
@click.option('-f', '--rdf-format', 'rdf_format'
              , type=click.STRING, default='nt', show_default=True,
              help="RDF output format. Must be accepted by RDFlib.")
def json2rdf(json_input, rdf_output, rdf_format='nt'):
    """Convert JSON_INPUT to RDF_OUTPUT."""
    graph = to_graph(from_json(json_input))
    graph.serialize(destination=rdf_output, format=rdf_format)


@cli.command()
@click.argument('rdf_input',
                type=click.File(mode="rb"), required=True)
@click.argument('config_dir',
                type=click.Path(exists=True, file_okay=False, resolve_path=True), required=True)
@click.argument('output_dir'
                , type=click.Path(file_okay=False, resolve_path=True, writable=True), required=True)
@click.option('-f', '--rdf-file-format', 'rdf_file_format'
              , type=click.STRING, default='nt', show_default=True,
              help="RDF input format. Must be accepted by RDFlib.")
def rdf2text(rdf_input, config_dir, output_dir, rdf_file_format="nt"):
    """Convert RDF_INPUT to lexicographer files placed at OUTPUT_DIR,
according to the configuration files in CONFIG_DIR."""
    global SYNSET_RELATIONS, WORD_RELATIONS
    (SYNSET_RELATIONS, WORD_RELATIONS) = read_config(config_dir)
    graph = Graph()
    graph.parse(rdf_input, format=rdf_file_format)
    print_graph(graph, output_dir)

if __name__ == '__main__':
    cli()
