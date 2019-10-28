#!/bin/python3

import json
import os
import rdflib as r
from rdflib import Graph, Namespace
from rdflib.namespace import RDF
from rdflib.term import Literal
import click

# [] join json2rdf and rdf2text into mill.py

WN30 = Namespace("https://w3id.org/own-pt/wn30/schema/")
WN30EN = Namespace("https://w3id.org/own-pt/wn30-en/instances/")
WN30PT = Namespace("https://w3id.org/own-pt/wn30-pt/instances/") # not used (yet)

WN30_LANG = {"en": WN30EN, "pt": WN30PT}

###
## json -> rdf
COMMENT            = "comment"
DEFINITION         = "definition"
EXAMPLE            = "example"
LEXICOGRAPHER_FILE = "lexicographerFile"
ID                 = "id"
EXAMPLES           = "examples"
WORDSENSES         = "wordsenses"
CONTAINS_WORDSENSE = "containsWordSense"
LEXICAL_FORM       = "lexicalForm"
LEXICAL_ID         = "lexicalId"
POINTERS           = "pointers"
NAME               = "name"
SENSEKEY           = "senseKey"
FRAME              = "frame"
FRAMES             = "frames"
WORDSENSE          = "wordsense"
SYNSET             = "synset"
RELATIONS          = "relations"

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
        g.add((head, WN30[FRAME], Literal(frame)))

    def add_word_sense(lang, wordsense, lexicographer_file, synset_id):
        wordsense_id = make_id(lang, lexicographer_file, wordsense[LEXICAL_FORM]
                               , wordsense[LEXICAL_ID], WORDSENSE)
        g.add((synset_id, WN30[CONTAINS_WORDSENSE], wordsense_id))
        g.add((wordsense_id, WN30[LEXICAL_ID], Literal(wordsense[LEXICAL_ID])))
        g.add((wordsense_id, WN30[LEXICAL_FORM], Literal(wordsense[LEXICAL_FORM])))
        g.add((wordsense_id, WN30[SENSEKEY], Literal(wordsense[SENSEKEY])))
        for relation in wordsense[POINTERS]:
            add_relation(wordsense_id, relation, WORDSENSE)
        for frame in wordsense[FRAMES]:
            add_frame(wordsense_id, frame)

    def add_synset(synset):
        lang, synset_id, lexicographer_file, synset_type = parse_id(synset[ID])
        g.add((synset_id, RDF.type, synset_type))
        g.add((synset_id, WN30[LEXICOGRAPHER_FILE], lexicographer_file))
        g.add((synset_id, WN30[DEFINITION], Literal(synset[DEFINITION])))
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
        for frame in synset[FRAMES]:
            add_frame(synset_id, frame)


    g = Graph()
    for synset in synsets_gen:
        add_synset(synset)
    return g

@click.command()
@click.argument('json_input', type=click.File(mode="r"), required=True)
@click.argument('rdf_output', type=click.File(mode="wb"), required=True)
@click.option('-f', '--rdf-format', 'rdf_format'
              , type=click.STRING, default='nt', show_default=True,
              help="RDF output format. Must be accepted by RDFlib.")
def json_to_rdf(json_input, rdf_output, rdf_format='nt'):
    """Convert JSON_INPUT to RDF_OUTPUT."""
    graph = to_graph(from_json(json_input))
    graph.serialize(destination=rdf_output, format=rdf_format)

POS1TOLONG = {"A" : "adj", "N" : "noun", "R" : "adv", "S" : "adjs", "V" : "verb"}
SYNSETTYPE = {"A" : Literal("AdjectiveSynset"), "N" :
              Literal("NounSynset"), "R" : Literal("AdverbSynset"),
              "S" : Literal("AdjectiveSatelliteSynset"), "V" :
              Literal("VerbSynset")}

if __name__ == '__main__':
    json_to_rdf()
