from rdflib import Graph
from rdflib.term import Literal, URIRef
from rdflib.namespace import RDF, OWL, split_uri
import click
import json
import sys

RDF_TYPE = RDF["type"]
OWL_SAME_AS = OWL["sameAs"]


def get_stats(graph):
    def init():
        r = {}
        r["elems"] = {}
        r["types"] = {}
        return r

    def do_value(thing, r):
        r_types = r["types"]
        r_elems = r["elems"]
        unique = True
        if isinstance(thing, Literal):
            r_types["Literal"] = r_types.get("Literal", 0) + 1
            # not sure if worthwhile to count unique literals
        else:  # is IRI
            for iri_type in graph.objects(thing, RDF_TYPE):
                r_types[iri_type] = r_types.get(iri_type, 0) + 1
            for same in graph.objects(thing, OWL_SAME_AS):
                if same in r_elems:
                    unique = False
        unique = thing not in r_elems and unique
        if unique:
            r_elems[thing] = True
        return r

    def close(r):
        elems = r.pop("elems")
        r["uniques"] = len(elems)
        return r

    stats = []
    for pred in set(graph.predicates(None, None)):
        subj_stats = init()
        obj_stats = init()
        n = 0
        for subj, obj in graph.subject_objects(pred):
            # subject
            subj_stats = do_value(subj, subj_stats)
            # object
            obj_stats = do_value(obj, obj_stats)
            n += 1
        # finally
        close(subj_stats)
        close(obj_stats)
        stats.append({"pred": pred, "subjs": subj_stats, "objs": obj_stats,
                      "n": n})
    return stats


def print_table(myDict, colList=None, fp=sys.stdout):
    """
    Pretty print a list of dictionaries (myDict) as a dynamically sized table.
    If column names (colList) aren't specified, they will show in random order.
    Author: Thierry Husson - Use it as you want but don't blame me.
    """
    if not colList:
        colList = list(myDict[0].keys() if myDict else [])
    myList = [colList]  # 1st row = header
    for item in myDict:
        myList.append([str(item[col] if item[col] is not None else '')
                      for col in colList])
    colSize = [max(map(len, col)) for col in zip(*myList)]
    formatStr = ' | '.join(["{{:<{}}}".format(i) for i in colSize])
    myList.insert(1, ['-' * i for i in colSize])  # Seperating line
    for item in myList:
        print(formatStr.format(*item), file=fp)


def make_human_readable(stats, out):
    def format_type(type_name, n):
        if isinstance(type_name, URIRef):
            _, type_name = split_uri(type_name)
        return "{}[{}]".format(type_name, n)

    def print_types(types):
        types = types.items()
        [type_names, ns] = [[i for i, _ in types],
                            [j for _, j in types]]
        return ", ".join(map(format_type, type_names, ns))

    def go(pred):
        r.append({"predicate": split_uri(pred["pred"])[1],
                  "n": pred["n"],
                  "unique_subj_n": pred["subjs"]["uniques"],
                  "unique_obj_n": pred["objs"]["uniques"],
                  "subj_types": print_types(pred["subjs"]["types"]),
                  "obj_types": print_types(pred["objs"]["types"])})
        return None

    r = []
    for pred_stats in stats:
        go(pred_stats)
    print_table(r, fp=out)
    return None


@click.command()
@click.argument('rdf_input',
                type=click.File(mode="rb"), required=True)
@click.argument('output_file',
                type=click.File(mode="w"), required=True)
@click.option('-f', '--rdf-file-format', 'rdf_file_format',
              type=click.STRING, default='nt', show_default=True,
              help="RDF input format. Must be accepted by RDFlib.")
@click.option('-h/-m', '--human-readable/--machine-readeable',
              'human_readable', default=True, show_default=True,
              help="Output table meant for human consumption, or JSON")
def main(rdf_input, output_file, rdf_file_format, human_readable):
    graph = Graph()
    graph.parse(rdf_input, format=rdf_file_format)
    stats = get_stats(graph)
    if human_readable:
        make_human_readable(stats, output_file)
    else:
        json.dump(stats, output_file, ensure_ascii=False)
    return None


if __name__ == '__main__':
    main()
