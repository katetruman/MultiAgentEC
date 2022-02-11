from pyswip import Prolog
from pyswip import Functor
from pyswip.prolog import PrologError
from pathlib import Path
import re
import sys
from io import StringIO
import contextlib

import logging
import pygraphviz as pgv
from graphviz import Digraph
from IPython.display import Image
from IPython.display import SVG, display
def draw(dot):
    return Image(pgv.AGraph(dot).draw(format='png', prog='dot'))

@contextlib.contextmanager
def stdoutIO(stdout=None):
    old = sys.stdout
    if stdout is None:
        stdout = StringIO()
    sys.stdout = stdout
    yield stdout
    sys.stdout = old


DEFAULT_LIMIT = 25

def format_value(value):
    output = ""
    if isinstance(value, list):
        output = "[ " + ", ".join([format_value(val) for val in value]) + " ]"
    elif isinstance(value, Functor) and value.arity == 2:
        output = "{0}{1}{2}".format(value.args[0], value.name, value.args[1])
    else:
        output = "{}".format(value)

    return output

def format_result(result):
    result = list(result)

    if len(result) == 0:
        return "false."

    if len(result) == 1 and len(result[0]) == 0:
        return "true."

    output = ""
    for res in result:
        tmpOutput = []
        for var in res:
            tmpOutput.append(var + " = " + format_value(res[var]))
        output += ", ".join(tmpOutput) + " ;\n"
    output = output[:-3] + " ."
    return output
magic_python_local_vars = {}
magic_python_global_vars={}
def run(code):
    global magic_python_local_vars
    global magic_python_global_vars
    prolog = Prolog()
    output = []
    python = False
    ok = True
    tmp = ""  # Temporary string to house working
    clauses = [] # A list of clauses from this cell, ignoring queries and comments
    isQuery = False # Boolean to check if a line is a query
    cell_files_dir = Path(Path.cwd(), "consulted_cells")
    output_files_dir = Path(Path.cwd(), "output_files")
    cell_files_dir.mkdir(mode=755, exist_ok=True)
    output_files_dir.mkdir(mode=755, exist_ok=True)
    cell_file_name = "cell.pl" # The default consultation cell name, used if no %file is used
    output_file_name = ""
    first_line = code.split("\n")[0].strip().upper()
    if re.match(r"%\s*PYTHON.*", first_line) is not None:
        # Execute each line in turn, ignoring the first (%PYTHON)
        code = "\n".join(code.split("\n")[1:])
        with stdoutIO() as s:
        # Handle errors being thrown out the wazoo
            try:
                # Execute this line with the local dictionary context
                exec(code, magic_python_global_vars, magic_python_local_vars)
            except Exception as e:
                output.append(f"ERROR: Script gave error {e}")
        line_out = s.getvalue().strip()
        if len(line_out)>0:
            output.append(line_out)
        return output, True
    for line in code.split("\n"):
        line = line.strip()
        match = re.fullmatch(r"%\s*[Ff]ile:\s*(\w+.*)", line)
        outputMatch = re.fullmatch(r"%\s*[Oo]utput:\s*(\w+.*)", line)
        if match is not None:
            # Get the specified name, the first group, after the %file:
            cell_file_name = match.group(1)
            # If the name does NOT end with .pl, make it end so
            if not cell_file_name.endswith(".pl"):
                cell_file_name += ".pl" 
        if outputMatch is not None:
            output_file_name = outputMatch.group(1)
        # If the line is empty or a comment, do nothing
        if line == "" or line[0] == "%":
            continue
        # If line is a query, do that instead but don't save it
        if line[:2] == "?-":
            isQuery = True
            line = line[2:]
            tmp += " " + line
        # Line is a clause, save it
        else:
            clauses.append(line)
        # If line a query, and that query is actually executed
        if isQuery and tmp[-1] == ".":
            # End of statement
            tmp = tmp[:-1] # Removes "."
            maxresults = DEFAULT_LIMIT
            # If ending in a }, check if there is no matching {
            # Then check if prolog spat an error to tell us the limit is bad
            if tmp[-1] == "}":
                tmp = tmp[:-1] # Removes "}"
                limitStart = tmp.rfind('{')
                if limitStart == -1:
                    ok = False
                    output.append("ERROR: Found '}' before '.' but opening '{' is missing!")
                else:
                    limit = tmp[limitStart+1:]
                    try:
                        maxresults = int(limit)
                    except:
                        ok = False
                        output.append("ERROR: Invalid limit {" + limit + "}!")
                    tmp = tmp[:limitStart]
            # Actually try the query
            try:
                if isQuery:
                    result = prolog.query(tmp, maxresult=maxresults)
                    output.append(format_result(result))
                    result.close()

            except PrologError as error:
                ok = False
                output.append("ERROR: {}".format(error))
            tmp = ""
            isQuery = False
    # If there are some clauses to look at
    if len(clauses) > 0:
        try:
            c = open(Path(cell_files_dir, cell_file_name), 'w+')
            o = open(Path(output_files_dir, "allCells.pl"), 'a+')
            c.write('\n'.join(clauses))
            #o.write('\n'.join(clauses) + "\n")
            o.write("%s File: %s\n%s\n\n" % (r"%",cell_file_name, '\n'.join(clauses)))

        finally:
            c.close()
            o.close()
            prolog.consult(c.name)
    # If there is some output
    if len(output) > 0:
        # If we wish to save output to a file
        if output_file_name != "":
            try:
                qo = open(Path(output_files_dir, output_file_name), 'w+')
                for o in output:
                    qo.write(o)
                    qo.write("\n")
            finally:
                qo.close()
    return output, ok
