"""Runs the compiler, including a main method.

Example invocation:
$ python -m bufscript.buf examples/p1.buf
"""
import os
import sys

from bufscript import ast
from bufscript import ast_codegen
from bufscript import bsl_codegen
from bufscript import declare
from bufscript import parser
from bufscript import registers
from bufscript import typecheck
from bufscript.util import CompError, print_comp_error


def run(source):
    parser.yacc.has_error = False
    ast_p = parser.yacc.parse(source, tracking=True)
    if parser.yacc.has_error:
        raise CompError("Invalid program")
    #print repr(ast_p)

    ast.visit_ast(declare.DeclarationVisitor(), ast_p)
    ast.visit_ast(typecheck.TypeVisitor(), ast_p)

    conv = ast_codegen.AstCodegen()
    ir_program = conv.generate(ast_p)
    ir_program.merge_empty_blocks()
    ir_program.remove_dead_blocks()

    # TODO: This.
    elems = []  # dataflow.get_undefined(ir_program)
    if len(elems):
        elem = elems[0]
        raise CompError(
            'Variable "%s" is used at %s without being defined first', elem, elem.location.start)
    reg_program = registers.allocate_registers(ir_program)

    return bsl_codegen.output_bsl_codes(reg_program)

def main():
    if len(sys.argv) != 2:
        print('Usage: %s [program]' % sys.argv[0])
        sys.exit(1)

    # Some rereouting I guess? So that 'print' uses stderr
    sys._stdout = sys.stdout  # pylint: disable=protected-access
    sys.stdout = sys.stderr

    source_name = sys.argv[1]
    if source_name == '-' and not sys.stdin.isatty():
        source = sys.stdin.read()
    elif os.path.exists(source_name):
        with open(source_name, 'r') as source_file:
            source = source_file.read()
    else:
        raise CompError("'%s' does not exist", source_name)

    bsl_codes = run(source)

    output = bsl_codes['command'].get_program()

    # Just always do this for now
    if source_name == '-' or True:
        sys._stdout.write(output)  # pylint: disable=protected-access
    else:
        prefix, _ = os.path.splitext(source_name)
        output_name = prefix + '.buf'
        with open(output_name, 'w') as output_file:
            output_file.write(output)


if __name__ == '__main__':
    # Show code pointers or not
    debug = True
    try:
        main()
    except CompError as e:
        print_comp_error(e, debug)
        sys.exit(1)
    except KeyboardInterrupt as e:
        print('Interrupted', file=sys.stderr)
        sys.exit(130)
    except:  # pylint: disable=bare-except
        if debug:
            raise
        print('Internal error', file=sys.stderr)

