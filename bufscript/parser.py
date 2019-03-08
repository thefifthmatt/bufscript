# -*- coding: utf-8 -*-
"""The parser, which uses ply.

Check out ply documentation for more info. Error handling can potentially be
improved here.
"""
import sys

import ply.yacc as yacc

from bufscript import ast
from bufscript.lexer import tokens  # pylint: disable=unused-import
from bufscript.lexer import find_token_location
from bufscript.optypes import BinaryOp, UnaryOp, LiteralOp, IncDec


precedence = (
    ('nonassoc', 'ASSIGN'),
    ('left', 'OR'),
    ('left', 'AND'),
    ('nonassoc', 'ISEQUAL', 'NOTEQUAL', 'LESSTHAN', 'LESSEQ', 'GREATERTHAN', 'GREATEREQ'),
    ('nonassoc', 'RANGE'),
    ('left', 'PLUS', 'MINUS'),
    ('right', 'NOT'),
    ('right', 'NEGATE'),
    ('left', 'DIV', 'MUL', 'MOD'),
)

def symloc(p, i):
    # Returns (line, column) for the given symbol
    lineno = p.lineno(i)
    last_cr = p.lexer.line_ends[lineno - 1]
    return ast.TextLocation(lineno, p.lexpos(i) - last_cr)

def symrange(p, i, j=None):
    # Returns entire location range either at i only or from i to j.
    # yacc.parse(..., tracking=True) must be used for it to work.
    lstart, lend = p.linespan(i)
    pstart, pend = p.lexspan(i)
    if j is not None:
        _, lend = p.linespan(j)
        _, pend = p.lexspan(j)
    end_loc = (lend, pend - p.lexer.line_ends[lend - 1])
    return ast.TextLocation(lstart, pstart - p.lexer.line_ends[lstart - 1], end_loc)

def p_start(p):
    'start : opteol statements'
    func = ast.FuncDeclaration('command', 'r', p[2], None)
    p[0] = ast.ProgramAst([func])

# For putting multiple functions in a file
# Note that declarations will be reversed
# def p_declarations(p):
#     'declarations : declaration declarations'
#     p[0] = p[2]
#     p[0].append(p[1])
# def p_declarations_empty(p):
#     'declarations : '
#     p[0] = []
# def p_declaration(p):
#     'declaration : funcdeclaration'
#     p[0] = p[1]
# Can also be just a number
# def p_funcdeclaration(p):
#     'funcdeclaration : WORD LEFT_PAREN RIGHT_PAREN statement'
#     p[0] = FuncDeclaration(p[1],'r', p[4], symloc(p, 1))

def p_statement(p):
    '''statement : literal
                 | assignment
                 | function_stmt
                 | if
                 | while
                 | switch
                 | quit
    '''
    p[0] = p[1]

def p_switch(p):
    '''switch : SWITCH parenexpr LEFT_BRACE cases RIGHT_BRACE opteol'''
    p[4].reverse()
    p[0] = ast.SwitchStmt(p[2], p[4])

def p_cases(p):
    '''cases : case cases'''
    p[0] = p[2]
    p[0].append(p[1])

def p_cases_default(p):
    '''cases : case_default'''
    p[0] = [p[1]]

def p_cases_empty(p):
    '''cases : '''
    p[0] = []

def p_case(p):
    'case : CASE exprlist COLON statements'
    p[0] = ast.CaseStmt(p[4], p[2])

def p_case_default(p):
    'case_default : DEFAULT COLON statements'
    p[0] = ast.CaseStmt(p[3], None)

def p_stmt_expr_var_assign(p):
    'assignment : VAR ASSIGN expression endstmt'
    var = ast.VarExpr(p[1])
    p[0] = ast.AssignStmt(var, p[3])
    var.location = symrange(p, 1)

def p_incdec(p):
    '''incdec : INCREMENT
              | DECREMENT'''
    p[0] = IncDec.from_value(p[1])

def p_stmt_expr_var_postincdec(p):
    'assignment : VAR incdec endstmt'
    var = ast.VarExpr(p[1])
    p[0] = ast.PostStmt(var, p[2])
    var.location = symrange(p, 1)

def p_function_stmt(p):
    'function_stmt : function_call endstmt'
    p[0] = ast.FunctionStmt(p[1])
    p[0].location = symrange(p, 1)

def p_endstmt(p):
    '''endstmt : NEWLINE
               | SEMICOLON
    '''
    p[0] = None

def p_opteol(p):
    '''opteol : NEWLINE
              | empty
    '''
    p[0] = None

def p_statement_quit(p):
    'quit : QUIT endstmt'
    p[0] = ast.QuitStmt()

def p_statement_literal(p):
    'literal : LITERAL litpieces endstmt'
    p[2].reverse()
    p[0] = ast.LiteralStmt(
        LiteralOp.from_value(p[1]),
        p[2])

# Literal and string stuff
def p_litpieces(p):
    'litpieces : litpiece litpieces'
    p[0] = p[2]
    p[0].append(p[1])

def p_litpieces_empty(p):
    'litpieces : '
    p[0] = []

def p_litpiece(p):
    '''litpiece : LITERALBIT
                | escape
                | var
                | flag'''
    p[0] = p[1]

def p_string(p):
    'string : STRING strpieces STRING'
    p[2].reverse()
    p[0] = ast.StringExpr(p[2])

def p_strpieces(p):
    'strpieces : strpiece strpieces'
    p[0] = p[2]
    p[0].append(p[1])

def p_strpieces_empty(p):
    'strpieces : '
    p[0] = []

def p_strpiece(p):
    '''strpiece : STRINGBIT
                | escape
                | var
                | flag'''
    p[0] = p[1]

def p_escape(p):
    'escape : ESCAPE'
    p[0] = p[1][1]

def p_escape_bad(p):
    'escape : BADESCAPE'
    line, col = find_token_location(p)
    print('Bad escape "%s" at %d:%d' % (p.value, line, col), file=sys.stderr)
    yacc.has_error = True
    p[0] = p[1]

def p_statements(p):
    '''statements : statements_inner'''
    p[1].reverse()
    p[0] = p[1]

def p_statements_inner(p):
    '''statements_inner : statement statements_inner'''
    p[0] = p[2]
    p[0].append(p[1])

def p_statements_inner_empty(p):
    'statements_inner : '
    p[0] = []

def p_parenexpr(p):
    'parenexpr : LEFT_PAREN expression RIGHT_PAREN'
    p[0] = p[2]

def p_block(p):
    'block : LEFT_BRACE statements RIGHT_BRACE opteol'
    p[0] = p[2]

def p_if(p):
    '''if : IF parenexpr block'''
    p[0] = ast.ConditionalStmt(p[2], p[3], None)

def p_if_else(p):
    'if : IF parenexpr block ELSE block'
    p[0] = ast.ConditionalStmt(p[2], p[3], p[5])

def p_if_else_if(p):
    'if : IF parenexpr block ELSE if'
    p[0] = ast.ConditionalStmt(p[2], p[3], [p[5]])

def p_while(p):
    '''while : WHILE parenexpr block'''
    p[0] = ast.LoopStmt(p[2], p[3])

def p_expression_fold(p):
    # Note: the location field needs to be set for every expression.
    # Every time anything other than 'expression' (this rule) is used
    # in another rule, symrange should be called for it.
    '''expression : op_expr
                  | primary_expr
                  | function_call
                  | var
                  | flag'''
    p[0] = p[1]
    p[0].location = symrange(p, 1)

def p_var(p):
    'var : VAR'
    p[0] = ast.VarExpr(p[1])
    p[0].location = symrange(p, 1)

# Check flag validity later. The separation is so that valid flags
# can be different tokens than what follows them.
def p_flag(p):
    '''flag : FLAG
            | ANYFLAG'''
    p[0] = ast.FlagExpr(p[1])
    p[0].location = symrange(p, 1)

def p_expression_binary(p):
    '''op_expr : expression PLUS expression
               | expression MINUS expression
               | expression MUL expression
               | expression DIV expression
               | expression MOD expression'''
    p[0] = ast.BinaryExpr(BinaryOp.from_value(p[2]), p[1], p[3])

# TODO: Enable these once AST generation is 100% correct for them in all cases
def p_expression_boolean(p):
    '''op_expr : expression AND expression
               | expression OR expression'''
    p[0] = ast.BinaryExpr(BinaryOp.from_value(p[2]), p[1], p[3])

def p_expression_comparison(p):
    '''op_expr : expression ISEQUAL expression
               | expression NOTEQUAL expression
               | expression LESSTHAN expression
               | expression LESSEQ expression
               | expression GREATERTHAN expression
               | expression GREATEREQ expression'''
    p[0] = ast.BinaryExpr(BinaryOp.from_value(p[2]), p[1], p[3])

def p_expression_range(p):
    'op_expr : expression RANGE expression'
    p[0] = ast.BinaryExpr(BinaryOp.from_value(p[2]), p[1], p[3])

def p_expression_negate(p):
    'op_expr : MINUS expression %prec NEGATE'
    p[0] = ast.UnaryExpr(UnaryOp.NEGATE, p[2])

def p_expression_not(p):
    'op_expr : NOT expression'
    p[0] = ast.UnaryExpr(UnaryOp.NOT, p[2])

def p_expression_const(p):
    '''primary_expr : INTCONST
                    | WORD
                    | string'''
    p[0] = ast.ConstExpr(p[1])

def p_expression_true(p):
    'primary_expr : TRUE'
    p[0] = ast.ConstExpr(True)

def p_expression_false(p):
    'primary_expr : FALSE'
    p[0] = ast.ConstExpr(False)

def p_expression_paren(p):
    'primary_expr : LEFT_PAREN expression RIGHT_PAREN'
    p[0] = p[2]

def p_function_call(p):
    'function_call : WORD LEFT_PAREN exprseq RIGHT_PAREN'
    p[0] = ast.FunctionCall(p[1], p[3])
    p[0].location = symrange(p, 1)

def p_exprseq(p):
    '''exprseq : empty_list
               | exprlist'''
    p[0] = p[1]
    p[0].reverse()

def p_exprlist(p):
    'exprlist : expression COMMA exprlist'
    p[0] = p[3]
    p[0].append(p[1])

def p_exprlist_expr(p):
    'exprlist : expression'
    p[0] = [p[1]]

def p_empty(p):
    'empty : '
    p[0] = None

def p_empty_list(p):
    'empty_list : '
    p[0] = []

def p_error(p):
    if not p:
        print('Program ended unexpectedly', file=sys.stderr)
        yacc.has_error = True
        return
    line, col = find_token_location(p)
    # TODO: Should not print this out directly, instead let caller handle it
    print('***********************************************')
    if p.value == '\n':
        print('Syntax error: unexpected newline at %d:%d' % (line, col), file=sys.stderr)
    else:
        print('Syntax error: unexpected "%s" at %d:%d' % (p.value, line, col), file=sys.stderr)
    # Flounder around until we find something.
    # Note: This only works for non-multi mode.
    # TODO: Should add error rules into grammar.
    if p.type not in ['SEMICOLON', 'NEWLINE']:
        while 1:
            tok = _parser.token()
            if not tok or tok.type in ['SEMICOLON', 'NEWLINE']:
                break
    yacc.has_error = True
    _parser.restart()

_parser = yacc.yacc()
