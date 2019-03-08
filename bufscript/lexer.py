# -*- coding: utf-8 -*-
# pylint: disable=global-statement,invalid-name,missing-docstring
"""The lexer, which uses ply.

There are 3 lexer modes: normal (initial), string-parsing, and literal-parsing.
Literal means either msg (bot output) or call (divert to another command).

LALR parsers do not go very well with optional newlines, so there is some
special handlings of that there. One postcondition of the lexer is that it
never outputs two NEWLINE in a row, and also swallows newlines after most
symbols which cannot end statements. At some later point we can reexamine
how those ambiguities resolve and fix things.

Major TODOs are switching to class-based lexer, as in
http://www.dabeaz.com/ply/ply.html#ply_nn17. Then most pylint exceptions can
be removed.
"""
import re
from ply import lex

from bufscript import optypes

reserved = {
    'IF':            'if',
    'ELSE':          'else',
    'TRUE':          'true',
    'FALSE':         'false',
    'SWITCH':        'switch',
    'CASE':          'case',
    'DEFAULT':       'default',
    'QUIT':          'quit',
    'WHILE':         'while',
}
symbols = {
    'LEFT_PAREN':    '(',
    'RIGHT_PAREN':   ')',
    'LEFT_BRACE':    '{',
    'RIGHT_BRACE':   '}',
    'COMMA':         ',',
    'ASSIGN':        '=',
    'SEMICOLON':     ';',
    'COLON':         ':',
    'RANGE':         '..',
    'INCREMENT':     '++',
    'DECREMENT':     '--',
    'PLUS':          '+',
    'MINUS':         '-',
    'MUL':           '*',
    'DIV':           '/',
    'MOD':           '%',
    'AND':           '&&',
    'OR':            '||',
    'ISEQUAL':       '==',
    'NOTEQUAL':      '!=',
    'LESSTHAN':      '<',
    'LESSEQ':        '<=',
    'GREATERTHAN':   '>',
    'GREATEREQ':     '>=',
    'NOT':           '!',
}
# Symbols that can never end statements, basically
no_swallow_eol = [
    reserved['TRUE'],
    reserved['FALSE'],
    symbols['RIGHT_PAREN'],
    symbols['RIGHT_BRACE'],
]
swallow_eol = set(list(reserved.values()) + list(symbols.values())) - set(no_swallow_eol)

reserved_map = dict((v, k) for k, v in list(reserved.items()))
symbol_map = dict((v, k) for k, v in list(symbols.items()))
# Longer symbols first. Exclude semicolons because it affects lexer states, it's a separate rule.
symbol_re = '|'.join(re.escape(r) for r in reversed(sorted(symbols.values())) if r != ';')

tokens = tuple(reserved.keys()) + tuple(symbols.keys()) + (
    # INTCONST above WORD
    'INTCONST',
    'WORD',
    # VAR and FLAG over anything else in strings/literals, FLAG over ANYFLAG
    'VAR',
    'FLAG',
    'ANYFLAG',
    # Special symbols which change states.
    # Probably STRING should be called QUOTE.
    'STRING',
    'LITERAL',
    'NEWLINE',
    # String/literal bits
    'LITERALBIT',
    'STRINGBIT',
    'ESCAPE',
    'BADESCAPE',
)
states = (
    ('string', 'exclusive'),
    ('literal', 'exclusive'),
)

# Position of the last swallow_eol symbol (or last newline)
# Once the lexer becomes a class, handling can be put in a common place.
last_symbol = None

@lex.TOKEN(symbol_re)
def t_symbol(t):
    t.type = symbol_map[t.value]
    #print t.value, t.value in swallow_eol
    if t.value in swallow_eol:
        global last_symbol
        last_symbol = t.lexpos + len(t.value)
    return t

def t_INITIAL_literal_SEMICOLON(t):
    r';'
    t.lexer.begin('INITIAL')
    if t.value in swallow_eol:
        global last_symbol
        last_symbol = t.lexpos + len(t.value)
    return t

# Ignore anything between a double slash and the end of the line - would this
# break if given, for example, "and now I use a double slash: //. End of Line"
# as a string? Needs investigation.
def t_LINE_COMMENT(t):
    r'\#[^\n]*'
    # Ignore complete line commands for the sake of these
    global last_symbol
    if last_symbol == t.lexpos:
        last_symbol += len(t.value)

def t_INTCONST(t):
    r'[0-9]+'
    t.value = int(t.value)
    return t

def t_WORD(t):
    r'[\w\d]+'
    #r'[\w\d]*[\w][\w\d]*'
    t.type = reserved_map.get(t.value, 'WORD')
    if t.value in swallow_eol:
        global last_symbol
        last_symbol = t.lexpos + len(t.value)
    return t

def t_ANY_VAR(t):
    r'\$[a-zA-Z][a-zA-Z0-9]*'
    return t

# This is a separate token to allow @_$1_more or @_u000
@lex.TOKEN(optypes.FLAG_RE)
def t_string_literal_FLAG(t):
    return t

# Flag in a regular code context, validated later.
def t_ANY_ANYFLAG(t):
    r'@_[a-zA-Z0-9\$\*_]*'
    return t

# Must be two characters long
def t_string_literal_ESCAPE(t):
    r'\\[;"$\\@]'
    return t

def t_string_literal_BADESCAPE(t):
    r'\\[^;"$\\@]'
    return t

# TODO: Because STRINGBIT and LITERALBIT exclude @ and $, standalone instances
# of those cause lex failures.
def t_string_STRINGBIT(t):
    r'[^\\"\n@\$]+'
    return t

def t_literal_LITERALBIT(t):
    r'[^\\;\n@\$]+'
    return t

def t_INITIAL_string_STRING(t):
    r'"'
    if t.lexer.in_string:
        t.lexer.begin('INITIAL')
    else:
        t.lexer.begin('string')
    t.lexer.in_string = not t.lexer.in_string
    return t

def t_LITERAL(t):
    r'[~`]'
    t.lexer.begin('literal')
    return t

def t_ANY_linebreak(t):
    r'\\\n'
    t.lexer.line_ends.append(t.lexpos + 1)
    t.lexer.lineno += 1


def t_ignore_space(t):
    r'[ \t]+'
    global last_symbol
    if last_symbol == t.lexpos:
        last_symbol += len(t.value)


def t_INITIAL_literal_NEWLINE(t):
    r'\n'
    t.lexer.begin('INITIAL')
    t.lexer.line_ends.append(t.lexpos)
    t.lexer.lineno += 1
    # TODO: If the last word allowed swallowing this (can not appear at end of statement),
    # swallow this instead.
    this_symbol = t.lexpos + len(t.value)
    global last_symbol
    if last_symbol == t.lexpos:
        last_symbol = this_symbol
        return None
    else:
        last_symbol = this_symbol
        return t

def t_ANY_error(t):
    row, col = find_token_location(t)
    if t.value[0] == '\n':
        print('Unexpected newline at %d:%d' % (row, col))
    else:
        print('Illegal character %s at %d:%d' % (t.value[0], row, col))
    # Maybe introduce a special state to handle this.
    if t.lexer.in_string and t.value[0] == '\n':
        t.lexer.in_string = False
        t.lexer.begin('INITIAL')
    t.lexer.skip(1)

def t_ANY_eof(t):
    t.lexer.begin('INITIAL')
    t.lexer.in_string = False

def find_token_location(token):
    last_cr = token.lexer.line_ends[token.lineno - 1]
    return (token.lineno, token.lexpos - last_cr)

lexer = lex.lex()
lexer.line_ends = [-1]
lexer.in_string = False

# Test only methods.
# TODO: Make this a class instead.
def reset():
    lexer.line_ends = [-1]
    lexer.in_string = False
    global last_symbol
    last_symbol = None

def plex(s):
    print(s)
    print('-----------------')
    lexer.input(s)
    u = True
    while u:
        u = lexer.token()
        if u:
            print(u)

