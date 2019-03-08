"""All AST objects.

Contains an AST visitor framework which should probably be split out.
"""
import collections

from bufscript.optypes import *


def _multi(collect, j=' '):
    return j.join(str(c) for c in collect)


class TextLocation(
        collections.namedtuple('TextLocation', ['line', 'col', 'end'])):
    """The location or location range of a statement or expression."""

    @property
    def start(self):
        return '%d:%d' % (self.line, self.col)

    @property
    def range(self):
        if self.end:
            eline, ecol = self.end
            if self.line == eline:
                if self.col == ecol:
                    return '%d:%d' % (self.line, self.col)
                return '%d:%d-%d' % (self.line, self.col, ecol)
            return '%d:%d-%d:%d' % (self.line, self.col, eline, ecol)
        else:
            return '%d:%d' % (self.line, self.col)

    def __str__(self):
        return self.range


class ProgramAst(collections.namedtuple('ProgramAst', ['declarations'])):
    """The entire program."""

    def __str__(self):
        return '%s' % _multi(self.declarations)


class FuncDeclaration(
        collections.namedtuple('FuncDeclaration', ['name', 'level', 'body', 'location'])):
    """An individual function."""

    def __str__(self):
        return '%s %s() { %s }' % (self.level, self.name, _multi(self.body))


class Statement:
    """Parent class to identify statements."""


class QuitStmt(
        collections.namedtuple('QuitStmt', []),
        Statement):
    """Quits immediately."""

    def __str__(self):
        return 'quit;'


class ConditionalStmt(
        collections.namedtuple('ConditionalStmt', ['condition', 'truestmt', 'falsestmt']),
        Statement):
    """An if statement."""

    def __str__(self):
        if self.falsestmt is None:
            return 'if (%s) { %s }' % (self.condition, _multi(self.truestmt))
        elif isinstance(self.falsestmt, ConditionalStmt):
            return 'if (%s) { %s } else %s' % (self.condition, _multi(self.truestmt), str(self.falsestmt))
        else:
            return 'if (%s) { %s } else { %s }' % (self.condition, _multi(self.truestmt), _multi(self.falsestmt))


class LoopStmt(
        collections.namedtuple('LoopStmt', ['condition', 'statements']),
        Statement):
    """A while loop."""

    def __str__(self):
        return 'while (%s) { %s }' % (self.condition, _multi(self.statements))


class FunctionStmt(
        collections.namedtuple('FunctionStmt', ['function']),
        Statement):
    """A function call with no result."""

    def __str__(self):
        return '%s;' % (self.function)


class AssignStmt(
        collections.namedtuple('AssignStmt', ['lhs', 'rhs']),
        Statement):
    """An assignment to a variable."""

    def __str__(self):
        return '%s = %s;' % (self.lhs, self.rhs)


class SwitchStmt(
        collections.namedtuple('SwitchStmt', ['arg', 'cases']),
        Statement):
    """A switch statement for an expression."""

    def __str__(self):
        return 'switch (%s) { %s }' % (self.arg, _multi(self.cases))


class CaseStmt(
        collections.namedtuple('CaseStmt', ['statements', 'vals']),
        Statement):
    """A case within a switch statement."""

    def __str__(self):
        stmt = ' '.join(str(s) for s in self.statements)
        if self.vals:
            return 'case %s: %s' % (_multi(self.vals, ', '), stmt)
        else:
            return 'default: %s' % stmt


class PostStmt(
        collections.namedtuple('PostStmt', ['arg', 'incdec']),
        Statement):
    """A variable increment/decrement."""

    def __str__(self):
        return '%s%s;' % (self.arg, self.incdec)


class LiteralStmt(
        collections.namedtuple('LiteralStmt', ['op', 'pieces']),
        Statement):
    """A command using literal syntax."""

    def __str__(self):
        return '%s%s;' % (self.op, _multi(self.pieces, ''))

    @property
    def args(self):
        return list(p for p in self.pieces if isinstance(p, Expr))


class Expr:
    """Parent class for expressions.

    Subclasses should all have a 'location' field added during parsing.
    """


class StringExpr(
        collections.namedtuple('StringExpr', ['pieces']),
        Expr):
    """A string expression, possibly with variables to be substituted."""

    def __str__(self):
        return '%s' % _multi(self.pieces, '')

    @property
    def args(self):
        return list(p for p in self.pieces if isinstance(p, Expr))


class VarExpr(
        collections.namedtuple('VarExpr', ['var']),
        Expr):
    """A variable used in an expression or string."""

    def __str__(self):
        return self.var


class FlagExpr(
        collections.namedtuple('FlagExpr', ['var']),
        Expr):
    """A static flag used in an expression or string."""

    def __str__(self):
        return self.var


class ConstExpr(
        collections.namedtuple('ConstExpr', ['value']),
        Expr):
    """A string, boolean, or int constant in the source."""

    def __str__(self):
        if self.value is True:
            return 'true'
        elif self.value is False:
            return 'false'
        else:
            return str(self.value)


class FunctionCall(
        collections.namedtuple('FunctionCall', ['name', 'args']),
        Expr):
    """A function call that returns a result used in an expression."""

    def __str__(self):
        return '%s(%s)' % (self.name, _multi(self.args, ', '))


class BinaryExpr(
        collections.namedtuple('BinaryExpr', ['op', 'lhs', 'rhs']),
        Expr):
    """A built-in binary op."""

    def __str__(self):
        return '%s %s %s' % (self.lhs, self.op, self.rhs)


class UnaryExpr(
        collections.namedtuple('UnaryExpr', ['op', 'arg']),
        Expr):
    """A built-in uniary op."""

    def __str__(self):
        return '%s %s' % (self.op, self.arg)


# pylint: disable=unused-variable
class AstVisitor:
    """Parent class for AST visitors. Call visit_ast to use."""

    def previsit_program(self, program):
        pass

    def postvisit_program(self, program):
        pass

    def previsit_function(self, function, program):
        pass

    def postvisit_function(self, function):
        pass

    def previsit_statement(self, stmt, parent, prev_stmt=None):
        pass

    def postvisit_statement(self, stmt, children):
        pass

    def previsit_expression(self, expr, parent):
        pass

    def postvisit_expression(self, expr, children):
        pass

def visit_ast(visitor, program):
    if visitor.previsit_program(program) is False:
        return
    for decl in program.declarations:
        visit_function(visitor, decl, program)
    visitor.postvisit_program(program)

def visit_function(visitor, function, parent):
    if visitor.previsit_function(function, parent) is False:
        return
    visit_ast_block(visitor, function.body, function)
    visitor.postvisit_function(function)

def visit_ast_block(visitor, stmts, parent):
    prev_stmt = None
    for stmt in stmts:
        visit_ast_stmt(visitor, stmt, parent, prev_stmt)
        prev_stmt = stmt

def visit_ast_stmt(visitor, stmt, parent, prev_stmt=None):
    if visitor.previsit_statement(stmt, parent, prev_stmt) is False:
        return
    children = []
    if isinstance(stmt, AssignStmt):
        visit_ast_expr(visitor, stmt.lhs, stmt)
        visit_ast_expr(visitor, stmt.rhs, stmt)
        children = [stmt.lhs, stmt.rhs]
    if isinstance(stmt, FunctionStmt):
        visit_ast_expr(visitor, stmt.function, stmt)
        children = [stmt.function]
    elif isinstance(stmt, PostStmt):
        visit_ast_expr(visitor, stmt.arg, stmt)
        children = [stmt.arg]
    elif isinstance(stmt, LiteralStmt):
        for arg in stmt.args:
            visit_ast_expr(visitor, arg, stmt)
        children = stmt.args
    elif isinstance(stmt, SwitchStmt):
        visit_ast_expr(visitor, stmt.arg, stmt)
        for case in stmt.cases:
            visit_ast_stmt(visitor, case, stmt)
        children = [stmt.arg] + stmt.cases
    elif isinstance(stmt, CaseStmt):
        vals = stmt.vals or []
        for val in vals:
            visit_ast_expr(visitor, val, stmt)
        visit_ast_block(visitor, stmt.statements, stmt)
        children = vals + stmt.statements
    elif isinstance(stmt, ConditionalStmt):
        visit_ast_expr(visitor, stmt.condition, stmt)
        visit_ast_block(visitor, stmt.truestmt, stmt)
        if stmt.falsestmt:
            visit_ast_block(visitor, stmt.falsestmt, stmt)
        children = [stmt.condition, stmt.truestmt, stmt.falsestmt]
    elif isinstance(stmt, LoopStmt):
        visit_ast_expr(visitor, stmt.condition, stmt)
        visit_ast_block(visitor, stmt.statements, stmt)
        children = [stmt.condition, stmt.statements]
    visitor.postvisit_statement(stmt, [child for child in children if child])

def visit_ast_expr(visitor, expr, parent):
    if visitor.previsit_expression(expr, parent) is False:
        return
    children = []
    if isinstance(expr, UnaryExpr):
        visit_ast_expr(visitor, expr.arg, expr)
        children = [expr.arg]
    elif isinstance(expr, BinaryExpr):
        visit_ast_expr(visitor, expr.lhs, expr)
        visit_ast_expr(visitor, expr.rhs, expr)
        children = [expr.lhs, expr.rhs]
    elif isinstance(expr, FunctionCall):
        for arg in expr.args:
            visit_ast_expr(visitor, arg, expr)
        children = expr.args
    elif isinstance(expr, StringExpr):
        for piece in expr.args:
            visit_ast_expr(visitor, arg, expr)
        children = expr.args
    visitor.postvisit_expression(expr, children)

