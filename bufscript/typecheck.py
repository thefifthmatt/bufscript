"""Checks types, to make sure we're not asking codegen to output nonsense.

This all needs a big refactoring. The polymorphism is inherently ad-hoc but the
current checking approach is way too much complex code for its own good. Should
be more declarative.
"""
from bufscript import ast
from bufscript.optypes import *
from bufscript.util import CompError


class TypeMismatchError(CompError):
    """A type error."""

    def __init__(self, expr, typ):
        # These errors could be more descriptive, but one-size-fits-all is good for now.
        CompError.__init__(self, 'Expression at %s has type %s; expected %s', expr.location, expr.typ, typ)
        self.expr = expr
        self.typ = typ


class TypeVisitor(ast.AstVisitor):
    """Does the typecheck. Annotates expressions with a typ field."""

    @staticmethod
    def resolve_intstring(typ1, typ2):
        if typ1 == MetaType.INT_OR_STRING:
            if typ2 not in [BaseType.INT, BaseType.STRING, MetaType.INT_OR_STRING]:
                return None
            return BaseType.INT if typ2 == MetaType.INT_OR_STRING else typ2
        elif typ2 == MetaType.INT_OR_STRING:
            if typ1 not in [BaseType.INT, BaseType.STRING, MetaType.INT_OR_STRING]:
                return None
            return BaseType.INT if typ1 == MetaType.INT_OR_STRING else typ1
        else:
            raise CompError('Internal error: %s, %s', typ1, typ2)

    def require(self, expr, typ):
        # INT_OR_STRING should get collapsed into INT or STRING (preferring the former)
        # I believe this always happen at the usage site, so we don't need weird
        # constraint-solving complexity.
        if expr.typ == MetaType.INT_OR_STRING or typ == MetaType.INT_OR_STRING:
            resolved = self.resolve_intstring(expr.typ, typ)
            if not resolved:
                raise TypeMismatchError(expr, typ)
            return resolved
        if expr.typ == MetaType.SAME_TYPE or typ == MetaType.SAME_TYPE:
            raise CompError('Internal error: %s, %s', expr, typ)
        if expr.typ != typ:
            raise TypeMismatchError(expr, typ)
        return expr.typ

    def require_func(self, expr, args):
        desc = ('Call %s at %s' % (expr.name, expr.location.start)) if isinstance(expr, ast.FunctionCall) else ('%s' % expr.op)
        optype = expr.optype
        ndecl, ncall = len(optype.args), len(args)
        if ndecl != ncall:
            diff = 'too %s parameters (saw %d, require %d)' % ('many' if ndecl < ncall else 'few', ncall, ndecl)
            raise CompError('%s has %s', desc, diff)
        sametypes = []
        for decltype, callexpr in zip(optype.args, args):
            if decltype == MetaType.SAME_TYPE:
                sametypes.append(callexpr)
            else:
                callexpr.typ = self.require(callexpr, decltype)
        # Fake polymorphism
        # This doesn't work anymore, we can compare string expr vs string const
        # TODO: Make a table?
        if len(sametypes):
            # Just == and != for now
            if len(sametypes) != 2:
                raise CompError('Internal error: %s, %s', expr, args)
            s1, s2 = sametypes  # pylint: disable=unbalanced-tuple-unpacking
            resolved = None
            if s1.typ == MetaType.INT_OR_STRING or s2.typ == MetaType.INT_OR_STRING:
                # May return None
                resolved = self.resolve_intstring(s1.typ, s2.typ)
            elif s1.typ == s2.typ:
                resolved = s1.typ
            if resolved is None:
                raise CompError('%s needs two compatible types; found %s and %s', desc, s1.typ, s2.typ)
            s1.typ = s2.typ = resolved
        return optype.ret

    def previsit_function(self, function, program):
        pass

    def postvisit_statement(self, stmt, children):
        if isinstance(stmt, ast.ConditionalStmt):
            self.require(stmt.condition, BaseType.BOOL)
        elif isinstance(stmt, ast.LoopStmt):
            self.require(stmt.condition, BaseType.BOOL)
        elif isinstance(stmt, ast.SwitchStmt):
            stmt.typ = self.require(stmt.arg, MetaType.INT_OR_STRING)
            for case in stmt.cases:
                for val in case.vals or []:
                    # TODO: Better polymorphism.
                    if stmt.typ == BaseType.INT and val.typ == BaseType.RANGE:
                        self.require(val, BaseType.RANGE)
                    else:
                        self.require(val, stmt.typ)
        elif isinstance(stmt, ast.PostStmt):
            self.require(stmt.arg, BaseType.INT)
        elif isinstance(stmt, ast.AssignStmt):
            self.require(stmt.rhs, BaseType.INT)

    def postvisit_expression(self, expr, children):
        if isinstance(expr, ast.VarExpr):
            # This wll just be int
            # TODO: Boolean variables
            expr.typ = expr.decl.typ
        if isinstance(expr, ast.FlagExpr):
            # TODO: Should do this in declare?
            flagtype = FLAG_MAP.get(expr.var)
            if flagtype is None:
                # TODO: Location?
                raise CompError('Bad flag %s', expr)
            expr.typ = flagtype
        elif isinstance(expr, ast.ConstExpr):
            if isinstance(expr.value, bool):
                expr.typ = BaseType.BOOL
            elif isinstance(expr.value, int):
                expr.typ = MetaType.INT_OR_STRING
            else:
                expr.typ = BaseType.STRING
        elif isinstance(expr, ast.StringExpr):
            # TODO: Make a complex string vs non-complex string type?
            # Just so complex strings can't be compared with each other
            expr.typ = BaseType.STRING
        elif isinstance(expr, ast.UnaryExpr):
            op = OPTYPE_MAP.get(expr.op)
            if op is None:
                raise CompError('Internal error. No op called %s', expr)
            expr.optype = op
            expr.typ = self.require_func(expr, [expr.arg])
        elif isinstance(expr, ast.BinaryExpr):
            op = OPTYPE_MAP.get(expr.op)
            if op is None:
                raise CompError('Internal error. No op called %s', expr)
            expr.optype = op
            expr.typ = self.require_func(expr, [expr.lhs, expr.rhs])
        elif isinstance(expr, ast.FunctionCall):
            # optyp gets assigned in declaration visiting
            expr.typ = self.require_func(expr, expr.args)

