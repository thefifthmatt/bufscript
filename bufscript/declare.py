"""Checks and annotates the AST with variable declaration info."""
import re

from bufscript import ast
from bufscript.optypes import *
from bufscript.util import CompError

TMPVAR_RE = re.compile(r'^tmp[0-9]+$')

class PersistDeclarationError(CompError):
    """Error in program use of persists."""
    def __init__(self, call):
        CompError.__init__(self, "persist(\"%s\") call at %s not assigned directly to a variable", call.args[0], call.location.start)
        self.call = call


class PersistTwiceError(CompError):
    """Error in program use of persists."""
    def __init__(self, first, second):
        CompError.__init__(self, "persist call at %s already defined as %s as %s", second.lhs.location.start, first.lhs, first.lhs.location.start)
        self.first = first
        self.second = second


class UseBeforeDeclareError(CompError):
    """Error in program use of variables."""
    def __init__(self, var, location):
        CompError.__init__(self, "Variable %s at %s used without being defined first", var, location.start)
        self.var = var
        self.location = location


class BadFunctionError(CompError):
    """Error in program use of function calls."""
    def __init__(self, call):
        CompError.__init__(self, "Function %s called at %s does not exist", call.name, call.location.start)
        self.call = call


# pylint: disable=arguments-differ,signature-differs,unused-argument
class DeclarationVisitor(ast.AstVisitor):
    """Decorates AST with defined variables and persisted variables."""

    def __init__(self):
        self.num_decls = 0

    def previsit_function(self, function, parent):
        function.defined = {}
        function.persist = {}

    def previsit_statement(self, stmt, parent, prev_stmt):
        stmt.defined = parent.defined
        if prev_stmt is not None:
            stmt.defined = prev_stmt.defined
        stmt.persist = parent.persist
        # TODO: Check ~ LiteralStmt actually calls a command, rather than whitespace
        if isinstance(stmt, ast.AssignStmt):
            # TODO: Remove this once registers.py is fully implemented.
            if re.match(TMPVAR_RE, stmt.lhs.var):
                raise CompError(
                    'Invalid variable name %s (tmp plus numbers), '
                    'until full IR renaming exists',
                    stmt.lhs)
            stmt.defined = stmt.defined.copy()
            stmt.lhs.typ = BaseType.INT
            prevdecl = stmt.defined.get(stmt.lhs.var)
            if prevdecl is not None and hasattr(prevdecl, 'persist'):
                stmt.lhs.persist = prevdecl.persist
            # TODO: Move this to visiting the expression. ??
            stmt.defined[stmt.lhs.var] = stmt.lhs
        elif isinstance(stmt, ast.LiteralStmt) and stmt.op == LiteralOp.CALL:
            pass


    def previsit_expression(self, expr, parent):
        expr.defined = parent.defined
        if isinstance(expr, ast.FunctionCall):
            op = OPTYPE_MAP.get(expr.name)
            if op is None:
                raise BadFunctionError(expr)
            expr.optype = op
            if expr.name == 'persist':
                if len(expr.args) != 1 or not isinstance(expr.args[0], ast.ConstExpr):
                    raise CompError("Bad persist call at %s", expr.location.start)
                if not isinstance(parent, ast.AssignStmt):
                    raise PersistDeclarationError(expr)
                name = str(expr.args[0])
                if name in parent.persist:
                    raise PersistTwiceError(parent.persist[name], parent)
                # Global persist dictionary is now set for this function
                parent.persist[name] = parent
                parent.lhs.persist = name
        if isinstance(expr, ast.VarExpr):
            # TODO: "Declaration" doesn't really exist anymore.
            # Move this checking later to IR stage? So the same variable
            # can be defined in two branches and allowed there.
            decl = expr.defined.get(expr.var)
            if decl is None:
                raise UseBeforeDeclareError(expr.var, expr.location)
            expr.decl = decl

