"""Transforms AST into IR.

The main task is splitting out branching logic into basic blocks: series
of statements which unconditionally go from start to end, and only branch
off at the end.

A major thing to overhaul both here and in the IR is how blocks get chained
together. Currently successor blocks are a list, where the indices are true
and false respectively - and where adjacent blocks are ideally a series of
false branches. THis code should not care about block indices though.
"""
from bufscript import hashimpl
from bufscript.ast import *
from bufscript.intermediate import *
from bufscript.optypes import *
from bufscript.util import CompError, CompInternalError

# TODO: See todos in bsl_codegen, make a separate CompOp.
opposite_comparison = {
    BinaryOp.ISEQUAL: BinaryOp.NOTEQUAL,
    BinaryOp.NOTEQUAL: BinaryOp.ISEQUAL,
    BinaryOp.LESSTHAN: BinaryOp.GREATEREQ,
    BinaryOp.LESSEQ: BinaryOp.GREATERTHAN,
    BinaryOp.GREATEREQ: BinaryOp.LESSTHAN,
    BinaryOp.GREATERTHAN: BinaryOp.LESSEQ,
    'isint': 'isstring',
    'isstring': 'isint',
}
incdec_op = {
    IncDec.INCREMENT: BinaryOp.PLUS,
    IncDec.DECREMENT: BinaryOp.MINUS,
}
boolean_ops = [
    BinaryOp.AND,
    BinaryOp.OR,
]
comparison_ops = [
    BinaryOp.ISEQUAL,
    BinaryOp.NOTEQUAL,
    BinaryOp.LESSTHAN,
    BinaryOp.LESSEQ,
    BinaryOp.GREATEREQ,
    BinaryOp.GREATERTHAN,
]
jmp_ops = boolean_ops + comparison_ops


class AstCodegen:
    """Codegen for an entire program.

    Currently a program is just one function body.
    """

    # pylint: disable=no-self-use
    def generate(self, prog_ast):
        prog = Program()
        for decl in prog_ast.declarations:
            gen = AstFunctionCodegen(decl)
            func = gen.generate()
            prog.add_function(func)
        return prog


class AstFunctionCodegen:
    """Builds a program consisting of basic blocks for given AST components."""

    def __init__(self, funcdecl):
        self.temp_count = 1
        self.funcdecl = funcdecl
        self.function = Function(funcdecl.name)
        self.cfg = self.function.cfg
        self.cfg.start = self.new_block()
        self.exits = []

    def generate(self):
        self.gen_statements(self.funcdecl.body)
        pre_exit = self.current_block()
        last_exit = self.new_block()
        pre_exit.add_successor(last_exit)
        for ret in self.exits:
            ret.add_successor(last_exit)
        # Reentrant-ish
        self.exits = []
        self.cfg.exit = last_exit
        return self.function

    def current_block(self):
        return self.cfg.last_block()

    # TODO: Change to (self, preds, branch=None)
    def new_block(self, *preds, **kwargs):
        branch = kwargs.get('branch')
        block = self.cfg.new_block()
        for pred in preds:
            pred.add_successor(block, branch)
        return block

    def append(self, instr):
        instr.validate()
        self.cfg.last_block().instrs.append(instr)

    def gen_statements(self, statements):
        for stmt in statements:
            self.gen_statement(stmt)

    def gen_statement(self, stmt):
        if isinstance(stmt, ConditionalStmt): # hello
            self.gen_if(stmt.condition, stmt.truestmt, stmt.falsestmt)
        elif isinstance(stmt, LoopStmt):
            self.gen_loop(stmt.condition, stmt.statements)
        elif isinstance(stmt, AssignStmt):
            if isinstance(stmt.lhs, VarExpr):
                var = self.gen_target(stmt.lhs)
                self.gen_assignment(stmt.rhs, var)
        elif isinstance(stmt, FunctionStmt):
            self.gen_call(stmt.function, None)
        elif isinstance(stmt, PostStmt):
            var = self.gen_target(stmt.arg)
            self.gen_incdec(stmt.incdec, var)
        elif isinstance(stmt, SwitchStmt):
            var = self.gen_target(stmt.arg)
            # List of (list of (op, value), statements)
            cases = []
            default = None
            # TODO: Optimize int better. Don't need bottom range if
            # all ranges are adjacent, and can do binary search even for large
            # numbers of cases, given 50 instruction limit.
            if stmt.typ not in [BaseType.INT, BaseType.STRING]:
                raise CompInternalError(stmt)
            for case in stmt.cases:
                anyof = []
                if not case.vals:
                    default = case.statements
                    continue
                for val in case.vals:
                    if stmt.typ == BaseType.STRING:
                        strval = self.extract_string(val)
                        anyof.append((BinaryOp.ISEQUAL, strval))
                    else:
                        if isinstance(val, ConstExpr):
                            anyof.append((BinaryOp.ISEQUAL, int(val.value)))
                        elif isinstance(val, BinaryExpr) and val.op == BinaryOp.RANGE:
                            anyof.append((BinaryOp.INRANGE, (int(val.lhs.value), int(val.rhs.value))))
                        else:
                            raise CompInternalError(stmt)
                cases.append((anyof, case.statements))
            # Generate all cases, then generate all statements
            # There are some weird inverted conditions here, mainly used so that
            # the expected branch is false, so it occurs directly after.
            # List of (list of blocks going to statements when true, statements)
            statement_blocks = []
            for conds, statements in cases:
                # Blocks going to statement when true
                case_statement_blocks = []
                for op, val in conds:
                    # If any blocks are false, go to the next cond.
                    # If all blocks are true, go to statements.
                    if op == BinaryOp.ISEQUAL:
                        const = StringConstant(val) if isinstance(val, str) else Constant(val)
                        self.append(CondInstr(BinaryOp.ISEQUAL, [var, const]))
                        check_block = self.current_block()

                        self.new_block(check_block, branch=False)
                        case_statement_blocks.append(check_block)
                    elif op == BinaryOp.INRANGE:
                        bottom, top = val
                        # If out of bottom range, go to next cond. Else, check upper count.
                        # TODO: This is optional if the ranges are adjacent.
                        self.append(CondInstr(BinaryOp.GREATERTHAN, [var, Constant(top)]))
                        first_check = self.current_block()

                        second_check = self.new_block(first_check, branch=False)
                        self.append(CondInstr(BinaryOp.GREATEREQ, [var, Constant(bottom)]))

                        next_block = self.new_block(second_check, branch=False)
                        first_check.add_successor(next_block, branch=True)
                        case_statement_blocks.append(second_check)
                statement_blocks.append((case_statement_blocks, statements))
            end_blocks = []
            if default:
                # If no other conditions get met, do the default action.
                self.gen_statements(default)
            # The default action (even if it's nothing) goes to the end.
            end_blocks.append(self.current_block())
            for blocks, statements in statement_blocks:
                self.new_block(*blocks, branch=True)
                self.gen_statements(statements)
                end_blocks.append(self.current_block())
            # New block where everything combines
            self.new_block(*end_blocks)

        #collections.namedtuple('SwitchStmt', ['arg', 'cases']),
        #collections.namedtuple('CaseStmt', ['statements', 'vals']),
        #collections.namedtuple('BinaryExpr', ['op', 'lhs', 'rhs']),
        elif isinstance(stmt, LiteralStmt):
            pieces = self.gen_pieces(stmt.pieces)
            self.append(LiteralInstr(stmt.op, pieces))
        elif isinstance(stmt, QuitStmt):
            # TODO: Should check that there aren't unreachable statements.
            self.exits.append(self.current_block())
            self.new_block()
        else:
            raise CompError('Unknown statement %s', stmt)

    @staticmethod
    def extract_string(expr):
        if not isinstance(expr, ConstExpr):
            raise CompError('%s', expr)
        if isinstance(expr.value, str):
            return expr.value
        elif isinstance(expr.value, int):
            return str(expr.value)
        elif isinstance(expr.value, StringExpr):
            pieces = []
            for piece in expr.value.pieces:
                if isinstance(piece, str):
                    pieces.append(piece)
                else:
                    # Do this in typechecking as well probably
                    raise CompError('Expected pure string, got %s', expr)
            return ''.join(pieces)
        else:
            raise CompInternalError(expr)


    def gen_assignment(self, expr, target):
        """
        After these instructions are emitted, target will be
        assigned to the value of the expression.
        Target must be a Variable or TmpVar.
        """
        if isinstance(expr, (VarExpr, FlagExpr, ConstExpr)):
            # TODO Special case: persist calls
            var = self.gen_target(expr)
            self.append(MoveInstr(var, target))
        elif isinstance(expr, BinaryExpr):
            # TODO: Add a negate argument here and use it here
            # This section should NOT call gen_assignment on expr, only subexprs
            # Because gen_target calls this
            if expr.op in [BinaryOp.AND, BinaryOp.OR]:
                stor = target
                # Boolean expressions are normally generated as:
                # z = x && y: [ z = x; if (z != 0) z = y; ]
                # This does not work when z is modified in x, so we use a temporary instead.
                # For, example: x = 0; x = (y = x++) || x;
                # Make sure this works for static variables - reason that this is fine is
                # that we can have some global x for the above example, so we need to do the same
                # assign to temp and store back in location
                # ... Don't do this?
                #if (isinstance(target, Variable) or isinstance(target, StaticVariable)) and target.name in expr.assigns_vars:
                #    stor = self.gen_temp()

                self.gen_assignment(expr.lhs, stor)
                # Skip over the secondary instruction if neccesary
                if expr.op == BinaryOp.AND:
                    #if x != 0 then y else x
                    self.append(CondInstr(BinaryOp.NOTEQUAL, [stor, CondInstr.ZERO]))
                elif expr.op == BinaryOp.OR:
                    #if x == 0 then y else x
                    self.append(CondInstr(BinaryOp.ISEQUAL, [stor, CondInstr.ZERO]))
                block = self.current_block() # in case the lhs generated new blocks
                noshort = self.new_block()
                block.add_successor(noshort)
                # This is the fallthrough (non-short-circuit) branch
                # We gen another assignment because we want the result to be in the same
                # location as that assignment, because the result is in that location
                self.gen_assignment(expr.rhs, stor)

                # And now the short-circuit and non-short-circuit branches point to the
                # block after
                noshort_end = self.current_block()
                after = self.new_block()
                block.add_successor(after)
                noshort_end.add_successor(after)
                if stor is not target:
                    self.append(MoveInstr(stor, target))
            elif expr.op == BinaryOp.RANGE:
                raise CompError('Cannot generate assignment for %s', expr)
            else:
                t1 = self.gen_target(expr.lhs)
                t2 = self.gen_target(expr.rhs)
                self.append(OpInstr(expr.op, target, [t1, t2]))
        elif isinstance(expr, UnaryExpr):
            # Unary expressions are a convenience of the language that get wiped before BSL.
            if isinstance(expr.arg, ConstExpr):
                if expr.op == UnaryOp.NOT:
                    value = int(not expr.arg.value)
                else:
                    value = -expr.arg.value
                self.append(MoveInstr(value, target))
            elif expr.op == UnaryOp.NOT:
                t1 = self.gen_target(expr.arg)
                self.append(OpInstr(BinaryOp.MINUS, target, [Constant(1), t1]))
            else:
                t1 = self.gen_target(expr.arg)
                self.append(OpInstr(BinaryOp.MINUS, target, [Constant(0), t1]))
        elif isinstance(expr, FunctionCall):
            self.gen_call(expr, target)
        else:
            raise CompError('Unknown expression %s', expr)

    def gen_incdec(self, incdec, target):
        op = incdec_op[incdec]
        self.append(OpInstr(op, target, [target, Constant(1)]))

    def gen_pieces(self, inpieces):
        pieces = []
        for piece in inpieces:
            if isinstance(piece, str):
                pieces.append(piece)
            else:
                tp = self.gen_target(piece)
                pieces.append(tp)
        return StringVars(pieces)

    def gen_target(self, expr):
        """
        Returns the target (variable or constant) corresponding to an expression
        If no target exists, a temporary variable is generated.
        No CSE is performed at this stage.
        """
        if isinstance(expr, VarExpr):
            # Should use declnum to distinguish scoped things
            # Useful for loops and stuff later
            if hasattr(expr.decl, 'persist'):
                return PersistVariable(expr.decl.persist, expr.var, expr.location)
            else:
                return Variable(expr.var, expr.location, 0)
        elif isinstance(expr, FlagExpr):
            return FlagVariable(expr.var, expr.location)
        elif isinstance(expr, ConstExpr):
            if isinstance(expr.value, StringExpr):
                return self.gen_pieces(expr.value.pieces)
            # TODO: Does this case happen?
            if isinstance(expr.value, str):
                return StringConstant(expr.value)
            return Constant(int(expr.value))
        elif isinstance(expr, BinaryExpr):
            # This shares a common routine with expr
            tmp = self.gen_temp()
            self.gen_assignment(expr, tmp)
            return tmp
        elif isinstance(expr, UnaryExpr):
            if isinstance(expr.arg, ConstExpr):
                if expr.op == UnaryOp.NOT:
                    return Constant(int(not expr.arg.value))
                else:
                    return Constant(-expr.arg.value)
            tmp = self.gen_temp()
            self.gen_assignment(expr, tmp)
            return tmp
        elif isinstance(expr, FunctionCall):
            tmp = self.gen_temp()
            self.gen_call(expr, tmp)
            return tmp
        else:
            raise CompError('Unknown expression %s', expr)

    def gen_call(self, expr, target):
        optype = expr.optype
        args = expr.args
        if target is None and optype.name != 'sleep':
            # Could allow this, but it is a code smell as well
            raise CompError('No assigned target for %s', expr)
        if optype.name == 'persist':
            # All persist variables are already marked
            # TODO: Make sure they are...
            return
        # If it's being called from here, it's outside of the context of
        # a conditional - it's being assigned to a variable.
        elif optype.name == 'isint':
            # Some duplication with gen_conditional, but we do not
            # need multiple blocks.
            # First, assume it's false
            arg_target = self.gen_target(args[0])
            self.append(MoveInstr(Constant(int(False)), target))
            self.append(CondInstr('isint', [arg_target]))
            cond_block = self.current_block()

            # Set it to true in the true block
            true_block = self.new_block(cond_block)
            self.append(MoveInstr(Constant(int(True)), target))

            # Then, everything goes to the end block.
            self.new_block(true_block, cond_block)
        elif optype.name == 'parseint':
            # For now, explicit quit.
            # BSL seems to do something similar.
            arg_target = self.gen_target(args[0])
            self.append(CondInstr('isstring', [arg_target]))
            cond_block = self.current_block()

            # Panic in the true block
            true_block = self.new_block(cond_block)
            self.exits.append(true_block)

            # Otherwise, go to end block.
            self.new_block(cond_block)
            self.append(MoveInstr(arg_target, target))
        else:
            # The rest are just normal FuncInstrs
            if optype.name == 'rand':
                func_name = 'rng'
                targets = [args[0].lhs, args[0].rhs]
            elif optype.name == 'sleep':
                func_name = 'slp'
                targets = [args[0]]
            elif optype.name == 'time':
                func_name = 'tme'
                targets = []
            elif optype.name == 'hash':
                func_name = 'fpt'
                # Last arg[2] should either be int constant or string constant.
                # And, do transformation here.
                if isinstance(args[2].value, int):
                    seed = args[2].value
                else:
                    s = self.extract_string(args[2])
                    seed = hashimpl.murmurhash3_32(s)
                targets = [args[1].lhs, args[1].rhs, args[0], ConstExpr(seed)]
            else:
                raise CompError('Unknown function in %s', expr)
            arg_targets = []
            for arg in targets:
                arg_target = self.gen_target(arg)
                arg_targets.append(arg_target)
            self.append(FuncInstr(func_name, arg_targets, target))

    def gen_conditional(self, expr, negate=False):
        """
        This is a specialization of gen_target when dealing with conditional
        expressions (i.e., ones used for branching).

        Emits a chain of blocks necessary to evaluate the conditional expression,
        the last of which will be a new block which is only reached if all of these
        blocks are successfully successfully passed through.

        When this function returns, the current block will be what happens if
        the overall condition is true, and the return value will be the list of
        blocks which need to be given a false branch if the overall condition
        is false at any stage.
        """
        # If a BooleanExpr, we are not generating CondInstrs, only gluing them together.
        # TODO: Rewrite all of this probably. Currently unreachable due to AST limits.
        if isinstance(expr, BinaryExpr) and expr.op in boolean_ops:
            # Tricky bit: if negating the expression, use the other kind of BinaryOp
            # and negate both lhs and rhs. This is just DeMorgan's law.
            # Note: negate is not currently used, but was previously, and doesn't add
            # much complexity.
            if (expr.op == BinaryOp.AND) != negate:
                #if (lhs && rhs) then [true branch] else [false branch]
                #if lhs then (if rhs then [true branch] else [false branch]) else [false branch]
                lblocks = self.gen_conditional(expr.lhs, negate)
                rblocks = self.gen_conditional(expr.rhs, negate)
                # If a block in lhs or in rhs is false, the overall expression will be false.
                false_blocks = lblocks + rblocks
            elif (expr.op == BinaryOp.OR) != negate:
                #if (lhs || rhs) then [true branch] else [false branch]
                #if lhs then [true branch] else (if rhs then [true branch] else [false branch])
                lblocks = self.gen_conditional(expr.lhs, negate)
                empty_true_block = self.current_block()
                rhs_block = self.new_block()
                rblocks = self.gen_conditional(expr.rhs, negate)
                true_block = self.current_block()
                # If a block in lhs is false, the overall expression depends on the rhs.
                for block in lblocks:
                    block.add_successor(rhs_block)
                # If all blocks in lhs are true, the overall expression will be true.
                # This requires an empty block, but it'll be merged.
                empty_true_block.add_successor(true_block)
                # If a block in rhs is false, the overall expression will be false.
                false_blocks = rblocks
            return false_blocks
        else:
            if isinstance(expr, BinaryExpr) and expr.op in comparison_ops:
                op = expr.op
                args = [expr.lhs, expr.rhs]
            elif isinstance(expr, FunctionCall) and expr.name == 'isint':
                op = 'isint'
                args = [expr.args[0]]
            else:
                raise CompError('Cannot do comparison %s', expr)

            op = opposite_comparison[op] if negate else op
            target_args = [self.gen_target(arg) for arg in args]
            self.append(CondInstr(op, target_args))

            cond_block = self.current_block()
            true_block = self.new_block()
            cond_block.add_successor(true_block)
            return [cond_block]

    def gen_loop(self, condition, statements):
        # Add an extra instruction at start of loop to avoid
        # two jumps in a row during loop
        # Basically, gen conditional after body. Can also try other way
        start_block = self.current_block()
        body_block = self.new_block()
        self.gen_statements(statements)
        # Block only for condition
        self.new_block(start_block)
        check_blocks = self.gen_conditional(condition)
        true_stmt_end = self.current_block()

        # If all true
        true_stmt_end.add_successor(body_block)
        # If any false
        continue_block = self.new_block()
        for check_block in check_blocks:
            check_block.add_successor(continue_block)

    def gen_temp(self):
        var = TmpVar(self.temp_count)
        self.temp_count += 1
        return var

    def gen_if(self, condition, truestmt, falsestmt=None):
        # TODO: Fix this so jmp at the end of the conditional block is not required.
        check_blocks = self.gen_conditional(condition)
        # In the current (true) block, all of check_blocks are true.
        self.gen_statements(truestmt)
        true_stmt_end = self.current_block()
        if falsestmt:
            # In the false block, at least one of the check_blocks are false.
            false_block = self.new_block()
            for check_block in check_blocks:
                check_block.add_successor(false_block)
            self.gen_statements(falsestmt)
            false_stmt_end = self.current_block()
            # Fall through false block to end block.
            end_block = self.new_block()
            false_stmt_end.add_successor(end_block)
        else:
            # With an empty false block, everything goes to the end block.
            end_block = self.new_block()
            for check_block in check_blocks:
                check_block.add_successor(end_block)

        # Fall through true block to end block.
        true_stmt_end.add_successor(end_block)

