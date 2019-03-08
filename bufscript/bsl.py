"""Helpers for generating BSL instructions."""
import itertools
import operator
import re

from bufscript.intermediate import *
from bufscript.optypes import *
from bufscript.util import CompInternalError, CompilerEnum

# 0 index is skipped in ast_codegen, this is contained entirely within a single
# IR instruction generation routine.
TMP_REGISTER = Register('_tmp0')


class BslCode:
    """Mutable representation of BSL code as list of strings."""

    def __init__(self):
        self.instrs = []
        self.next_label = None

    @staticmethod
    def get_op(op):
        if isinstance(op, Register):
            return op.name
        elif isinstance(op, StringVars):
            return ''.join(BslCode.get_string_piece(p) for p in op.pieces)
        elif isinstance(op, Constant):
            return '%d' % op.value
        elif isinstance(op, StringConstant):
            return op.value
        elif isinstance(op, Label):
            return str(op)
        elif isinstance(op, str):
            # TODO: We allow this to parameterize instruction name.
            # But maybe that should just be a separate parameter.
            return op
        else:
            raise CompInternalError(op)

    @staticmethod
    def get_string_piece(op):
        if isinstance(op, str):
            # s will have no escapes on it, so we can escape it properly
            return re.sub(r'([.;\%@])', r'\\\1', op)
        elif isinstance(op, Register):
            # Register will already be renamed as necessary
            return op.name
        else:
            raise CompInternalError(op)

    def append(self, instr, *ops):
        op_str = []
        for op in ops:
            op_str.append(self.get_op(op))
        out = instr % tuple(op_str)
        if self.next_label:
            out = '%s: %s' % (str(self.next_label), out)
            self.next_label = None
        self.instrs.append(out)

    def append_move(self, dest, src):
        if isinstance(dest, Register):
            if isinstance(src, (Register, Constant)):
                self.append('set.%s.%s;', dest, src)
            else:
                raise TypeError
        else:
            raise TypeError

    def append_label(self, label):
        self.next_label = label

    def get_program(self):
        return ''.join('%s\n' % instr for instr in self.instrs)


class BinaryOpGenerator:
    """Generator for all binary ops."""

    def __init__(self, instr, python_op):
        self.instr = instr
        self.commute = instr in ['add', 'mul']
        self.python_op = python_op

    def generate(self, code, dest, srcs):
        if len(srcs) != 2:
            raise CompInternalError((self.instr, srcs))
        # Unlike branching, the arg order does matter in BSL, and at the BSL
        # level a variable must always be used in first slot.
        # Also see OpInstr todo in codegen
        # May want to split instr == 'xxx' logic up into subclasses especially
        # if we get more instructions.
        p1, p2 = srcs
        # Case: dest = 1 + 2, or similar
        if isinstance(p1, Constant) and isinstance(p2, Constant):
            result = (self.python_op)(p1.value, p2.value)
            code.append('set.%s.%s;', dest, Constant(result))
        # Case: dest = dest + p2 or dest = dest - p2
        elif p1 == dest:
            code.append('%s.%s.%s;', self.instr, dest, p2)
        # Case: dest = p1 + dest
        # Becomes: dest += p1
        elif p2 == dest and self.commute:
            code.append('%s.%s.%s;', self.instr, dest, p1)
        # Case: dest = p1 + p2
        # Becomes: dest = p1; dest += p2;
        elif self.commute:
            code.append('set.%s.%s;', dest, p1)
            code.append('%s.%s.%s;', self.instr, dest, p2)
        # Case: dest = p1 - dest
        # Becomes: tmp = p1; tmp -= dest; dest = tmp;
        # Maybe would be easier with a negate instruction?
        elif p2 == dest and not self.commute:
            code.append('set.%s.%s;', TMP_REGISTER, p1)
            code.append('%s.%s.%s;', self.instr, TMP_REGISTER, dest)
            code.append('set.%s.%s;', dest, TMP_REGISTER)
        # Case: dest = p1 - p2
        # Becomes: dest = p1; dest -= p2;
        elif not self.commute:
            code.append('set.%s.%s;', dest, p1)
            code.append('%s.%s.%s;', self.instr, dest, p2)
        else:
            raise CompInternalError('%s = %s %s', self.instr, dest, srcs)


class BranchGenerator:
    """Generator for all branch ops."""

    def __init__(self, instr, op, python_op=None):
        self.instr = instr
        self.op = op
        self.python_op = python_op

    def generate(self, code, srcs, true_label, negate):
        # Constant specialization
        if all(isinstance(src, Constant) for src in srcs) and self.python_op:
            values = [src.value for src in srcs]
            result = (self.python_op)(*values)
            if result != negate:
                # If result is true and negate is false, or result is false and negate is true,
                # then we will want to branch.
                # TODO: Probably communicate this back to codegen
                # So we don't have two jumps in a row.
                code.append('jmp.%s;', true_label)
            return
        # Otherwise, generate code
        instr = self.instr
        negate_instr = branchInstrs[negateOp[self.op]]
        # We have op-order specializations for >=2
        if len(srcs) == 1:
            code.append('%s.%s.%s;', negate_instr if negate else instr, srcs[0], true_label)
            return
        elif len(srcs) != 2:
            raise CompInternalError(self.instr)
        # Now, find the instruction from 2 srcs
        arglist = instructions[self.instr]
        combo, swap_needed = arglist.find_combo(srcs + [true_label], (0, 1))
        if not combo:
            raise CompInternalError('No %s in %s %s', srcs, self.instr, arglist)
        regs = list(srcs)
        if negate:
            instr = negate_instr
        if swap_needed:
            regs[0], regs[1] = regs[1], regs[0]
            # ... find a cleaner way to do this
            # Maybe preserve BinaryOp (or CompOp) until the last moment
            instr = branchInstrs[argSwitchOp[branchInstrOps[instr]]]
        code.append('%s.%s.%s.%s;', instr, regs[0], regs[1], true_label)

    def evaluate(self, srcs):
        return (self.python_op)(*srcs)


class BslArgType(CompilerEnum):
    """Different arg types according to BSL."""
    # Integer
    IMMEDIATE = 'i'
    # Constant string
    STRING = 's'
    # Fixed flags
    FLAG = 'f'
    # Variables, either temporary or persistent
    VARIABLE = 'v'
    # Labels only
    LABEL = 'l'


class BslArgList:
    """Allowed arg types for a BSL instruction."""

    def __init__(self, argspec, additional=None):
        # Mainly for repr
        self.argspec = argspec
        self.additional = additional
        # Parse out arg types and calculate combos
        self.argsets = tuple(frozenset(BslArgType.from_value(typ) for typ in arg) for arg in argspec)
        combos = set(itertools.product(*self.argsets))
        if additional:
            combos.update(tuple(BslArgType.from_value(typ) for typ in ad) for ad in additional)
        self.combos = frozenset(combos)

    def __repr__(self):
        return 'BslArgList(%r, %r)' % (self.argspec, self.additional)

    # Find a combo that applies for these args, if one exists.
    # Equiv is a pair of two argument positions which can be interchanged.
    def find_combo(self, args, equiv=None):
        arg_combo = tuple(self.get_argtype(arg) for arg in args)
        if arg_combo in self.combos:
            return (arg_combo, False)
        if equiv:
            alt = list(arg_combo)
            alt[equiv[0]], alt[equiv[1]] = alt[equiv[1]], alt[equiv[0]]
            alt_combo = tuple(alt)
            if alt_combo in self.combos:
                return (self.combos, True)
        return (None, False)

    @staticmethod
    def get_argtype(loc):
        if isinstance(loc, Constant):
            return BslArgType.IMMEDIATE
        elif isinstance(loc, Register):
            # Outside of StringVars, all flags start with @
            # Could also check for presence in flag list currently in ast.py
            if loc.name.startswith('@_'):
                return BslArgType.FLAG
            else:
                return BslArgType.VARIABLE
        elif isinstance(loc, StringVars):
            return BslArgType.STRING
        elif isinstance(loc, StringConstant):
            return BslArgType.STRING
        elif isinstance(loc, Label):
            return BslArgType.LABEL
        else:
            raise CompInternalError(loc)

_arithmetic_args = BslArgList(['v', 'ivf'])
# In theory: ['vs', 'ivf', 'l']
_eq_args = BslArgList(['vs', 'iv', 'l'], ['fsl', 'vfl'])
_comp_args = BslArgList(['v', 'ivf', 'l'])
_ifstr_args = BslArgList(['f', 'l'])
instructions = {
    # Has generators
    'add': _arithmetic_args,
    'sub': _arithmetic_args,
    'mul': _arithmetic_args,
    'div': _arithmetic_args,
    'mod': _arithmetic_args,
    'jeq': _eq_args,
    'jne': _eq_args,
    'jlt': _comp_args,
    'jgt': _comp_args,
    'jle': _comp_args,
    'jge': _comp_args,
    'jfi': _ifstr_args,
    'jfs': _ifstr_args,
    # Does not
    'nop': BslArgList([]),
    'jmp': BslArgList(['l']),
    'slp': BslArgList(['i']),
    'clr': BslArgList(['v']),
    'tme': BslArgList(['v']),
    # TODO: Output these.
    'dec': BslArgList(['v']),
    'inc': BslArgList(['v']),
    'cal': BslArgList(['s']),
    'msg': BslArgList(['s']),
    # Documented as v.if, but can do v-to-v.
    'set': BslArgList(['v', 'ivf']),
    'rng': BslArgList(['iv', 'iv', 'v']),
    'fpt': BslArgList(['v', 'iv', 'iv', 'f', 'i']),
}

generators = {
    BinaryOp.PLUS:  BinaryOpGenerator('add', operator.add),
    BinaryOp.MINUS: BinaryOpGenerator('sub', operator.sub),
    BinaryOp.MUL: BinaryOpGenerator('mul', operator.mul),
    BinaryOp.DIV: BinaryOpGenerator('div', operator.floordiv),
    BinaryOp.MOD: BinaryOpGenerator('mod', operator.mod),
}

branchGenerators = {
    BinaryOp.LESSTHAN:    BranchGenerator('jlt', BinaryOp.LESSTHAN, operator.lt),
    BinaryOp.GREATERTHAN: BranchGenerator('jgt', BinaryOp.GREATERTHAN, operator.gt),
    BinaryOp.LESSEQ:      BranchGenerator('jle', BinaryOp.LESSEQ, operator.le),
    BinaryOp.GREATEREQ:   BranchGenerator('jge', BinaryOp.GREATEREQ, operator.ge),
    BinaryOp.ISEQUAL:     BranchGenerator('jeq', BinaryOp.ISEQUAL, operator.eq),
    BinaryOp.NOTEQUAL:    BranchGenerator('jne', BinaryOp.NOTEQUAL, operator.ne),
    'isint':              BranchGenerator('jfi', 'isint'),
    'isstring':           BranchGenerator('jfs', 'isstring'),
}
branchInstrs = dict((op, gen.instr) for (op, gen) in branchGenerators.items())
branchInstrOps = dict((gen.instr, op) for (op, gen) in branchGenerators.items())

# This is effectively duplicated somewhere else.
# Maybe have a CompOp, where BinaryOp gets transformed away at IR level.
# Also means we don't have to deal with 'isint' etc. being a string.
negateOp = {
    BinaryOp.LESSTHAN:    BinaryOp.GREATEREQ,
    BinaryOp.GREATERTHAN: BinaryOp.LESSEQ,
    BinaryOp.LESSEQ:      BinaryOp.GREATERTHAN,
    BinaryOp.GREATEREQ:   BinaryOp.LESSTHAN,
    BinaryOp.ISEQUAL:     BinaryOp.NOTEQUAL,
    BinaryOp.NOTEQUAL:    BinaryOp.ISEQUAL,
    'isint':              'isstring',
    'isstring':           'isint',
}

argSwitchOp = {
    BinaryOp.LESSTHAN:    BinaryOp.GREATERTHAN,
    BinaryOp.GREATERTHAN: BinaryOp.LESSTHAN,
    BinaryOp.LESSEQ:      BinaryOp.GREATEREQ,
    BinaryOp.GREATEREQ:   BinaryOp.LESSEQ,
    BinaryOp.ISEQUAL:     BinaryOp.ISEQUAL,
    BinaryOp.NOTEQUAL:    BinaryOp.NOTEQUAL,
}
