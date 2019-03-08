"""The bufscript IR, a useful format for doing things.

Short for intermediate representation, it is the structure that the assembly
will eventually be in, but can be manipulated (optimizations, dataflow analysis,
etc.) without having to worry about either syntactic sugar or BSL specifics.

It lots of classes ranging from variables to instructions to basic blocks
to entire commands. Is spat out by ast_codegen, and has plenty of other
transformations performed by other modules. See the 'buf' module for a full
list.
"""

import collections

from bufscript.util import CompInternalError

def _multi(collect, j=' '):
    return j.join(str(c) for c in collect)


# pylint: disable=unused-argument,no-self-use
class Location:
    """Parent class for values, variables, and registers used in instructions."""

    def alive(self, used):
        """Return any live variables at this location.

        Used specifies whether the variable is being contextually used or defined.
        """
        return set()

    def as_reg(self):
        """Transform this to a register, i.e. something which can be used directly in BSL."""
        return self


class Variable(
        collections.namedtuple('Variable', ['name', 'location', 'decl']),
        Location):
    """A variable defined in the source code."""

    def __str__(self):
        return '%s' % self.name

    def alive(self, used):
        return set([self])

    def as_reg(self):
        # Take out the leading $. No register analysis currently.
        return Register('_%s' % self.name.lstrip('$'))


class StringVars(
        collections.namedtuple('Variable', ['pieces']),
        Location):
    """Pieces of a string literal in the source, including variables and flags."""

    def __str__(self):
        return _multi(self.pieces, '')

    def alive(self, used):
        liveset = set()
        for var in self.pieces:
            if isinstance(var, Variable):
                liveset.update(var.alive(used))
        return set(self.pieces)

    def as_reg(self):
        pieces = []
        for piece in self.pieces:
            if isinstance(piece, Location):
                reg = piece.as_reg()
                if isinstance(reg, Register) and not isinstance(piece, FlagVariable):
                    pieces.append(Register('@__%s' % reg.name))
                else:
                    pieces.append(reg)
            else:
                pieces.append(piece)
        return StringVars(pieces)


class TmpVar(
        collections.namedtuple('TmpVar', ['which']),
        Location):
    """A variable produced in codegen, not defined in the original source."""

    def __str__(self):
        return 'tmp%s' % self.which

    def alive(self, used):
        return set([self])

    def as_reg(self):
        return Register('_tmp%s' % self.which)


class FlagVariable(
        collections.namedtuple('FlagVariable', ['name', 'location']),
        Location):
    """A flag variable like @_u, always available in the bot."""

    def __str__(self):
        return self.name

    def as_reg(self):
        return Register(self.name)


class PersistVariable(
        collections.namedtuple('PersistVariable', ['name', 'variable', 'location']),
        Location):
    """A variable declared in the source to be stored across command invocations."""

    def __str__(self):
        return self.name

    def as_reg(self):
        # Notably, no prefix underscore
        return Register(self.name)


class Register(
        collections.namedtuple('Register', ['name']),
        Location):
    """A variable used in BSL directly."""

    def __str__(self):
        return '%s' % self.name


class Constant(
        collections.namedtuple('Constant', ['value']),
        Location):
    """An integer constant."""

    def __str__(self):
        return '%s' % self.value


# TODO: Is this actually used? Or only StringVars?
class StringConstant(
        collections.namedtuple('StringConstant', ['value']),
        Location):
    """A string constant, with no variables."""

    def __str__(self):
        return '%s' % self.value


class Label(
        collections.namedtuple('Label', ['index'])):
    """A label heading off a basic block."""

    def __str__(self):
        return 'L%d' % self.index


class Program:
    """A collection of functions, representing a source file."""

    def __init__(self):
        self.functions = collections.OrderedDict()

    def add_function(self, func):
        self.functions[func.name] = func

    def merge_empty_blocks(self):
        for func in self:
            func.cfg.merge_empty_blocks()

    def remove_dead_blocks(self):
        for func in self:
            func.cfg.remove_dead_blocks()

    def __iter__(self):
        return iter(self.functions.values())

    def __str__(self):
        return ''.join(str(func) + '\n' for func in self)


class Function:
    """A function in the source code.

    Currently, a program only consists of one function body, syntactically.

    The main use case for adding functions is to produce multiple commands from one
    file, as well as having shared code between commands.
    """

    def __init__(self, name, cfg=None):
        self.name = name
        self.cfg = FunctionCfg() if cfg is None else cfg

    # TODO: This logic should probably be in the CFG itself?
    def assign_regs(self):
        """Returns a new function with all instructions register-assigned.

        The registers module is the program-level version of this.
        """
        newcfg = self.cfg.empty_copy()
        for block in self.cfg.blocks:
            # Rewrite each block
            newblock = newcfg.blocks[block.index]
            for instr in block.instrs:
                newblock.instrs.append(instr.assign_regs())
        return Function(self.name, newcfg)

    def __str__(self):
        return '%s(): %s' % (self.name, self.cfg)


class FunctionCfg:
    """A mutable control flow graph within a function.

    This is used to build up code during AST codegen, and also to later perform
    transformations/optimizations.
    """

    def __init__(self):
        # A collection of blocks, where blocks[i].index == i.
        # First block may not necessarily be entry point, likewise for
        # last block. Will probably be the case, though.
        self.blocks = []
        # A basic block
        self.start = None
        # Also a basic block
        self.exit = None

    def new_block(self):
        block = BasicBlock(len(self.blocks))
        self.blocks.append(block)
        return block

    def last_block(self):
        return self.blocks[-1]

    def remove_dead_blocks(self):
        # First, remove unreachable blocks, which makes dataflow analysis
        # complicated (and, in the case of things like use-define checks,
        # meaningless). This is made possible by return statements in the
        # middle of blocks.
        stack = [block for block in self.blocks if len(block.pred) == 0 and block.index != self.start.index]
        if not len(stack):
            return
        seen = set(block.index for block in stack)
        while len(stack):
            block = stack.pop()
            dests = [edge.dest for edge in block.succ]
            block.remove_self()
            self.blocks.remove(block)
            stack.extend(dest for dest in dests if dest.index not in seen and len(dest.pred) == 0)
            seen.update(dest.index for dest in dests)
        for i in range(len(self.blocks)):
            self.blocks[i].index = i

    def merge_empty_blocks(self):
        toremove = []
        # Mark empty blocks to remove.
        for block in self.blocks:
            if not block.make_non_empty():
                toremove.append(block)
        # Actually remove them.
        for block in toremove:
            block.remove_self()
            self.blocks.remove(block)
        if len(toremove) > 0:
            for i in range(len(self.blocks)):
                self.blocks[i].index = i
            self.start = [block for block in self.blocks if len(block.pred) == 0][0]
            self.exit = [block for block in self.blocks if len(block.succ) == 0][0]

    def empty_copy(self):
        # Makes a shallow copy of basic blocks' structure without
        # any instructions.
        newself = FunctionCfg()
        newblocks = newself.blocks
        for block in self.blocks:
            newblocks.append(BasicBlock(block.index))
        newself.start = newblocks[self.start.index]
        newself.exit = newblocks[self.exit.index]
        for block in self.blocks:
            for e in block.succ:
                newblocks[e.src.index].add_successor(newblocks[e.dest.index])
        return newself

    def __str__(self):
        return ('%s -> %s\n' % (self.start.index, self.exit.index)) + ''.join(str(block) + '\n' for block in self.blocks)


class BasicBlock:
    """A series of instructions which run unconditionally from beginning to end.

    This is connected with other blocks in a graph.
    """

    def __init__(self, index):
        # A unique numerical identifier for this block.
        self.index = index
        # A list of instructions in this block.
        self.instrs = []
        # A list of edges of where to go after this block. If this block ends in
        # a conditional, succ[0] should be the True branch and succ[1] should
        # be the False branch.
        # TODO: Split this into succ and altsucc? List is weird.
        self.succ = []
        # A list of edges of blocks which jump here.
        self.pred = []
        # True when initial edges are still getting constructed.
        # self.building = True ??

    def __str__(self):
        instructions = ' '.join(str(instr) + ';' for instr in self.instrs if str(instr))
        sources = ', '.join(str(edge.src.index) for edge in self.pred)
        destinations = ', '.join(str(edge.dest.index) for edge in self.succ if edge)
        return '%d: [%s] -> [%s] -> [%s]' % (self.index, sources, instructions, destinations)

    def __len__(self):
        return len(self.instrs)

    def get_label(self):
        return Label(self.index)

    # does other directly follow self
    def precedes(self, other):
        for successor in self.succ:
            if successor and successor.dest == other:
                return True
        return False

    def make_non_empty(self):
        # Returns true if made non-empty, false if impossible.
        # if not empty, we don't want to merge
        if len(self.instrs) != 0:
            return True
        # if we're empty but also the last block, we want to add a no-op rather than
        # merging; also, given these blocks, probably just add the instruction
        # 'j exit' to always quit (in case stuff is out of order)
        if len(self.succ) == 0:
            self.instrs.append(NoOpInstr())
            return True
        return False

    def remove_self(self):
        # We must only have one successor or no predecessors.
        if len(self.succ) == 1:
            succ_block = self.succ[0].dest
            succ_block.pred.remove(self.succ[0])
            for edge in self.pred:
                src = edge.src
                e = Edge(src, succ_block)
                if not src.precedes(succ_block):
                    succ_block.pred.append(e)
                pred_ind = src.succ.index(edge)
                src.succ[pred_ind] = e
            self.index = -self.index
        elif len(self.pred) == 0:
            for edge in self.succ:
                dest = edge.dest
                dest.pred.remove(edge)
            self.succ = []

    def add_successor(self, other, branch=None):
        # TODO: Revamp this.
        # Make branch a required parameter for conditional blocks.
        # Maybe also fix a block's exit strategy on construction.
        if self.precedes(other):
            raise CompInternalError(self)
        e = Edge(self, other)
        if branch is None:
            self.succ.append(e)
        elif branch is True and len(self.succ) == 0:
            self.succ.append(e)
        elif branch is True and len(self.succ) == 2 and self.succ[0] is None:
            self.succ[0] = e
        elif branch is False and len(self.succ) == 0:
            self.succ.extend([None, e])
        elif branch is False and len(self.succ) == 1:
            self.succ.append(e)
        else:
            # TODO: Better error message - internal error should be able to take string?
            raise CompInternalError(self)
        other.pred.append(e)


class Edge:
    """A link between one basic block and the next one.

    This is shared btween the two blocks.
    """

    def __init__(self, src, dest):
        # The source block.
        self.src = src
        # The destination block.
        self.dest = dest

    def __eq__(self, other):
        return isinstance(other, Edge) and (self.src == other.src) and (self.dest == other.dest)

    def __ne__(self, other):
        return not self == other


# pylint: disable=no-member
class Instruction:
    """Parent class for all instructions.

    Subclasses are expected to be effectively immutable namedtuples, and there is
    some logic which allows for 'src', 'srcs', or 'dest' fields to either be present
    or not. All locations are expected to be in one of those variables, so that
    register allocation can happen automatically.
    """

    def _optdest(self):
        return self.dest if hasattr(self, 'dest') else None

    @property
    def srclist(self):
        if hasattr(self, 'srcs'):
            return self.srcs
        elif hasattr(self, 'src'):
            return [self.src]
        else:
            return []

    def alive(self):
        return self._optdest() is not None

    def defined(self):
        return self.dest.alive(False) if self._optdest() else set()

    def used(self):
        liveset = set()
        for src in self.srclist:
            liveset.update(src.alive(True))
        return liveset

    def validate(self):
        # No duplication
        if hasattr(self, 'src') and hasattr(self, 'srcs'):
            raise CompInternalError(self)
        # Ensure all location args are locations.
        srcs = list(self.srcs) if hasattr(self, 'srcs') else []
        others = [getattr(self, at) for at in ['src', 'dest'] if hasattr(self, at) and getattr(self, at)]
        for x in srcs + others:
            if not isinstance(x, Location):
                raise CompInternalError((self, x))

    def assign_regs(self):
        replace = {}
        # TODO: Use tuples instead of lists, because these are actually immutable
        # When creating instructions, not just here
        if hasattr(self, 'srcs'):
            replace['srcs'] = [l.as_reg() for l in self.srcs]
        for at in ['src', 'dest']:
            if hasattr(self, at) and getattr(self, at):
                replace[at] = getattr(self, at).as_reg()
        return self._replace(**replace)


class MoveInstr(
        collections.namedtuple('MoveInstr', ['src', 'dest']),
        Instruction):
    """Assigns a constant or variable to a variable."""

    def __str__(self):
        return '%s = %s' % (self.dest, self.src)


class FuncInstr(
        collections.namedtuple('FuncInstr', ['name', 'srcs', 'dest']),
        Instruction):
    """Calls a function.

    These are not used-defined functions currently, and instead are specially
    handled in BSL gen.
    """

    def __str__(self):
        return '%s%s(%s)' % (('%s = ' % str(self.dest)) if self.dest else '', self.name, _multi(self.srcs, ', '))


class QuitInstr(
        collections.namedtuple('QuitInstr', []),
        Instruction):
    """Exits the program immediately."""

    def __str__(self):
        return 'quit'


class CondInstr(
        collections.namedtuple('CondInstr', ['op', 'srcs']),
        Instruction):
    """A conditional jump at the end of a basic block.

    This must be present when there is more than one jump destination, and vice
    versa. If there is only one successor, an unconditional jump is implied.
    """

    ZERO = Constant(0)

    def __str__(self):
        if len(self.srcs) == 2:
            return 'if %s %s %s' % (self.srcs[0], self.op, self.srcs[1])
        else:
            return 'if %s %s' % (self.op, _multi(self.srcs))


class OpInstr(
        collections.namedtuple('OpInstr', ['op', 'dest', 'srcs']),
        Instruction):
    """A built-in unary or binary op."""

    def __str__(self):
        if len(self.srcs) == 1:
            return '%s = %s %s' % (self.dest, self.op, self.srcs[0])
        elif len(self.srcs) == 2:
            return '%s = %s %s %s' % (self.dest, self.srcs[0], self.op, self.srcs[1])
        raise CompInternalError(self)


class LiteralInstr(
        # Should probably make string piece thing
        collections.namedtuple('LiteralInstr', ['op', 'src']),
        Instruction):
    """A command using literal syntax; can output text or call other commands."""

    def __str__(self):
        return '%s%s' % (self.op, _multi(self.src.pieces, ''))


class NoOpInstr(
        collections.namedtuple('NoOpInstr', []),
        Instruction):
    """Do nothing."""

    def __str__(self):
        return 'nop'

