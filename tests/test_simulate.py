"""Simulates a list of BSL commands for testing purposes.

This accepts a BslCode (the last phase of compilation) as an input, and can
accept any set of starting flags, mocked out randoms/hashes, and persistent
variables. Script output and variable changes are recorded during execution.
"""
import collections
import itertools
import re
import time
import sys

from bufscript import bsl
from bufscript import optypes
from bufscript.bsl import BslArgType


class Runner:
    """A builder to set up state and run a program."""

    def __init__(self, code=None):
        # Give flags defaults
        self.flags = {}
        for flag, _ in optypes.FLAG_MAP.items():
            self.flags[flag] = 0 if type == optypes.BaseType.INT else ''
        self.var = {}
        self.random = None
        self.time = None
        self.fpt = {}
        # All indices to track. This is mutable but may not perform great?
        # Also bit of a hack. BslState shouldn't be a namedtuple probably
        self.index = {'main': 0, 'rng': 0, 'time': 0}
        # Only used by the run method
        self.code = code

    def with_flag(self, flag, value):
        if flag not in optypes.FLAG_MAP:
            raise ValueError('Invalid flag {}'.format(flag))
        # TODO: Figure out what to do about @_$1 flags...
        # Don't test any int ones for now.
        if optypes.FLAG_MAP[flag] == optypes.BaseType.INT:
            value = int(value)
        else:
            value = str(value)
        self.flags[flag] = value
        return self

    def with_persist(self, name, value):
        self.var[name] = int(value)
        return self

    def with_random(self, random):
        return self.with_randoms([random])

    def with_randoms(self, randoms):
        self.random = randoms
        return self

    def with_hash(self, s, seed, result):
        self.fpt[(s, seed)] = int(result)
        return self

    def with_time(self, timeint):
        self.time = timeint
        return self

    def with_args(self, args):
        self.flags['@_$*'] = args
        arglist = args.split()
        for i, arg in enumerate(arglist):
            self.flags['@_$' + str(i + 1)] = arg
        return self

    def build(self):
        state = BslState(
            self.var.copy(),
            self.flags.copy(),
            self.random[:] if self.random else None,
            self.time,
            self.fpt.copy(),
            self.index.copy(),
            [])
        state.purge_temps()
        return state

    def run(self):
        state = self.build()
        simulate_bsl(self.code, state)
        return state


class BslState(
        collections.namedtuple('BslState', ['var', 'flags', 'random', 'time', 'fpt', 'index', 'outputs'])):
    """Big pile of state for a program.

    It is a namedtuple mainly for repr but is highly mutable.
    """

    def purge_temps(self):
        for var in list(self.var.keys()):
            if var.startswith('_'):
                del self.var[var]

    def output(self, s):
        self.outputs.append(s)

    def advance(self):
        self.index['main'] += 1

    def jump(self, index):
        self.index['main'] = index

    def current(self):
        return self.index['main']

    def next_random(self, start, end):  # pylint: disable=unused-argument
        # We could support full random, but randomness in unit tests is
        # generally not a great idea.
        if self.random is None:
            raise ValueError('Random not set in test')
        if self.index['rng'] >= len(self.random):
            raise ValueError('Ran out of randoms! Provided: {}'.format(self.random))
        val = self.random[self.index['rng']]
        self.index['rng'] += 1
        return val

    def get_hash(self, start, end, s, seed):  # pylint: disable=unused-argument
        if (s, seed) in self.fpt:
            return self.fpt[(s, seed)]
        # Should we calculate real hash here?
        raise ValueError('No fpt exists for {!r} with seed {}'.format(s, seed))

    def get_time(self):
        if self.time is None:
            raise ValueError('Time not set')
        return self.time + self.index['time']

    def increment_time(self, amount):
        self.index['time'] += max(amount, 0)

    def copy(self):
        return self._replace(
            var=self.var.copy(),
            flags=self.flags.copy(),
            index=self.index.copy(),
            outputs=self.outputs[:])

    def finish(self):
        if self.random is not None and self.index['rng'] != len(self.random):
            raise ValueError('Only asked for {} randoms out of {}'.format(self.index['rng'], self.random))
        self.purge_temps()


class BslArg:
    """Parent class representing an arg in the BSL source."""

    def eval_int(self, state):
        raise ValueError('Attempt to evaluate {} as int'.format(self))

    def eval_str(self, state):
        # Sane default, only possible for int things
        return str(self.eval_int(state))


class BslImmediate(
        collections.namedtuple('BslImmediate', ['value']),
        BslArg):
    """An int constant arg in BSL."""

    def eval_int(self, state):
        return self.value


class BslVariable(
        collections.namedtuple('BslVariable', ['name']),
        BslArg):
    """A variable arg in BSL, which may be persisted or not."""

    def eval_int(self, state):
        if self.name not in state.var:
            # Persists can be 0
            if self.name.startswith('_'):
                raise ValueError('Var {} not declared'.format(self.name))
            return 0
        # return state.var.get(self.name, 0)
        return state.var[self.name]

    def set_int(self, val, state):
        state.var[self.name] = int(val)


class BslFlag(
        collections.namedtuple('BslFlag', ['name']),
        BslArg):
    """A static flag in BSL. The name must be in state.flags (and global flag list)."""

    def eval_int(self, state):
        if optypes.FLAG_MAP[self.name] == optypes.BaseType.STRING:
            raise ValueError('Flag {} cannot be int'.format(self.name))
        # Evaluate as int if possible
        return int(state.flags[self.name])

    def eval_str(self, state):
        return str(state.flags[self.name])


# For simplicity, 'name' includes the L prefix here
class BslLabel(
        collections.namedtuple('BslLabel', ['name']),
        BslArg):
    """A named label in BSL, as used as an arg."""

    def eval_str(self, state):
        return self.name

class BslString(
        collections.namedtuple('BslString', ['pieces']),
        BslArg):
    """A dynamic string in BSL."""

    def eval_str(self, state):
        pieces = []
        for piece in self.pieces:
            if isinstance(piece, str):
                pieces.append(piece)
            else:
                pieces.append(piece.eval_str(state))
        return ''.join(pieces)


_VAR_RE = '^[_a-zA-Z][_a-zA-Z0-9]*$'
_NUM_RE = re.compile(r'^[0-9]+$')
_VAR_RE = re.compile(r'^[_a-zA-Z][_a-zA-Z0-9]*$')
_LABEL_RE = re.compile('^L[a-zA-Z0-9]+$')
# Three options: variable, flag, string escape
# All others are just assumed to be regular string
# Don't handle percent escapes for now, as we don't output them
_STR_RE = re.compile(r'(@__[_a-zA-Z][_a-zA-Z0-9]*)|(' + optypes.FLAG_RE + r')|(\\[.;{}\(\)\\%@])')
_INSTR_RE = re.compile(r'^(?:(L[a-zA-Z0-9]+):)? *(.*); *$')
_INSTR_PIECE_RE = re.compile(r'(\.)|(\\\.)')

# So with a list of BslInstrs
# Index by label
# Then just evaluate 1 by 1

# 9007199254740992
# Has some special handling of things for test cases.
# Can mock out
def simulate_bsl(code, state=None):
    state = state or Runner().build()
    label_map = {}
    instrs = []
    # TODO: Don't need to do this every time
    if isinstance(code, bsl.BslCode):
        code_instrs = code.instrs
    elif isinstance(code, list):
        code_instrs = code
    else:
        raise ValueError('Unknown code format {}'.format(type(code)))
    for i, s in enumerate(code_instrs):
        instr = BslInstr.parse(s)
        if instr.label is not None:
            label_map[instr.label] = i
        instrs.append(instr)
    count = 0
    # Use BSL generators for interpretation. Weird but it works.
    # Could also move this kind of logic into BslInstr.
    generators = {}
    for gen in itertools.chain(list(bsl.generators.values()), list(bsl.branchGenerators.values())):
        generators[gen.instr] = gen

    def branch(result, label):
        name = label.name
        if name not in label_map:
            raise ValueError(name)
        if result:
            index = label_map[name]
            state.jump(index)
            return True
        return False
    while True:
        count += 1
        if count == 50:
            raise ValueError('Too looooong!')
        instr = instrs[state.current()]
        args = instr.args
        def argstr(i):
            return args[i].eval_str(state)
        def argint(i):
            return args[i].eval_int(state)
        if instr.name in generators:
            gen = generators[instr.name]
            if isinstance(gen, bsl.BinaryOpGenerator):
                result = (gen.python_op)(argint(0), argint(1))
                args[0].set_int(result, state)
            elif isinstance(gen, bsl.BranchGenerator):
                if gen.python_op is not None:
                    # Distinguish between int comparisons and string equals
                    # This is a bit fragile, though.
                    if isinstance(args[0], BslVariable):
                        result = (gen.python_op)(argint(0), argint(1))
                    else:
                        # Case-insensitive comparison
                        result = (gen.python_op)(argstr(0).lower(), argstr(1).lower())
                    if branch(result, args[2]):
                        continue
                elif instr.name == 'jfi':
                    if branch(_NUM_RE.match(argstr(0)), args[1]):
                        continue
                elif instr.name == 'jfs':
                    if branch(not _NUM_RE.match(argstr(0)), args[1]):
                        continue
                else:
                    raise ValueError(str(instr))
            else:
                raise ValueError(str(instr))
        elif instr.name == 'jmp':
            branch(True, args[0])
            continue
        elif instr.name == 'cal':
            # To distinguish calls and messages
            state.output('~' + argstr(0))
        elif instr.name == 'msg':
            state.output(argstr(0))
        elif instr.name == 'set':
            args[0].set_int(argint(1), state)
        elif instr.name == 'rng':
            args[2].set_int(state.next_random(argint(0), argint(1)), state)
        elif instr.name == 'fpt':
            args[0].set_int(state.get_hash(argint(1), argint(2), argstr(3), argint(4)), state)
        elif instr.name == 'tme':
            args[0].set_int(state.get_time(), state)
        elif instr.name == 'slp':
            state.increment_time(argint(0) / 1000)  # Python 3 note: integer division
        elif instr.name == 'nop':
            pass
        else:
            # This includes inc, dec, and clr
            raise ValueError('{} currently not supported'.format(instr))
        state.advance()
        if state.current() == len(instrs):
            break
    state.finish()
    return state



class BslInstr(
        collections.namedtuple('BslInstr', ['name', 'args', 'label'])):
    """An instruction in BSL with a very ad hoc parsing scheme.

    This is not representative of what BSL will actually accept.
    """

    @staticmethod
    def parse(s):
        match = _INSTR_RE.match(s)
        if not match:
            raise ValueError('Bad instruction: ' + s)
        labelbit, m = match.groups()
        # Split into dots, taking into account escapes
        lastdot = 0
        pieces = []
        for match in re.finditer(_INSTR_PIECE_RE, m):
            start, end = match.span()
            # Using regex for this as cute shortcut.
            # Match all groups to sanity check regex.
            dot, _ = match.groups()
            if dot is not None:
                start, end = match.span()
                pieces.append(m[lastdot:start].replace(r'\.', '.'))
                lastdot = end
        if lastdot != len(m):
            pieces.append(m[lastdot:])
        instr = pieces[0]
        if instr not in bsl.instructions:
            raise ValueError('Bad instruction name {}: {}'.format(instr, s))
        # Now parse based on type
        args = pieces[1:]
        argsets = bsl.instructions[instr].argsets
        if len(args) != len(argsets):
            raise ValueError('Expected {} args: {}'.format(len(argsets), s))
        # jeq/jne type is v/s, but currently depends on typing of second.
        if (instr in ('jeq', 'jne')) and (args[0] in optypes.FLAG_MAP or args[1] in optypes.FLAG_MAP):
            arglist = list(argsets)
            # TODO: Figure out what this should do exactly
            # For now, avoid writing flag-to-variable comparisons
            for flag in [0, 1]:
                if args[flag] not in optypes.FLAG_MAP:
                    continue
                other = 1 - flag
                if args[flag] == optypes.BaseType.INT:
                    arglist[other] = frozenset([BslArgType.VARIABLE])
                else:
                    arglist[other] = frozenset([BslArgType.STRING])
            argsets = tuple(arglist)
        try:
            bslargs = [BslInstr.parse_arg(arg, types) for (arg, types) in zip(args, argsets)]
        except ValueError:
            raise AssertionError('Bad arguments for instruction {}'.format(s))
        return BslInstr(instr, bslargs, labelbit)

    @staticmethod
    def parse_arg(s, types):
        # In most specific to least specific order
        if BslArgType.IMMEDIATE in types and _NUM_RE.match(s):
            return BslImmediate(int(s))
        elif BslArgType.VARIABLE in types and _VAR_RE.match(s):
            return BslVariable(s)
        elif BslArgType.FLAG in types and s in optypes.FLAG_MAP:
            return BslFlag(s)
        elif BslArgType.LABEL in types and _LABEL_RE.match(s):
            return BslLabel(s)
        elif BslArgType.STRING in types:
            index = 0
            pieces = []
            for match in re.finditer(_STR_RE, s):
                start, end = match.span()
                if start != index:
                    pieces.append(s[index:start])
                varg, flagg, escg = match.groups()
                if varg is not None:
                    pieces.append(BslVariable(varg[3:]))
                elif flagg is not None:
                    pieces.append(BslFlag(flagg))
                elif escg is not None:
                    pieces.append(escg[1])
                index = end
            if index != len(s):
                pieces.append(s[index:])
            return BslString(pieces)
        else:
            raise ValueError()


def main():
    if sys.stdin.isatty():
        raise ValueError('No stdin input')
    source = sys.stdin.readlines()
    source = [line.strip() for line in source if not line.startswith('#')]
    # Now some lightweight arg parsing
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('--args', metavar='arg1 arg2 arg3')
    parser.add_argument('--randoms', metavar='rand1,rand2,rand3')
    # Both flags and persists
    parser.add_argument('--vars', metavar='var1=value1,flag2=value2')
    args = parser.parse_args()
    runner = Runner(source)
    runner.with_time(int(time.time()))
    if args.args:
        runner.with_args(args.args)
    if args.randoms:
        runner.with_randoms([int(r) for r in args.randoms.split(',')])
    if args.vars:
        for pair in args.vars.split(','):
            key, value = pair.split('=')
            if key in optypes.FLAG_MAP:
                runner.with_flag(key, value)
            else:
                runner.with_persist(key, value)
    print(runner.run())


if __name__ == '__main__':
    main()

