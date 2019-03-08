"""Generates BSL code from a program in IR."""
import collections

from bufscript import bsl
from bufscript.intermediate import *
from bufscript.optypes import *

def output_bsl_codes(program):
    all_bsl = collections.OrderedDict()
    for function in program:
        code = bsl.BslCode()
        all_bsl[function.name] = code

        # Generate code for each block
        for block in function.cfg.blocks:
            code.append_label(block.get_label())
            for instr in block.instrs:
                if isinstance(instr, MoveInstr):
                    code.append_move(instr.dest, instr.src)
                elif isinstance(instr, OpInstr):
                    # TODO: Consider adding IR phase where all a+b=c ops are transformed to a+=b.
                    bsl.generators[instr.op].generate(code, instr.dest, instr.srcs)
                elif isinstance(instr, CondInstr):
                    # Conditional Jump: first if "true", second if "false" -
                    # but this means that the jump goes to the second target
                    # (and if the following block is not the immediate successor,
                    # unconditional jump to it)
                    if len(block.succ) != 2:
                        raise RuntimeError('Block %s used with a CondInstr' % block)
                    true_label = block.succ[0].dest.get_label()
                    # Negation should probably happen at the IR level, rather than the BSL level.
                    bsl.branchGenerators[instr.op].generate(code, instr.srcs, true_label, negate=False)
                elif isinstance(instr, FuncInstr):
                    if instr.name == 'rng':
                        code.append('rng.%s.%s.%s;', instr.srcs[0], instr.srcs[1], instr.dest)
                    elif instr.name == 'fpt':
                        code.append('fpt.%s.%s.%s.%s.%s;', instr.dest, instr.srcs[0], instr.srcs[1], instr.srcs[2], instr.srcs[3])
                    elif instr.name == 'slp':
                        code.append('slp.%s;', instr.srcs[0])
                    elif instr.name == 'tme':
                        code.append('tme.%s;', instr.dest)
                    else:
                        raise CompInternalError(instr)
                elif isinstance(instr, LiteralInstr):
                    if instr.op == LiteralOp.MSG:
                        code.append('msg.%s;', instr.src)
                    elif instr.op == LiteralOp.CALL:
                        code.append('cal.%s;', instr.src)
                    else:
                        raise CompInternalError(instr)
                elif isinstance(instr, NoOpInstr):
                    code.append('nop;')
                else:
                    raise CompInternalError(instr)
            # We're done with instructions; Now we might need to add an unconditional jump to the
            # next block, if the index isn't ours + 1.
            if len(block.succ):
                # For unconditional, go to only next branch. For conditional, go to false branch.
                if block.succ[-1].dest.index != (block.index + 1):
                    code.append('jmp.%s;', block.succ[-1].dest.get_label())
    return all_bsl

