"""Transforms all code-level variables to registers, meaning BSL variables.

This will eventually optimize variable names away (excluding persistent
variables, of course).
"""


def allocate_registers(program):
    """
    Allocate "registers", which are actually unlimited in BSL.

    Effectively this transforms all variables into the Register class, the
    representation actually used in BSL.

    For now this just transforms variables rather than renaming anything.
    """
    for name, function in program.functions.items():
        program.functions[name] = allocate_function_registers(function)
    return program

def allocate_function_registers(function):
    """
    In the future, use some liveness analysis and add more parameters.
    """
    return function.assign_regs()

