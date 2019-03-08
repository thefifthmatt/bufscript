"""Common data used through the entire compiler.

Mainly ops and types, and mainly enums.
"""
import collections
import re

from bufscript.util import CompilerEnum


def _multi(collect, j=' '):
    return j.join(str(c) for c in collect)


class BinaryOp(CompilerEnum):
    """Built-in binary operations."""
    PLUS = '+'
    MINUS = '-'
    MUL = '*'
    DIV = '/'
    MOD = '%'
    AND = '&&'
    OR = '||'
    ISEQUAL = '=='
    NOTEQUAL = '!='
    LESSTHAN = '<'
    LESSEQ = '<='
    GREATERTHAN = '>'
    GREATEREQ = '>='
    RANGE = '..'
    # Cannot be used directly, produced by switch cases
    # Also technically a trinary operator
    INRANGE = '..?'


class UnaryOp(CompilerEnum):
    """Built-in unary operations."""
    NEGATE = '-'
    NOT = '!'


class LiteralOp(CompilerEnum):
    """Freeform string statements."""
    MSG = '`'
    CALL = '~'


class IncDec(CompilerEnum):
    """Inc/dec operations."""
    INCREMENT = '++'
    DECREMENT = '--'


class BaseType(CompilerEnum):
    """Actual types expressions can have."""
    INT = 'int'
    BOOL = 'bool'
    # Also need a type for flag-only?
    STRING = 'string'
    RICH_STRING = 'rstring'
    RANGE = 'range'


class MetaType(CompilerEnum):
    """Ad-hoc types for polymorphic operations.

    Not quite Hindley-Milner (yet).

    These should not survive past typechecking phase.
    """
    SAME_TYPE = '<a>'
    # For literals and flags (not variables)
    INT_OR_STRING = 'int or string'


class OpType(
        collections.namedtuple('OpType', ['name', 'ret', 'args'])):
    """Type information for built-in and explicit ops."""
    def __str__(self):
        return '%s %s(%s)' % (self.ret or 'void', self.name, _multi(self.args, ', '))

_ops = [
    OpType(BinaryOp.PLUS, BaseType.INT, [BaseType.INT, BaseType.INT]),
    OpType(BinaryOp.MINUS, BaseType.INT, [BaseType.INT, BaseType.INT]),
    OpType(BinaryOp.MUL, BaseType.INT, [BaseType.INT, BaseType.INT]),
    OpType(BinaryOp.DIV, BaseType.INT, [BaseType.INT, BaseType.INT]),
    OpType(BinaryOp.MOD, BaseType.INT, [BaseType.INT, BaseType.INT]),
    OpType(BinaryOp.AND, BaseType.BOOL, [BaseType.BOOL, BaseType.BOOL]),
    OpType(BinaryOp.OR, BaseType.BOOL, [BaseType.BOOL, BaseType.BOOL]),
    OpType(BinaryOp.ISEQUAL, BaseType.BOOL, [MetaType.SAME_TYPE, MetaType.SAME_TYPE]),
    OpType(BinaryOp.NOTEQUAL, BaseType.BOOL, [MetaType.SAME_TYPE, MetaType.SAME_TYPE]),
    OpType(BinaryOp.LESSTHAN, BaseType.BOOL, [BaseType.INT, BaseType.INT]),
    OpType(BinaryOp.LESSEQ, BaseType.BOOL, [BaseType.INT, BaseType.INT]),
    OpType(BinaryOp.GREATERTHAN, BaseType.BOOL, [BaseType.INT, BaseType.INT]),
    OpType(BinaryOp.GREATEREQ, BaseType.BOOL, [BaseType.INT, BaseType.INT]),
    OpType(BinaryOp.RANGE, BaseType.RANGE, [BaseType.INT, BaseType.INT]),
    OpType(UnaryOp.NEGATE, BaseType.INT, [BaseType.INT]),
    OpType(UnaryOp.NOT, BaseType.BOOL, [BaseType.BOOL]),
    OpType('persist', BaseType.INT, [BaseType.STRING]),
    OpType('rand', BaseType.INT, [BaseType.RANGE]),
    OpType('hash', BaseType.INT, [BaseType.STRING, BaseType.RANGE, MetaType.INT_OR_STRING]),
    OpType('parseint', BaseType.INT, [BaseType.STRING]),
    OpType('isint', BaseType.BOOL, [BaseType.STRING]),
    OpType('sleep', None, [BaseType.INT]),
    OpType('time', BaseType.INT, []),
]
OPTYPE_MAP = dict((op.name, op) for op in _ops)

FLAG_MAP = {
    '@_$*': BaseType.STRING,
    # Up to 5?
    '@_$1': MetaType.INT_OR_STRING,
    '@_$2': MetaType.INT_OR_STRING,
    '@_$3': MetaType.INT_OR_STRING,
    '@_$4': MetaType.INT_OR_STRING,
    '@_$5': MetaType.INT_OR_STRING,
    # Alphabetical non-arg flags
    '@_amount': BaseType.STRING,
    '@_b': BaseType.STRING,
    '@_donator': BaseType.STRING,
    '@_g': BaseType.STRING,
    '@_message': BaseType.STRING,
    '@_s': BaseType.STRING,
    '@_sign': BaseType.STRING,
    '@_source': BaseType.STRING,
    '@_subber': BaseType.STRING,
    '@_u': BaseType.STRING,
    '@_v': BaseType.INT,
}
FLAG_RE = '|'.join(re.escape(r) for r in reversed(sorted(FLAG_MAP.keys())))
