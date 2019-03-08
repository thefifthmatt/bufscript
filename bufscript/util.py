"""Error-checking and enums.

No business logic here.
"""

import collections
import enum
import sys
import traceback


# pylint: disable=protected-access
class CompilerEnum(enum.Enum):
    """A port of the codebase's custom enums to use Python 3 built-ins.

    These are basically just named strings.
    """

    def __init__(self, value):
        cls = self.__class__
        if not hasattr(cls, '_revmap'):
            cls._revmap = collections.OrderedDict()
        if value in cls._revmap:
            raise CompInternalError('%s already present in %s', self.value, cls)
        cls._revmap[value] = self

    def __str__(self):
        return str(self.value)

    def __repr__(self):
        return '%s.%s' % (self.__class__.__name__, self.name)

    @classmethod
    def from_value(cls, value):
        return cls._revmap[value]


class CompError(Exception):
    """A compiler error which can pass messages to users."""

    def __init__(self, message, *args):
        Exception.__init__(self)
        self.msg = message % args

    def __str__(self):
        return 'Error: %s' % self.msg


class CompInternalError(CompError):
    """A compiler error which appears to users as an internal error, with further data for debugging."""

    def __init__(self, value_or_desc, *args):
        CompError.__init__(self, 'Internal error. File a bug!')
        if len(args) and isinstance(value_or_desc, str):
            # TODO: Use format rather than %
            self.internal_desc = value_or_desc % args
            self.value = list(args)
        else:
            self.internal_desc = None
            self.value = value_or_desc


def print_comp_error(e, debug):
    """Prints a compiler error.

    A separate method, so it can look at subclasses cleanly.
    """
    if debug:
        traceback.print_exc()
        if isinstance(e, CompInternalError):
            # Print repr first, less likely to have bugs
            print('Unexpected value: {!r}'.format(e.value), file=sys.stderr)
            if e.internal_desc:
                print(e.internal_desc, file=sys.stderr)
            else:
                print('[{}]'.format(e.value), file=sys.stderr)
    else:
        print(e, file=sys.stderr)

