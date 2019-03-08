"""Tests a bunch of things.

Right now this uses just a bunch of assertions, one after the other, with an
mini-framework based on Google's Truth library. May switch to use unittest
for harnessing only.
"""
import inspect

from bufscript import buf
from bufscript import util

from tests import test_simulate


class Subject:
    """A value under test, to compare expected values with."""

    def __init__(self, actual):
        self.actual = actual

    # Only meant for internal callers
    # From Truth's FailureStrategy object
    def fail_msg(self, msg):  # pylint: disable=no-self-use
        raise AssertionError(msg)

    def fail_prop(self, prop):
        self.fail_msg('Not true that <{}> {}'.format(self.actual, prop))

    def fail_comparing(self, verb, other):
        self.fail_msg('Not true that <{}> {} <{}>'.format(self.actual, verb, other))

    # Public API
    def is_equal_to(self, other):
        if self.actual != other:
            self.fail_comparing('is equal to', other)

    def is_not_equal_to(self, other):
        if self.actual == other:
            self.fail_comparing('is not equal to', other)

    def is_none(self):
        if self.actual is not None:
            self.fail_prop('is none')

    def is_not_none(self):
        if self.actual is None:
            self.fail_prop('is not none')



_subjects = {}


def subject(typ):
    """Decorator to register subjects."""

    def add_subject(cls):
        if typ in _subjects:
            raise AssertionError('{} already registered as {}'.format(typ, _subjects[typ]))
        _subjects[typ] = cls
        return cls
    return add_subject


def assert_that(target):
    # Look up superclasses in mro order.
    # When possible, the inheritance hierarchy of
    # subjects should match that of their corresponding types.
    supers = inspect.getmro(type(target))
    for sup in supers:
        if sup in _subjects:
            return _subjects[sup](target)
    # Fallback. In Python 3 could also register as object.
    return Subject(target)


@subject(list)
class ListSubject(Subject):
    """A subject for lists."""

    def is_empty(self):
        if len(self.actual) != 0:
            self.fail_prop('is empty')

    def is_not_empty(self):
        if len(self.actual) == 0:
            self.fail_prop('is not empty')

    def has_size(self, expected):
        if len(self.actual) != expected:
            self.fail_comparing('has size', expected)

    def contains_only(self, elem):
        if len(self.actual) != 1 or self.actual[0] != elem:
            self.fail_comparing('contains only', elem)


@subject(str)
class StrSubject(Subject):
    """A subject for strings. These are custom to the compiler."""

    def does_not_compile(self):
        try:
            buf.run(self.actual)
        except util.CompError:
            return
        self.fail_prop('doesn\'t compile')


@subject(test_simulate.BslState)
class BslStateSubject(Subject):
    """An assertion for BSL state after code simulation."""

    def outputs(self, *lines):
        if len(lines) == 1:
            assert_that(self.actual.outputs).contains_only(lines[0])
        else:
            assert_that(self.actual.outputs).is_equal_to(list(lines))

    def has_no_output(self):
        self.outputs()

    def persists(self, name, value):
        assert_that(self.actual.var[name]).is_equal_to(value)


def compile_bsl(source):
    """Gets the BSL source from the main compiler."""
    return buf.run(source)['command']


def make_runner(source):
    """Makes a runner from source. Can be used inline for tests."""
    program = compile_bsl(source)
    return test_simulate.Runner(program)


# Split this up using a testing framework, but okay for now.
def run_tests():
    # TODO: Suppress syntax error output from these?
    assert_that('$roll = ;').does_not_compile()
    assert_that('$blerp = $you$you;').does_not_compile()
    assert_that('$post = 0; `$Hello $pots my friend;').does_not_compile()
    # Needs parseint
    # assert_that('$v = @_$1;').does_not_compile()
    # Overlapping ranges
    # 'switch(rand(1..5)) { case: 1..4: `Yes; case 2: `No; }'
    # Also, empty switch cases allowed?

    runner = make_runner('''
        $roll = rand(1..20)
        switch ($roll) {
            case 1: `You done bad, @_u
            case 17..20: `Nice!
            default: `Your roll is $roll
        }
    ''').with_flag('@_u', 'person')
    assert_that(runner.with_random(1).run()).outputs('You done bad, person')
    assert_that(runner.with_random(5).run()).outputs('Your roll is 5')
    assert_that(runner.with_random(18).run()).outputs('Nice!')

    runner = make_runner('''
        switch (@_u) {
            case streamer, "other_guy":
                `@_u, your t-count is: 100/100
            default:
                $count = hash(@_u, 0..100, 0)
                `@_u, your t-count is: $count/100
        }
    ''')
    assert_that(runner.with_flag('@_u', 'streamer').run()).outputs(
        'streamer, your t-count is: 100/100')
    assert_that(runner.with_flag('@_u', 'other_guy').run()).outputs(
        'other_guy, your t-count is: 100/100')
    assert_that(runner.with_flag('@_u', 'me').with_hash('me', 0, 50).run()).outputs(
        'me, your t-count is: 50/100')

    runner = make_runner('''
        $count = hash(@_u, 0..100, "mycount")
        $count2 = hash(@_u, 0..100, othercount)
        `Now you have $count and $count2
    ''')
    # These are mmh3 values for the seeds and should never change.
    assert_that(runner
                .with_flag('@_u', 'name')
                .with_hash('name', 63638057, 99)
                .with_hash('name', 363533744, 100)
                .run()).outputs('Now you have 99 and 100')

    # TODO: Make it accept ++ without colon
    runner = make_runner('''
        $swerves = persist("swervecount")
        $swerves++;
        `$swerves people have been SWERVED
    ''')
    state = runner.with_persist('swervecount', 5).run()
    assert_that(state).outputs('6 people have been SWERVED')
    assert_that(state).persists('swervecount', 6)

    runner = make_runner('''
        ~call @_$*
        ~kadgar @_$*
    ''')
    assert_that(runner.with_args('me you').run()).outputs(
        '~call me you', '~kadgar me you')

    runner = make_runner('''
        switch (@_$*) {
            case "": `ROLL NEED?
            case "roll": `NEED?
            case "roll need": `?
            case "roll need?": `No
        }
    ''')
    assert_that(runner.with_args('').run()).outputs('ROLL NEED?')
    assert_that(runner.with_args('ROLL').run()).outputs('NEED?')
    assert_that(runner.with_args('ROLL NEED').run()).outputs('?')
    assert_that(runner.with_args('ROLL NEED?').run()).outputs('No')
    assert_that(runner.with_args('pineapple').run()).has_no_output()

    runner = make_runner('''
        switch (rand(1..7)) {
            case 1: `Hello
            case 2: ~quote
            case 3..5: `Go play @_g
            case 6..7: `Hammer time
        }
    ''').with_flag('@_g', 'Isaac')
    assert_that(runner.with_random(1).run()).outputs('Hello')
    assert_that(runner.with_random(2).run()).outputs('~quote')
    assert_that(runner.with_random(3).run()).outputs('Go play Isaac')
    assert_that(runner.with_random(4).run()).outputs('Go play Isaac')
    assert_that(runner.with_random(5).run()).outputs('Go play Isaac')
    assert_that(runner.with_random(6).run()).outputs('Hammer time')
    assert_that(runner.with_random(7).run()).outputs('Hammer time')

    runner = make_runner('''
        if (isint(@_$1)) {
            $v = parseint(@_$1)
            $v--;
            `Now: $v
        } else {
            `Not a number boo
        }
    ''')
    assert_that(runner.with_args('hey').run()).outputs('Not a number boo')
    assert_that(runner.with_args('5').run()).outputs('Now: 4')

    runner = make_runner('''
        $v = parseint(@_$1)
        `Look: $v
    ''')
    assert_that(runner.with_args('hey').run()).has_no_output()
    assert_that(runner.with_args('5').run()).outputs('Look: 5')

    runner = make_runner('''
        $v = rand(1..6)
        if ($v == 1) {
            `Bad luck
            quit;
        }
        `Good luck
    ''')
    assert_that(runner.with_random(1).run()).outputs('Bad luck')
    assert_that(runner.with_random(4).run()).outputs('Good luck')

    # TODO: Can't use @_$1 directly?
    # No [Register(name='@_$1'), Constant(value=26)] in jeq BslArgList(['vs', 'iv', 'l'], ['fsl', 'vfl'])
    runner = make_runner('''
      $v = parseint(@_$1)
      if ($v > 20 && $v < 30) {
        if ($v == 26 || $v == 25) {
          `Extra
        } else {
          `Yes
        }
      } else {
        `No
      }
    ''')
    assert_that(runner.with_args('6').run()).outputs('No')
    assert_that(runner.with_args('22').run()).outputs('Yes')
    assert_that(runner.with_args('25').run()).outputs('Extra')
    assert_that(runner.with_args('31').run()).outputs('No')

    # while (!($v != 3 && $v >= 6)) {
    runner = make_runner('''
      $v = 0
      while ($v == 3 || $v <= 6) {
        $v = $v + 2
      }
      `$v
    ''')
    assert_that(runner.with_args('6').run()).outputs('8')

    print('Success!')


if __name__ == '__main__':
    try:
        run_tests()
    except util.CompError as e:
        util.print_comp_error(e, True)

