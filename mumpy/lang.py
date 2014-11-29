"""MUMPY Language Components

The functions and objects in this file represent various functional parts
of the MUMPS language (such as expressions and commands). The parser
pieces all of these parts together into larger components and then
executes them. In that way, each component is designed to work together
to achieve the actions dictated by the M standard.

Licensed under a BSD license. See LICENSE for more information.

Author: Christopher Rink"""
import datetime
import random
import string
import os
import subprocess
import sys
import time
import traceback
import blist
import mumpy


# Date and time values are pre-calculated since they won't change
_horolog_origin = datetime.datetime(1840, 12, 31)
_horolog_midnight = datetime.time()


###################
# COMMAND FUNCTIONS
# Command functions execute the actions given by a MUMPS keyword command
# such as 'w' (write). Each matches the signature func(args, env). Functions
# will either implicitly return None or raise one of the following exceptions
# to indicate to the parser how to proceed next:
# - MUMPSReturn() encapsulates a return value (or None) and indicates that
#   the parser should exit the current scope (subroutine or function)
# - MUMPSCommandEnd() indicates to the parser that it should continue
#   to the next line of execution (i.e. a conditional command evaluated False)
# - MUMPSGotoLine() indicates that the parser should cease execution of
#   the current line and immediately transfer control to a new tag without
#   changing stack levels.
###################
def new_var(args, env):
    """New the variables given in the argument list."""
    for var in args:
        env.new(var)


def do_cmd(args, env):
    """Perform a subroutine call."""
    for sub in args:
        sub.execute()


def halt(args, env):
    """Quit from the MUMPy environment."""
    raise SystemExit(0)


def hang(args, env):
    """Sleep the system for the given number of seconds."""
    for slp in args:
        time.sleep(slp)


def if_cmd(args, env):
    """Process an IF MUMPS command."""
    for expr in args:
        if not expr:
            env.set("$T", mumps_false())
            raise MUMPSCommandEnd()
    env.set("$T", mumps_true())


def if_no_args(args, env):
    """Process an argumentless IF MUMPS command."""
    if not env.get("$T").equals(mumps_true()):
        raise MUMPSCommandEnd()


def else_cmd(args, env):
    """Process a MUMPS ELSE command."""
    if not env.get("$T").equals(mumps_false()):
        raise MUMPSCommandEnd()


def goto_cmd(args, env):
    """Process a MUMPS GOTO command."""
    for arg in args:
        # Process the argument post-conditional
        if arg.post is not None and not arg.post:
            continue

        # Check for valid routine
        if arg.rou is None:
            current_rou = env.get_current_rou()

            try:
                _ = current_rou.tag_line(arg.tag)
            except AttributeError:
                raise MUMPSSyntaxError("No current routine. Cannot GOTO "
                                       "tag without routine.",
                                       err_type="NO LINE")
            except KeyError:
                raise MUMPSSyntaxError("Tag not found in current routine.",
                                       err_type="NO LINE")

        # Raise a GotoLine exception to the parser
        raise MUMPSGotoLine(arg)


def for_start(args, env):
    """Raise the MUMPSForLine exception to indicate to the line that it
    should let the `FOR` command take over execution of the rest of the line."""
    raise MUMPSForLine(_for_cmd)


def _for_cmd(args, env, cmds):
    """Implement the MUMPS FOR command."""
    def _execute_commands():
        """Loop to execute each remaining command we were given. Return
        True if the loop should continue, False if a `QUIT` was executed
        and the loop should end."""
        for cmd in cmds:
            try:
                cmd.execute()
            except MUMPSReturn as ret:
                if ret.value() is not None:
                    raise MUMPSSyntaxError("Cannot QUIT with value "
                                           "from FOR loop",
                                           err_type="ILLEGAL QUIT ARG")
                else:
                    return False
        return True

    # Unlimited FOR loops will have no args and loop until they quit
    if args is None:
        done = False
        while not done:
            done = not _execute_commands()
        return

    # In limited FOR loops, we'll create a generator for the values
    var, gen = _process_for_args(args)
    for arg in gen():
        # Update the value of the control variable in the environment
        set_var(((var, arg),), env)

        # Execute the remaining commands
        if not _execute_commands():
            break


def _process_for_args(args):
    """Create a generator which can be used by the FOR command for iteration.
    If the loop conditions are invalid, return an empty generator."""
    # Prepare some simple starting values
    start = args['start'].reduce()
    inc = args['inc'].reduce() if 'inc' in args else None
    end = args['end'].reduce() if 'end' in args else None
    others = args['others'] if 'others' in args else ()

    # Create a comparator based on the increment value
    if inc is not None:
        if inc.as_number() > 0:
            cmp = lambda v, e: (v < e).reduce()
        else:
            cmp = lambda v, e: (v > e).reduce()
    else:
        cmp = lambda v, e: False

    # Create our generator
    def _generator():
        # Yield the first value
        val = start
        yield val.reduce()

        # Increment as long as we haven't exceeded the end value
        while ((end is None) and (inc is not None)) or cmp(val, end):
            val += inc
            yield val.reduce()

        # If there are any other expressions listed afterwards, yield those
        for other in others:
            yield other.reduce()

    # Create an empty generator for invalid ranges
    def _empty_generator():
        yield from ()

    # Check that we got a legal range of values
    # i.e. the end minus the start divided by the
    # increment must be non-negative
    if end is not None and inc is not None:
        diff = (end - start) if end is not None else 1
        if not (diff / inc) > 0:
            return args['var'], _empty_generator

    return args['var'], _generator


def kill(args, env):
    """Kill the variables given in the argument list."""
    for var in args:
        env.kill(var)


def kill_all(args, env):
    """Kill all the symbols in the environment."""
    env.kill_all()


def job_cmd(args, env):
    """Spawn a new job at the given tag^routine with the given arguments."""
    for arg in args:
        # Unpack the argument
        sub, params, timeout = arg
        timeout = None if timeout is None else timeout.as_number()

        # Prepare the new process parameters
        cmd = [
            sys.argv[0],
            "-f", sub.rou.name(),
            "-t", str(sub.tag),
            "-r",
        ]

        # Add arguments if they are given
        if sub.args is not None:
            cmd.append("-a")
            cmd.append(" ".join(tuple(str(i) for i in sub.args)))

        # Handle any special parameters
        if params is not None:
            # Set the default device for this job
            if "device" in params:
                cmd.append("-dev")
                cmd.append(str(params["device"]))

        # Initiate the new process
        try:
            subprocess.call(cmd, timeout=timeout)
        except subprocess.TimeoutExpired:
            env.set("$T", mumps_false())


def quit_cmd(args, env):
    """Quit from the current scope."""
    # Quits from a DO block should have no argument
    if args is None:
        raise MUMPSReturn(None)

    # Check for multiple quit arguments
    if len(args) > 1:
        raise MUMPSSyntaxError("QUIT commands cannot have multiple arguments",
                               err_type="INVALID ARGUMENTS")

    # Return the evaluated expression otherwise
    raise MUMPSReturn(str(args[0]))


def open_dev(args, env):
    """Open the devices given in the argument list."""
    for arg in args:
        # Unpack the device and device parameters
        dev = arg[0]
        opts = arg[1]

        # Open the device
        env.open(dev)


def close_dev(args, env):
    """Close the devices given in the argument list."""
    for arg in args:
        # Unpack the device and device parameters
        dev = arg[0]
        opts = arg[1]

        # Open the device
        env.close(dev)


def use_dev(args, env):
    """Use the device listed in the argument list."""
    # We can only have one argument
    arg = args[0]

    # Unpack the device and any parameters
    dev = arg[0]
    opts = arg[1]

    # Set the device
    env.use(dev)


def read(args, env):
    """Read inputs from the current environment device."""
    read_last = False
    for item in args:
        if not item.is_ident():
            env.write(item)
            read_last = False
        else:
            ident = item.as_ident()
            temp = env.input(size=ident.get_max(), timeout=ident.get_timeout())
            env.set(ident, MUMPSExpression(temp))
            read_last = True

    # If we did not read last, output an extra newline
    if not read_last:
        env.write("\n")


def set_var(args, env):
    """Set the symbols in the argument list to the given expression."""
    for item in args:
        env.set(item[0], item[1].reduce())


def write(args, env):
    """Write out the given expressions."""
    for item in args:
        env.write(item)


def write_symbols(args, env):
    """Write out all of the symbols in the environment."""
    env.print()


def view_cmd(args, env):
    """Set an environmental factor."""
    #TODO: this
    pass


###################
# COMMAND RETURN EXCEPTION
# Commands raise exceptions if something needs to happen after they run.
# IF/ELSE commands will raise a MUMPSCommandEnd to indicate that the
# parser should continue to the next line. QUIT commands raise MUMPSReturn
# exceptions with their return value to indicate to the parser to return
# and return to the previous stack frame.
###################
class MUMPSReturn(Exception):
    """The expression return value from a function is encapsulated in this
    exception. Quit commands will raise this exception to let the
    interpreter know that it should return to the previous stack level."""
    def __init__(self, val):
        self._val = val

    def __str__(self):
        return str(self._val)

    def __repr__(self):
        return "MUMPSReturn({val})".format(val=self._val)

    def value(self):
        return self._val


class MUMPSCommandEnd(Exception):
    """An indicator to the parser that the current line should finish
    execution. Conditional commands raise this exception."""
    def __init__(self):
        pass


class MUMPSGotoLine(Exception):
    """An indicator to the parser to shift execution to the attached tag
    and routine. """
    def __init__(self, func):
        self.func = func


class MUMPSForLine(Exception):
    """An indicator to the MUMPS line execute function to defer the
    remainder of command executions to the `FOR` command contained herein."""
    def __init__(self, func):
        self.func = func


###################
# INTRINSIC FUNCTIONS
# Intrinsic functions are those functions in MUMPS which are prefixed
# with a single dollar sign and provide some functionality which is
# difficult or otherwise impossible to complete using the standard
# MUMPS commands.
###################
def intrinsic_ascii(expr, which=None):
    """Return the UTF-8 ordinal value for the character number in expr given
    by the input parameter `which` or -1 if `which` is larger than the
    length of the expression."""
    return MUMPSExpression(
        lambda e=expr, w=which: _ascii(e, w)
    )


def _ascii(expr, which=None):
    """Private ASCII function to allow repeated processing (in a loop)."""
    try:
        which = int(which.as_number())
    except AttributeError:
        which = 1

    try:
        if which-1 >= 0:
            char = ord(str(expr)[which-1])
        else:
            raise IndexError
    except IndexError:
        char = -1

    return char


def intrinsic_char(args):
    """Return the UTF-8 character value for the ordinal number or numbers
    given in the argument list."""
    return MUMPSExpression(
        lambda a=args: _char(a)
    )


def _char(args):
    """Private data function to allow repeated processing (in a loop)."""
    chars = []
    for arg in args:
        chars.append(chr(int(arg.as_number())))
    return ''.join(chars)


def intrinsic_data(ident, env):
    """Return a numeric value corresponding to the type of data in the
    given variable. If the variable is not defined, return 0. If the variable
    is defined and has no children nodes, return 1. If the variable base node
    has no value, but the variable with that name has a value, return 10. If
    the variable base node has a value and has children, return 11."""
    return MUMPSExpression(
        lambda i=ident, e=env: _data(i, e)
    )


def _data(ident, env):
    """Private data function to allow repeated processing (in a loop)."""
    # Check if the value is defined anywhere in the environment
    if not ident in env:
        return 0

    # If so, get it and query it for it's data
    var = env.get(ident, get_var=True)
    return var.data(ident)


def intrinsic_extract(expr, low=None, high=None):
    """Extract the first character from `expr` if neither `low` and `high`
    are given. If just `low` is given, then return the character at that
    index. If both `low` and `high` are given, return the substring from
    `low` to `high` index."""
    return MUMPSExpression(
        lambda e=expr, l=low, h=high: _extract(e, l, h)
    )


def _extract(expr, low=None, high=None):
    """Private extract function to allow repeated processing (in a loop)."""
    # Convert the low and high bounds to integers
    try:
        low = int(low.as_number())
    except AttributeError:
        low = 1

    try:
        high = int(high.as_number())
    except AttributeError:
        high = None

    try:
        s = str(expr)
        slen = len(s)
        if isinstance(low, int) and isinstance(high, int):
            # Get the indices (noting that the low index is inclusive)
            low -= 1

            # The low index cannot be higher than the high index
            # The low index cannot be below 0 either
            if low > high or low < 0:
                raise IndexError

            # The length cannot exceed the string length
            if high > slen:
                high = slen

            return MUMPSExpression(s[low:high])
        elif isinstance(low, int):
            # Get the index
            idx = low-1

            # The low index cannot be below 0
            if idx < 0 or idx > slen:
                raise IndexError

            return MUMPSExpression(s[idx])
        else:
            return MUMPSExpression(s[0])
    except IndexError:
        return mumps_null()


def intrinsic_find(expr, search, start=None):
    """Find the first instance of the substring `search` in `expr` after
    the`start`th character or from the beginning if `start` is None."""
    return MUMPSExpression(
        lambda e=expr, s=search, st=start: _find(e, s, st)
    )


def _find(expr, search, start=None):
    """Private find function to allow repeated processing (in a loop)."""
    try:
        start = int(start.as_number())
    except AttributeError:
        start = None

    try:
        sub = str(search)
        idx = str(expr).index(sub, start) + len(sub) + 1
    except ValueError:
        idx = mumpy.MUMPSExpression(0)

    return idx


def intrinsic_justify(expr, rspace, ndec=None):
    """Right justify the value given in `expr` by the number of spaces in
    `rspace`. If the optional `ndec` argument is given, then `expr` will
    be treated as numeric and rounded to `ndec` decimal places or zero
    padded if there are not at least `ndec` decimal places."""
    return MUMPSExpression(
        lambda e=expr, rs=rspace, nd=ndec: _justify(e, rs, nd)
    )


def _justify(expr, rspace, ndec=None):
    """Private justify function to allow repeated processing (in a loop)."""
    try:
        ndec = int(ndec.as_number())
    except AttributeError:
        ndec = None

    if ndec is not None:
        expr = round(expr.as_number(), ndec)
        s = str(expr).split(".")
        dig, dec = len(s[0]), len(s[1]) if len(s) > 1 else 0
        expr = expr if dec >= ndec else str(expr).ljust(ndec + dig + 1, '0')

    return str(expr).rjust(rspace)


def intrinsic_length(expr, char=None):
    """Compute the length of the input `expr` as a string or, if `char` is
    not None, count the number of occurrences of `char` in `expr`."""
    if char is not None:
        return MUMPSExpression(
            lambda e=expr, c=char: str(e).count(str(c))
        )
    return MUMPSExpression(
        lambda e=expr: len(str(e))
    )


def intrinsic_order(ident, env, rev=None):
    """Return the next subscript in the subscript level given by the input
    variable. If no more subscripts are defined, return null."""
    try:
        rev = rev.as_number()
    except AttributeError:
        rev = 1

    var = env.get(ident, get_var=True)
    return MUMPSExpression(
        lambda v=var, i=ident, r=rev: v.order(ident, rev=r)
    )


def intrinsic_piece(expr, char, num=None):
    """Give the `num`th piece of `expr` split about `char` (1 indexed)."""
    return MUMPSExpression(
        lambda e=expr, c=char, n=num: _piece(e, c, n)
    )


def _piece(expr, char, num=None):
    """Private piece function to allow repeated processing (in a loop)."""
    try:
        num = int(num.as_number())
    except AttributeError:
        num = 1

    try:
        return str(expr).split(sep=str(char), maxsplit=num)[num-1]
    except (IndexError, ValueError):
        return mumpy.mumps_null()


def intrinsic_random(num):
    """Given `num`, return a random integer in the range [0, num). Raise
    a syntax error if num is less than 1."""
    return MUMPSExpression(
        lambda n=num: _random(n)
    )


def _random(num):
    """Private random function to allow repeated processing (in a loop)."""
    num = int(num.as_number())

    if num < 1:
        raise mumpy.MUMPSSyntaxError("RANDOM argument less than 1.",
                                     err_type="RANDARGNEG")

    return random.randint(0, num)


def intrinsic_select(args):
    """Given a list of `args` (a list of expression tuples), return the
    expression in index 1 for the first expression in index 0 which
    evaluates to True."""
    return MUMPSExpression(
        lambda a=args: _select(a)
    )


def _select(args):
    """Private select function to allow repeated processing (in a loop)."""
    for arg in args:
        if arg[0]:
            return arg[1]
    raise mumpy.MUMPSSyntaxError("No select arguments evaluated true.",
                                 err_type="SELECTFALSE")


def intrinsic_translate(expr, trexpr, newexpr=None):
    """Remove every character in `trexpr` from `expr` if `newexpr` is None,
    or translate each character in `trexpr` to the identical index character
    from `newexpr`. If `newexpr` is shorter than `trexpr`, then remove the
    characters from `trexpr` which do not have siblings in `newexpr`."""
    return MUMPSExpression(
        lambda e=expr, tr=trexpr, nw=newexpr: _translate(e, tr, nw)
    )


def _translate(expr, trexpr, newexpr=None):
    """Private translate function to allow repeated processing (in a loop)."""
    # We use the string and translation map for both paths
    expr = str(expr)
    trexpr = str(trexpr)
    trmap = dict()

    if newexpr is not None:
        # Create a translation map between the two strings
        # In this iteration, we map positionally identical characters
        # between the input string and the replacement string
        # Characters without matches are deleted (i.e. if the
        # replacement string is shorter than the translation string)
        newmap = str(newexpr)
        for i, c in enumerate(trexpr):
            try:
                trmap[ord(c)] = ord(newmap[i])
            except IndexError:
                trmap[ord(c)] = None
    else:
        # Create a translation map which deletes each character
        for c in trexpr:
            trmap[ord(c)] = None

    # Return the translated string
    return expr.translate(trmap)


###################
# SPECIAL VARIABLE FUNCTIONS
# Special variables are values in MUMPS which return some state information
# about the environment to the caller. They cannot be assigned to as a
# normal variable and are always provided *by* the environment.
###################
def horolog():
    """Return the MUMPS $H time and date variable."""
    # Produce the string
    return MUMPSExpression(
        lambda: _horolog()
    )


def _horolog():
    """Private horolog function."""
    # Get today and now
    today = datetime.datetime.today()
    now = datetime.datetime.now()

    # Figure out the midnight of today so we can compute the timedelta
    midnight = datetime.datetime.combine(today, _horolog_midnight)

    # Produce the deltas
    daydelta = today - _horolog_origin
    tdelta = now - midnight

    # Produce the string
    return "{d},{t}".format(
        d=daydelta.days,
        t=tdelta.seconds,
    )


def current_job():
    """Return the MUMPS $J value, which is the current process ID."""
    return MUMPSExpression(os.getpid())


###################
# LANGUAGE COMPONENTS
# Language components are various components of the language which can be
# assembled and executed by the parser to provide the action or behavior
# specified in the ANSI/ISO M standard.
#
# Most of the components are designed in such a way that they can be
# assembled or compounded from repeated calls to the __init__ function.
###################
class MUMPSLine:
    """Represent a full line of MUMPS code."""
    def __init__(self, cmd, other=None):
        """Initialize the line"""
        if isinstance(other, MUMPSLine):
            self.list = other.list
            self.list.append(cmd)
        else:
            self.list = [cmd]

    def __repr__(self):
        """Return a string representation of this command."""
        return "MUMPSLine({cmds})".format(
            cmds=self.list,
        )

    def execute(self):
        """Execute the entire line of commands."""
        for i, cmd in enumerate(self.list):
            try:
                cmd.execute()
            except AttributeError:
                # Empty lines will return a command of None
                # Empty lines are generally comments - we ignore these
                pass
            except MUMPSForLine as f:
                # For exceptions instruct us to transfer execution of the line
                # to the FOR command and stop our own execution path
                f.func(cmd.args, cmd.env, self.list[i+1:])
                break


class MUMPSCommand:
    """Represents a MUMPS command execution."""
    def __init__(self, cmd, args, env, post=None):
        """Initialize the MUMPS command instance with a command and args and
        any given post-conditional expression."""
        # Check that we can call this command function
        if not callable(cmd):
            raise TypeError("A callable command function is expected.")

        # Check that we got an actual argument list
        if not isinstance(args, (type(None), dict, MUMPSArgumentList)):
            raise MUMPSSyntaxError("Commands require a valid argument list.",
                                   err_type="INVALID ARGUMENTS")

        # Check that our expression can be evaluated
        if not isinstance(post, (type(None), MUMPSExpression)):
            raise MUMPSSyntaxError(
                "Post-conditionals must be valid expressions.",
                err_type="INVALID POST-CONDITIONAL"
            )

        # Check that we got a valid environment
        if not isinstance(env, mumpy.MUMPSEnvironment):
            raise TypeError("A valid MUMPS environment must be specified.")

        self.cmd = cmd
        self.args = args
        self.env = env
        self.post = post

    def __repr__(self):
        """Return a string representation of this command."""
        return "MUMPSCommand({cmd}, {args}, {post})".format(
            cmd=self.cmd,
            args=self.args,
            post=self.post
        )

    def execute(self):
        """Execute the action of this command."""
        # Check the value of the post-conditional
        # Return if this expression evaluates as False
        if self.post is not None and not self.post:
            return None

        # Execute the command with the given arguments
        return self.cmd(self.args, self.env)


class MUMPSFuncSubCall:
    """Represents a function or subroutine call in a MUMPS routine."""
    def __init__(self, tag, env, parser, args=None,
                 is_func=False, rou=None, post=True):
        """Initialize a MUMPS Function or Subroutine call."""
        # The tag should be a valid MUMPS Identifier
        if not isinstance(tag, MUMPSIdentifier):
            raise MUMPSSyntaxError("Invalid Function or Subroutine name given.",
                                   err_type="INVALID FUNC OR SUB")

        # Check that we got an actual argument list
        if not isinstance(args,
                          (type(None), MUMPSArgumentList)) and not args == ():
            raise MUMPSSyntaxError("Functions and subroutines require a "
                                   "valid argument list.",
                                   err_type="INVALID ARGUMENTS")

        # Check that we got a valid environment
        if not isinstance(env, mumpy.MUMPSEnvironment):
            raise TypeError("A valid MUMPS environment must be specified.")

        # ...and parser
        if not isinstance(parser, mumpy.MUMPSParser):
            raise TypeError("A valid MUMPS parser must be specified.")

        self.tag = tag
        self.args = args
        self.env = env
        self.parser = parser
        self.is_func = is_func
        self.rou = self.env.get_routine(rou)
        self.post = post

        # If we don't have a routine at this point, we're in an error state
        if self.rou is None:
            raise MUMPSSyntaxError("No routine found.", err_type="NO LINE")

    def __repr__(self):
        return "MUMPSFuncSubCall({tag}, {args}, {as_func}, {rou})".format(
            tag=self.tag,
            args=self.args,
            as_func=self.is_func,
            rou=self.rou,
        )

    def execute(self):
        """Execute the subroutine or function call."""
        # Check for a post-conditional (for subroutines only)
        if not self.post:
            return None

        # Execute the function or subroutine
        self.env.push_func_to_stack(self)
        ret = mumpy.trampoline(self.parser._parse_tag, self.rou, self.tag)
        self.env.pop_func_from_stack()

        # Check for syntax errors
        if self.is_func and isinstance(ret, type(None)):
            raise MUMPSSyntaxError("Function call did not return value.",
                                   err_type="FUNCTION NO RETURN")
        if not self.is_func and not isinstance(ret, type(None)):
            raise MUMPSSyntaxError("Subroutine call returned a value.",
                                   err_type="SUBROUTINE RETURN")

        # Return our value
        return ret


class MUMPSExpression:
    """Performs arbitrarily complex expression computation using a deferred
    evaluation thunk with Python lambdas. Each operation returns a new
    unevaluated expression which can be str()'ed to calculate the value
    one time (but not save that value) or reduce()'ed to permanently
    reduce the expression value."""
    def __init__(self, expr):
        # Retain a reference to the identifier, if this expression is one
        self._ident = None

        # Handle cases where we may be assigned an
        # Identifier or another Expression
        if isinstance(expr, MUMPSExpression):
            self._val = expr._val
        elif isinstance(expr, MUMPSIdentifier):
            self._val = lambda: expr.value()
            self._ident = expr
        elif isinstance(expr, MUMPSFuncSubCall):
            self._val = lambda: expr.execute()
        elif callable(expr):
            self._val = expr
        elif expr is None:
            self._val = lambda: ""
        else:
            self._val = lambda: expr

    def as_number(self):
        """Return the canonical MUMPS numeric form of this expression."""
        return _mumps_number(str(self))

    def is_ident(self):
        """Return True if this expression is also an Identifier."""
        return True if self._ident is not None else False

    def as_ident(self):
        """Return the Identifier represented by this expression."""
        return self._ident

    def equals(self, other):
        """Returns True if this expression is equal to the other expression.

        Unlike the == overloaded method, this method will not modify the
        contents of the current expression."""
        return int(str(self) == str(MUMPSExpression(other)))

    def reduce(self):
        """Reduce the value of this expression into its internal value.

        Ex. `set x=1,x=x+1` should yield a value of `2`; reduce() reduces the
        value of `x+1` as it is saved into `x` again to avoid infinite
        recursion when trying to return the value."""
        #self._val =
        return MUMPSExpression(
            lambda v=self._val(): v
        )

    def __len__(self):
        """Return the expression length."""
        return len(str(self))

    def __bool__(self):
        """Return True if this expression evaluates to true."""
        return bool(self.as_number())

    def __hash__(self):
        """Return the hash of the current value."""
        return hash(str(self))

    def __str__(self):
        """Return the string value of the expression."""
        return str(self._val())

    def __repr__(self):
        """Return a string representation of this object."""
        return "MUMPSExpression('{expr}',{num},{bool})".format(
            expr=str(self),
            num=self.as_number(),
            bool=bool(self)
        )

    def __eq__(self, other):
        """Return 1 if both MUMPS expressions are equal."""
        return MUMPSExpression(
            lambda left=self, right=other: (
                int(str(left) == str(MUMPSExpression(right)))
            )
        )

    def __ne__(self, other):
        """Return 1 if this MUMPS expression is not equal to other."""
        return MUMPSExpression(
            lambda left=self, right=other: (
                int(str(left) != str(MUMPSExpression(right)))
            )
        )

    def __gt__(self, other):
        """Return 1 if this MUMPS expression is greater than other."""
        return MUMPSExpression(
            lambda left=self, right=other: (
                int(left.as_number() > _other_as_number(right))
            )
        )

    def __ge__(self, other):
        """Return 1 if this MUMPS expression is greater than or equal to
        other."""
        return MUMPSExpression(
            lambda left=self, right=other: (
                int(left.as_number() >= _other_as_number(right))
            )
        )

    def __lt__(self, other):
        """Return 1 if this MUMPS expression is less than other."""
        return MUMPSExpression(
            lambda left=self, right=other: (
                int(left.as_number() < _other_as_number(right))
            )
        )

    def __le__(self, other):
        """Return 1 if this MUMPS expression is less than or equal to other."""
        return MUMPSExpression(
            lambda left=self, right=other: (
                int(left.as_number() <= _other_as_number(right))
            )
        )

    def __add__(self, other):
        """Add two MUMPS expressions together."""
        return MUMPSExpression(
            lambda left=self, right=other: (
                _mumps_number(left.as_number() + _other_as_number(right))
            )
        )

    def __sub__(self, other):
        """Subtract two MUMPS expressions."""
        return MUMPSExpression(
            lambda left=self, right=other: (
                _mumps_number(left.as_number() - _other_as_number(right))
            )
        )

    def __mul__(self, other):
        """Multiple two MUMPS expressions."""
        return MUMPSExpression(
            lambda left=self, right=other: (
                _mumps_number(left.as_number() * _other_as_number(right))
            )
        )

    def __truediv__(self, other):
        """Divide two MUMPS expressions."""
        return MUMPSExpression(
            lambda left=self, right=other: (
                _mumps_number(left.as_number() / _other_as_number(right))
            )
        )

    def __floordiv__(self, other):
        """Integer divide two MUMPS expressions.

        See here for truncation towards zero:
        http://stackoverflow.com/questions/19919387/in-python-what-is-a-good-way-to-round-towards-zero-in-integer-division"""
        n = self.as_number()
        d = _other_as_number(other)
        v = n // d if n * d > 0 else (n + (-n % d)) // d
        return MUMPSExpression(
            lambda num=v: _mumps_number(num)
        )

    def __mod__(self, other):
        """Return the modulus of two MUMPS expressions."""
        return MUMPSExpression(
            lambda left=self, right=other: (
                _mumps_number(left.as_number() % _other_as_number(right))
            )
        )

    def __pow__(self, power, modulo=None):
        """Return the power of two MUMPS expressions."""
        return MUMPSExpression(
            lambda left=self, right=power: (
                _mumps_number(left.as_number() ** _other_as_number(right))
            )
        )

    def __neg__(self):
        """Return the unary negative of this MUMPS expression."""
        return MUMPSExpression(
            lambda right=self: -right.as_number()
        )

    def __pos__(self):
        """Return the unary positive of this MUMPS expression."""
        return MUMPSExpression(
            lambda right=self: right.as_number()
        )

    def __and__(self, other):
        """Return the AND result of two MUMPS expressions."""
        return MUMPSExpression(
            lambda left=self, right=other: (
                int(left.as_number() and _other_as_number(right))
            )
        )

    def __or__(self, other):
        """Return the OR result of two MUMPS expressions."""
        return MUMPSExpression(
            lambda left=self, right=other: (
                int(left.as_number() or _other_as_number(right))
            )
        )

    def __invert__(self):
        """Return the NOT result of this MUMPS expression."""
        return MUMPSExpression(
            lambda right=self: int(not right.as_number())
        )

    def concat(self, other):
        """Concatenates two MUMPS values together and returns itself.."""
        return MUMPSExpression(
            lambda left=self, right=other: (
                "{}{}".format(str(left), str(MUMPSExpression(right)))
            )
        )

    def sorts_after(self, other):
        """Return True if this MUMPS expression sorts after other in
        collation (UTF-8) order."""
        o = MUMPSExpression(other)
        s = str(self)
        l = sorted([s, str(o)])
        return MUMPSExpression(
            lambda me=s: 1 if (me == l[1]) else 0
        )

    def follows(self, other):
        """Return True if this MUMPS expression follows other in
        binary order."""
        o = MUMPSExpression(other)
        b = bytes(str(self), encoding='utf8')
        l = sorted([b, bytes(str(o), encoding='utf8')])
        return MUMPSExpression(
            lambda me=b: 1 if (b == l[1]) else 0
        )

    def contains(self, other):
        return MUMPSExpression(
            lambda left=self, right=other: 0 if (
                str(left).find(str(MUMPSExpression(right))) == -1
            ) else 1
        )


def mumps_null():
    """Return the MUMPS null value, ""."""
    return MUMPSExpression(None)


def mumps_true():
    """Return the MUMPS true value, 1."""
    return MUMPSExpression(1)


def mumps_false():
    """Return the MUMPS false value, 0."""
    return MUMPSExpression(0)


class MUMPSIdentifier:
    """Represents a MUMPS identifier in code."""
    def __init__(self, ident, env, subscripts=None, max=None, timeout=None):
        # Handle the case that we may be passed another instance of
        # a MUMPSIdentifier object
        if isinstance(ident, MUMPSIdentifier):
            self._ident = str(ident._ident)
        else:
            self._ident = str(ident)

        # Make sure we got valid subscripts
        if not isinstance(subscripts, (type(None), MUMPSArgumentList)):
            raise MUMPSSyntaxError("Invalid subscript list given.",
                                   err_type="INVALID SUBSCRIPTS")

        self._subscripts = subscripts

        # Accept an environment so we can resolve our own value
        self._env = env

        # Check that we are a valid identifier
        self.is_valid()

        # Set the maximum number of bytes for this variable
        self._max = max

        # Set the read timeout for this variable
        self._timeout = timeout

    def __eq__(self, other):
        """Return True if this Identifier is equal to other."""
        if isinstance(other, MUMPSIdentifier):
            return self._ident == other._ident
        return False

    def __hash__(self):
        """Return the hash associated with this identifier."""
        return hash(self._ident)

    def __str__(self):
        """Return the string name of the identifier."""
        return str(self._ident)

    def __repr__(self):
        """Return a string representation of this object."""
        return "MUMPSIdentifier('{ident}',{val})".format(
            ident=self._ident,
            val=self.value()
        )

    def get_max(self):
        """Return the number of bytes to read into this variable."""
        return self._max

    def set_max(self, m):
        """Sets the maximum number of bytes that will be read into this
        variable."""
        if not isinstance(m, (type(None), int)):
            raise MUMPSSyntaxError("Maximum read size for variable invalid.",
                                   err_type="INVALID READ SIZE")

        self._max = m
        return self

    def get_timeout(self):
        """Gets the maximum number of seconds to wait for input."""
        return self._timeout

    def set_timeout(self, t):
        """Sets the maximum number of seconds to wait for input."""
        if not isinstance(t, (type(None), int)):
            raise MUMPSSyntaxError("Maximum read size for variable invalid.",
                                   err_type="INVALID READ SIZE")

        self._timeout = t
        return self

    def value(self):
        """Return the resolved value of this Identifier."""
        self.subscripts_valid()
        return self._env.get(self)

    def subscripts(self):
        """Return the subscripts associated with this Identifier."""
        return self._subscripts

    def is_valid(self):
        """Returns True if this is a valid MUMPS identifier."""
        c = self._ident[0]
        if c.isdigit():
            raise MUMPSSyntaxError("Variable names cannot start with digits.")
        if not c in "{}{}".format(string.ascii_letters, "%"):
            raise MUMPSSyntaxError("Variable names must be valid "
                                   "ASCII letters or the '%' character.")

    def subscripts_valid(self):
        """Check for valid subscripts in this identifier."""
        if self._subscripts is not None and mumps_null() in self._subscripts:
            raise MUMPSSyntaxError("Null subscript given for identifier.",
                                   err_type="NULL SUBSCRIPT")


class MUMPSPointerIdentifier(MUMPSIdentifier):
    """Represents a normal MUMPS identifier which will be used as a pointer
    when passed into a function or subroutine."""
    def __init__(self, ident, env):
        super().__init__(ident, env)


class MUMPSArgumentList:
    """Holds a list of MUMPS arguments for a command to process."""
    def __init__(self, item, others=None):
        if isinstance(others, MUMPSArgumentList):
            self.list = others.list
            self.list.append(item)
        else:
            self.list = [item]

    def __repr__(self):
        """A string representation of the argument list."""
        return "MUMPSArgumentList({})".format(self.list)

    def __iter__(self):
        """Provide an iterator for the arguments."""
        return iter(self.list)

    def __len__(self):
        """Return the number of arguments in this list."""
        return len(self.list)

    def __getitem__(self, item):
        """Return the argument at the specified index."""
        return self.list[item]

    def __contains__(self, item):
        return item in self.list

    def reverse(self):
        """Return the reverse of the argument list."""
        self.list.reverse()
        return self


class MUMPSLocal:
    """Wrap a SortedDict to provide MUMPS local variable functionality."""
    def __init__(self, value=None):
        self._b = SortedDict()
        self._n = 0
        if value is not None:
            self._b[""] = value

    def __repr__(self):
        return "MUMPSLocal({root}, {n})".format(
            root=self._b[""],
            n=self._n,
        )

    def __str__(self):
        """Return the value of the root node."""
        try:
            return str(self._b[""])
        except KeyError:
            return ""

    def get(self, ident):
        """Return the value given by the input identifier. If the identifier
        has no subscripts, then return the root node. If the subscript or
        subscripts don't exist, simply return null."""
        try:
            s = ident.subscripts() if not isinstance(ident, str) else None

            # Return the root value
            if s is None:
                return self._b[""]

            # Otherwise, return the value at the requested subscripts
            b = self._b
            for sub in s:
                b = b[str(sub)]
            return b[""]
        except AttributeError:
            raise MUMPSSyntaxError("Invalid identifier given for this var.",
                                   "INVALID IDENTIFIER")
        except KeyError:
            return mumps_null()

    def set(self, ident, value):
        """Set the value at the given identifier. If the identifier has no
        subscripts, then set the root node."""
        try:
            s = ident.subscripts() if not isinstance(ident, str) else None

            # Set the root value
            if s is None:
                self._b[""] = value
                return

            # Otherwise, set the value at the requested subscripts
            b = self._b
            for sub in s:
                ss = str(sub)
                try:
                    b = b[ss]
                except KeyError:
                    b[ss] = SortedDict()
                    b = b[ss]
            b[""] = value
        except AttributeError:
            raise MUMPSSyntaxError("Invalid identifier given for this var.",
                                   "INVALID IDENTIFIER")

    def delete(self, ident):
        """Delete the value at the given identifier. If the root node is
        deleted, then every child node is also deleted. As a consequence of
        this behavior, the environment actually performs the delete for
        that particular case. This means that we should always have a list
        of subscripts."""
        try:
            s = ident.subscripts()

            if len(s) < 1:
                raise MUMPSSyntaxError("Variable with no subscripts given.",
                                       err_type="INVALID SUBSCRIPTS")

            self._delete(self._b, s)
        except AttributeError:
            raise MUMPSSyntaxError("Invalid identifier given for this var.",
                                   err_type="INVALID IDENTIFIER")

    def _delete(self, d, subscripts):
        """Recursive delete helper function. Since MUMPS kill command permits"""
        l = len(subscripts)

        # If there is only one subscript, delete that node and return
        if l == 1:
            try:
                s = str(subscripts[0])
                del d[s]
            except KeyError:
                pass

            return

        # If there are more subscripts, recurse
        try:
            s = str(subscripts[0])
            self._delete(d[s], subscripts[1:])
        except KeyError:
            pass

    def data(self, ident):
        """Return the `$DATA` value for this variable. Note that since you
        have to query this object for that value, you automatically know that
        it cannot be 0, as at least one node at some point is defined."""
        try:
            s = ident.subscripts() if not isinstance(ident, str) else None

            # Set the root value
            b = self._b

            # If we were given subscripts, recurse down to that node level
            if s is not None:
                for sub in s:
                    ss = str(sub)
                    try:
                        b = b[ss]
                    except KeyError:
                        return MUMPSExpression(0)

            # Determine which data value we have
            num = len(b)
            root = int("" in b)
            children = int((root and num > 1) or (not root and num > 0)) * 10
            return MUMPSExpression(root + children)
        except AttributeError:
            raise MUMPSSyntaxError("Invalid identifier given for this var.",
                                   "INVALID IDENTIFIER")

    def order(self, ident, rev=0):
        """Return an iterator for the given identifier. If the identifier
        has no subscripts, then raise a syntax error. If `rev` is -1, then
        iterate on subscripts in reverse order."""
        try:
            s = ident.subscripts() if not isinstance(ident, str) else None

            # Set the root value
            if s is None:
                raise MUMPSSyntaxError("Cannot $ORDER over a scalar value.",
                                       err_type="INVALID $ORDER PARAM")

            # Otherwise, set the value at the requested subscripts
            b = self._b
            if rev >= 0:
                ss = str(s[0])
                for sub in s[:-1]:
                    ss = str(sub)
                    try:
                        b = b[ss]
                    except KeyError:
                        pass

                return b.next_key(ss)
            else:
                ss = str(s[len(s)-1])
                for sub in s[1:]:
                    ss = str(sub)
                    try:
                        b = b[ss]
                    except KeyError:
                        pass

                return b.prev_key(ss)
        except AttributeError:
            raise MUMPSSyntaxError("Invalid identifier given for this var.",
                                   "INVALID IDENTIFIER")

    def pprint_str(self, name):
        """Return a pretty-print style string which can be output when
        the user issues an argumentless `WRITE` command (spill symbols)."""
        return self._pprint_str(name)

    def _pprint_str(self, name, sub=None, depth=0):
        """Recursive pretty-print function for this MUMPSLocal value."""
        # Set up the output string and which sub node to iterate on
        if sub is None:
            out = "{name}={val}".format(name=name, val=str(self))
            sub = self._b
            name = "{name}(".format(name=name)
        else:
            out = "{name})={val}".format(name=name, val=str(sub[""]))

        # Iterate on each key in the node
        for k in sub:
            if k == "":
                continue
            out = "{base}\n{next}".format(base=out,
                                          next=self._pprint_str(
                                              '{name}{sep}"{k}"'.format(
                                                  name=name,
                                                  sep="," if depth > 0 else "",
                                                  k=k),
                                              sub=sub[k],
                                              depth=depth+1
                                          )
            )
        return out


class SortedDict(blist.sorteddict):
    """Sub-class the blist Sorted Dictionary to provide a next-key
    functionality. Standard blist.sorteddict doesn't let you get the next
    key in lexicographical order - it just throws a KeyError."""
    def __init__(self, *args, **kwargs):
        super(SortedDict, self).__init__(*args, **kwargs)
        self._keys = self.keys()

    def next_key(self, key):
        """Return the next key in sorted order."""
        try:
            # We need to handle the null key differently, since that
            # actually holds our so-called "root node" value, which doesn't
            # sort in the rest of the tree in MUMPS
            if key == "" and "" in self._keys:
                i = 1
            else:
                i = self._keys.index(key) + 1

            # Get the next key sequentially
            k = self._keys[i]
            return k
        except ValueError:
            # If we don't have an existing key, we unfortunately have to
            # iterate through the entire key list to find the next
            # key lexicographically
            for k in self._keys:
                if k < key:
                    continue
                elif k > key:
                    return k

            # If we don't find another one, then return the null key
            return ""
        except IndexError:
            return ""

    def prev_key(self, key):
        """Return the previous key in sorted order."""
        try:
            # The null key matters less in the previous key case, since it
            # will not sort "first"
            if key == "":
                i = len(self)
            else:
                i = self._keys.index(key)

            # Do not return the null key for the reverse iteration
            if (i == 1 and "" in self._keys) or (i == 0):
                return ""

            # Get the next key sequentially
            k = self._keys[i-1]
            return k
        except ValueError:
            # If we don't have an existing key, we unfortunately have to
            # iterate through the entire key list to find the next
            # key lexicographically
            for k in reversed(self._keys):
                if k > key:
                    continue
                elif k < key:
                    return k

            # If we don't find another one, then return the null key
            return ""
        except IndexError:
            return ""


class MUMPSSyntaxError(Exception):
    """Represents a MUMPS syntax error."""
    def __init__(self, err, err_type=None, line=None):
        if isinstance(err, MUMPSSyntaxError):
            self.msg = err.msg
            self.err_type = err.err_type
            self.line = line
        else:
            self.msg = str(err)
            self.err_type = "UNKNOWN" if err_type is None else err_type
            self.line = line
            #TODO.md: Remove this print_exc call
            try:
                traceback.print_exc()
            except:
                pass

    def __str__(self):
        return "SYNTAX ERROR <{type}{line}>: {msg}".format(
            type=self.err_type,
            msg=self.msg,
            line=":{num}".format(num=self.line) if self.line is not None else "",
        )

    def __repr__(self):
        return str(self)


def _mumps_number(v):
    """Given a number (perhaps evaluated by expression), return the
    integral value if it is an integer, or a float otherwise."""
    try:
        return _to_mumps_number(v)
    except ValueError:
        return 0


def _to_mumps_number(v):
    """Given a value, attempt to coerce it to either an integer or float."""
    sign = 1
    ndec = 0
    try:
        tmp = float(v)

        if tmp.is_integer():
            return int(tmp)
        else:
            return tmp
    except ValueError:
        v = str(v)
        n = []

        # Build a number based on the MUMPS numeric conversion rules
        for c in v:
            # Look for numeric characters (digits, decimal, or sign)
            if c.isnumeric() or c in ('.', '+', '-'):
                # Make sure we only add one decimal
                if c == '.':
                    if ndec >= 1:
                        break
                    else:
                        ndec += 1

                # Correctly swap the sign
                if c == '-':
                    sign *= -1
                    continue

                # Ignore the plus signs
                if c == '+':
                    continue

                # If we made it this far, this is a valid numeric character
                n.append(c)
            else:
                # If we don't find any,
                break

        # Re-assemble the digits and attempt to convert it
        n = float("".join(n)) * sign
        return n if not n.is_integer() else int(n)


def _other_as_number(other):
    """Return the `as_number` value from the other MUMPSExpression or
    0 if the other value is not a MUMPSExpression."""
    return MUMPSExpression(other).as_number()
