"""MUMPY MUMPS language components"""
__author__ = 'christopher'
import string
import time
import mumpy


###################
# COMMAND FUNCTIONS
###################
def new_var(args, env):
    """New the variables given in the argument list."""
    for var in args:
        env.new(var)
    return None


def do_cmd(args, env):
    """Perform a subroutine call."""
    for sub in args:
        env.push_func_to_stack(sub)
        sub.execute()


def halt(args, env):
    """Quit from the MUMPy environment."""
    raise SystemExit(0)


def hang(args, env):
    """Sleep the system for the given number of seconds."""
    for slp in args:
        time.sleep(slp)
    return None


def if_cmd(args, env):
    """Process an IF MUMPS command."""
    for expr in args:
        if not expr:
            env.set("$T", False)
            return False
    env.set("$T", True)
    return True


def kill(args, env):
    """Kill the variables given in the argument list."""
    for var in args:
        env.kill(var)
    return None


def kill_all(args, env):
    """Kill all the symbols in the environment."""
    env.kill_all()
    return None


def quit_cmd(args, env):
    """Quit from the current scope."""
    if args is None:
        pass
    else:
        pass
    return None


def read(args, env):
    """Read inputs from the current environment device."""
    read_last = False
    for item in args:
        if not item.is_ident():
            env.write(item)
            read_last = False
        else:
            temp = env.input()
            env.set(item.as_ident(), MUMPSExpression(temp))
            read_last = True

    # If we did not read last, output an extra newline
    if not read_last:
        env.write("\n")

    return None


def set_var(args, env):
    """Set the symbols in the argument list to the given expression."""
    for item in args:
        env.set(item[0], item[1])
    return None


def write(args, env):
    """Write out the given expressions."""
    for item in args:
        env.write(item)
    env.write("\n")
    return None


def write_symbols(args, env):
    """Write out all of the symbols in the environment."""
    env.print()
    return None


###################
# LANGUAGE COMPONENTS
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
        last = True
        for cmd in self.list:
            # If the last command returned False, quit
            if last is not None and not last:
                return

            # Execute the next command and get it's return value
            last = cmd.execute()


class MUMPSCommand:
    """Represents a MUMPS command execution."""
    def __init__(self, cmd, args, env, post=None):
        """Initialize the MUMPS command instance with a command and args and
        any given post-conditional expression."""
        # Check that we can call this command function
        if not callable(cmd):
            raise TypeError("A callable command function is expected.")

        # Check that we got an actual argument list
        if not isinstance(args, (type(None), MUMPSArgumentList)):
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
        return "MUMPSCommand({cmd}, {args}, {env}, {post})".format(
            cmd=self.cmd,
            args=self.args,
            env=self.env,
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
    def __init__(self, tag, env, args=None, is_func=False, rou=None):
        """Initialize a MUMPS Function or Subroutine call."""
        # The tag should be a valid MUMPS Identifier
        if not isinstance(tag, MUMPSIdentifier):
            raise MUMPSSyntaxError("Invalid Function or Subroutine name given.",
                                   err_type="INVALID FUNC OR SUB")

        # Check that we got an actual argument list
        if not isinstance(args, (type(None), MUMPSArgumentList)):
            raise MUMPSSyntaxError("Functions and subroutines require a "
                                   "valid argument list.",
                                   err_type="INVALID ARGUMENTS")

        # Check that we got a valid environment
        if not isinstance(env, mumpy.MUMPSEnvironment):
            raise TypeError("A valid MUMPS environment must be specified.")

        self.tag = tag
        self.args = args
        self.env = env
        self.as_func = is_func
        self.rou = rou

    def __repr__(self):
        return ("MUMPSFuncSubCall({tag}, {args}, {env}, "
                "{as_func}, {rou})".format(
                    tag=self.tag,
                    args=self.args,
                    env=self.env,
                    as_func=self.as_func,
                    rou=self.rou,
                ))

    def execute(self):
        pass


class MUMPSExpression:
    """Performs arbitrarily complex expression computation."""
    def __init__(self, expr):
        # Handle cases where we may be assigned an
        # Identifier or another Expression
        if isinstance(expr, MUMPSExpression):
            self.expr = expr.expr
        elif isinstance(expr, MUMPSIdentifier):
            self.expr = expr
        elif expr is None:
            self.expr = ""
        else:
            self.expr = expr

    def as_number(self):
        """Return the canonical MUMPS numeric form of this expression."""
        if isinstance(self.expr, MUMPSIdentifier):
            return _mumps_number(str(self.expr.value()))
        return _mumps_number(self.expr)

    def is_ident(self):
        """Return True if this expression is also an Identifier."""
        return True if isinstance(self.expr, MUMPSIdentifier) else False

    def as_ident(self):
        """Return the Identifier represented by this expression."""
        return self.expr if isinstance(self.expr, MUMPSIdentifier) else None

    def __getitem__(self, item):
        """Enable MUMPSExpressions to be subscriptable."""
        return self.expr.__getitem__(item)

    def __len__(self):
        """Return the expression length."""
        return len(self.expr)

    def __bool__(self):
        """Return True if this expression evaluates to true."""
        return bool(self.as_number())

    def __str__(self):
        """Return the string value of the expression."""
        if isinstance(self.expr, MUMPSIdentifier):
            return str(self.expr.value())
        return str(self.expr)

    def __repr__(self):
        """Return a string representation of this object."""
        return "MUMPSExpression('{expr}',{str},{num},{bool})".format(
            expr=repr(self.expr),
            str=str(self),
            num=self.as_number(),
            bool=bool(self)
        )

    def __eq__(self, other):
        """Return 1 if both MUMPS expressions are equal."""
        self.expr = int(str(self) == str(MUMPSExpression(other)))
        return self

    def __ne__(self, other):
        """Return 1 if this MUMPS expression is not equal to other."""
        self.expr = int(str(self) != str(MUMPSExpression(other)))
        return self

    def __gt__(self, other):
        """Return 1 if this MUMPS expression is greater than other."""
        self.expr = int(self.as_number() > _other_as_number(other))
        return self

    def __lt__(self, other):
        """Return 1 if this MUMPS expression is less than other."""
        self.expr = int(self.as_number() < _other_as_number(other))
        return self

    def __add__(self, other):
        """Add two MUMPS expressions together."""
        self.expr = _mumps_number(self.as_number() + _other_as_number(other))
        return self

    def __sub__(self, other):
        """Subtract two MUMPS expressions."""
        self.expr = _mumps_number(self.as_number() - _other_as_number(other))
        return self

    def __mul__(self, other):
        """Multiple two MUMPS expressions."""
        self.expr = _mumps_number(self.as_number() * _other_as_number(other))
        return self

    def __truediv__(self, other):
        """Divide two MUMPS expressions."""
        self.expr = _mumps_number(self.as_number() / _other_as_number(other))
        return self

    def __idiv__(self, other):
        """Integer divide two MUMPS expressions."""
        self.expr = _mumps_number(self.as_number() // _other_as_number(other))
        return self

    def __mod__(self, other):
        """Return the modulus of two MUMPS expressions."""
        self.expr = _mumps_number(self.as_number() % _other_as_number(other))
        return self

    def __pow__(self, power, modulo=None):
        """Return the power of two MUMPS expressions."""
        self.expr = _mumps_number(self.as_number() ** _other_as_number(power))
        return self

    def __neg__(self):
        """Return the unary negative of this MUMPS expression."""
        self.expr = -self.as_number()
        return self

    def __pos__(self):
        """Return the unary positive of this MUMPS expression."""
        self.expr = self.as_number()
        return self

    def __and__(self, other):
        """Return the AND result of two MUMPS expressions."""
        self.expr = int(self.as_number() and _other_as_number(other))
        return self

    def __or__(self, other):
        """Return the OR result of two MUMPS expressions."""
        self.expr = int(self.as_number() or _other_as_number(other))
        return self

    def __invert__(self):
        """Return the NOT result of this MUMPS expression."""
        self.expr = int(not self.as_number())
        return self

    def concat(self, other):
        """Concatenates two MUMPS values together and returns itself.."""
        self.expr = "{}{}".format(self.expr, str(MUMPSExpression(other)))
        return self

    def sorts_after(self, other):
        """Return True if this MUMPS expression sorts after other in
        collation (UTF-8) order."""
        o = MUMPSExpression(other)
        s = str(self.expr)
        l = sorted([s, str(o)])
        self.expr = 1 if (s == l[1]) else 0
        return self

    def follows(self, other):
        """Return True if this MUMPS expression follows other in
        binary order."""
        o = MUMPSExpression(other)
        b = bytes(self.expr)
        l = sorted([b, bytes(str(o))])
        self.expr = 1 if (b == l[1]) else 0
        return self

    def contains(self, other):
        self.expr = 0 if (
            str(self.expr).find(str(MUMPSExpression(other))) == -1
        ) else 1
        return self


class MUMPSIdentifier:
    """Represents a MUMPS identifier in code."""
    def __init__(self, ident, env):
        # Handle the case that we may be passed another instance of
        # a MUMPSIdentifier object
        if isinstance(ident, MUMPSIdentifier):
            self._ident = str(ident._ident)
        else:
            self._ident = str(ident)

        # Accept an environment so we can resolve our own value
        self._env = env

        # Check that we are a valid identifier
        self.is_valid()

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
        return "MUMPSIdentifier('{ident}',{env},{val})".format(
            ident=self._ident,
            env=self._env,
            val=self.value()
        )

    def value(self):
        """Return the resolved value of this Identifier."""
        return self._env.get(self)

    def is_valid(self):
        """Returns True if this is a valid MUMPS identifier."""
        c = self._ident[0]
        if c.isdigit():
            raise MUMPSSyntaxError("Variable names cannot start with digits.")
        if not c in "{}{}".format(string.ascii_letters, "%"):
            raise MUMPSSyntaxError("Variable names must be valid "
                                   "ASCII letters or the '%' character.")


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

    def reverse(self):
        """Return the reverse of the argument list."""
        self.list.reverse()
        return self


class MUMPSSyntaxError(Exception):
    """Represents a MUMPS syntax error."""
    def __init__(self, err, err_type=None):
        if isinstance(err, MUMPSSyntaxError):
            self.msg = err.msg
            self.err_type = err.err_type
        else:
            self.msg = str(err)
            self.err_type = "UNKNOWN" if err_type is None else err_type

    def __str__(self):
        return "SYNTAX ERROR <{type}>: {msg}".format(
            type=self.err_type,
            msg=self.msg
        )

    def __repr__(self):
        return str(self)


def _mumps_number(v):
    """Given a number (perhaps evaluated by expression), return the
    integral value if it is an integer, or a float otherwise."""
    try:
        tmp = float(v)

        if tmp.is_integer():
            return int(tmp)
        else:
            return tmp
    except ValueError:
        return 0


def _other_as_number(other):
    """Return the `as_number` value from the other MUMPSExpression or
    0 if the other value is not a MUMPSExpression."""
    return MUMPSExpression(other).as_number()
