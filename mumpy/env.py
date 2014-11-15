"""MUMPy environment

Provide environment stack management and other environment functions
such as maintaining the current input and output devices."""
__author__ = 'christopher'
import sys
import mumpy


class MUMPSEnvironment:
    """A MUMPy execution stack."""
    def __init__(self, in_dev=sys.stdin,
                 out_dev=sys.stdout, err_dev=sys.stderr):
        # Current stack level and variable stack
        self.cur = 0
        self.stack = [{}]

        # Input and output devices in the environment
        self.in_dev = in_dev
        self.out_dev = out_dev
        self.err_dev = err_dev

        # Function and subroutine call stack
        self.call_stack = []

        # Already opened routines
        self.routines = {}

    def __repr__(self):
        """String representation of this environment."""
        return "Environment({lvl}, {rou})".format(
            lvl=self.cur,
            rou=self.get_current_rou(),
        )

    def get_routine(self, rou):
        """Query the environment for a routine. The environment keeps a
        cache of previously accessed routines so they can be accessed
        more quickly in the future."""
        # If the routine is None, then return the current routine
        if rou is None:
            return self.get_current_rou()

        # Check the cache first
        if rou in self.routines:
            return self.routines[rou]

        # Try to load the routine now
        try:
            f = mumpy.MUMPSFile(rou)
            self.routines[rou] = f
            return f
        except mumpy.MUMPSCompileError as e:
            raise mumpy.MUMPSSyntaxError(e, err_type="NO LINE")

    def get_current_rou(self):
        """Return the current environment routine."""
        if len(self.call_stack) > 0:
            return self.call_stack[self.cur-1][1]
        return None

    def push_func_to_stack(self, func):
        """Given a MUMPS Function or Subroutine call, push the necessary
        variables onto the next stack level at the correct name from the
        tag's argument list."""
        # Verify first that we got a function or subroutine call
        if not isinstance(func, mumpy.MUMPSFuncSubCall):
            raise TypeError(
                "Expecting Function call, got {}".format(type(func)))

        # Push a new stack frame
        self.push()
        self.call_stack.append((func.tag, func.rou))

        # Get the argument list and push existing values onto the new frame
        args = func.rou.tag_args(func.tag)

        # Check for syntax errors:
        # If args is None and the function call provided arguments, or
        # If args is not None and the call did not provide arguments, or
        # If args is a shorter list than the input argument list
        if args is None and func.args is not None:
            raise mumpy.MUMPSSyntaxError("Function or subroutine does not "
                                         "have argument list.",
                                         err_type="NO ARGUMENTS")
        elif args is not None and func.args is None:
            raise mumpy.MUMPSSyntaxError("Function or subroutine has "
                                         "arguments, but none provided.",
                                         err_type="TOO MANY ARGUMENTS")
        elif ((args is not None and func.args is not None) and
                (len(args) < len(func.args))):
            raise mumpy.MUMPSSyntaxError("Function or subroutine has fewer "
                                         "arguments than provided.",
                                         err_type="TOO MANY ARGUMENTS")

        # Return if no arguments for this function
        if args is None:
            return

        # Push the argument list on the stack
        for i, arg in enumerate(args):
            # Convert the tag argument name to an identifier
            # This is the new name of the symbol on the current stack frame
            ident = mumpy.MUMPSIdentifier(arg, self)

            # Try to get the matching value from the input list
            # If we can't find it, that's fine; MUMPS functions do not
            # require any or all parameters to be input - just set it null.
            #
            # Check for TypeError in case the input argument list is None.
            try:
                in_arg = str(mumpy.MUMPSExpression(func.args[i]))
            except (IndexError, TypeError):
                in_arg = mumpy.mumps_null()

            # New the argument list name
            self.new(ident)

            # Set the new value
            self.set(ident, str(in_arg))

    def pop_func_from_stack(self):
        """Return execution to the original function or subroutine."""
        self.pop()
        self.call_stack.pop()

    ###################
    # SYMBOL FUNCTIONS
    ###################
    def get(self, key):
        """Return the item named at the current stack level or fall down
        the stack until we find an item with that name."""
        for frame in reversed(self.stack):
            if key in frame:
                return frame[key]

        return mumpy.mumps_null()

    def set(self, key, value):
        """Set the item with the given name at the highest stack level
        we can find it at. If it does not exist at any stack level,
        set it at the current stack level."""
        for frame in reversed(self.stack):
            if key in frame:
                frame[key] = value
                return
        self.stack[self.cur][key] = value

    def new(self, key):
        """Create a new symbol with the given name on the current stack
        level with a None value."""
        if not key in self.stack[self.cur]:
            self.stack[self.cur][key] = mumpy.mumps_null()

    def kill(self, key):
        """Kill a symbol with the given key at the highest stack level we
        can find it at. If that symbol doesn't exist, do nothing."""
        for frame in reversed(self.stack):
            if key in frame:
                del frame[key]
                return

    def kill_all(self):
        """Clears the entire symbol table (all the way down the stack)."""
        for frame in reversed(self.stack):
            frame.clear()

    def push(self):
        """Push a new frame onto the stack."""
        self.stack.append({})
        self.cur += 1

    def pop(self):
        """Pop the last frame off the stack."""
        self.stack.pop()
        self.cur -= 1

    def print(self):
        """Print the stack frames in reverse order."""
        for i in range(self.cur, -1, -1):
            self.writeln("[Stack Frame {}]".format(i))
            for k, v in self.stack[i].items():
                self.writeln("{key}={val}".format(key=k, val=v))

    ###################
    # OUTPUT FUNCTIONS
    ###################
    def input(self, size=None, prompt=None):
        """Input from the current input device."""
        # Provide a prompt for the read
        if isinstance(prompt, str):
            self.out_dev.write(prompt)

        # Allow reading input of a certain size
        if isinstance(size, int):
            val = self.in_dev.read(size)
        else:
            val = self.in_dev.readline()
        return val[:-1] if val.endswith("\n") else val

    def write(self, data, flush=True):
        """Output to the current output device."""
        self.out_dev.write(str(data))
        if flush:
            self.out_dev.flush()

    def writeln(self, data):
        """Write to the current output device; appends a newline to output."""
        self.out_dev.write("{data}\n".format(data=str(data)))

    def write_error(self, data):
        """Output to the current error device."""
        self.err_dev.write(str(data))

    def writeln_error(self, data):
        """Output the current error device; appends a newline to output."""
        self.err_dev.write("{data}\n".format(data=str(data)))

    def write_stack(self):
        """Output a stack trace to the current error device."""
        for i, frame in enumerate(self.call_stack):
            self.writeln_error("{i} :: {tag}^{rou}".format(
                i=i,
                tag=frame[0],
                rou=frame[1].name(),
            ))
