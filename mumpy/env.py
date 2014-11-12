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
        # Current stack level
        self.cur = 0

        # Stack
        self.stack = [{}]

        # Input and output devices in the environment
        self.in_dev = in_dev
        self.out_dev = out_dev
        self.err_dev = err_dev

        # Environment defaults
        self.current_tag = None
        self.current_rou = None

    def __repr__(self):
        """String representation of this environment."""
        return "Environment()"

    def set_current_tag(self, tag, rou=None):
        """Set the currently executing tag in the environment. If rou is
        not specified, the current routine is assumed."""
        if not isinstance(tag, str):
            raise TypeError("Expecting TAG, got {}".format(type(tag)))
        self.current_tag = tag
        if rou is not None:
            self.set_current_rou(rou)

    def set_current_rou(self, rou):
        """Set the current routine in the environment."""
        if not isinstance(rou, mumpy.MUMPSFile):
            raise TypeError("Expecting ROUTINE, got {}".format(type(rou)))
        self.current_rou = rou

    def push_func_to_stack(self, func):
        """Given a MUMPS Function or Subroutine call, push the necessary
        variables onto the next stack level at the correct name from the
        tag's argument list."""
        # Verify first that we got a function or subroutine call
        if not isinstance(func, mumpy.MUMPSFuncSubCall):
            raise TypeError(
                "Expecting Function call, got {}".format(type(func)))

        # Get the routine
        rou = mumpy.MUMPSFile()
        pass

    def pop_func_from_stack(self):
        pass

    ###################
    # SYMBOL FUNCTIONS
    ###################
    def get(self, key):
        """Return the item named at the current stack level or fall down
        the stack until we find an item with that name."""
        for i in range(self.cur, -1, -1):
            if key in self.stack[i]:
                return self.stack[i][key]

        return None

    def set(self, key, value):
        """Set the item with the given name at the highest stack level
        we can find it at. If it does not exist at any stack level,
        set it at the current stack level."""
        for i in range(self.cur, -1, -1):
            if key in self.stack[i]:
                self.stack[i][key] = value
                return
        self.stack[self.cur][key] = value

    def new(self, key):
        """Create a new symbol with the given name on the current stack
        level with a None value."""
        if not key in self.stack[self.cur]:
            self.stack[self.cur][key] = None

    def kill(self, key):
        """Kill a symbol with the given key at the highest stack level we
        can find it at. If that symbol doesn't exist, do nothing."""
        for i in range(self.cur, -1, -1):
            if key in self.stack[i]:
                del self.stack[i][key]
                return

    def kill_all(self):
        """Clears the entire symbol table (all the way down the stack)."""
        for i in range(self.cur, -1, -1):
            self.stack[i].clear()

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