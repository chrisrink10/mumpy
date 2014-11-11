"""MUMPy environment"""
__author__ = 'christopher'
import sys


class MUMPSEnvironment:
    """A MUMPy execution stack."""
    def __init__(self, in_dev=sys.stdin, out_dev=sys.stdout):
        # Current stack level
        self.cur = 0

        # Stack
        self.stack = [{}]

        # Input and output devices in the environment
        self.in_dev = in_dev
        self.out_dev = out_dev

    def __repr__(self):
        """String representation of this environment."""
        return "Environment()"

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
            print("[Stack Frame {}]".format(i))
            for k, v in self.stack[i].items():
                print("{key}={val}".format(key=k, val=v))

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
