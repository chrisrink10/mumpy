"""MUMPy environment

The MUMPS environment provides stack management and other environment or
process-wide functions such as maintaining the current input and output
devices.

Licensed under a BSD license. See LICENSE for more information.

Author: Christopher Rink"""
import io
import os
import select
import socket
import sys
import urllib.parse as urlparse
import mumpy


# Valid file modes for a file device. These modes are ignored for sockets.
_modes = ('r',      # Read-only. File pointer at beginning.
          'w',      # Write-only. Overwrites file if exists, else creates.
          'x',      # Exclusive creation.
          'a',      # Append. Write-only. File pointer at end or creates.
          'r+',     # Read-write. File pointer at beginning.
          'w+',     # Read-write. Overwrites file if exists, else creates.
          'a+',     # Read-write. File pointer at end. Creates if not exists.
)

# Default $PRINCIPAL file
_default_device = 'STANDARD'


class MUMPSEnvironment:
    """A MUMPy execution stack."""
    def __init__(self, device='STANDARD'):
        # Default I/O device
        self._def_x = 0
        self._def_y = 0

        # Current stack level and variable stack
        self._cur = 0
        self._stack = [{}]
        self._init_sys_vars()

        # Create the $PRINCIPAL device
        _principal = MUMPSDevice('STANDARD')
        _principal._file = io.TextIOWrapper(
            io.BufferedRWPair(sys.stdin.buffer,
                              sys.stdout.buffer),
            encoding='utf8'
        )

        # Input and output devices in the environment
        self._devices = {
            _default_device: _principal
        }
        self._err_dev = sys.stderr

        # Set the current device
        if device != _default_device:
            self.open(device)
            self._cur_dev = self._devices[device]
        else:
            self._cur_dev = _principal

        # Function and subroutine call stack
        self._call_stack = []

        # A list of routines that the environment has already loaded
        self._routines = {}

    def __repr__(self):
        """String representation of this environment."""
        return "Environment({lvl}, {rou})".format(
            lvl=self._cur,
            rou=self.get_current_rou(),
        )

    def __del__(self):
        """Release any remaining file resources."""
        # If no devices are open, exit
        if len(self._devices) == 0:
            return

        # Close any remaining devices
        for _, dev in self._devices.items():
            try:
                dev.close()
            except AttributeError:
                pass

    def _init_sys_vars(self):
        """Initialize some of the system default variables."""
        self.set("$T", mumpy.mumps_true())

    ###################
    # CALL STACK FUNCTIONS
    ###################
    def get_routine(self, rou):
        """Query the environment for a routine. The environment keeps a
        cache of previously accessed routines so they can be accessed
        more quickly in the future."""
        # If the routine is None, then return the current routine
        if rou is None:
            return self.get_current_rou()

        # Check the cache first
        if rou in self._routines:
            return self._routines[rou]

        # Try to load the routine now
        try:
            f = mumpy.MUMPSFile(rou)
            self._routines[rou] = f
            return f
        except mumpy.MUMPSCompileError as e:
            raise mumpy.MUMPSSyntaxError(e, err_type="NO LINE")

    def get_current_rou(self):
        """Return the current environment routine."""
        if len(self._call_stack) > 0:
            return self._call_stack[self._cur-1][1]
        return None

    def set_current_rou(self, rou, tag=None):
        """Set the current executing routine.

        In most cases, callers should use push_func_to_stack and
        pop_func_from_stack to control environment stack management.
        However, in the case of executing an M script directly from
        the command line, we need to set the base level executing
        routine for any subsequent non-extended reference function
        and subroutines (i.e. $$Func() rather than $$Func^ROU())."""
        self._call_stack.append((tag, rou))

    def init_stack_frame(self, rou, tag=None, in_args=None):
        """Set the current routine, tag, and any relevant arguments on the
        current stack frame.

        This function can be used to initiate the base level stack frame
        when a tag is initiated with arguments from the command line or when
        a tag^routine is executed from a JOB command from another MUMPy
        process."""
        # Verify that we got a valid routine
        if not isinstance(rou, mumpy.MUMPSFile):
            raise TypeError("Expecting Routine, got {}".format(type(rou)))

        # Set the current routine and tag
        self.set_current_rou(rou, tag)

        # Get the tag and any arguments
        tag = rou.name() if tag is None else tag
        try:
            args = rou.tag_args(tag)
        except KeyError:
            raise mumpy.MUMPSSyntaxError("Tag '{}' not found"
                                         "in routine.".format(tag),
                                         err_type="NOLINE")

        # Check for syntax errors in the argument call
        _check_args(args, in_args)

        # No need to continue if there are no arguments
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
                # Create the pointer if necessary
                v = in_args[i]
                in_arg = str(mumpy.MUMPSExpression(v))
            except (IndexError, TypeError):
                in_arg = mumpy.mumps_null()

            # New the argument list name
            self.new(ident)

            # Set the new value
            self._set(ident, in_arg)

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
        self._call_stack.append((func.tag, func.rou))

        # According the GT.M programmers guide, $T should stack only
        # for extrinsic functions and argumentless DO commands
        if func.is_func:
            self.new("$T")
            self.set("$T", mumpy.mumps_true())

        # Get the argument list and push existing values onto the new frame
        args = func.rou.tag_args(func.tag)

        # Check for syntax errors in argument list
        _check_args(args, func.args)

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
            ptr = None
            try:
                # Create the pointer if necessary
                v = func.args[i]
                if isinstance(v, mumpy.MUMPSPointerIdentifier):
                    in_arg = None
                    ptr = v
                else:
                    in_arg = str(mumpy.MUMPSExpression(v))
            except (IndexError, TypeError):
                in_arg = mumpy.mumps_null()

            # New the argument list name
            if ptr is None:
                self.new(ident)
            else:
                self._new_pointer(ident)

            # Set the new value
            self._set(ident, in_arg, pointer=ptr)

    def pop_func_from_stack(self):
        """Return execution to the original function or subroutine."""
        self.pop()
        self._call_stack.pop()

    ###################
    # SYMBOL FUNCTIONS
    ###################
    def __contains__(self, item):
        """Return False if `item` is not defined in the environment. This
        will be used for the `$DATA` operation on local variables."""
        for frame in reversed(self._stack):
            if item in frame:
                return True

        return False

    def get(self, key, get_var=False):
        """Return the item named at the current stack level or fall down
        the stack until we find an item with that name. If `get_var` is
        specified, return access to the entire local variable object.
        Most clients should not need to use `get_var`, but it is needed
        for `$DATA` function calls."""
        # Check to see if the variable is a pointer, so we can return the
        # actual value from the stack it resides in
        item = self._get(key)
        if item[0] is not None:
            item = self._get_pointer(item[0], self._cur)

        # Otherwise, we can just return the scalar value
        try:
            return item[1].get(key) if not get_var else item[1]
        except AttributeError:
            return mumpy.mumps_null()

    def _get_pointer(self, key, end):
        """Private pointer get function needed to recurse down the stack
        if a pointer points back several levels on the stack."""
        item = self._get(key, end=end)
        if item[0] is not None and (end-1) >= 0:
            return self._get_pointer(item[0], end-1)

        # Otherwise, we can just return the scalar value
        return item

    def _get(self, key, start=0, end=None):
        """Private environment variable retrieval function. This function
        retrieves the (pointer, value) tuple from the stack. In general, clients
        will just want the value, so the public function returns only the
        value."""
        end = len(self._stack) if end is None or end < start else end

        for frame in reversed(self._stack[start:end]):
            if key in frame:
                return frame[key]

        return None, mumpy.mumps_null()

    def set(self, key, value):
        """Set the item with the given name at the highest stack level
        we can find it at. If it does not exist at any stack level,
        set it at the current stack level."""
        # Check if the value is a pointer, so we can update that value
        ptr = self._get(key)[0]
        if ptr is not None:
            self._set_pointer(ptr, value, self._cur)

        # If the value is not a pointer, we can modify it easily
        self._set(key, value, pointer=ptr)

    def _set_pointer(self, key, value, end):
        """Private pointer set function needed to recurse down the stack
        if a pointer points back several levels on the stack."""
        ptr = self._get(key, end=end)[0]
        if ptr is not None and (end-1) >= 0:
            self._set_pointer(ptr, value, end-1)

        # If the value is not a pointer, we can modify it easily
        self._set(key, value, pointer=ptr, end=end)

    def _set(self, key, value, pointer=None, start=0, end=None):
        """Private environment variable set function. This function can
        be used internally to save a (pointer, value) tuple onto the
        stack. External clients should never have to do this (since the
        environment is responsible for function stack transfers), so
        the public function always sets a None pointer value."""
        end = len(self._stack) if end is None or end < start else end

        for frame in reversed(self._stack[start:end]):
            if key in frame:
                try:
                    # The variable is a value, update its value
                    var = frame[key][1]
                    var.set(key, value)
                    frame[key] = (pointer, var)
                except AttributeError:
                    # The variable is a pointer, update its pointer
                    frame[key] = (pointer, None)
                return

        var = mumpy.MUMPSLocal()
        var.set(key, value)
        self._stack[self._cur][key] = (pointer, var)

    def new(self, key):
        """Create a new symbol with the given name on the current stack
        level with a null value."""
        if not key in self._stack[self._cur]:
            self._stack[self._cur][key] = (None,
                                           mumpy.MUMPSLocal(mumpy.mumps_null()))

    def _new_pointer(self, key):
        """Create a new symbol with the given name on the current stack
        level with a None value, which is used for values which are strictly
        pointers."""
        if not key in self._stack[self._cur]:
            self._stack[self._cur][key] = (None, None)

    def kill(self, key):
        """Kill a symbol with the given key at the highest stack level we
        can find it at. If that symbol doesn't exist, do nothing."""
        for frame in reversed(self._stack):
            if key in frame:
                if key.subscripts() is None:
                    del frame[key]
                else:
                    var = frame[key][1]
                    var.delete(key)
                return

    def kill_all(self):
        """Clears the entire symbol table (all the way down the stack)."""
        for frame in reversed(self._stack):
            frame.clear()

    def push(self):
        """Push a new frame onto the stack."""
        self._stack.append({})
        self._cur += 1

    def pop(self):
        """Pop the last frame off the stack."""
        self._stack.pop()
        self._cur -= 1

    def print(self):
        """Print the stack frames in reverse order."""
        for i, frame in enumerate(reversed(self._stack)):
            self.writeln("[Stack Frame {}]".format(i))
            for k, v in frame.items():
                self.writeln(v[1].pprint_str(k))

    ###################
    # OUTPUT FUNCTIONS
    ###################
    def current_device(self):
        """Return the name of the current device."""
        return mumpy.MUMPSExpression(str(self._cur_dev))

    def default_device(self):
        """Return the default device for this session."""
        return mumpy.MUMPSExpression(_default_device)

    def device_x(self):
        """Return the device $X value for the currently selected device."""
        return self._cur_dev.x

    def device_y(self):
        """Return the device $Y value for the currently selected device."""
        return self._cur_dev.y

    def open(self, dev, opts=None):
        """Open a file device and add it to the file device list."""
        # Store the device in string form only
        dev = str(dev)

        # We don't need to open this device again
        if dev in self._devices:
            return

        # Try to open the device
        self._devices[dev] = MUMPSDevice(dev, opts)
        self._devices[dev].open()

    def close(self, dev):
        """Close a file device and remove it from use if it is in use."""
        dev = str(dev)

        # We cannot close a device we are not using
        if dev not in self._devices:
            raise mumpy.MUMPSSyntaxError("Selected IO device not found.",
                                         err_type="NODEV")

        # We cannot close the $PRINCIPAL device
        if dev == _default_device:
            raise mumpy.MUMPSSyntaxError("Cannot close $PRINCIPAL.",
                                         err_type="BADDEV")

        # Close the device
        self._devices[dev].close()

        # Check if the device we just closed was the current device;
        # If so, revert back to the $PRINCIPAL IO device
        if self._cur_dev == dev:
            self.use(_default_device)

    def use(self, dev):
        """Specify the current device."""
        dev = str(dev)

        # Make sure we have this device
        if dev not in self._devices:
            raise mumpy.MUMPSSyntaxError("Selected IO device not found.",
                                         err_type="NODEV")

        # Set the device
        self._cur_dev = self._devices[dev]

    def input(self, size=None, timeout=None, prompt=None):
        """Input from the current input device."""
        # Provide a prompt for the read
        if isinstance(prompt, str):
            self._cur_dev.write(prompt)

        # Read from the current other device
        return self._cur_dev.read(size=size, timeout=timeout)

    def write(self, data, flush=True):
        """Output to the current output device."""
        s = str(data)
        self._cur_dev.write(s, flush=flush, newline=False)

    def writeln(self, data):
        """Write to the current output device; appends a newline to output."""
        s = str(data)
        self._cur_dev.write(s, flush=True, newline=True)

    def write_error(self, data):
        """Output to the current error device."""
        self._err_dev.write(str(data))

    def writeln_error(self, data):
        """Output the current error device; appends a newline to output."""
        self._err_dev.write("{data}\n".format(data=str(data)))

    def write_stack(self):
        """Output a stack trace to the current error device."""
        for i, frame in enumerate(self._call_stack):
            self.writeln_error("{i} :: {tag}^{rou}".format(
                i=i,
                tag=frame[0],
                rou=frame[1].name(),
            ))


def _check_args(tag_args, in_args):
    """Check the input argument list against the tag argument list. Throw
    syntax errors if users call a tag incorrectly.

    The syntax errors are as follows:
    - If args is None and the function call provided arguments, or
    - If args is not None and the call did not provide arguments, or
    - If args is a shorter list than the input argument list"""
    if tag_args is None and in_args is not None:
        raise mumpy.MUMPSSyntaxError("Function or subroutine does not "
                                     "have argument list.",
                                     err_type="TAGNOARGS")
    elif tag_args is not None and in_args is None:
        raise mumpy.MUMPSSyntaxError("Function or subroutine has "
                                     "arguments, but none provided.",
                                     err_type="TAGNEEDSARGS")
    elif ((tag_args is not None and in_args is not None) and
            (len(tag_args) < len(in_args))):
        raise mumpy.MUMPSSyntaxError("Function or subroutine has fewer "
                                     "arguments than provided.",
                                     err_type="TAGFEWERARGS")


def _select_input(dev, timeout=None, is_file=True):
    """Perform an select timeout for an input device on POSIX systems.

    The Python documentation indicates that select will not work for
    file objects in Windows, so this function will return the device
    if on Windows and the object is indicated as a file. Note that this
    means there is no timeout on Windows."""
    # Implement read timeout for POSIX systems
    # Per the Python documentation, select does not work for
    # file objects in a Windows environment
    if timeout is not None and not (is_file and os.name == 'nt'):
        r, _, _ = select.select((dev,), (), (), timeout)
        if len(r) == 0:
            return None
        return r[0]
    else:
        return dev


class MUMPSDevice:
    """Represents a file or network device usable by an M routine.

    Sockets and files have a number of functional differences, so it became
    necessary to abstract these differences from the environment management
    code."""
    def __init__(self, dev, opts=None):
        # Device name and input options
        self._dev = dev
        self._opts = opts

        # $X and $Y values for this device
        self.x = 0
        self.y = 0

        # File and socket objects
        self._file = None
        self._socket = None
        self._socktype = None
        self._sockaddr = None

        # Process the input options
        self._process_opts()

    def _process_opts(self):
        """Process input options. Check for any invalid options.

        Handles all of the different devices parameters listed below:
        * 'listen' = an IP address or Port to listen on
        * 'mode' = file opening mode ('r+','a','w','r','x') (DEFAULT: 'r+')
        * 'connect' = an IP address/Port to connect to """
        # Handle no input options
        if self._opts is None:
            self._opts = {'mode': 'r+'}
            return

        # Check for valid file input options
        self._opts['mode'] = self._opts['mode'] if 'mode' in self._opts else 'r+'
        if self._opts['mode'] not in _modes:
            raise mumpy.MUMPSSyntaxError("Invalid IO mode selected; choose "
                                         "one of {}.".format(_modes),
                                         err_type="BADIOMODE")

        # We're done if this isn't a socket
        if not self._is_socket():
            return

        # We cannot listen and connect on the same device
        if 'listen' in self._opts and 'connect' in self._opts:
            raise mumpy.MUMPSSyntaxError("Cannot listen and connect on "
                                         "same device.",
                                         err_type="BADOPTS")

        # Attempt to parse the given address
        if 'listen' in self._opts:
            self._socktype = 'listen'
            url = _make_url(self._opts['listen'])
            addr = urlparse.urlparse(url)
        else:
            self._socktype = 'connect'
            url = _make_url(self._opts['connect'])
            addr = urlparse.urlparse(url)

        # Create the socket path
        self._sockaddr = (addr.path, addr.port)

    def __str__(self):
        """Return the name of this device."""
        return str(self._dev)

    def open(self):
        """Open the file and socket devices."""
        # Socket files will have either a listen or connect parameter
        if self._is_socket():
            self._open_socket()
        else:
            self._open_file()

    def _is_socket(self):
        """Return true if this device should be treated as a socket."""
        return 'listen' in self._opts or 'connect' in self._opts

    def _open_file(self):
        """Open a file device."""
        try:
            self._file = open(self._dev,
                              mode=self._opts['mode'],
                              encoding="utf8")
        except FileNotFoundError:
            self._file = open(self._dev,
                              mode='a+',
                              encoding="utf8")
        except OSError as e:
            raise mumpy.MUMPSSyntaxError("Invalid IO operation; operating "
                                         "system returned '{}'.".format(str(e)))

    def _open_socket(self):
        """Open a socket device."""
        try:
            self._socket = socket.socket()
            if self._socktype == 'listen':
                self._socket.bind(self._sockaddr)
                self._socket.listen(5)
                mode = 'r'
            else:
                self._socket.connect(self._sockaddr)
                mode = 'w'
            self._file = self._socket.makefile(mode, encoding='utf8')
        except OSError:
            raise mumpy.MUMPSSyntaxError("Invalid network socket specified.",
                                         err_type="BADSOCKET")

    def close(self):
        """Attempt to close both the file and socket objects."""
        try:
            self._file.close()
        except AttributeError:
            pass

        try:
            self._socket.close()
        except AttributeError:
            pass

    def read(self, size=None, timeout=None):
        """Read from the file device."""
        # Perform a select on the device
        dev = _select_input(self._file, timeout=timeout, is_file=True)

        # Allow reading input of a certain size
        if isinstance(size, int):
            val = dev.read(size)
        else:
            val = dev.readline()

        return val[:-1] if val.endswith("\n") else val

    def write(self, data, flush=True, newline=False):
        """Write out to the file device. This function will automatically
        flush sockets."""
        data = "{}\n".format(data) if newline else data
        self._file.write(data)
        if flush or self._is_socket():
            self._file.flush()
        self._update_cursor(data, newline=newline)

    def _update_cursor(self, data, newline=False):
        """Update the X and Y position for the current device."""
        lines = data.split("\n")
        num_lines = len(lines)
        num_newlines = (num_lines - 1) + int(newline)
        num_chars = len(lines[num_lines-1]) if not newline else 0
        self.x = self.x + num_chars if num_newlines == 0 else num_chars
        self.y += num_newlines


def _make_url(addr):
    """Create a URL that will be recognized by urlparse."""
    return addr if str(addr).startswith("//") else "//{}".format(addr)
