"""MUMPy environment

The MUMPS environment provides stack management and other environment or
process-wide functions such as maintaining the current input and output
devices.

Licensed under a BSD license. See LICENSE for more information.

Author: Christopher Rink"""
import os
import select
import sys
import mumpy


class MUMPSEnvironment:
    """A MUMPy execution stack."""
    def __init__(self, in_dev=sys.stdin, out_dev=sys.stdout,
                 err_dev=sys.stderr):
        # Default I/O device
        self._default_device = 'STANDARD'
        self._def_x = 0
        self._def_y = 0

        # Current stack level and variable stack
        self._cur = 0
        self._stack = [{}]
        self._init_sys_vars()

        # Input and output devices in the environment
        self._in_dev = in_dev
        self._out_dev = out_dev
        self._err_dev = err_dev
        self._cur_dev = self._default_device
        self._devices = {}
        self._modes = ('r', 'w', 'x', 'a', 'r+')

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
        """Release any remaining file resources. Raise a MUMPSSyntaxError
        for failing to close open devices."""
        # If no devices are open, exit
        if len(self._devices) == 0:
            return

        # Close any remaining devices
        for dev in self._devices:
            try:
                dev.close()
            except AttributeError:
                pass

        # Raise the syntax error
        raise mumpy.MUMPSSyntaxError("Failed to close an open I/O device.",
                                     err_type="FAILED TO CLOSE IO")

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
        return mumpy.MUMPSExpression(self._cur_dev)

    def default_device(self):
        """Return the default device for this session."""
        return mumpy.MUMPSExpression(self._default_device)

    def device_x(self):
        """Return the device $X value for the currently selected device."""
        try:
            return self._devices[self._cur_dev]['x']
        except KeyError:
            return self._def_x

    def device_y(self):
        """Return the device $Y value for the currently selected device."""
        try:
            return self._devices[self._cur_dev]['y']
        except KeyError:
            return self._def_y

    def open(self, dev, mode='r+'):
        """Open a file device and add it to the file device list."""
        # Store the device in string form only
        dev = str(dev)

        # We don't need to open this device again
        if dev in self._devices or dev == self._default_device:
            return

        # Check for valid device modes
        if mode not in self._modes:
            raise mumpy.MUMPSSyntaxError("Invalid IO mode selected; choose "
                                         "one of {}.".format(self._modes),
                                         err_type="INVALID IO MODE")

        # Try to open the device
        try:
            self._devices[dev] = {
                'dev': open(dev, mode=mode, encoding="utf8"),
                'x': 0,
                'y': 0,
            }
        except OSError as e:
            raise mumpy.MUMPSSyntaxError("Invalid IO operation; operating "
                                         "system returned '{}'.".format(str(e)))

    def close(self, dev):
        """Close a file device and remove it from use if it is in use."""
        dev = str(dev)

        # We cannot close a device we are not using
        if dev not in self._devices:
            raise mumpy.MUMPSSyntaxError("Selected IO device not found.",
                                         err_type="IO DEVICE NOT FOUND")

        # We cannot close the $PRINCIPAL device
        if dev == self._default_device:
            raise mumpy.MUMPSSyntaxError("Cannot close $PRINCIPAL.",
                                         err_type="INVALID DEVICE")

        # Close the device
        self._devices[dev]['dev'].close()

        # Check if the device we just closed was the current device;
        # If so, revert back to the $PRINCIPAL IO device
        if self._cur_dev == dev:
            self.use(self._default_device)

    def use(self, dev):
        """Specify the current device."""
        dev = str(dev)
        is_principal = (dev == self._default_device)

        # Make sure we have this device
        if dev not in self._devices and not is_principal:
            raise mumpy.MUMPSSyntaxError("Selected IO device not found.",
                                         err_type="IO DEVICE NOT FOUND")

        # Set the device
        self._cur_dev = dev
        if is_principal:
            self._in_dev, self._out_dev = sys.stdin, sys.stdout
        else:
            dev = self._devices[dev]['dev']
            self._in_dev, self._out_dev = dev, dev

    def input(self, size=None, timeout=None, prompt=None):
        """Input from the current input device."""
        # Provide a prompt for the read
        if isinstance(prompt, str):
            self._out_dev.write(prompt)

        # Implement read timeout for POSIX systems
        # Per the Python documentation, select does not work for
        # file objects in a Windows environment
        if timeout is not None and os.name != 'nt':
            r, _, _ = select.select((self._in_dev,), (), (), timeout)
            if len(r) == 0:
                return ""
            dev = r[0]
        else:
            dev = self._in_dev

        # Allow reading input of a certain size
        if isinstance(size, int):
            val = dev.read(size)
        else:
            val = dev.readline()
        return val[:-1] if val.endswith("\n") else val

    def write(self, data, flush=True):
        """Output to the current output device."""
        s = str(data)
        self._out_dev.write(s)
        if flush:
            self._out_dev.flush()
        self._update_cursor(s)

    def writeln(self, data):
        """Write to the current output device; appends a newline to output."""
        s = str(data)
        self._out_dev.write("{data}\n".format(data=s))
        self._update_cursor(s, newline=True)

    def _update_cursor(self, data, newline=False):
        """Update the X and Y position for the current device."""
        lines = data.split("\n")
        num_lines = len(lines)
        num_newlines = (num_lines - 1) + int(newline)
        num_chars = len(lines[num_lines-1]) if not newline else 0
        new_x = self.device_x() + num_chars if num_newlines == 0 else num_chars
        new_y = self.device_y() + num_newlines
        try:
            self._devices[self._cur_dev]['x'] = new_x
            self._devices[self._cur_dev]['y'] = new_y
        except KeyError:
            self._def_x = new_x
            self._def_y = new_y

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
