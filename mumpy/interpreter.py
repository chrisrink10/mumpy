"""MUMPy Interpreter

The functions in this module represent various functions that may need
to be carried out from the command line (including starting the REPL
and compiling and executing a routine file).

Licensed under a BSD license. See LICENSE for more information.

Author: Christopher Rink"""
try:
    # Used by Python's input() to provide readline functionality
    # Does not work on Windows, so we'll just pass
    import readline
except ImportError:
    pass
import argparse
import mumpy


def main():
    """The main command line entry point for MUMPy."""
    parser = argparse.ArgumentParser(
        description="MUMPS interpreter. "
                    "Summoning this script without any arguments will open the "
                    "included MUMPS REPL capability."
    )
    parser.add_argument("-d", "--debug",
                        help="Enable debug output in REPL mode",
                        required=False,
                        action='store_true'
                        )
    parser.add_argument("-c", "--compile",
                        help="A list of MUMPS scripts to compile.",
                        required=False,
                        nargs='*'
                        )
    parser.add_argument("-f", "--file",
                        help="A MUMPS routine to execute.",
                        required=False,
                        nargs=1
                        )
    parser.add_argument("-t", "--tag",
                        help="The tag to execute in the specified routine",
                        required=False,
                        nargs=1
                        )
    parser.add_argument("-dev", "--device",
                        help="The I/O device this process should start with",
                        required=False,
                        nargs=1
                        )
    parser.add_argument("-a", "--args",
                        help="The arguments to pass to the specified tag",
                        required=False,
                        nargs="*"
                        )
    parser.add_argument("-r", "--recompile",
                        help="Recompile any routines before interpreting.",
                        required=False,
                        action='store_true'
                        )
    args = parser.parse_args()

    # Process routine compilations first
    if args.compile:
        compile_routine(args.compile,
                        args.debug)

    # Then interpret any files
    if args.file:
        interpret(args.file[0],
                  tag=None if args.tag is None else args.tag[0],
                  device=None if args.device is None else args.device[0],
                  args=args.args,
                  recompile=args.recompile,
                  debug=args.debug)

    # If the user wants to neither compile any routines or interpret any files,
    # start the REPL
    if not args.compile and not args.file:
        start_repl(args.debug)


def start_repl(debug=False):
    """Start the interpreter loop."""
    env = mumpy.MUMPSEnvironment()
    p = mumpy.MUMPSParser(env, debug=debug)

    # Catch the Keyboard Interrupt to let us exit gracefully
    try:
        # Accept user input
        while True:
            current_line = input("mumpy > ")

            # Allow empty lines from the REPL
            if current_line.strip() == "":
                continue

            # Catch any Syntax errors from the user input
            try:
                p.parse_repl(current_line)
            except mumpy.MUMPSSyntaxError as e:
                print(e)

            # If output was emitted, we need to add an extra newline
            if p.output:
                print("")
    except KeyboardInterrupt:
        print("")
        pass


def compile_routine(files, debug=False):
    """Compile a list of routines."""
    # Compile the routines to an intermediate format
    intf = []
    for file in files:
        print("Compiling {file}...".format(file=file))
        try:
            intf.append(mumpy.MUMPSFile(rou=file, debug=debug, recompile=True))
            print("Success!")
        except mumpy.MUMPSCompileError as e:
            print(e)
            print("Failed to compile {rou}!".format(rou=file))


def interpret(file, tag=None, args=None, device=None,
              recompile=False, debug=False):
    """Interpret a routine file.."""
    # Prepare the file
    try:
        f = mumpy.MUMPSFile(file, recompile=recompile, debug=debug)
    except mumpy.MUMPSCompileError as e:
        print(e)
        return

    # IF we recompiled and we made it this far, then there were no errors
    if recompile:
        print("{} recompiled successfully!".format(file))

    # Prepare the environment and parser
    env = mumpy.MUMPSEnvironment()
    p = mumpy.MUMPSParser(env, debug=debug)

    # If the user specifies another default device, use that
    if device is not None:
        env.open(device)
        env.use(device)

    # Parse the file
    try:
        p.parse_file(f, tag=tag, args=args)
    except mumpy.MUMPSSyntaxError as e:
        print(e)
