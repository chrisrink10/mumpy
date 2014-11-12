"""MUMPy Interpreter"""
import readline     # Used by Python's input() to provide readline functionality
from mumpy.compiler import (MUMPSFile, MUMPSCompileError)
from mumpy.env import MUMPSEnvironment
from mumpy.parser import MUMPSParser
from mumpy.lang import MUMPSSyntaxError


def start_repl(debug=False):
    """Start the interpreter loop."""
    env = MUMPSEnvironment()
    p = MUMPSParser(env, interpreter=True, debug=debug)

    # Catch the Keyboard Interrupt to let us exit gracefully
    try:
        # Accept user input
        while True:
            current_line = input("mumpy > ")

            # Catch any Syntax errors from the user input
            try:
                p.parse(current_line)
            except MUMPSSyntaxError as e:
                print(e)
    except KeyboardInterrupt:
        env.write("\n")
        pass


def compile_routine(files, debug=False):
    """Compile a list of routines."""
    # Compile the routines to an intermediate format
    intf = []
    for file in files:
        print("Compiling {file}...".format(file=file))
        try:
            intf.append(MUMPSFile(rou=file, debug=debug, recompile=True))
            print("Success!")
        except MUMPSCompileError as e:
            print(e)
            print("Failed to compile {rou}!".format(rou=file))


def interpret(file, recompile=False, debug=False):
    """Interpret a routine file.."""
    # Prepare the file
    try:
        f = MUMPSFile(file, recompile=recompile, debug=debug)
    except MUMPSCompileError as e:
        print(e)
        return

    # IF we recompiled and we made it this far, then there were no errors
    if recompile:
        print("{} recompiled successfully!".format(file))

    # Prepare the environment and parser
    env = MUMPSEnvironment()
    p = MUMPSParser(env, interpreter=False, debug=debug)

    # Parse the file
    p.parse_file(f)
