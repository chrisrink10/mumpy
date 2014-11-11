#!/usr/bin/env python3

"""MUMPy :: MUMPS interpreter"""
import argparse
import mumpy

__author__ = 'Christopher Rink'


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
    parser.add_argument("-r", "--recompile",
                        help="Recompile any routines before interpreting.",
                        required=False,
                        action='store_true'
                        )
    args = parser.parse_args()

    # Process routine compilations first
    if args.compile:
        mumpy.compile_routine(args.compile, args.debug)

    # Then interpret any files
    if args.file:
        mumpy.interpret(args.file[0], args.recompile, args.debug)

    # If the user wants to neither compile any routines or interpret any files,
    # start the REPL
    if not args.compile and not args.file:
        mumpy.start_repl(args.debug)


if __name__ == "__main__":
    main()
