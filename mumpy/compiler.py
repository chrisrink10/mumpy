"""MUMPy Routine Compiler

The MUMPy compiler converts a MUMPS routine into a Python intermediate
module form that the interpreter can interact with using more standard
Python idioms.

Licensed under a BSD license. See LICENSE for more information.

Author: Christopher Rink"""
import importlib
import os.path
import mumpy


class MUMPSFile:
    """Represents a MUMPy intermediate representation of a MUMPS routine."""
    def __init__(self, rou, recompile=False, debug=False):
        """Attempt to open a MUMPS routine file. First, we search for a
        pre-compiled MUMPy intermediate representation. If that exists and
        the user did not specify to recompile, we'll use that. Otherwise, we
        will search for the base routine file and compile that to the
        intermediate representation."""
        # Convert identifiers to strings
        rou = str(rou) if isinstance(rou, mumpy.MUMPSIdentifier) else rou

        # Make sure we even got a string
        if not isinstance(rou, str):
            raise TypeError("Routine names must be a valid string.")

        # Set up the Lexer
        self.lex = mumpy.MUMPSLexer(is_rou=True, debug=debug)

        # Get a few base items
        self.rou = os.path.basename(rou)
        self.path = os.path.dirname(rou)
        self.debug = debug

        # Try to open the intermediate representation first
        self.int_name = "{}.py".format(self.rou)
        self.int_path = os.path.join(self.path, self.int_name)
        self.rou_name = "{}.rou".format(self.rou)
        self.rou_path = os.path.join(self.path, self.rou_name)

        # Compile the routine and import the new intermediate rep
        if not recompile and os.path.isfile(self.int_path):
            self.inter = importlib.import_module(self.rou)
        elif os.path.isfile(self.rou_path):
            self._compile()
            self.inter = importlib.import_module(self.rou)
        else:
            raise MUMPSCompileError("'{}' is not a valid file. Please specify "
                                    "either a routine or an "
                                    "intermediate file.".format(rou),
                                    line=None,
                                    err_type="INVALID FILE")

    def __repr__(self):
        return "MUMPSFile(^{rou}, {path})".format(rou=self.rou, path=self.path)

    def _compile(self):
        """Compile a MUMPS routine into a MUMPy intermediate representation.
        Very little syntax checking is done by this stage of compilation."""
        lines, tags = self._read_rou()
        self._write_int(tags, lines)

    def _read_rou(self):
        """Read in the Routine and return lines and tags."""
        # Set up some data structures that we'll use to represent a routine
        lines = []
        tags = {}

        # Read in the routine file
        with open(self.rou_path, mode='r', encoding='UTF-8') as f:
            for i, line in enumerate(f):
                # Lex the line
                try:
                    tokens = self.lex.lex(line)
                except mumpy.MUMPSSyntaxError as e:
                    raise MUMPSCompileError(e)

                # MUMPS has specific rules about the first chars in a line
                # The Lexer should throw Syntax errors if these rules are
                # violated
                symb = tokens[0]
                if symb.type == 'SYMBOL':
                    # Routine names must match the first tag in the routine body
                    if len(tags) == 0 and symb.value != self.rou:
                        raise MUMPSCompileError("Routine name '{rou}' must "
                                                "match file '{file}' "
                                                "name.".format(rou=symb.value,
                                                               file=self.rou),
                                                line=i+1,
                                                err_type="ROUTINE NAME")

                    # Check that we don't have duplicate tags
                    if symb.value in tags:
                        raise MUMPSCompileError("Tag names must be unique.",
                                                line=i+1,
                                                err_type="DUPLICATE TAG NAME")

                    # Build an argument list for the tag
                    args = _build_arg_list(tokens)

                    # Build the tag index
                    tags[symb.value] = {
                        "line": i,
                        "args": None if args is None else tuple(args),
                    }
                elif symb.type == 'SPACE':
                    # TODO.md: Remove comments from compiled code
                    # Since DO and GOTO tag calls can use line offsets
                    # this will require more complex processing.
                    #try:
                    #    if lex[1].type == 'COMMENT':
                    #        continue
                    #except (KeyError, AttributeError):
                    #    pass
                    pass

                # And the list of lines
                lines.append(_process_line(line))

        return lines, tags

    def _write_int(self, tags, lines):
        """Write out the intermediate file."""
        # Output the intermediate representation
        with open(self.int_path, mode='w', encoding='UTF-8') as f:
            # Write out the header
            f.write('"""MUMPy Intermediate Representation of {rou}\n'
                    '\n'
                    'This file was automatically compiled by MUMPy. '
                    'Do not modify!"""\n'
                    '\n'
                    'name = "{rou}"\n'
                    '\n'.format(rou=self.rou)
                    )

            # Output the tag index
            f.write('tags = {\n')
            for tag, data in tags.items():
                f.write("    '{tag}': {{\n".format(tag=tag))
                f.write("        'line': {line},\n".format(line=data['line']))
                f.write("        'args': {args},\n".format(args=data['args']))
                f.write("    },\n")
            f.write('}\n\n')

            # Output the line list
            f.write('lines = [\n')
            for line in lines:
                f.write("    r'''{line}''',\n".format(line=line))
            f.write(']\n')

    def name(self):
        """Return the routine name."""
        return self.inter.name

    def tags(self):
        """Return the dict of tags and their respective start lines."""
        return self.inter.tags.keys()

    def tag_line(self, tag):
        """Return the line that the specified tag starts at."""
        return self.inter.tags[str(tag)]['line']

    def tag_args(self, tag):
        """Return the argument list for the specified tag."""
        return self.inter.tags[str(tag)]['args']

    def tag_body(self, tag):
        """Return the tag body of the given tag."""
        lntag = sorted([v['line'] for _, v in self.inter.tags.items()])
        start = lntag.index(self.inter.tags[str(tag)]['line'])
        try:
            return self.inter.lines[lntag[start]:lntag[start+1]]
        except IndexError:
            return self.inter.lines[lntag[start]:]

    def lines(self):
        """Return the list of routine lines."""
        return self.inter.lines

    def line(self, ln):
        """Return the specified line."""
        return self.inter.lines[ln]


def _build_arg_list(tokens):
    """Given a list of Tokens for a line, return an argument list. A
    tag with no arguments which includes parentheses should be returned
    as an empty tuple. A tag with no arguments which does not have
    parentheses will be returned as None."""
    try:
        # Check for an opening parenthesis;
        if tokens[1].type != 'LPAREN':
            return None

        # If we got this far, we're assuming there are args
        args = []
        for arg in tokens[2:]:
            if arg.type == 'RPAREN':
                break
            if arg.type == 'SYMBOL':
                args.append(arg.value)
    except KeyError:
        return ()

    return tuple(args)


def _process_line(line):
    """Escape string literals and strip newline characters."""
    if not isinstance(line, str):
        raise TypeError("Routine lines should be strings.")

    if line.endswith('\n'):
        line = line[:-1]

    return line


class MUMPSCompileError(Exception):
    """Raised if there was an error compiling a routine to intermediate form."""
    def __init__(self, msg, line=None, err_type=None):
        if isinstance(msg, mumpy.MUMPSSyntaxError):
            self.msg = msg.msg
            self.err_type = msg.err_type
        else:
            self.msg = msg
            self.line = line
            self.err_type = "UNKNOWN" if err_type is None else err_type

    def __str__(self):
        return "COMPILE ERROR <{type}{line}>: {msg}".format(
            type=self.err_type,
            line="" if self.line is None else ":{num}".format(num=self.line),
            msg=self.msg
        )

    def __repr__(self):
        return str(self)
