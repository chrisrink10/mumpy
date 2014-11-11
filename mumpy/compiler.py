"""MUMPy Routine Compiler

Compile a MUMPS routine into a Python intermediate form that the
interpreter can use."""
__author__ = 'christopher'
import importlib
import os.path
from mumpy.tokenizer import MUMPSLexer


class MUMPSFile:
    """Represents a MUMPy intermediate representation of a MUMPS routine."""
    def __init__(self, rou, recompile=False, debug=False):
        """Attempt to open a MUMPS routine file. First, we search for a
        pre-compiled MUMPy intermediate representation. If that exists and
        the user did not specify to recompile, we'll use that. Otherwise, we
        will search for the base routine file and compile that to the
        intermediate representation."""
        # Make sure we even got a string
        if not isinstance(rou, str):
            raise TypeError("Routine names must be a valid string.")

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
                                    errtype="INVALID FILE")

    def _compile(self):
        """Compile a MUMPS routine into a MUMPy intermediate representation.
        Very little syntax checking is done by this stage of compilation."""
        # Prepare the lexer
        lex = MUMPSLexer()

        # Set up some data structures that we'll use to represent a routine
        lines = []
        tags = {}

        # Read in the routine file
        with open(self.rou_path, mode='r', encoding='US-ASCII') as f:
            for i, line in enumerate(f):
                # Lex the line
                lex.lex(line)

                # MUMPS has specific rules about the first chars in a line
                # Only tags (symbols) and spaces may appear in the first
                # column of the Routine. Since tags can actually use the
                # command keywords, we need to check for these tokens too.
                symb = lex[0]
                if symb.type == 'SYMBOL' or lex.symb_is_keyword(symb.value):
                    # Routine names must match the first tag in the routine body
                    if len(tags) == 0 and symb.value != self.rou:
                        raise MUMPSCompileError("Routine name '{rou}' must "
                                                "match file '{file}' "
                                                "name.".format(rou=symb.value,
                                                               file=self.rou),
                                                line=i+1,
                                                errtype="ROUTINE NAME")

                    # Check that we don't have duplicate tags
                    if symb.value in tags:
                        raise MUMPSCompileError("Tag names must be unique.",
                                                line=i+1,
                                                errtype="DUPLICATE TAG NAME")

                    # Build the tag index
                    tags[symb.value] = i
                elif symb.type == 'SPACE':
                    # TODO: Remove comments from compiled code
                    # Since DO and GOTO tag calls can use line offsets
                    # this will require more complex processing.
                    #try:
                    #    if lex[1].type == 'COMMENT':
                    #        continue
                    #except (KeyError, AttributeError):
                    #    pass
                    pass
                else:
                    raise MUMPSCompileError("Lines must start with a "
                                            "space or a tag.",
                                            line=i+1,
                                            errtype="LINE START")

                # And the list of lines
                lines.append(_process_line(line))

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
            for tag, line in tags.items():
                f.write("    '{tag}': {line},\n".format(tag=tag, line=line))
            f.write('}\n\n')

            # Output the line list
            f.write('lines = [\n')
            for line in lines:
                f.write("    '''{line}''',\n".format(line=line))
            f.write(']\n')

    def name(self):
        """Return the routine name."""
        return self.inter.name

    def tags(self):
        """Return the dict of tags and their respective start lines."""
        return self.inter.tags

    def tag_line(self, tag):
        """Return the line that the specified tag starts at."""
        return self.inter.tags[tag]

    def tag_body(self, tag):
        """Return the tag body of the given tag."""
        return self.inter

    def lines(self):
        """Return the list of routine lines."""
        return self.inter.lines

    def line(self, ln):
        """Return the specified line."""
        return self.inter.lines[ln]


def _process_line(line):
    """Escape string literals and strip newline characters."""
    if not isinstance(line, str):
        raise TypeError("Routine lines should be strings.")

    if line.endswith('\n'):
        line = line[:-1]

    return line


class MUMPSCompileError(Exception):
    """Raised if there was an error compiling a routine to intermediate form."""
    def __init__(self, msg, line=None, errtype=None):
        self.msg = msg
        self.line = line
        self.errtype = "UNKNOWN" if errtype is None else errtype

    def __str__(self):
        return "COMPILE ERROR <{type}{line}>: {msg}".format(
            type=self.errtype,
            line="" if self.line is None else ":{num}".format(num=self.line),
            msg=self.msg
        )

    def __repr__(self):
        return str(self)
