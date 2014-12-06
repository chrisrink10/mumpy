"""MUMPy Tokenizer

The MUMPS tokenizer written in PLY format.

Licensed under a BSD license. See LICENSE for more information.

Author: Christopher Rink"""
import re
import ply.lex as lex
import mumpy


# noinspection PyMethodMayBeStatic,PyPep8Naming
class MUMPSLexer:
    """The MUMPS lexer."""
    ###################
    # LEXING STATES
    ###################
    states = (
        ('command', 'exclusive'),
    )

    ###################
    # KEYWORDS
    ###################
    # List of MUMPS keywords
    keywords = {
        's': 'SET',
        'set': 'SET',
        'w': 'WRITE',
        'write': 'WRITE',
        'r': 'READ',
        'read': 'READ',
        'f': 'FOR',
        'for': 'FOR',
        'd': 'DO',
        'do': 'DO',
        'q': 'QUIT',
        'quit': 'QUIT',
        'h': 'HALT_HANG',
        'hang': 'HANG',
        'halt': 'HALT',
        'n': 'NEW',
        'new': 'NEW',
        'i': 'IF',
        'if': 'IF',
        'g': 'GOTO',
        'goto': 'GOTO',
        'b': 'BREAK',
        'break': 'BREAK',
        'e': 'ELSE',
        'else': 'ELSE',
        'c': 'CLOSE',
        'close': 'CLOSE',
        'o': 'OPEN',
        'open': 'OPEN',
        'l': 'LOCK',
        'lock': 'LOCK',
        'm': 'MERGE',
        'merge': 'MERGE',
        'j': 'JOB',
        'job': 'JOB',
        'k': 'KILL',
        'kill': 'KILL',
        'u': 'USE',
        'use': 'USE',
        'v': 'VIEW',
        'view': 'VIEW',
        'x': 'XECUTE',
        'xecute': 'XECUTE'
    }

    # Create a tuple of the keyword token values
    keyword_tokens = tuple(set(keywords.values()))

    ###################
    # INTRINSICS
    ###################
    # List of MUMPS intrinsic functions
    intrinsics = {
        '$a': 'ASCII',
        '$ascii': 'ASCII',
        '$c': 'CHAR',
        '$char': 'CHAR',
        '$d': 'DATA',
        '$data': 'DATA',
        '$e': 'EXTRACT',
        '$extract': 'EXTRACT',
        '$f': 'FIND',
        '$find': 'FIND',
        '$fn': 'FNUMBER',
        '$fnumber': 'FNUMBER',
        '$g': 'GET',
        '$get': 'GET',
        '$j': 'JUSTIFY_DOLLARJ',
        '$justify': 'JUSTIFY',
        '$l': 'LENGTH',
        '$length': 'LENGTH',
        '$na': 'NAME',
        '$name': 'NAME',
        '$o': 'ORDER',
        '$order': 'ORDER',
        '$p': 'PIECE_PRINCIPAL',
        '$piece': 'PIECE',
        '$ql': 'QLENGTH',
        '$qlength': 'QLENGTH',
        '$qs': 'QSUBSCRIPT',
        '$qsubscript': 'QSUBSCRIPT',
        '$q': 'QUERY',
        '$query': 'QUERY',
        '$r': 'RANDOM',
        '$random': 'RANDOM',
        '$re': 'REVERSE',
        '$reverse': 'REVERSE',
        '$s': 'SELECT',
        '$select': 'SELECT',
        '$st': 'STACK',
        '$stack': 'STACK',
        '$t': 'TEST_TEXT',
        '$text': 'TEXT',
        '$tr': 'TRANSLATE',
        '$translate': 'TRANSLATE',
        '$v': 'VIEW_FN',
        '$view': 'VIEW_FN'
    }

    # Create a tuple of intrinsic token values
    intrinsic_tokens = tuple(set(intrinsics.values()))

    ###################
    # MUMPS VARIABLES
    ###################
    # List of MUMPS variable tokens
    variables = {
        '$h': 'HOROLOG',
        '$horolog': 'HOROLOG',
        '$io': 'DOLLARIO',
        '$j': 'JUSTIFY_DOLLARJ',
        '$job': 'DOLLARJ',
        '$p': 'PRINCIPAL',
        '$principal': 'PIECE_PRINCIPAL',
        '$t': 'TEST_TEXT',
        '$test': 'TEST',
        '$x': 'DOLLARX',
        '$y': 'DOLLARY',
        '$zj': 'ZJOB',
        '$zjob': 'ZJOB',
    }

    # Create a tuple of
    variable_tokens = tuple(set(variables.values()))

    ###################
    # TOKEN LIST
    ###################
    # List of failed token errors
    errors = (
        'FN_DOES_NOT_EXIST',
    )

    # List of all valid tokens
    tokens = tuple(set((
        'PLUS', 'MINUS', 'TIMES', 'DIVIDE',
        'IDIVIDE', 'EQUALS', 'EXPONENT',
        'COMMENT', 'STRING', 'NUMBER',
        'MODULUS', 'NOT', 'GREATER_THAN',
        'LESS_THAN', 'AND', 'OR', 'COMMA',
        'CONCAT', 'NEWLINE', 'LPAREN', 'RPAREN',
        'SPACE', 'CARET', 'COLON', 'EXTRINSIC',
        'INDIRECTION', 'CONTAINS', 'FOLLOWS',
        'PATTERN', 'PERIOD', 'SYMBOL'
    ) + keyword_tokens + intrinsic_tokens + variable_tokens + errors))

    ###################
    # COMMAND STATE
    #
    # The command state is the state we enter after having processed
    # any tag symbols, full-line comments, and command keywords.
    # Essentially this is every other input we may receive.
    ###################
    t_command_PLUS = r'\+'
    t_command_MINUS = r'-'
    t_command_TIMES = r'\*'
    t_command_DIVIDE = r'\/'
    t_command_IDIVIDE = r'\\'
    t_command_EQUALS = r'='
    t_command_EXPONENT = r'\*\*'
    t_command_MODULUS = r'\#'
    t_command_NOT = r'\''
    t_command_GREATER_THAN = r'>'
    t_command_LESS_THAN = r'<'
    t_command_AND = r'&'
    t_command_OR = r'!'
    t_command_COMMA = r','
    t_command_CONCAT = r'_'
    t_command_CARET = r'\^'
    t_command_LPAREN = r'\('
    t_command_RPAREN = r'\)'
    t_command_COLON = r':'
    t_command_EXTRINSIC = r'\$\$'
    t_command_INDIRECTION = r'@'
    t_command_FOLLOWS = r'\]{1}'
    t_command_CONTAINS = r'\[{1}'
    t_command_PATTERN = r'\?'
    t_command_PERIOD = r'\.'
    t_command_NEWLINE = r'\n'

    @lex.TOKEN(r'(\d+(\.\d+)?|(\.\d+))')
    def t_command_NUMBER(self, t):
        """Match a NUMBER token in command mode."""
        if float(t.value).is_integer():
            t.value = int(t.value)
        else:
            t.value = float(t.value)
        return t

    @lex.TOKEN(r'([%a-zA-Z]+[%a-zA-Z0-9]*)')
    def t_command_SYMBOL(self, t):
        """Match a SYMBOL token in command mode."""
        kw = str(t.value).strip()
        t.value = kw
        return t

    @lex.TOKEN(r'"(?:[^"]|"")*"')
    def t_command_STRING(self, t):
        """This simple rule is defined as a function so it is evaluated at
        a higher level than the SPACE token, which would otherwise cause
        problems with Lexer state.

        See this StackOverflow post for information on this RegEx:
        http://stackoverflow.com/questions/9981624/match-a-double-quoted-string\
        -with-double-quote-inside

        This function also reduces double double-quotes inside MUMPS strings
        to single double-quotes. Double double-quotes are the standard
        escape convention for that language."""
        t.value = re.sub(self.escape, '"', t.value)
        return t

    @lex.TOKEN(r'[ ]{1}')
    def t_command_SPACE(self, t):
        """Match SPACE tokens in the command mode. Note that a new command
        should happen after exactly two spaces in all cases. Argument-less
        commands have two spaces and commands with arguments are followed
        by a single space."""
        # New commands always occur after two spaces
        self.command['spaces'] += 1
        if self.command['spaces'] >= 2:
            self.command['spaces'] = 0
            self.lexer.begin('INITIAL')
            #print("t_command_SPACE: beginning initial state")
        t.type = 'SPACE'
        return t

    @lex.TOKEN(r'\$[a-zA-Z]+')
    def t_command_INTRINSIC(self, t):
        """Match INTRINSIC function tokens in command mode."""
        kw = str(t.value).lower()
        try:
            t.type = self.intrinsics[kw]
        except KeyError:
            t.type = self.variables.get(kw, 'FN_DOES_NOT_EXIST')
        return t

    @lex.TOKEN(r';.*')
    def t_command_COMMENT(self, t):
        """Match COMMENT tokens and ignore them."""
        return t

    def t_command_error(self, t):
        """Lexer generic error function."""
        raise mumpy.MUMPSSyntaxError("Error in command state: {err}".format(
            err=t.value), err_type="LEX ERROR")

    ###################
    # INITIAL STATE
    #
    # In the initial state, we are either handling a routine or are
    # handling user input from the REPL interpreter.
    #
    # For routines, we're looking for one of the patterns here:
    # |ROUTINENAME          <- Routine or Tag name
    # |TagName(symb,symb)   <- Routine or Tag name with Parens and Arguments
    # | cmd                 <- Space followed by Command keyword
    # | ;                   <- Space followed by COMMENT token
    # | .                   <- Space followed by Period (increased stack)
    #
    # For REPL input, we're looking for this pattern:
    # |cmd                  <- Command keyword
    ###################
    @lex.TOKEN(r'([%a-zA-Z]+[%a-zA-Z0-9]*)')
    def t_KEYWORD(self, t):
        """Match a SYMBOL token if we are Lexing a routine and we have not
        passed any spaces. Otherwise, we we will check that our token is
        a valid command keyword. Failure to match on a valid command will
        result in a Syntax error."""
        if self.is_rou and self.initial['spaces'] == 0:
            t.type = 'SYMBOL'
        else:
            kw = str(t.value).strip().lower()
            try:
                t.type = self.keywords[kw]
                t.value = kw
                self.lexer.begin('command')
                #print("t_KEYWORD: beginning command state")
            except KeyError:
                raise mumpy.MUMPSSyntaxError("Expected COMMAND, "
                                             "got {symb}.".format(symb=kw),
                                             err_type="INVALID COMMAND")
        return t

    @lex.TOKEN(r'[ ]{1}')
    def t_SPACE(self, t):
        """Match a SPACE token in the INITIAL state.

        If we are in a routine, then we need to enter the command state after
        two spaces. In the interpreter loop, we should enter the command
        state after only the first space."""
        self.initial['spaces'] += 1
        if ((not self.is_rou and self.initial['spaces'] == 1) or
                (self.is_rou and self.initial['spaces'] == 2)):
            self.initial['spaces'] = 0
            self.lexer.begin('command')
        t.type = 'SPACE'
        return t

    # We need to be able to handle argument lists and newlines
    t_COMMA = r','
    t_LPAREN = r'\('
    t_RPAREN = r'\)'
    t_NEWLINE = r'\n'
    t_COMMENT = r';.*'

    def t_error(self, t):
        """Lexer generic error function."""
        # At Lexer position 0, we expect to see a Tag/Routine name
        if t.lexpos == 0:
            raise mumpy.MUMPSSyntaxError("Expected SYMBOL or SPACE, "
                                         "got {}.".format(t.value),
                                         err_type="INVALID TAG")

        # Otherwise, raise a generic error
        raise mumpy.MUMPSSyntaxError("No '{err}' command found".format(
            err=t.value), err_type="LEX ERROR")

    ###################
    # CLASS METHODS
    ###################
    def __init__(self, is_rou=True, **kwargs):
        """Create a new Lexer."""
        self.lexer = lex.lex(module=self, **kwargs)
        self.line_tokens = []       # self.tokens is used by lex.lex
        self.is_rou = is_rou

        # Lexer state variables
        self.command = {'spaces': 0}
        self.initial = {'spaces': 0}
        self.reset()

        # Escape string regex
        self.escape = re.compile('"{2}')

    def __repr__(self):
        return "MUMPSLexer()"

    def reset(self):
        """Reset the Lexer to the default state."""
        self.lexer.begin('INITIAL')
        self.command = {'spaces': 0}
        self.initial = {'spaces': 0}

    def symb_is_keyword(self, symb):
        """Returns True if the given symbol is also a valid keyword."""
        return symb.lower() in self.keywords

    def test(self, data):
        """Output each token in the given input data."""
        self.reset()
        self.lexer.input(data)
        for tok in self.lexer:
            if not tok:
                break
            print(tok)

    def lex(self, data):
        """Lex a line of input."""
        self.reset()
        self.lexer.input(data)
        self.line_tokens = []
        for tok in self.lexer:
            self.line_tokens.append(tok)

        return self.line_tokens

    def __iter__(self):
        """Allow clients to iterate on the current list of Tokens (if
        they called MUMPSLexer.lex())."""
        return self.line_tokens

    def __getitem__(self, item):
        """Expose the token list."""
        return self.line_tokens[item]
