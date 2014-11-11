"""MUMPy Tokenizer"""
import ply.lex as lex
from mumpy.lang import MUMPSSyntaxError


# noinspection PyMethodMayBeStatic,PyPep8Naming
class MUMPSLexer:
    """The MUMPS lexer."""
    ###################
    # LEXING STATES
    ###################
    states = (
        ('command', 'exclusive')
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
        '$j': 'JUSTIFY',
        '$justify': 'JUSTIFY',
        '$l': 'LENGTH',
        '$length': 'LENGTH',
        '$na': 'NAME',
        '$name': 'NAME',
        '$o': 'ORDER',
        '$order': 'ORDER',
        '$p': 'PIECE',
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
        '$t': 'TEXT',
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
    variables = {}

    # Create a tuple of
    variable_tokens = tuple(set(variables.values()))

    errors = (
        'FN_DOES_NOT_EXIST',
    )

    ###################
    # TOKEN LIST
    ###################

    # List of all valid tokens
    tokens = (
        'PLUS', 'MINUS', 'TIMES', 'DIVIDE',
        'IDIVIDE', 'EQUALS', 'EXPONENT',
        'COMMENT', 'STRING', 'NUMBER',
        'MODULUS', 'NOT', 'GREATER_THAN',
        'LESS_THAN', 'AND', 'OR', 'COMMA',
        'CONCAT', 'NEWLINE', 'KEYWORD',
        'LPAREN', 'RPAREN', 'SPACE', 'CARET',
        'DOUBLE_SPACE', 'COLON', 'EXTRINSIC',
        'INTRINSIC', 'INDIRECTION', 'SORTS_AFTER',
        'CONTAINS', 'FOLLOWS', 'PATTERN',
        'PERIOD', 'SYMBOL'   # 'TAG_ROUTINE', 'ROUTINE_GLOBAL',
    ) + keyword_tokens + intrinsic_tokens + variable_tokens + errors

    ###################
    # GENERIC TOKEN RULES
    ###################
    t_PLUS = r'\+'
    t_MINUS = r'-'
    t_TIMES = r'\*'
    t_DIVIDE = r'\/'
    t_IDIVIDE = r'\\'
    t_EQUALS = r'='
    t_EXPONENT = r'\*\*'
    t_COMMENT = r';.*'
    t_STRING = r'"(?:[^"]|"")*"'  # http://stackoverflow.com/questions/9981624/match-a-double-quoted-string-with-double-quote-inside
    t_MODULUS = r'\#'
    t_NOT = r'\''
    t_GREATER_THAN = r'>'
    t_LESS_THAN = r'<'
    t_AND = r'&'
    t_OR = r'!'
    t_COMMA = r','
    t_CONCAT = r'_'
    #t_TAG_ROUTINE = r'[%a-zA-Z0-9]+\^[%a-zA-Z]+[%a-zA-Z0-9]*'
    #t_ROUTINE_GLOBAL = r'\^[%a-zA-Z]+[%a-zA-Z0-9]*'
    t_CARET = r'\^'
    t_LPAREN = r'\('
    t_RPAREN = r'\)'
    t_SPACE = r'[ ]{1}'
    t_COLON = r':'
    t_EXTRINSIC = r'\$\$'
    t_INDIRECTION = r'@'
    t_SORTS_AFTER = r'\]{2}'
    t_FOLLOWS = r'\]{1}'
    t_CONTAINS = r'\[{1}'
    t_PATTERN = r'\?'
    t_PERIOD = r'\.'
    t_NEWLINE = r'\n'
    #t_SYMBOL = r'([%a-zA-Z]+[%a-zA-Z0-9]*)'

    @lex.TOKEN(r'(\d+(\.\d+)?|(\.\d+))')
    def t_NUMBER(self, t):
        if float(t.value).is_integer():
            t.value = int(t.value)
        else:
            t.value = float(t.value)
        return t

    @lex.TOKEN(r'([%a-zA-Z]+[%a-zA-Z0-9]*)')
    def t_SYMBOL(self, t):
        kw = str(t.value).strip()
        t.type = self.keywords.get(kw, 'SYMBOL')
        t.value = kw
        return t

    @lex.TOKEN(r'\$[a-zA-Z]+')
    def t_INTRINSIC(self, t):
        t.type = self.intrinsics.get(str(t.value).lower(),
                                     'FN_DOES_NOT_EXIST')
        return t

    def t_error(self, t):
        """Lexer generic error function."""
        print("Error occurred lexing: {}".format(t.value))

    def symb_is_keyword(self, symb):
        """Returns True if the given symbol is also a valid keyword."""
        return symb.lower() in self.keywords

    def __init__(self, **kwargs):
        """Create a new Lexer."""
        self.lexer = lex.lex(module=self, **kwargs)
        self.line_tokens = []       # self.tokens is used by lex.lex

    def test(self, data):
        """Output each token in the given input data."""
        self.lexer.input(data)
        for tok in self.lexer:
            if not tok:
                break
            print(tok)

    def lex(self, data):
        """Lex a line of input."""
        self.lexer.input(data)
        self.line_tokens = []
        for tok in self.lexer:
            self.line_tokens.append(tok)

    def __iter__(self):
        """Allow clients to iterate on the current list of Tokens (if
        they called MUMPSLexer.lex())."""
        return self.line_tokens

    def __getitem__(self, item):
        """Expose the token list."""
        return self.line_tokens[item]
