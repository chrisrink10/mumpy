# MUMPy
MUMPy is an [ANSI M](http://en.wikipedia.org/wiki/MUMPS) interpreter written
in pure Python. It provides both a functional Read-Eval-Print Loop (REPL) for
the M language and a routine interpreter, allowing routines to be executed
directly from the command shell.

For now, MUMPy is very much an pre-alpha quality product. Many core features
of the M language are not yet implemented. Things such as argumentless `DO` 
commands to extend conditional line scope are not yet functional. Users 
interested in an actual functional M interpreter should investigate 
FIS GT.M,  which is an open source M interpreter that fully conforms to 
the ANSI M standard and will probably be a lot faster to boot (after all, 
it's written in C). 

MUMPy is mostly just a fun
learning project for me, though I would eventually like for it to be
fully featured.

## Installation
MUMPy can be installed using `pip`:

    pip install git+git://github.com/chrisrink10/mumpy@master

## Use
To use MUMPy interactively, simply fire it up from the command line: `mumpy.py`. 

MUMPy can interpret M source code files (files ending in a `*.m` extension) by
typing `mumpy.py -f <NAME>` where `<NAME>` is the name of the routine,
excluding the extension. MUMPy will compile a Python module with the same
base name. Users should note that routine base names should match the first
tag (line label, explained below) in the routine file. This means that 
M routine names are limited to ASCII characters `%a-zA-Z0-9`, where the
first character cannot be numeric `0-9`. Users can read more about 
routines in the Routines section below.

Other command line options are available by invoking the `--help` parameter. 
Users can enter a routine at a certain tag and specify input parameters
now. Use the `-f` parameter to specify a routine. Optionally, users can
specify a tag to use with `-t` and a list of space-delimited arguments with
the `-a` parameter. Summoning the `AskQuestion^LEARNM` function given at
the end of this document can be done with the following command:

    ./mumpy.py -f LEARNM -r -t AskQuestion -a "What time is it?" 10

## MUMPS Primer
I will provide a basic MUMPS primer for users who are unfamiliar with 
its peculiar syntax. It is important to note that in MUMPS, white space
outside of string literals is **very** important. Due to the limitations
of the REPL format, the syntactic requirements enforced on routines are 
relaxed in REPL mode. White space remains important, but there are 
slightly different syntactic requirements.

### Commands
MUMPS commands indicate an action for the interpreter to take. These 
commands have two forms, the full word and an abbreviated format 
(`write` and `w`, for example). A command is evaluated case insensitive, 
so `WRITE` is equivalent to `w`. MUMPS permits multiple commands on one
line, each separated by a space (well, rather a space should separate the
argument list of the previous command and the new command).

In REPL mode, MUMPy always expects either a command or a comment as the
leading input token. In interpreter mode, MUMPy expects a tag (a line label) 
and a space or a just space. Commands or comments may follow the space in
either case. For those who like to read ahead, there is a full routine
example given below.

MUMPS commands accept zero or more arguments in a comma delimited list. The
argument list should be separated from the command by a single space. 
The value of the arguments and the number that each command accepts vary
by the command and function. The list elements in the argument list
should not be separated by any space characters - spaces are, of course,
allowed in any string literals in the argument list.

Commands which accept zero arguments should still be followed by one space and,
if followed by another command on the same line, would be followed by the
space that would normally separate the next command and the previous command's
argument list. That is, there should always be two spaces between any two
commands (excluding any spaces contained within string literals in an 
argument list).

Since MUMPS is a programming language, it would be only appropriate to have
a "Hello, world!" example:

    mumpy > write "Hello, world!"
    Hello, world!
    
Here is the same example but using an argument list instead of a single
string literal argument. Using an argument list for a command is merely
syntactic sugar for performing the command twice in a row:
    
    mumpy > write "Hello, ","world!"
    Hello, world!
    mumpy > write "Hello, " write "world!"
    Hello, world!
    
All non-conditional commands (conditional commands are `IF`, `ELSE`, and
`FOR`) permit the caller to affix a post-conditional. This is an expression
which evaluates to a truth value (see Expressions below). If the expression
evaluates True, then the command proceeds with any arguments. If the
expression evaluates as False, then the command will not be executed by
the interpreter. While the post-conditional is a very powerful tool,
callers should be careful to recognize that the scope of the conditional
for the command _is just the command_. Line-scoped conditionals are
performed with the `IF` or `ELSE` commands.

    mumpy > write:(0) "This will not output."
    mumpy > write:(0) "Nor will this." write:(1) "But this will!"
    But this will!
    mumpy > write:(0) "All arguments ","are affected!"
    mumpy > write:(10*4) "This expression evaluates to true!"
    This expression evaluates to true!

### Data Types
Strictly speaking, the only data type in MUMPS is the string. MUMPS does 
handle numeric values as well, though these are really just a
specialized case of strings.

MUMPS strings can be evaluated as numbers readily with a very well defined
conversion. The unary plus operator `+` can be affixed to any string value
to produce a number from a string. The operation converts any numeric 
characters starting from the left (including `+`, `-`, and `.`) into
a number, quitting at the first non-numeric character it finds. Likewise,
certain operations are considered strictly _numeric_ and these operations
will also coerce the value using the same rules.

The conversions can be seen as below:
    
    mumpy > write +"27 dollars"
    27
    mumpy > write +"I need 27 dollars"
    0
    mumpy > write "27 dollars"+"12 dollars"
    39
    mumpy > write +"+---3.5.5"
    -3.5
    
There is no boolean data type in MUMPS, but certain operations evaluate
to so called 'truth-valued' expressions. In reality, these expressions
evaluate to either `0` (False) or `1` (True). Note that this does still
cause the M interpreter to perform the numeric cast described above on
string values. Any numeric value which is not 0 (including negative 
values) is evaluated as true. 
    
### Expressions
The M language implements many of the same operators that you are familiar
with in other languages. Unlike other languages, however, you are not
permitted to use any language elements outside of the context of a command.
Thus it would not be legal for the (otherwise valid) expression `1+1` outside
of some command. You could make that expression legal if you were to write
it like `write 1+1`, which would produce the value `2`. Likewise, you could
assign the result of that expression to a variable with `set x=1+1`. There
are many other legal places where programmers can include expressions.

Expressions can be arbitrarily complex, but programmers should note that
all binary operators operate at the same level of precedence. In practice,
this means that all binary operations evaluate in strict left-to-right order.
Since this differs from most common programming languages and typical
arithmetic computations, this can be quite inconvenient. However, programmers
can modify the order of precedence by surrounding expressions with 
parentheses. In the example below, we demonstrate the unexpected default
output and the easy modification to force standard order of operations.

    mumpy > write 1+2*4
    12
    mumpy > write 1+(2*4)
    9
    
Unary operators (such as the unary plus shown earlier) operate at a higher
level of precedence than the binary operators. These operators always
associate right. Given their higher precedence, programmers should not
need to make any special provision to force these operators to act as
they would normally expect.

MUMPS provides the following binary operators standard. Note that if the
operand is noted as __strictly__ numeric, this means that both operands
will be casted to numbers as described in the previous section. Likewise,
operations which are __strictly__ string will not perform any numeric
evaluation of the operands, even if both are numeric. Truth-valued operations
are the operations which produce truth-valued results, as described above.

 * Strictly numeric operations (return result of operation)
     * Addition: `+`
     * Subtraction: `-`
     * Multiplication: `*`
     * Division: `/`
     * Integer division: `\`
     * Modulus: `#`
     * Exponentiation: `**`
 * Strictly numeric comparisons (return truth-valued result)
     * Greater than: `>`
     * Not greater than: `'>`
     * Less than: `<`
     * Not less than: `'<`
 * Strictly string operations (return result of operation)
     * Concatenation: `_`
 * String comparisons (return truth-valued result)
     * Follows (left operand follows right in binary byte order): `]`
     * Sorts after (left operand sorts after right in collation order): `]]`
     * Contains (left operand contains right operand): `[`
     * Pattern match (left operand matches pattern in right operand): `?`
 * Truth-valued operations (return truth-valued result)
     * And: `&`
     * Not and: `'&`
     * Or: `!`
     * Not or: `'!`
     
The MUMPS unary operators are `+` and `-`, producing numeric values of either
positive or negative (or zero) value from any value (casting strings as
seen above). There is also a truth-valued `'` (Not) operator which will
negate the numeric value of an expression.

The only operator left out of the above list is the equals operator `=` and
it's negation `'=`. Equality comparison does not perform any strict casting
as many of the other operators do. A comparison between two strings will
test for string equality. A comparison between two numbers will test for
numeric equality. A comparison between a string and number will test for
string equality. This can lead to some perhaps unintuitive results:

    mumpy > write "0.1"=.100
    0
    mumpy > write 1="01"
    0
    mumpy > write 1=+"01"
    1

### Variables
MUMPS variables come in one of two flavors, local and global. Local variables
will be familiar to users of nearly every other programming language. 
Global variables may _sound_ familiar, but they have a somewhat different
implementation and meaning in MUMPS than in other languages. 

In other languages, a global variable is one which has global scope in the
current process - meaning that every execution unit of the program can access
and modify that value. In MUMPS, it is true that every routine in a process
(and indeed on the entire system) can share access to these globals. This is
because MUMPS global variables are actually persistent. MUMPS stores globals
on the hard-drive of the current operating environment, meaning that these
values survive the lifetime of the current process. MUMPS provides the 
facilities to lock and unlock global variable nodes to permit safe 
concurrent usage.
 
In their most simple case, these variables act as scalar values. However, 
both local and global variables act as multi-dimensional sparse arrays without'
any special handling by the programmer. Indeed this is one of the defining
features of MUMPS. The array nodes may be strings or numbers:

    mumpy > set person=45
    mumpy > set person("name")="Chris Smith"
    mumpy > set person("name","first")="Chris"
    mumpy > set person("name","last")="Smith"
    mumpy > set person("child",1)="Celia Smith"
    mumpy > set person("child",2)="Cameron Smith"

MUMPS stores the given array nodes in sorted order and provides the `$ORDER`
intrinsic function to allow programmers to step through array nodes:

    mumpy > set next=$ORDER(person("child",""))
    mumpy > write person("child",next)
    "Celia Smith"
    mumpy > set next=$ORDER(person("child",next))
    mumpy > write person("child",next)
    "Cameron Smith"
    
The examples above show operations on local variables. The same operations
can easily be performed on global variables merely by prefixing the name of
the variable with a `^` caret character; `^person` is a global variable, 
whereas `person` is a local variable.     

Programmers in MUMPS should also be mindful of the rather simplistic and
loose scoping rules that exist in MUMPS. MUMPS does _not_ enforce strict 
scoping rules. If a function or subroutine references a variable name not
explicitly defined on the current stack frame, MUMPS will search back through
the stack in reverse order and provide the caller with the first instance
of a variable with the given name. Programmers may use the `NEW` command
in a stack frame to explicitly declare a variable with the given name on
the current stack frame. This variable will be deleted from the stack once
the function or subroutine completes and MUMPS unwinds its stack frame.

### Input and Output
By default, the REPL and the routine interpreter set the Standard Input and
Standard Output as the default input and output devices, respectively.
Programmers can control the current device (unfortunately only as a unified
device, per the standard) by issuing a `open`, `use`, or `close` command.
For the explanation below, we will assume the user is just using the
default IO device, referred to in MUMPy as `STANDARD` (which can always be
accessed by the system variable `$PRINCIPAL`).

MUMPS Input is done largely through the `read` command, which accepts a
list of MUMPS expressions or variable names. For each variable name in its
argument list, the `read` command will read in from the current device
until the user terminates input using the Return key. The value of the 
user's input will be stored in the variable given.

MUMPS Output is performed using the `write` command. The `write` command
accepts one or more arguments, all of which are valid MUMPS expressions
or local or global variable names. The `write` command will output the
evaluated expressions or stored values to the current output device in
strict left-to-right order.

### Routines
Routines are briefly introduced in the Use section of this document. In M,
routines are the modular code-units by which programmers organize their
code. Inside of routines, programmers can include 1 or more lines of
M commands which perform some action or computation. Code can be further
organized in these routines by tags, which are simply line labels. 
Tag names start in the first character column of any given line (unlike
commands which must start in the second character column of a line). 

The routine filename (excluding extension) should always be the first tag
in the routine. Subsequent tags may be in the same format as the routine
tag defined above. Body tags (i.e. those tags which are not the routine
tag) may also be integers. Tags starting with a numeric character must
be entirely numeric, however. 

Any tag in the routine may also have a list of argument names immediately
following which are enclosed in parentheses and separated by commas 
(without any spaces). Tags are also permitted to have no arguments; in 
this format, they may either choose to have parentheses or not. Callers
must match their call format to the format of the tag in the routine. Thus,
a tag without parentheses may not be called with parentheses and a tag
with parentheses must be called with parentheses. Note that in M, all
arguments are technically optional. The interpreter performs an implicit
`NEW` on any arguments which are not explicitly passed in by the caller.
Thus, it is incumbent on the code within tags to accommodate null inputs
if they are expecting non-null inputs.

Programmers are not required to follow any strict organizational requirements
with their tags. One tag may freely flow into another or execution may
halt (using a `HALT` command), return to a caller (using a `QUIT` command), or
simply be redirected to another tag, line or routine (using a `GOTO` command).
In practice, programmers typically format their routines into subroutines
(tags which do not return a value via a `QUIT`) and extrinsic functions 
(tags which do return a value). This allows M programmers to safely emulate
other programming languages with more rigid code structure.

### Routine Example

     ;************************
     ;* Learn M example
     ;*
     ;* Users could copy this example into a file named
     ;* LEARNM.m and then invoke `mumpy.py -f LEARNM` to
     ;* see this routine in action.
     ;************************
    LEARNM ;
     new var,name,resp
     ;
     ; Ask the user their name
     read "What is your name? ",name
     ;
     ; Welcome them to MUMPy
     set var="Hello and welcome to MUMPy, "_name_"!"
     write var
     ;
     ; Ask them a question
     set resp=$$AskQuestion("How are you today?",10)
     ;
     ; Quit this subroutine
     q
     ;
     ; Ask the user a question and return their response.
     ; Allow the caller to indicate the maximum number of characters. 
    AskQuestion(question,max) ;
     new resp
     ;
     ; Set a default maximum number of characters if none was given
     set:(+max<1) max=40
     ;
     ; Write the question first (read cannot write non-string literals)
     write !,question," "
     ;
     ; Read their response (maximum of 'max' chars)
     read resp#max
     ;
     ; Return that value to the user
     quit resp

## Resources
The following resources have been invaluable to me as I have been writing
MUMPy:

* [Annotated MUMPS Standards](http://71.174.62.16/Demo/AnnoStd) - the closest
  thing to the ANSI/ISO M standards that is available online.
* [GT.M Programmers Guide]
  (http://tinco.pair.com/bhaskar/gtm/doc/books/pg/UNIX_manual/index.html) -
  GT.M is a (mostly) ANSI M compliant, open source MUMPS distribution. Its 
  documentation is superior to the Annotated Standard, but it does differ
  from the Standard in a few ways.
* [PLY](http://www.dabeaz.com/ply/ply.html) - Python Lex-Yacc is the
  Python library which provides Lexing and Parsing capabilities to MUMPy.
* [blist Documentation](http://stutzbachenterprises.com/blist/) - _blist_ 
  is the Python library MUMPy use for local variables (specifically nested 
  `sorteddict` instances).
* [Thunks, Trampolines, and Continuation Passing]
  (http://jtauber.com/blog/2008/03/30/thunks,_trampolines_and_continuation_passing/) -
  is a great resource on usage of all three title elements in Python.

## License
MUMPy is licensed under the 3-clause BSD license. See the LICENSE file 
included with the source code for more details.
