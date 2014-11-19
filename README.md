# MUMPy
MUMPy is an [ANSI M](http://en.wikipedia.org/wiki/MUMPS) interpreter written
in pure Python. It provides both a functional Read-Eval-Print Loop (REPL) for
the M language and a routine interpreter, allowing routines to be executed
directly from the command shell.

Features:

* Allows calling

## Installation
MUMPy can be installed using `pip`:

    pip install git+git://github.com/chrisrink10/mumpy@master

## Use
To use MUMPy, simply fire it up from the command line: `mumpy.py`. 

Command line options are available by invoking the `--help` parameter.

## MUMPS Primer
I will provide a basic MUMPS primer for users who are unfamiliar with 
its peculiar syntax. It is important to note that in MUMPS, white space
outside of string literals is **very** important. Due to the limitations
of the REPL format, the syntactic requirements enforced on routines are 
relaxed in REPL mode. White space remains important, but there are 
slightly different syntactic requirements.

### MUMPS Commands
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

### MUMPS Data Types and Expressions
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

### MUMPS Variables
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

### Routine Example

     ;************************
     ;* Comment Header
     ;************************
    ROUTINE ;
     new var,name
     ;
     ; Ask the user their name
     read "What is your name? ",name
     ;
     ; Welcome them to MUMPy
     set var="Hello and welcome to MUMPy, "_name_"!"
     write var
     ;
     ; Quit this subroutine
     q

## Resources
The following resources have been invaluable to me as I have been writing
MUMPy:
* [Annotated MUMPS Standards](http://71.174.62.16/Demo/AnnoStd) - the closest
  thing to the ANSI/ISO M standards that is available online.
* [GT.M Programmers Guide](http://tinco.pair.com/bhaskar/gtm/doc/books/pg/UNIX_manual/index.html) -
  GT.M is a (mostly) ANSI M compliant, open source MUMPS distribution. Its 
  documentation is superior to the Annotated Standard, but it does differ
  from the Standard in a few ways.

## License
MUMPy is licensed under the 3-clause BSD license. See the LICENSE file 
included with the source code for more details.