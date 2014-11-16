"""MUMPy Parser"""
import logging
import random
import ply.yacc as yacc
import mumpy
import mumpy.lang as lang


# noinspection PyMethodMayBeStatic
class MUMPSParser:
    def __init__(self, env, interpreter=False, debug=False):
        # The environment is the execution stack
        self.env = env
        self.debug = debug

        # Output log file that PLY uses to report Parse errors
        logging.basicConfig(
            level=logging.DEBUG if debug else logging.ERROR,
            filename="parse.log",
            filemode="w",
            format="mumpy :: %(message)s"
        )
        self.debug_log = logging.getLogger()

        # Define operator precedence
        # In MUMPS, all binary operators operate at the same level of precedence
        # which in practice means they all associate left.
        # Unary operators operate above binary operators.
        self.precedence = (
            ('left', 'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'IDIVIDE', 'AND', 'OR',
                     'GREATER_THAN', 'LESS_THAN', 'MODULUS', 'EXPONENT',
                     'CONCAT', 'FOLLOWS', 'CONTAINS', 'EQUALS'),
            ('right', 'NOT', 'UMINUS', 'UPLUS'),
        )

        # Define the routine parser and lexer
        self.rou = dict()
        self.rou['lex'] = mumpy.MUMPSLexer(is_rou=True, debug=debug)
        self.tokens = self.rou['lex'].tokens
        self.rou['parser'] = yacc.yacc(module=self, start='start',
            debug=logging.DEBUG if debug else logging.ERROR,
            debuglog=self.debug_log,
            tabmodule='routab')

        # If we're entering from a REPL, we need a separate Lexer and Parser
        if interpreter:
            self.repl = dict()
            self.repl['lex'] = mumpy.MUMPSLexer(is_rou=False, debug=debug)
            self.repl['parser'] = yacc.yacc(module=self,
                                            debug=debug,
                                            start='valid_input',
                                            tabmodule='repltab')

    def parse_repl(self, data):
        """Parse an arbitrary line of MUMPS code and return the output to
        the caller. This function is designed to be run from the REPL
        terminal.

        In the case of the REPL, the output will be discarded by
        the interpreter loop. Callers are advised to prepend any calls
        resulting in an expression with a `w` like `w $$func^ROU()` to
        see the resultant expression."""
        try:
            # Output the Lexer tokens
            if self.debug:
                self.repl['lex'].test(data)

            # Reset the Lexer and parse the data
            self.repl['lex'].reset()
            p = self.repl['parser'].parse(data, lexer=self.repl['lex'].lexer)

            # Execute the parsed command(s)
            try:
                return p.execute()
            except Exception as e:
                raise mumpy.MUMPSSyntaxError(e)
        except KeyError:
            print("The REPL was not set up correctly. Quitting...")

    def parse(self, data):
        """Parse an arbitrary line of MUMPS code and return the output to
        the caller."""
        # Output the Lexer tokens
        if self.debug:
            self.rou['lex'].test(data)

        # Reset the Lexer and parse the data
        self.rou['lex'].reset()
        p = self.rou['parser'].parse(data, lexer=self.rou['lex'].lexer)

        # Execute the parsed command(s)
        try:
            return p.execute()
        except Exception as e:
            raise mumpy.MUMPSSyntaxError(e)

    def parse_file(self, f, tag=None):
        """Parse a MUMPSFile from the first tag or at a specified tag."""
        # Check that we got a compiled file
        if not isinstance(f, mumpy.MUMPSFile):
            raise TypeError("Please specify a valid MUMPS routine.")

        # Set the executing routine in the environment
        self.env.set_current_rou(f, tag=tag)

        # If no tag is specified, start at the beginning
        lines = f.tag_body(tag if tag is not None else f.rou)
        ret = None
        for line in lines:
            if self.debug:
                self.rou['lex'].test(line)

            self.rou['lex'].reset()
            p = self.rou['parser'].parse(line, lexer=self.rou['lex'].lexer)
            ret = p.execute()
            if ret is not None:
                break

        # Reset the Lexer and Parser to the correct state
        self.rou['lex'].reset()

        # Return any resulting expression to the caller
        return ret

    def parse_tag(self, f, tag):
        """Parse a MUMPSFile starting at the specified tag. This function is
        intended to be called internally when the parser identifies a
        function or subroutine call."""
        # Check that we got a compiled file
        if not isinstance(f, mumpy.MUMPSFile):
            raise TypeError("Please specify a valid MUMPS routine.")

        # Get the tag body
        lines = f.tag_body(tag)
        ret = None
        for line in lines:
            if self.debug:
                self.rou['lex'].test(line)

            self.rou['lex'].reset()
            p = self.rou['parser'].parse(line, lexer=self.rou['lex'].lexer)
            ret = p.execute()
            if ret is not None:
                break

        # Reset the Lexer and Parser to the correct state
        self.rou['lex'].reset()

        # Return any resulting expression to the caller
        return ret

    def parse_xecute(self, args, env):
        """Parse an expression for an XECUTE command."""
        for expr in args:
            self.repl['lex'].reset()
            p = self.repl['parser'].parse(str(expr), lexer=self.repl['lex'].lexer)
            try:
                return p.execute()
            except Exception as e:
                raise mumpy.MUMPSSyntaxError(e)

    def p_error(self, p):
        if p is not None:
            raise lang.MUMPSSyntaxError(str(p), err_type="PARSE ERROR",
                                        line=p.lineno)

    ###################
    # GENERIC INPUT
    ###################
    def p_start(self, p):
        """start : tag_line
                 | command_line
                 | comment_line"""
        p[0] = p[1]

    def p_tag_line(self, p):
        """tag_line : tag SPACE valid_input
                    | tag SPACE comment"""
        p[0] = p[3]

    def p_command_line(self, p):
        """command_line : SPACE valid_input"""
        p[0] = p[2]

    def p_comment_line(self, p):
        """comment_line : SPACE comment"""
        p[0] = p[2]

    def p_comment(self, p):
        """comment : COMMENT"""
        p[0] = mumpy.MUMPSEmptyLine(p[1])

    def p_input(self, p):
        """valid_input : valid_input SPACE command
                       | valid_input any_command
                       | any_command"""
        l = len(p)
        if l == 4:
            p[0] = mumpy.MUMPSLine(p[3], p[1])
        elif l == 3:
            p[0] = mumpy.MUMPSLine(p[2], p[1])
        else:
            p[0] = mumpy.MUMPSLine(p[1])

    def p_any_command(self, p):
        """any_command : command
                       | command SPACE
                       | command_no_arg"""
        p[0] = p[1]

    def p_command_argument(self, p):
        """command : write_command
                   | set_command
                   | quit_command
                   | quit_post_command
                   | hang_command
                   | new_command
                   | kill_command
                   | read_command
                   | do_command
                   | xecute_command
                   | if_command"""
        p[0] = p[1]

    def p_command_no_arg(self, p):
        """command_no_arg : kill_all_command
                          | halt_command
                          | write_symbols
                          | if_command_no_args
                          | else_command"""
        p[0] = p[1]

    ###################
    # COMMANDS
    ###################
    def p_new_command(self, p):
        """new_command : NEW SPACE symbol_list"""
        p[0] = mumpy.MUMPSCommand(lang.new_var, p[3], self.env)

    def p_do_command(self, p):
        """do_command : DO SPACE subroutine_call_list
                      | DO COLON expression SPACE subroutine_call_list"""
        if len(p) > 4:
            post = p[3]
            args = p[5]
        else:
            post = None
            args = p[3]

        p[0] = mumpy.MUMPSCommand(lang.do_cmd, args, self.env, post=post)

    def p_if_command(self, p):
        """if_command : IF SPACE argument_list"""
        p[0] = mumpy.MUMPSCommand(lang.if_cmd, p[3], self.env)

    def p_if_command_no_arg(self, p):
        """if_command_no_args : IF no_argument"""
        p[0] = mumpy.MUMPSCommand(lang.if_no_args, None, self.env)

    def p_else_command(self, p):
        """else_command : ELSE no_argument"""
        p[0] = mumpy.MUMPSCommand(lang.else_cmd, None, self.env)

    def p_kill_command(self, p):
        """kill_command : KILL SPACE symbol_list
                        | KILL COLON expression SPACE symbol_list"""
        # Handle the post-conditional if it exists
        if len(p) > 4:
            post = p[3]
            symbols = p[5]
        else:
            post = None
            symbols = p[3]

        # Kill the specified symbols
        p[0] = mumpy.MUMPSCommand(lang.kill, symbols, self.env, post=post)

    def p_kill_all_command(self, p):
        """kill_all_command : KILL no_argument
                            | KILL COLON expression no_argument"""
        # Handle the post-conditional if it exists
        post = p[3] if len(p) > 4 else None

        # Kill the specified symbols
        p[0] = mumpy.MUMPSCommand(lang.kill_all, None, self.env, post=post)

    def p_set_command(self, p):
        """set_command : SET SPACE assignment_list
                       | SET COLON expression SPACE assignment_list"""
        # Evaluate the post-conditional if it exists
        if len(p) > 4:
            post = p[3]
            args = p[5]
        else:
            post = None
            args = p[3]

        # Set the values
        p[0] = mumpy.MUMPSCommand(lang.set_var, args, self.env, post=post)

    def p_read_command(self, p):
        """read_command : READ SPACE argument_list
                        | READ COLON expression SPACE argument_list"""
        # Evaluate the post-conditional if it exists
        if len(p) > 4:
            post = p[3]
            args = p[5]
        else:
            post = None
            args = p[3]

        # Write out the outputs
        p[0] = mumpy.MUMPSCommand(lang.read, args, self.env, post=post)

    def p_write_command(self, p):
        """write_command : WRITE SPACE argument_list
                         | WRITE COLON expression SPACE argument_list"""
        # Evaluate the post-conditional if it exists
        if len(p) > 4:
            post = p[3]
            args = p[5]
        else:
            post = None
            args = p[3]

        # Write out the outputs
        p[0] = mumpy.MUMPSCommand(lang.write, args, self.env, post=post)

    def p_write_symbols(self, p):
        """write_symbols : WRITE no_argument"""
        p[0] = mumpy.MUMPSCommand(lang.write_symbols, None, self.env)

    def p_xecute_command(self, p):
        """xecute_command : XECUTE SPACE argument_list
                          | XECUTE COLON expression SPACE argument_list"""
        # Evaluate the post-conditional if it exists
        if len(p) > 4:
            post = p[3]
            args = p[5]
        else:
            post = None
            args = p[3]

        # Execute the code in each expression
        p[0] = mumpy.MUMPSCommand(self.parse_xecute, args, self.env, post=post)

    def p_quit(self, p):
        """quit_command : QUIT
                        | QUIT SPACE expression"""
        # Handle the post-conditional if it exists
        args = mumpy.MUMPSArgumentList(p[3]) if len(p) == 4 else None
        p[0] = mumpy.MUMPSCommand(lang.quit_cmd, args, self.env)

    def p_quit_post(self, p):
        """quit_post_command : QUIT COLON expression
                             | QUIT COLON expression SPACE expression"""
        args = mumpy.MUMPSArgumentList(p[5]) if len(p) > 4 else None
        p[0] = mumpy.MUMPSCommand(lang.quit_cmd, args, self.env, post=p[3])

    def p_halt(self, p):
        """halt_command : HALT_HANG
                        | HALT
                        | HALT_HANG COLON expression
                        | HALT COLON expression"""
        post = p[3] if len(p) == 4 else None
        p[0] = mumpy.MUMPSCommand(lang.halt, None, self.env, post=post)

    def p_hang(self, p):
        """hang_command : HALT_HANG SPACE numeric
                        | HANG SPACE numeric
                        | HALT_HANG COLON expression SPACE numeric
                        | HANG COLON expression SPACE numeric"""
        # Handle the post-conditional if it exists
        if len(p) > 4:
            post = p[3]
            num = mumpy.MUMPSArgumentList(p[5].as_number())
        else:
            post = None
            num = mumpy.MUMPSArgumentList(p[3].as_number())

        # Put the system to sleep for the specified number of seconds
        p[0] = mumpy.MUMPSCommand(lang.hang, num, self.env, post=post)

    ###################
    # COMMAND MISC
    ###################
    def p_routine_global(self, p):
        """routine_global : CARET identifier"""
        p[0] = p[2]

    def p_subroutine_call_list(self, p):
        """subroutine_call_list : subroutine_call_list COMMA subroutine_call
                                | subroutine_call"""
        if len(p) == 4:
            p[0] = mumpy.MUMPSArgumentList(p[3], p[1])
        else:
            p[0] = mumpy.MUMPSArgumentList(p[1])

    def p_subroutine_call(self, p):
        """subroutine_call : subroutine_call_tag
                           | subroutine_call_no_tag
                           | subroutine_call_no_rou
                           | subroutine_call COLON expression"""
        if len(p) == 4:
            p[1].post = p[3]
        p[0] = p[1]

    def p_subroutine_call_tag(self, p):
        """subroutine_call_tag : identifier routine_global
                               | identifier routine_global LPAREN RPAREN
                               | identifier routine_global LPAREN func_sub_argument_list RPAREN"""
        l = len(p)
        if l == 6:
            args = p[4]
        elif l == 5:
            args = ()
        else:
            args = None
        p[0] = mumpy.MUMPSFuncSubCall(p[1], self.env, self, args=args,
                                      is_func=False, rou=p[2])

    def p_subroutine_call_no_tag(self, p):
        """subroutine_call_no_tag : routine_global
                                  | routine_global LPAREN RPAREN
                                  | routine_global LPAREN func_sub_argument_list RPAREN"""
        l = len(p)
        if l == 5:
            args = p[3]
        elif l == 4:
            args = ()
        else:
            args = None
        p[0] = mumpy.MUMPSFuncSubCall(p[1], self.env, self, args=args,
                                      is_func=False, rou=p[1])

    def p_subroutine_call_no_rou(self, p):
        """subroutine_call_no_rou : identifier
                                  | identifier LPAREN RPAREN
                                  | identifier LPAREN func_sub_argument_list RPAREN"""
        l = len(p)
        if l == 5:
            args = p[3]
        elif l == 4:
            args = ()
        else:
            args = None
        p[0] = mumpy.MUMPSFuncSubCall(p[1], self.env, self, args=args,
                                      is_func=False)

    def p_function_call(self, p):
        """function_call : function_call_tag
                         | function_call_no_tag
                         | function_call_no_rou"""
        p[0] = mumpy.MUMPSExpression(p[1])

    def p_function_call_tag(self, p):
        """function_call_tag : EXTRINSIC identifier routine_global
                             | EXTRINSIC identifier routine_global LPAREN RPAREN
                             | EXTRINSIC identifier routine_global LPAREN func_sub_argument_list RPAREN"""
        l = len(p)
        if l == 7:
            args = p[5]
        elif l == 6:
            args = ()
        else:
            args = None
        p[0] = mumpy.MUMPSFuncSubCall(p[2], self.env, self, args=args,
                                      is_func=True, rou=p[3])

    def p_function_call_no_tag(self, p):
        """function_call_no_tag : EXTRINSIC routine_global
                                | EXTRINSIC routine_global LPAREN RPAREN
                                | EXTRINSIC routine_global LPAREN func_sub_argument_list RPAREN"""
        l = len(p)
        if l == 6:
            args = p[4]
        elif l == 5:
            args = ()
        else:
            args = None
        p[0] = mumpy.MUMPSFuncSubCall(p[2], self.env, self, args=args,
                                      is_func=True, rou=p[2])

    def p_function_call_no_rou(self, p):
        """function_call_no_rou : EXTRINSIC identifier
                                | EXTRINSIC identifier LPAREN RPAREN
                                | EXTRINSIC identifier LPAREN func_sub_argument_list RPAREN"""
        l = len(p)
        if l == 6:
            args = p[4]
        elif l == 5:
            args = ()
        else:
            args = None
        p[0] = mumpy.MUMPSFuncSubCall(p[2], self.env, self, args=args,
                                      is_func=True)

    def p_func_sub_argument_list(self, p):
        """func_sub_argument_list : func_sub_argument_list COMMA func_sub_argument
                                  | func_sub_argument"""
        if len(p) == 4:
            p[0] = mumpy.MUMPSArgumentList(p[3], p[1])
        else:
            p[0] = mumpy.MUMPSArgumentList(p[1])

    def p_func_sub_argument(self, p):
        """func_sub_argument : pointer_argument
                             | expression"""
        p[0] = p[1]

    def p_pointer_argument(self, p):
        """pointer_argument : PERIOD identifier"""
        p[0] = mumpy.MUMPSPointerIdentifier(p[2], self.env)

    def p_symbol_list(self, p):
        """symbol_list : symbol_list COMMA identifier
                       | identifier"""
        if len(p) == 4:
            p[0] = mumpy.MUMPSArgumentList(p[3], p[1])
        else:
            p[0] = mumpy.MUMPSArgumentList(p[1])

    def p_argument_list(self, p):
        """argument_list : argument_list COMMA expression
                         | expression"""
        if len(p) == 4:
            p[0] = mumpy.MUMPSArgumentList(p[3], p[1])
        else:
            p[0] = mumpy.MUMPSArgumentList(p[1])

    def p_sel_argument_list(self, p):
        """sel_argument_list : sel_argument_list COMMA sel_argument
                             | sel_argument"""
        if len(p) == 4:
            p[0] = mumpy.MUMPSArgumentList(p[3], p[1])
        else:
            p[0] = mumpy.MUMPSArgumentList(p[1])

    def p_sel_argument(self, p):
        """sel_argument : expression COLON expression"""
        p[0] = (p[1], p[3])

    def p_assignment(self, p):
        """assignment : identifier EQUALS expression"""
        p[0] = (p[1], p[3])

    def p_assignment_list(self, p):
        """assignment_list : assignment_list COMMA assignment
                           | assignment"""
        if len(p) == 4:
            p[0] = mumpy.MUMPSArgumentList(p[3], p[1])
        else:
            p[0] = mumpy.MUMPSArgumentList(p[1])

    def p_no_arguments(self, p):
        """no_argument : SPACE SPACE"""
        p[0] = p[1]

    ###################
    # GENERIC DEFS
    ###################
    def p_expression(self, p):
        """expression : string_contents
                      | not_and_expr
                      | not_or_expr
                      | not_gt_expr
                      | not_lt_expr
                      | not_equals_expr
                      | not_follows_expr
                      | not_sorts_after_expr
                      | not_contains_expr
                      | not_expr
                      | and_expr
                      | or_expr
                      | gt_expr
                      | lt_expr
                      | equals_expr
                      | contains_expr
                      | sorts_after_expr
                      | follows_expr
                      | string_concat
                      | numeric_op
                      | expression_parens
                      | identifier
                      | function_call
                      | intrinsic_func
                      | special_var"""
        p[0] = mumpy.MUMPSExpression(p[1])

    def p_expression_parens(self, p):
        """expression_parens : LPAREN expression RPAREN"""
        p[0] = p[2]

    def p_string_contents(self, p):
        """string_contents : STRING"""
        start_quote = p[1].startswith("\"")
        end_quote = p[1].endswith("\"")
        if start_quote and end_quote:
            v = p[1][1:-1]
        elif end_quote:
            v = p[1][:-1]
        else:
            v = p[1][1:]

        p[0] = mumpy.MUMPSExpression(v)

    def p_numeric_op(self, p):
        """numeric_op : numeric
                      | addition
                      | subtraction
                      | multiplication
                      | division
                      | idivision
                      | modulus
                      | exponent"""
        p[0] = p[1]

    def p_numeric(self, p):
        """numeric : NUMBER
                   | uplus
                   | uminus"""
        p[0] = mumpy.MUMPSExpression(p[1])

    def p_string_concat(self, p):
        """string_concat : expression CONCAT expression"""
        p[0] = mumpy.MUMPSExpression(p[1]).concat(p[3])

    def p_identifier(self, p):
        """identifier : SYMBOL"""
        p[0] = mumpy.MUMPSIdentifier(p[1], self.env)

    def p_tag(self, p):
        """tag : identifier LPAREN func_sub_argument_list RPAREN
               | identifier LPAREN RPAREN
               | identifier"""
        p[0] = mumpy.MUMPSEmptyLine("{ident}".format(
            ident=p[1],
            #args=p[3] if len(p) == 5 else ""
        ))

    ###################
    # INTRINSICS
    ###################
    def p_intrinsic_func(self, p):
        """intrinsic_func : ascii_func
                          | char_func
                          | extract_func
                          | find_func
                          | justify_func
                          | length_func
                          | name_func
                          | piece_func
                          | random_func
                          | reverse_func
                          | select_func
                          | translate_func
                          | intrinsic_not_exist"""
        p[0] = p[1]

    def p_instrinsic_dne(self, p):
        """intrinsic_not_exist : FN_DOES_NOT_EXIST error"""
        raise mumpy.MUMPSSyntaxError("Function does not exist.",
                                     err_type="FN DOES NOT EXIST")

    def p_ascii(self, p):
        """ascii_func : ASCII LPAREN expression COMMA expression RPAREN
                      | ASCII LPAREN expression RPAREN"""
        pnum = int(p[5].as_number()) if len(p) == 7 else 1
        try:
            if pnum-1 >= 0:
                char = ord(str(p[3])[pnum-1])
            else:
                raise IndexError
        except IndexError:
            char = mumpy.MUMPSExpression(-1)
        p[0] = mumpy.MUMPSExpression(char)

    def p_char(self, p):
        """char_func : CHAR LPAREN argument_list RPAREN"""
        chars = []
        for arg in p[3]:
            chars.append(chr(arg.as_number()))
        p[0] = mumpy.MUMPSExpression(''.join(chars))

    def p_extract(self, p):
        """extract_func : EXTRACT LPAREN expression COMMA expression COMMA expression RPAREN
                        | EXTRACT LPAREN expression COMMA expression RPAREN
                        | EXTRACT LPAREN expression RPAREN"""
        l = len(p)
        try:
            if l == 9:
                # Get the indices
                slen = len(str(p[3]))
                low = p[5].as_number()-1
                high = p[7].as_number()         # High index is inclusive

                # The low index cannot be higher than the high index
                # The low index cannot be below 0 either
                if low > high or low < 0:
                    raise IndexError

                # The length cannot exceed the string length
                if high > slen:
                    high = slen

                p[0] = mumpy.MUMPSExpression(str(p[3])[low:high])
            elif l == 7:
                # Get the index
                idx = p[5].as_number()-1

                # The low index cannot be below 0
                if idx < 0:
                    raise IndexError

                p[0] = mumpy.MUMPSExpression(str(p[3])[idx])
            else:
                p[0] = mumpy.MUMPSExpression(str(p[3])[0])
        except IndexError:
            p[0] = mumpy.mumps_null()

    def p_find(self,p):
        """find_func : FIND LPAREN expression COMMA expression COMMA expression RPAREN
                     | FIND LPAREN expression COMMA expression RPAREN"""
        start = p[7].as_number() if len(p) == 9 else None
        try:
            sub = str(p[5])
            p[0] = mumpy.MUMPSExpression(
                str(p[3]).index(sub, start) + len(sub) + 1)
        except ValueError:
            p[0] = mumpy.MUMPSExpression(0)

    def p_justify(self, p):
        """justify_func : JUSTIFY LPAREN expression COMMA expression COMMA expression RPAREN
                        | JUSTIFY LPAREN expression COMMA expression RPAREN"""
        l = len(p)
        if l == 9:
            fdec = p[7].as_number()
            expr = round(p[3].as_number(), p[7].as_number())
            s = str(expr).split(".")
            dig, dec = len(s[0]), len(s[1])
            expr = expr if dec >= fdec else str(expr).ljust(fdec + dig + 1, '0')
        else:
            expr = p[3]
        p[0] = mumpy.MUMPSExpression(str(expr).rjust(p[5].as_number()))

    def p_length(self, p):
        """length_func : LENGTH LPAREN expression COMMA expression RPAREN
                       | LENGTH LPAREN expression RPAREN"""
        if len(p) == 7:
            p[0] = mumpy.MUMPSExpression(str(p[3]).count(str(p[5])))
        else:
            p[0] = mumpy.MUMPSExpression(len(str(p[3])))

    def p_name(self, p):
        """name_func : NAME LPAREN identifier RPAREN"""
        p[0] = mumpy.MUMPSExpression(str(p[3]))

    def p_piece(self, p):
        """piece_func : PIECE LPAREN expression COMMA expression COMMA expression RPAREN
                      | PIECE LPAREN expression COMMA expression RPAREN"""
        pnum = int(p[7].as_number()) if len(p) == 9 else 1
        try:
            piece = str(p[3]).split(sep=str(p[5]))[pnum-1]
        except IndexError:
            piece = mumpy.mumps_null()
        p[0] = mumpy.MUMPSExpression(piece)

    def p_random(self, p):
        """random_func : RANDOM LPAREN expression RPAREN"""
        num = p[3].as_number()
        if num < 1:
            raise mumpy.MUMPSSyntaxError("RANDOM argument less than 1.",
                                         err_type="$R ARG INVALID")
        p[0] = mumpy.MUMPSExpression(random.randint(0, num))

    def p_reverse(self, p):
        """reverse_func : REVERSE LPAREN expression RPAREN"""
        p[0] = mumpy.MUMPSExpression(str(p[3])[::-1])

    def p_select(self, p):
        """select_func : SELECT LPAREN sel_argument_list RPAREN"""
        for arg in p[3]:
            if arg[0]:
                p[0] = arg[1]
                return
        raise mumpy.MUMPSSyntaxError("No select arguments evaluated true.",
                                     err_type="NO $S ARGS TRUE")

    def p_translate(self, p):
        """translate_func : TRANSLATE LPAREN expression COMMA expression COMMA expression RPAREN
                          | TRANSLATE LPAREN expression COMMA expression RPAREN"""
        # We use the string and translation map for both paths
        s = str(p[3])
        trs = str(p[5])
        trmap = dict()

        if len(p) == 9:
            # Create a translation map between the two strings
            # In this iteration, we map positionally identical characters
            # between the input string and the replacement string
            # Characters without matches are deleted (i.e. if the
            # replacement string is shorter than the translation string)
            newmap = str(p[7])
            for i, c in enumerate(trs):
                try:
                    trmap[ord(c)] = ord(newmap[i])
                except IndexError:
                    trmap[ord(c)] = None
        else:
            # Create a translation map which deletes each character
            for c in trs:
                trmap[ord(c)] = None

        # Return the translated string
        p[0] = mumpy.MUMPSExpression(s.translate(trmap))

    ###################
    # SPECIAL VARIABLES
    ###################
    def p_special_var(self, p):
        """special_var : horolog_var
                       | test_var
                       | x_var
                       | y_var"""
        p[0] = p[1]

    def p_horolog(self, p):
        """horolog_var : HOROLOG"""
        p[0] = mumpy.MUMPSExpression(lang.horolog())

    def p_test_var(self, p):
        """test_var : TEST_TEXT
                    | TEST"""
        p[0] = mumpy.MUMPSExpression(self.env.get("$T"))

    def p_x_var(self, p):
        """x_var : DOLLARX"""
        p[0] = mumpy.MUMPSExpression(self.env.get("$X"))

    def p_y_var(self, p):
        """y_var : DOLLARY"""
        p[0] = mumpy.MUMPSExpression(self.env.get("$Y"))

    ###################
    # LOGIC
    ###################
    def p_not_expr(self, p):
        """not_expr : NOT expression"""
        p[0] = ~p[2]

    def p_and_expr(self, p):
        """and_expr : expression AND expression"""
        p[0] = p[1] & p[3]

    def p_not_and_expr(self, p):
        """not_and_expr : expression NOT AND expression"""
        p[0] = ~ (p[1] & p[4])

    def p_or_expr(self, p):
        """or_expr : expression OR expression"""
        p[0] = p[1] | p[3]

    def p_not_or_expr(self, p):
        """not_or_expr : expression NOT OR expression"""
        p[0] = p[1] | p[4]

    ###################
    # COMPARISON
    ###################
    def p_greater_than(self, p):
        """gt_expr : expression GREATER_THAN expression"""
        p[0] = p[1] > p[3]

    def p_not_greater_than(self, p):
        """not_gt_expr : expression NOT GREATER_THAN expression"""
        p[0] = ~ (p[1] > p[4])

    def p_less_than(self, p):
        """lt_expr : expression LESS_THAN expression"""
        p[0] = p[1] < p[3]

    def p_not_less_than(self, p):
        """not_lt_expr : expression NOT LESS_THAN expression"""
        p[0] = ~ (p[1] < p[4])

    def p_equal_to(self, p):
        """equals_expr : expression EQUALS expression"""
        p[0] = p[1] == p[3]

    def p_not_equal_to(self, p):
        """not_equals_expr : expression NOT EQUALS expression"""
        p[0] = p[1] != p[4]

    def p_follows(self, p):
        """follows_expr : expression FOLLOWS expression"""
        p[0] = mumpy.MUMPSExpression(p[1]).follows(p[3])

    def p_not_follows(self, p):
        """not_follows_expr : expression NOT FOLLOWS expression"""
        p[0] = ~mumpy.MUMPSExpression(p[1]).follows(p[4])

    def p_sorts_after(self, p):
        """sorts_after_expr : expression FOLLOWS FOLLOWS expression"""
        p[0] = mumpy.MUMPSExpression(p[1]).sorts_after(p[4])

    def p_not_sorts_after(self, p):
        """not_sorts_after_expr : expression NOT FOLLOWS FOLLOWS expression"""
        p[0] = ~mumpy.MUMPSExpression(p[1]).sorts_after(p[5])

    def p_contains(self, p):
        """contains_expr : expression CONTAINS expression"""
        p[0] = mumpy.MUMPSExpression(p[1]).contains(p[3])

    def p_not_contains(self, p):
        """not_contains_expr : expression NOT CONTAINS expression"""
        p[0] = ~mumpy.MUMPSExpression(p[1]).contains(p[4])

    ###################
    # ARITHMETIC
    ###################
    def p_uplus(self, p):
        """uplus : PLUS expression %prec UPLUS"""
        p[0] = +mumpy.MUMPSExpression(p[2])

    def p_uminus(self, p):
        """uminus : MINUS expression %prec UMINUS"""
        p[0] = -mumpy.MUMPSExpression(p[2])

    def p_addition(self, p):
        """addition : expression PLUS expression"""
        p[0] = p[1] + p[3]

    def p_subtraction(self, p):
        """subtraction : expression MINUS expression"""
        p[0] = p[1] - p[3]

    def p_multiplication(self, p):
        """multiplication : expression TIMES expression"""
        p[0] = p[1] * p[3]

    def p_division(self, p):
        """division : expression DIVIDE expression"""
        try:
            p[0] = p[1] / p[3]
        except ZeroDivisionError:
            raise mumpy.MUMPSSyntaxError("Cannot divide by zero.",
                                         err_type="DIVIDE BY ZERO")

    def p_idivision(self, p):
        """idivision : expression IDIVIDE expression"""
        try:
            p[0] = p[1] // p[3]
        except ZeroDivisionError:
            raise mumpy.MUMPSSyntaxError("Cannot divide by zero.",
                                         err_type="DIVIDE BY ZERO")

    def p_modulus(self, p):
        """modulus : expression MODULUS expression"""
        try:
            p[0] = p[1] % p[3]
        except ZeroDivisionError:
            raise mumpy.MUMPSSyntaxError("Cannot divide by zero.",
                                         err_type="DIVIDE BY ZERO")

    def p_exponent(self, p):
        """exponent : expression EXPONENT expression"""
        try:
            p[0] = p[1] ** p[3]
        except TypeError:
            raise mumpy.MUMPSSyntaxError("The result is a complex number.",
                                         err_type="COMPLEX RESULT")
