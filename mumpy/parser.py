"""MUMPy Parser"""
import logging
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
                     'CONCAT', 'FOLLOWS', 'SORTS_AFTER', 'CONTAINS', 'EQUALS'),
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
        """Parse a MUMPSFile from the first tag or at a specified tag.

        If execution of this function occurs from the interpreter, the state
        of the parser will be modified during execution to account for
        differences in parsing routines and individual command lines."""
        # Check that we got a compiled file
        if not isinstance(f, mumpy.MUMPSFile):
            raise TypeError("Please specify a valid MUMPS routine.")

        # If no tag is specified, start at the beginning
        lines = f.lines() if tag is None else f.tag_body(tag)
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

    def parse_xecute(self, expr, env):
        """Parse an expression for an XECUTE command."""
        self.rou['lex'].reset()
        p = self.rou['parser'].parse(expr, lexer=self.rou['lex'].lexer)
        return p.execute()

    def p_error(self, p):
        if p is not None:
            raise lang.MUMPSSyntaxError(str(p), err_type="PARSE ERROR")

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
                       | command"""
        if len(p) >= 4:
            p[0] = mumpy.MUMPSLine(p[3], p[1])
        else:
            p[0] = mumpy.MUMPSLine(p[1])

    def p_command_argument(self, p):
        """command : write_command
                   | set_command
                   | quit_command
                   | quit_post_command
                   | hang_command
                   | new_command
                   | kill_command
                   | kill_all_command
                   | read_command
                   | do_command
                   | xecute_command
                   | halt_command
                   | write_symbols
                   | if_command"""
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
        """if_command : IF argument_list"""
        p[0] = mumpy.MUMPSCommand(lang.if_cmd, p[2], self.env)

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
                           | subroutine_call_no_rou"""
        p[0] = p[1]

    def p_subroutine_call_tag(self, p):
        """subroutine_call_tag : identifier routine_global
                               | identifier routine_global LPAREN RPAREN
                               | identifier routine_global LPAREN func_sub_argument_list RPAREN"""
        args = p[4] if len(p) == 6 else None
        p[0] = mumpy.MUMPSFuncSubCall(p[1], self.env, self, args=args,
                                      is_func=False, rou=p[2])

    def p_subroutine_call_no_tag(self, p):
        """subroutine_call_no_tag : routine_global
                                  | routine_global LPAREN RPAREN
                                  | routine_global LPAREN func_sub_argument_list RPAREN"""
        args = p[3] if len(p) == 5 else None
        p[0] = mumpy.MUMPSFuncSubCall(p[1], self.env, self, args=args,
                                      is_func=False, rou=p[1])

    def p_subroutine_call_no_rou(self, p):
        """subroutine_call_no_rou : identifier
                                  | identifier LPAREN RPAREN
                                  | identifier LPAREN func_sub_argument_list RPAREN"""
        args = p[3] if len(p) == 5 else None
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
        args = p[5] if len(p) == 7 else None
        p[0] = mumpy.MUMPSFuncSubCall(p[2], self.env, self, args=args,
                                      is_func=True, rou=p[3])

    def p_function_call_no_tag(self, p):
        """function_call_no_tag : EXTRINSIC routine_global
                                | EXTRINSIC routine_global LPAREN RPAREN
                                | EXTRINSIC routine_global LPAREN func_sub_argument_list RPAREN"""
        args = p[4] if len(p) == 6 else None
        p[0] = mumpy.MUMPSFuncSubCall(p[2], self.env, self, args=args,
                                      is_func=True, rou=p[2])

    def p_function_call_no_rou(self, p):
        """function_call_no_rou : EXTRINSIC identifier
                                | EXTRINSIC identifier LPAREN RPAREN
                                | EXTRINSIC identifier LPAREN func_sub_argument_list RPAREN"""
        args = p[4] if len(p) == 6 else None
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
        """pointer_argument : PERIOD expression"""
        p[0] = p[2]

    def p_symbol_list(self, p):
        """symbol_list : symbol_list COMMA identifier
                       | identifier"""
        if len(p) == 4:
            p[0] = mumpy.MUMPSArgumentList(p[1], p[3])
        else:
            p[0] = mumpy.MUMPSArgumentList(p[1])

    def p_argument_list(self, p):
        """argument_list : argument_list COMMA expression
                         | expression"""
        if len(p) == 4:
            p[0] = mumpy.MUMPSArgumentList(p[3], p[1])
        else:
            p[0] = mumpy.MUMPSArgumentList(p[1])

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
                      | not_expr
                      | and_expr
                      | not_and_expr
                      | or_expr
                      | not_or_expr
                      | gt_expr
                      | not_gt_expr
                      | lt_expr
                      | not_lt_expr
                      | equals_expr
                      | not_equals_expr
                      | contains_expr
                      | sorts_after_expr
                      | follows_expr
                      | string_concat
                      | numeric_op
                      | expression_parens
                      | identifier
                      | function_call"""
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
        p[0] = ~ (p[1] & p[3])

    def p_or_expr(self, p):
        """or_expr : expression OR expression"""
        p[0] = p[1] | p[3]

    def p_not_or_expr(self, p):
        """not_or_expr : expression NOT OR expression"""
        p[0] = p[1] | p[3]

    ###################
    # COMPARISON
    ###################
    def p_greater_than(self, p):
        """gt_expr : expression GREATER_THAN expression"""
        p[0] = p[1] > p[3]

    def p_not_greater_than(self, p):
        """not_gt_expr : expression NOT GREATER_THAN expression"""
        p[0] = ~ (p[1] > p[3])

    def p_less_than(self, p):
        """lt_expr : expression LESS_THAN expression"""
        p[0] = p[1] < p[3]

    def p_not_less_than(self, p):
        """not_lt_expr : expression NOT LESS_THAN expression"""
        p[0] = ~ (p[1] < p[3])

    def p_equal_to(self, p):
        """equals_expr : expression EQUALS expression"""
        p[0] = p[1] == p[3]

    def p_not_equal_to(self, p):
        """not_equals_expr : expression NOT EQUALS expression"""
        p[0] = p[1] != p[3]

    def p_follows(self, p):
        """follows_expr : expression FOLLOWS expression"""
        p[0] = mumpy.MUMPSExpression(p[1]).follows(p[3])

    def p_sorts_after(self, p):
        """sorts_after_expr : expression FOLLOWS FOLLOWS expression"""
        p[0] = mumpy.MUMPSExpression(p[1]).sorts_after(p[3])

    def p_contains(self, p):
        """contains_expr : expression CONTAINS expression"""
        p[0] = mumpy.MUMPSExpression(p[1]).contains(p[3])

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
        p[0] = p[1] / p[3]

    def p_idivision(self, p):
        """idivision : expression IDIVIDE expression"""
        p[0] = p[1] // p[3]

    def p_modulus(self, p):
        """modulus : expression MODULUS expression"""
        p[0] = p[1] % p[3]

    def p_exponent(self, p):
        """exponent : expression EXPONENT expression"""
        p[0] = p[1] ** p[3]
