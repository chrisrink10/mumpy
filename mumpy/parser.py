"""MUMPy Parser

The MUMPS parsing rules written as dictated by the PLY package.

Licensed under a BSD license. See LICENSE for more information.

Author: Christopher Rink"""
import logging
import ply.yacc as yacc
import mumpy
import mumpy.lang as lang


# noinspection PyMethodMayBeStatic
class MUMPSParser:
    def __init__(self, env, debug=False):
        # The environment is the execution stack
        self.env = env
        self.debug = debug

        # Boolean flag if the last line caused output
        self.output = False

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
                                       debug=debug,
                                       debuglog=self.debug_log,
                                       tabmodule='routab')

        # The REPL and XECUTE commands require slightly different
        # lexing and parsing rules, so we maintain two lexer and parsers
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
            self.output = False

            # Output the Lexer tokens
            if self.debug:
                self.repl['lex'].test(data)

            # Reset the Lexer and parse the data
            self.repl['lex'].reset()

            # Execute the parsed command(s)
            try:
                p = self.repl['parser'].parse(data,
                                              lexer=self.repl['lex'].lexer)
                p.execute()
            except (mumpy.MUMPSReturn, mumpy.MUMPSCommandEnd):
                pass
            except mumpy.MUMPSGotoLine as goto:
                fn = goto.func
                trampoline(self._parse_tag, fn.rou, fn.tag)
            except Exception as e:
                raise mumpy.MUMPSSyntaxError(e)
        except KeyError:
            print("The REPL was not set up correctly. Quitting...")

    def parse_file(self, f, tag=None, args=None):
        """Parse a MUMPSFile from the first tag or at a specified tag.

        This function is called from the interpreter if the user specifies
        to run a routine."""
        # Check that we got a compiled file
        if not isinstance(f, mumpy.MUMPSFile):
            raise TypeError("Please specify a valid MUMPS routine.")

        # Set the executing routine in the environment
        args = None if args is None else tuple(args)
        tag = tag if tag is not None else f.rou
        self.env.init_stack_frame(f, tag=tag, in_args=args)

        # If no tag is specified, start at the beginning
        lines = f.tag_body(tag)
        for line in lines:
            self.output = False

            if self.debug:
                self.rou['lex'].test(line)

            self.rou['lex'].reset()
            try:
                p = self.rou['parser'].parse(line, lexer=self.rou['lex'].lexer)
                p.execute()
            except mumpy.MUMPSReturn as ret:
                return ret.value()
            except mumpy.MUMPSCommandEnd:
                continue
            except mumpy.MUMPSGotoLine as goto:
                fn = goto.func
                return trampoline(self._parse_tag, fn.rou, fn.tag)

        # Reset the Lexer and Parser to the correct state
        self.rou['lex'].reset()

        # Return any resulting expression to the caller
        return None

    def _parse_tag(self, f, tag):
        """Parse a MUMPSFile starting at the specified tag.

        This function is intended to be called internally when the parser
        identifies a function or subroutine call.

        Note that the return from this function is always a lambda which
        requires no parameters. Callers are advised to use the provided
        trampoline function to avoid potentially enormous stack expansion
        since MUMPS code may potentially implement a GOTO loop, which
        would result in subsequent calls to this function (and, thus,
        an ever-increasing stack size)."""
        # Check that we got a compiled file
        if not isinstance(f, mumpy.MUMPSFile):
            raise TypeError("Please specify a valid MUMPS routine.")

        # Get the tag body
        lines = f.tag_body(tag)
        for line in lines:
            self.output = True

            if self.debug:
                self.rou['lex'].test(line)

            self.rou['lex'].reset()

            try:
                p = self.rou['parser'].parse(line, lexer=self.rou['lex'].lexer)
                p.execute()
            except mumpy.MUMPSReturn as ret:
                return lambda v=ret: v.value()
            except mumpy.MUMPSGotoLine as goto:
                return lambda cmd=self._parse_tag, fn=goto.func: cmd(fn.rou,
                                                                     fn.tag)
            except mumpy.MUMPSCommandEnd:
                continue

        # Reset the Lexer and Parser to the correct state
        self.rou['lex'].reset()

        # Return any resulting expression to the caller
        return lambda: None

    def _parse_xecute(self, args, env):
        """Parse an expression for an XECUTE command.

        This function is called internally when an XECUTE command is
        encountered."""
        self.output = False
        for expr in args:
            self.repl['lex'].reset()
            try:
                p = self.repl['parser'].parse(str(expr),
                                              lexer=self.repl['lex'].lexer)
                p.execute()
            except mumpy.MUMPSReturn as ret:
                return ret.value()
            except mumpy.MUMPSCommandEnd:
                continue
            except mumpy.MUMPSGotoLine as goto:
                fn = goto.func
                return trampoline(self._parse_tag, fn.rou, fn.tag)
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
                    | tag SPACE comment
                    | tag SPACE
                    | tag multiple_spaces
                    | tag"""
        if len(p) < 4:
            raise mumpy.MUMPSSyntaxError("Expected COMMENT or COMMAND "
                                         "after TAG.", err_type="NO LINE")
        p[0] = p[3]

    def p_command_line(self, p):
        """command_line : SPACE valid_input SPACE COMMENT
                        | SPACE valid_input multiple_spaces COMMENT
                        | SPACE valid_input multiple_spaces
                        | SPACE valid_input"""
        p[0] = p[2]

    def p_comment_line(self, p):
        """comment_line : SPACE comment"""
        p[0] = p[2]

    def p_multiple_spaces(self, p):
        """multiple_spaces : multiple_spaces SPACE
                           | SPACE"""
        pass

    def p_comment(self, p):
        """comment : COMMENT"""
        p[0] = mumpy.MUMPSLine(None)

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
                   | if_command
                   | open_command
                   | close_command
                   | use_command
                   | goto_command
                   | for_command
                   | view_command
                   | job_command"""
        p[0] = p[1]

    def p_command_no_arg(self, p):
        """command_no_arg : kill_all_command
                          | halt_command
                          | write_symbols
                          | if_command_no_args
                          | else_command
                          | for_unlimited"""
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

    def p_for_command(self, p):
        """for_command : for_limited_all
                       | for_limited_inc
                       | for_limited_start"""
        p[0] = p[1]

    def p_for_limited_all(self, p):
        """for_limited_all : FOR SPACE variable EQUALS expression COLON expression COLON expression COMMA argument_list
                           | FOR SPACE variable EQUALS expression COLON expression COLON expression"""
        args = {
            'var': p[3],
            'start': p[5],
            'inc': p[7],
            'end': p[9],
        }

        if len(p) == 12:
            args['others'] = p[11]

        p[0] = mumpy.MUMPSCommand(lang.for_start, args, self.env)

    def p_for_limited_inc(self, p):
        """for_limited_inc : FOR SPACE variable EQUALS expression COLON expression COMMA argument_list
                           | FOR SPACE variable EQUALS expression COLON expression"""
        args = {
            'var': p[3],
            'start': p[5],
            'inc': p[7],
        }

        if len(p) == 10:
            args['others'] = p[9]

        p[0] = mumpy.MUMPSCommand(lang.for_start, args, self.env)

    def p_for_limited_start(self, p):
        """for_limited_start : FOR SPACE variable EQUALS expression COMMA argument_list
                             | FOR SPACE variable EQUALS expression"""
        args = {
            'var': p[3],
            'start': p[5],
        }

        if len(p) == 8:
            args['others'] = p[7]

        p[0] = mumpy.MUMPSCommand(lang.for_start, args, self.env)

    def p_for_unlimited(self, p):
        """for_unlimited : FOR no_argument"""
        p[0] = mumpy.MUMPSCommand(lang.for_start, None, self.env)

    def p_goto_command(self, p):
        """goto_command : GOTO SPACE goto_call_list
                        | GOTO COLON expression SPACE goto_call_list"""
        if len(p) > 4:
            post = p[3]
            args = p[5]
        else:
            post = None
            args = p[3]

        p[0] = mumpy.MUMPSCommand(lang.goto_cmd, args, self.env, post=post)

    def p_job_command(self, p):
        """job_command : JOB SPACE job_argument_list
                       | JOB COLON expression SPACE job_argument_list"""
        if len(p) > 4:
            post = p[3]
            args = p[5]
        else:
            post = None
            args = p[3]

        p[0] = mumpy.MUMPSCommand(lang.job_cmd, args, self.env, post=post)

    def p_kill_command(self, p):
        """kill_command : KILL SPACE variable_list
                        | KILL COLON expression SPACE variable_list"""
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

    def p_open_command(self, p):
        """open_command : OPEN SPACE device_list
                        | OPEN COLON expression SPACE device_list"""
        if len(p) == 6:
            post = p[3]
            args = p[5]
        else:
            post = None
            args = p[3]

        p[0] = mumpy.MUMPSCommand(lang.open_dev, args, self.env, post=post)

    def p_close_command(self, p):
        """close_command : CLOSE SPACE device_list
                         | CLOSE COLON expression SPACE device_list"""
        if len(p) == 6:
            post = p[3]
            args = p[5]
        else:
            post = None
            args = p[3]

        p[0] = mumpy.MUMPSCommand(lang.close_dev, args, self.env, post=post)

    def p_use_command(self, p):
        """use_command : USE SPACE device
                       | USE COLON expression SPACE device"""
        if len(p) == 6:
            post = p[3]
            args = mumpy.MUMPSArgumentList(p[5])
        else:
            post = None
            args = mumpy.MUMPSArgumentList(p[3])

        p[0] = mumpy.MUMPSCommand(lang.use_dev, args, self.env, post=post)

    def p_read_command(self, p):
        """read_command : READ SPACE read_argument_list
                        | READ COLON expression SPACE read_argument_list"""
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
        """write_command : WRITE SPACE write_argument_list
                         | WRITE COLON expression SPACE write_argument_list"""
        self.output = True

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
        self.output = True
        p[0] = mumpy.MUMPSCommand(lang.write_symbols, None, self.env)

    def p_view_command(self, p):
        """view_command : VIEW SPACE view_argument_list
                        | VIEW COLON expression SPACE view_argument_list"""
        # Evaluate the post-conditional if it exists
        if len(p) > 4:
            post = p[3]
            args = p[5]
        else:
            post = None
            args = p[3]

        # Execute the code in each expression
        p[0] = mumpy.MUMPSCommand(lang.view_cmd, args, self.env, post=post)

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
        p[0] = mumpy.MUMPSCommand(self._parse_xecute, args, self.env, post=post)

    def p_quit(self, p):
        """quit_command : QUIT
                        | QUIT no_argument
                        | QUIT SPACE expression"""
        # Handle the post-conditional if it exists
        args = mumpy.MUMPSArgumentList(p[3]) if len(p) == 4 else None
        p[0] = mumpy.MUMPSCommand(lang.quit_cmd, args, self.env)

    def p_quit_post(self, p):
        """quit_post_command : QUIT COLON expression no_argument
                             | QUIT COLON expression
                             | QUIT COLON expression SPACE expression"""
        args = mumpy.MUMPSArgumentList(p[5]) if len(p) > 5 else None
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
    # ENTRYREF COMMAND MISC
    ###################
    def p_routine_global(self, p):
        """routine_global : CARET identifier"""
        p[0] = p[2]

    def p_goto_call_list(self, p):
        """goto_call_list : goto_call_list COMMA goto_call
                          | goto_call"""
        if len(p) == 4:
            p[0] = mumpy.MUMPSArgumentList(p[3], p[1])
        else:
            p[0] = mumpy.MUMPSArgumentList(p[1])

    def p_goto_call(self, p):
        """goto_call : goto_tag_routine
                     | goto_tag
                     | goto_routine
                     | goto_call COLON expression"""
        if len(p) == 4:
            p[1].post = p[3]
        p[0] = p[1]

    def p_goto_tag_routine(self, p):
        """goto_tag_routine : identifier routine_global"""
        p[0] = mumpy.MUMPSFuncSubCall(p[1], self.env, self, rou=p[2])

    def p_goto_tag(self, p):
        """goto_tag : identifier"""
        p[0] = mumpy.MUMPSFuncSubCall(p[1], self.env, self)

    def p_goto_routine(self, p):
        """goto_routine : routine_global"""
        p[0] = mumpy.MUMPSFuncSubCall(p[1], self.env, self, rou=p[1])

    def p_job_argument_list(self, p):
        """job_argument_list : job_argument_list COMMA job_argument
                             | job_argument"""
        if len(p) == 4:
            p[0] = mumpy.MUMPSArgumentList(p[3], p[1])
        else:
            p[0] = mumpy.MUMPSArgumentList(p[1])

    def p_job_argument(self, p):
        """job_argument : job_sub_call COLON LPAREN command_keyword_list RPAREN COLON expression
                        | job_sub_call COLON COLON expression
                        | job_sub_call COLON LPAREN command_keyword_list RPAREN
                        | job_sub_call"""
        l = len(p)
        if l == 8:
            p[0] = (p[1], _cmd_params_to_dict(p[4]), p[7])
        elif l == 5:
            p[0] = (p[1], None, p[4])
        elif l == 6:
            p[0] = (p[1], _cmd_params_to_dict(p[4]), None)
        else:
            p[0] = (p[1], None, None)

    def p_job_sub_call(self, p):
        """job_sub_call : subroutine_call_tag
                        | subroutine_call_no_tag
                        | subroutine_call_no_rou"""
        p[0] = p[1]

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

    ###################
    # COMMAND MISC
    ###################
    def p_symbol_list(self, p):
        """symbol_list : symbol_list COMMA identifier
                       | identifier"""
        if len(p) == 4:
            p[0] = mumpy.MUMPSArgumentList(p[3], p[1])
        else:
            p[0] = mumpy.MUMPSArgumentList(p[1])

    def p_variable_list(self, p):
        """variable_list : variable_list COMMA variable
                         | variable"""
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

    def p_view_argument_list(self, p):
        """view_argument_list : view_argument_list COMMA view_argument
                              | view_argument"""
        if len(p) == 4:
            p[0] = mumpy.MUMPSArgumentList(p[3], p[1])
        else:
            p[0] = mumpy.MUMPSArgumentList(p[1])

    def p_view_argument(self, p):
        """view_argument : view_argument COLON expression
                         | expression"""
        if len(p) == 4:
            p[0] = p[1] + p[3]
        else:
            p[0] = (p[1],)

    def p_assignment(self, p):
        """assignment : variable EQUALS expression"""
        p[0] = (p[1], p[3])

    def p_assignment_list(self, p):
        """assignment_list : assignment_list COMMA assignment
                           | assignment"""
        if len(p) == 4:
            p[0] = mumpy.MUMPSArgumentList(p[3], p[1])
        else:
            p[0] = mumpy.MUMPSArgumentList(p[1])

    def p_command_keyword_list(self, p):
        """command_keyword_list : command_keyword_list COMMA keyword_value
                                | keyword_value"""
        if len(p) == 4:
            p[0] = p[1].append(p[3])
        else:
            p[0] = [p[1]]

    def p_keyword_value(self, p):
        """keyword_value : string_contents EQUALS expression"""
        p[0] = (p[1], p[3])

    def p_no_arguments(self, p):
        """no_argument : SPACE SPACE"""
        p[0] = p[1]

    ###################
    # I/O COMMAND MISC
    ###################
    def p_device_list(self, p):
        """device_list : device_list COMMA device
                       | device"""
        l = len(p)
        if l == 4:
            p[0] = mumpy.MUMPSArgumentList(p[3], p[1])
        else:
            p[0] = mumpy.MUMPSArgumentList(p[1])

    def p_device(self, p):
        """device : expression
                  | expression COLON LPAREN command_keyword_list RPAREN"""
        l = len(p)
        if l == 6:
            p[0] = (p[1], _cmd_params_to_dict(p[4]))
        else:
            p[0] = (p[1], None)

    def p_read_argument_list(self, p):
        """read_argument_list : read_argument_list COMMA read_argument
                              | read_argument"""
        if len(p) == 4:
            p[0] = mumpy.MUMPSArgumentList(p[3], p[1])
        else:
            p[0] = mumpy.MUMPSArgumentList(p[1])

    def p_read_argument(self, p):
        """read_argument : string_contents
                         | io_format
                         | read_variable
                         | read_timeout"""
        p[0] = mumpy.MUMPSExpression(p[1])

    def p_read_timeout(self, p):
        """read_timeout : read_variable COLON expression"""
        p[0] = p[1].set_timeout(int(p[3].as_number()))

    def p_read_variable(self, p):
        """read_variable : read_one_char
                         | read_n_chars
                         | read_line"""
        p[0] = p[1]

    def p_read_one_char(self, p):
        """read_one_char : TIMES variable"""
        p[0] = p[2].set_max(1)

    def p_read_n_chars(self, p):
        """read_n_chars : variable MODULUS expression"""
        p[0] = p[1].set_max(int(p[3].as_number()))

    def p_read_line(self, p):
        """read_line : variable"""
        p[0] = p[1]

    def p_write_argument_list(self, p):
        """write_argument_list : write_argument_list COMMA write_argument
                               | write_argument"""
        if len(p) == 4:
            p[0] = mumpy.MUMPSArgumentList(p[3], p[1])
        else:
            p[0] = mumpy.MUMPSArgumentList(p[1])

    def p_write_argument(self, p):
        """write_argument : expression
                          | io_format
                          | write_char"""
        p[0] = p[1]

    def p_write_char(self, p):
        """write_char : TIMES expression"""
        p[0] = lang.intrinsic_char((p[2],))

    def p_io_format(self, p):
        """io_format : io_format format_newline
                     | io_format format_clear_screen
                     | io_format format_column
                     | format_newline
                     | format_clear_screen
                     | format_column"""
        p[0] = p[1]

    def p_format_newline(self, p):
        """format_newline : format_newline OR
                          | OR"""
        if len(p) == 3:
            p[0] = "{}{}".format(p[1], "\n")
        else:
            p[0] = "\n"

    def p_format_clear_screen(self, p):
        """format_clear_screen : format_clear_screen MODULUS
                               | MODULUS"""
        # ANSI terminal escape sequence
        clr = chr(27) + "[2J"

        if len(p) == 3:
            p[0] = "{}{}".format(p[1], clr)
        else:
            p[0] = clr

    def p_format_column(self, p):
        """format_column : format_column PATTERN expression
                         | PATTERN expression"""
        off = int(p[2].as_number()) if len(p) == 3 else int(p[3].as_number())
        x_pos = lambda env=self.env: self.env.device_x()

        p[0] = mumpy.MUMPSExpression(
            lambda c=off, x=x_pos: " " * (c-x()) if c > x() else ""
        )

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
                      | local_var
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
        pass

    def p_variable(self, p):
        """variable : local_var
                    | global_var"""
        p[0] = p[1]

    def p_local_var(self, p):
        """local_var : identifier LPAREN argument_list RPAREN
                     | identifier"""
        if len(p) == 5:
            p[0] = mumpy.MUMPSIdentifier(p[1], self.env, subscripts=p[3])
        else:
            p[0] = mumpy.MUMPSIdentifier(p[1], self.env)

    def p_global_var(self, p):
        """global_var : routine_global LPAREN argument_list RPAREN
                      | routine_global"""
        #if len(p) == 5:
        #    p[0] = mumpy.MUMPSIdentifier(p[0], self.env, subscripts=p[3])
        #else:
        #    p[0] = mumpy.MUMPSIdentifier(p[1], self.env)
        pass

    ###################
    # INTRINSICS
    ###################
    def p_intrinsic_func(self, p):
        """intrinsic_func : ascii_func
                          | char_func
                          | extract_func
                          | data_func
                          | find_func
                          | justify_func
                          | length_func
                          | name_func
                          | order_func
                          | piece_func
                          | random_func
                          | reverse_func
                          | select_func
                          | translate_func
                          | intrinsic_not_exist"""
        p[0] = p[1]

    def p_intrinsic_dne(self, p):
        """intrinsic_not_exist : FN_DOES_NOT_EXIST error"""
        raise mumpy.MUMPSSyntaxError("Function does not exist.",
                                     err_type="FN DOES NOT EXIST")

    def p_justify_token(self, p):
        """justify_token : JUSTIFY_DOLLARJ
                         | JUSTIFY"""
        p[0] = p[1]

    def p_piece_token(self, p):
        """piece_token : PIECE_PRINCIPAL
                       | PIECE"""
        p[0] = p[1]

    def p_ascii(self, p):
        """ascii_func : ASCII LPAREN expression COMMA expression RPAREN
                      | ASCII LPAREN expression RPAREN"""
        which = p[5] if len(p) == 7 else None
        p[0] = lang.intrinsic_ascii(p[3], which=which)

    def p_char(self, p):
        """char_func : CHAR LPAREN argument_list RPAREN"""
        p[0] = lang.intrinsic_char(p[3])

    def p_data(self, p):
        """data_func : DATA LPAREN variable RPAREN"""
        p[0] = lang.intrinsic_data(p[3], self.env)

    def p_extract(self, p):
        """extract_func : EXTRACT LPAREN expression COMMA expression COMMA expression RPAREN
                        | EXTRACT LPAREN expression COMMA expression RPAREN
                        | EXTRACT LPAREN expression RPAREN"""
        l = len(p)
        low = p[5] if l >= 7 else None
        high = p[7] if l == 9 else None
        p[0] = lang.intrinsic_extract(p[3], low=low, high=high)

    def p_find(self, p):
        """find_func : FIND LPAREN expression COMMA expression COMMA expression RPAREN
                     | FIND LPAREN expression COMMA expression RPAREN"""
        start = p[7] if len(p) == 9 else None
        p[0] = lang.intrinsic_find(p[3], p[5], start=start)

    def p_justify(self, p):
        """justify_func : justify_token LPAREN expression COMMA expression COMMA expression RPAREN
                        | justify_token LPAREN expression COMMA expression RPAREN"""
        ndec = p[7] if len(p) == 9 else None
        p[0] = lang.intrinsic_justify(p[3], int(p[5].as_number()), ndec)

    def p_length(self, p):
        """length_func : LENGTH LPAREN expression COMMA expression RPAREN
                       | LENGTH LPAREN expression RPAREN"""
        char = p[5] if len(p) == 7 else None
        p[0] = lang.intrinsic_length(p[3], char=char)

    def p_name(self, p):
        """name_func : NAME LPAREN identifier RPAREN"""
        p[0] = mumpy.MUMPSExpression(
            lambda v=p[3]: str(v)
        )

    def p_order(self, p):
        """order_func : ORDER LPAREN variable COMMA expression RPAREN
                      | ORDER LPAREN variable RPAREN"""
        rev = p[5] if len(p) == 7 else None
        p[0] = lang.intrinsic_order(p[3], self.env, rev=rev)

    def p_piece(self, p):
        """piece_func : piece_token LPAREN expression COMMA expression COMMA expression RPAREN
                      | piece_token LPAREN expression COMMA expression RPAREN"""
        pnum = p[7] if len(p) == 9 else None
        p[0] = lang.intrinsic_piece(p[3], p[5], num=pnum)

    def p_random(self, p):
        """random_func : RANDOM LPAREN expression RPAREN"""
        p[0] = lang.intrinsic_random(p[3])

    def p_reverse(self, p):
        """reverse_func : REVERSE LPAREN expression RPAREN"""
        p[0] = mumpy.MUMPSExpression(
            lambda v=p[3]: str(v)[::-1]
        )

    def p_select(self, p):
        """select_func : SELECT LPAREN sel_argument_list RPAREN"""
        p[0] = lang.intrinsic_select(p[3])

    def p_translate(self, p):
        """translate_func : TRANSLATE LPAREN expression COMMA expression COMMA expression RPAREN
                          | TRANSLATE LPAREN expression COMMA expression RPAREN"""
        newexpr = p[7] if len(p) == 9 else None
        p[0] = lang.intrinsic_translate(p[3], p[5], newexpr=newexpr)

    ###################
    # SPECIAL VARIABLES
    ###################
    def p_special_var(self, p):
        """special_var : horolog_var
                       | io_var
                       | job_var
                       | principal_var
                       | test_var
                       | x_var
                       | y_var"""
        p[0] = p[1]

    def p_horolog(self, p):
        """horolog_var : HOROLOG"""
        p[0] = lang.horolog()

    def p_io_var(self, p):
        """io_var : DOLLARIO"""
        p[0] = self.env.current_device()

    def p_job_var(self, p):
        """job_var : DOLLARJ
                   | JUSTIFY_DOLLARJ"""
        p[0] = lang.current_job()

    def p_principal_var(self, p):
        """principal_var : PIECE_PRINCIPAL
                         | PRINCIPAL"""
        p[0] = self.env.default_device()

    def p_test_var(self, p):
        """test_var : TEST_TEXT
                    | TEST"""
        p[0] = mumpy.MUMPSExpression(self.env.get("$T"))

    def p_x_var(self, p):
        """x_var : DOLLARX"""
        p[0] = mumpy.MUMPSExpression(lambda env=self.env: env.device_x())

    def p_y_var(self, p):
        """y_var : DOLLARY"""
        p[0] = mumpy.MUMPSExpression(lambda env=self.env: env.device_y())

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
        p[0] = ~ (p[1] | p[4])

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


def _cmd_params_to_dict(params):
    """Convert a list of device parameters to a dictionary."""
    return {t[0]: t[1] for t in params}


def trampoline(f, *args, **kwargs):
    """Function to trampoline any functions which may have a tendency to
    otherwise cause enormous amounts of stack inflation or expansion.

    Given a function `f` with arguments `args` and `kwargs` which returns
    another function, execute the return until it is no longer callable."""
    return _trampoline_private(f(*args, **kwargs))


def _trampoline_private(func):
    """Private helper for the trampoline function above."""
    while callable(func):
        func = func()
    return func
