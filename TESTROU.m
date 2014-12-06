 ;**************************
 ;* ANSI M Level Unit Test
 ;**************************
 ;
 ; Licensed under a BSD license. See LICENSE for more information.
 ;
 ; Author: Chris Rink
 ;
TESTROU ;
 n fails
 ;
 ; Nice welcome message before the beat drops
 w "---------------------------------------------------------"
 w !,"Welcome to MUMPy!"
 w !,"This script will run the MUMPS-side Unit Tests for MUMPy."
 w !,"---------------------------------------------------------"
 w !,"Hold on..."
 ;
 ; Check that EQUALS comparisons and basic logic are working
 s fails=fails+$$TestEquals()
 s fails=fails+$$TestLogic()
 ;
 ; Likewise, we will need Regular and Pointer function arguments to work
 s fails=fails+$$TestRegularArgs()
 s fails=fails+$$TestPointerArgs()
 ;
 ; Perform tests
 s fails=fails+$$TestExpressionReduction()
 s fails=fails+$$TestArithmetic()
 s fails=fails+$$TestIfElse()
 s fails=fails+$$TestStringOp()
 s fails=fails+$$TestIntrinsic()
 s fails=fails+$$TestAssumedVars()
 s fails=fails+$$TestGoto()
 s fails=fails+$$TestGotoLoop()
 s fails=fails+$$TestForLoops()
 s fails=fails+$$TestSockets()
 ;
 ; Report the results
 w !,"---------------------------------------------------------"
 w:(fails=1) !,"There was 1 test which failed."
 w:(fails>1) !,"There were "_fails_" tests which failed."
 w:(fails=0) !,"Success!"
 w !,"---------------------------------------------------------",!
 q
 ;
 ;**************************
 ;* Test utility functions
 ;**************************
 ; Input the result from the test and the expected value.
 ; Pass in the failure count as a pointer and an error message.
 ; If the test fails, the failure count will increment and the
 ; message will be output to the current device.
EvalTest(res,exp,fails,msg) ;
 q:(res=exp)
 s fails=fails+1
 w !,msg_" >>> res="_res_" exp="_exp
 q
 ;
 ; To use the Xecute test, instead of passing in the evaluated expression,
 ; pass in the expression as a string and  it will be evaluated within.
 ; This helps since this function can then report the failing expression.
 ;
 ; Ex. d XecuteTest("'1",0,.fail,msg)
XecuteTest(expr,res,fail,msg) ;
 n val,code
 s code="s val="_expr
 x code
 q:(val=res)
 s fails=fails+1
 s msg=msg_$C(10)_" - "_expr_" gave "_val_", expected "_res
 q
 ;
 ; Report the results of a set of tests
ReportResults(fail,msg) ;
 i +fail=0 w ?48,"[SUCCESS]"
 e  w ?51,"[FAIL]" w:(msg'="") !,msg
 q
 ;
 ; If you can't use one of the EvalTest or XecuteTest methods above
 ; to add to your output error message, use this so it can be output
 ; properly using ReportResults.
AddMessage(msg,add) ;
 s msg=msg_$C(10)_add
 q
 ;
 ;**************************
 ;* Equals Tests
 ;**************************
 ; The rest of the tests are invalid if equals doesn't work since we
 ; use the equals and not equals operators to check if a test succeeded.
TestEquals() ;
 n val,msg,fail
 w !,"Testing equals operators..."
 ;
 ; String equals and not equals
 d XecuteTest("""A""=""B""",0,.fail,.msg)
 d XecuteTest("""A""'=""B""",1,.fail,.msg)
 d XecuteTest("""C""=""C""",1,.fail,.msg)
 d XecuteTest("""C""'=""C""",0,.fail,.msg)
 d XecuteTest("""a""'=""A""",1,.fail,.msg)
 ;
 ; Numeric equals
 d XecuteTest("(1=1)",1,.fail,.msg)
 d XecuteTest("'(1=1)",0,.fail,.msg)
 d XecuteTest("(1'=1)",0,.fail,.msg)
 d XecuteTest("(1=2)",0,.fail,.msg)
 d XecuteTest("(1=""1"")",1,.fail,.msg)
 d XecuteTest("(1=01)",1,.fail,.msg)
 d XecuteTest("(1=+""01"")",1,.fail,.msg)
 ;
 ; Cross-type equals
 d XecuteTest("(1=""01"")",0,.fail,.msg)
 d XecuteTest("(""0.100""=.1)",0,.fail,.msg)
 d XecuteTest("(""0.1""=.100)",0,.fail,.msg)
 d XecuteTest("(.1=""0.1"")",0,.fail,.msg)
 d XecuteTest("(.1=+""0.1"")",0,.fail,.msg)
 ;
 d ReportResults(fail,msg)
 q +fail
 ;
 ;**************************
 ;* Logic Tests
 ;**************************
TestLogic() ;
 n val,fail,msg
 w !,"Testing logic operators..."
 ;
 ; Testing logical NOT...
 d XecuteTest("'1",0,.fail,.msg)
 d XecuteTest("'0",1,.fail,.msg)
 ;
 ; Testing logical AND...
 d XecuteTest("(1&1)",1,.fail,.msg)
 d XecuteTest("(1&0)",0,.fail,.msg)
 d XecuteTest("(0&0)",0,.fail,.msg)
 ;
 ; Testing logical NOT AND...
 d XecuteTest("(1'&1)",0,.fail,.msg)
 d XecuteTest("(1'&0)",1,.fail,.msg)
 d XecuteTest("(0'&0)",1,.fail,.msg)
 ;
 ; Testing logical OR...
 d XecuteTest("(1!1)",1,.fail,.msg)
 d XecuteTest("(1!0)",1,.fail,.msg)
 d XecuteTest("(0!0)",0,.fail,.msg)
 ;
 ; Testing logical NOT OR...
 d XecuteTest("(1'!1)",0,.fail,.msg)
 d XecuteTest("(1'!0)",0,.fail,.msg)
 d XecuteTest("(0'!0)",1,.fail,.msg)
 ;
 d ReportResults(fail,msg)
 q +fail
 ;
 ;**************************
 ;* Regular Argument Tests
 ;**************************
 ; We need to make sure that sub-nodes for values do not get passed in for
 ; non-pointer types of arguments. Since the correct evaluation of these
 ; tests depends on this, we need to check this early.
TestRegularArgs() ;
 n val,name,fail
 w !,"Testing regular arguments..."
 ;
 s val=1
 s name="Chris"
 s name("last")="Rink"
 s fail=$$testRegArgs(val,name)
 ;
 d ReportResults(fail)
 q +fail
 ;
testRegArgs(val,name) ;
 q:(val'=1) 1
 q:(name'="Chris") 1
 q:(name("last")'="") 1
 q 0
 ;
 ;**************************
 ;* Pointer Argument Tests
 ;**************************
 ; Pointer arguments are also used for the test evaluator, so we'll need
 ; to make sure that these work.
TestPointerArgs() ;
 n val,name,fail
 w !,"Testing pointer arguments..."
 ;
 ; Test that calling functions receive modifications to pointer arguments
 s val=1
 d testPtrArgs(.val)
 i val'=6 s fail=fail+1
 ;
 ; Test that pointer arguments also pass the subnode access
 s name="Chris"
 s name("Last")="Rink"
 s name("Last","Suffix")="Jr"
 s name("MI")="L"
 i $$testPtrArgs4(.name) s fail=fail+1
 ;
 d ReportResults(fail)
 q +fail
 ;
testPtrArgs(arg) ;
 s arg=arg+1
 d testPtrArgs2(.arg)
 q
testPtrArgs2(arg) ;
 s arg=arg+1
 d testPtrArgs3(.arg)
 q
testPtrArgs3(param) ;
 s param=param*2
 q
testPtrArgs4(arg) ;
 q:(arg'="Chris") 1
 q:(arg("Last")'="Rink") 1
 q:(arg("Last","Suffix")'="Jr") 1
 q:(arg("MI")'="L") 1
 q 0
 ;
 ;**************************
 ;* Expression Tests
 ;**************************
TestArithmetic() ;
 n res,exp,fail,msg
 w !,"Testing arithmetic..."
 ;
 ; Test unary PLUS operators
 s msg=" - A unary PLUS operator failed"
 d EvalTest(+1,1,.fail,msg)
 d EvalTest(+"27 apples",27,.fail,msg)
 d EvalTest(+++---+--+3,-3,.fail,msg)
 d EvalTest(+"+++---+--+3",-3,.fail,msg)
 d EvalTest(+"+++---+--+3.5",-3.5,.fail,msg)
 d EvalTest(+"+++---+--+3.5.5",-3.5,.fail,msg)
 ;
 ; Test unary MINUS operators
 s msg=" - A unary MINUS operator failed"
 d EvalTest(-1,-1,.fail,msg)
 d EvalTest(-"27 apples",-27,.fail,msg)
 d EvalTest(+++--+--+3,3,.fail,msg)
 d EvalTest(-"+++---+--+3",3,.fail,msg)
 d EvalTest(-"+++---+--+3.5",3.5,.fail,msg)
 d EvalTest(-"+++---+--+3.5.5",3.5,.fail,msg)
 ;
 ; Test addition
 s msg=" - An addition operation failed"
 d EvalTest(1+4,5,.fail,msg)
 d EvalTest((-3)+(-4),-7,.fail,msg)
 d EvalTest("2 apples"+"3 oranges",5,.fail,msg)
 ;
 ; Test subtraction
 s msg=" - A subtraction operation failed"
 d EvalTest(1-4,-3,.fail,msg)
 d EvalTest(7-2,5,.fail,msg)
 d EvalTest("5 apples"-"3 oranges",2,.fail,msg)
 ;
 ; Test multiplication
 s msg=" - Multiplication failed."
 d EvalTest(3*3,9,.fail,msg)
 d EvalTest(7.5*2,15,.fail,msg)
 d EvalTest(-2.5*2.5,-6.25,.fail,msg)
 d EvalTest(2.5*-2.5,-6.25,.fail,msg)
 d EvalTest((-3)*(-4),12,.fail,msg)
 d EvalTest("5 apples"*"3 oranges",15,.fail,msg)
 ;
 ; Test division
 s msg=" - Division failed."
 d EvalTest(9/3,3,.fail,msg)
 d EvalTest(5/2,2.5,.fail,msg)
 d EvalTest(5/-2,-2.5,.fail,msg)
 d EvalTest(-5/2,-2.5,.fail,msg)
 d EvalTest(1/128,0.0078125,.fail,msg)                  ; #TODO: handle decimal args with no leading zero
 d EvalTest("15 apples"/"3 oranges",5,.fail,msg)
 ;
 ; Test integer division
 s msg=" - Integer division failed."
 d EvalTest(9\3,3,.fail,msg)
 d EvalTest(5\2,2,.fail,msg)
 d EvalTest(-5\2,-2,.fail,msg)
 d EvalTest(1\2,0,.fail,msg)
 d EvalTest(-1\2,0,.fail,msg)
 d EvalTest(1\3*3,0,.fail,msg)
 ;
 ; Test modulo operator
 s msg=" - Modulo operation failed."
 d EvalTest(-9#5,1,.fail,msg)
 d EvalTest(-3#5,2,.fail,msg)
 d EvalTest(-6#-5,-1,.fail,msg)
 d EvalTest(4#-5,-1,.fail,msg)
 ;
 ; Test power operator
 s msg=" - Exponentiation failed."
 d EvalTest(32**.2,2,.fail,msg)
 d EvalTest(2**5,32,.fail,msg)
 d EvalTest(16**.25,2,.fail,msg)
 d EvalTest(2**3**4,4096,.fail,msg)
 d EvalTest(4**(-2),0.0625,.fail,msg)
 d EvalTest(27**(1/3),3,.fail,msg)
 ;
 d ReportResults(fail)
 q +fail
 ;
 ;**************************
 ;* String Operator Tests
 ;**************************
TestStringOp() ;
 n str,val,fail
 w !,"Testing string operations..."
 ;
 ; Test contains operator
 s msg=" - String contains failed."
 s str="flu-patient"
 d EvalTest(str["pat",1,.fail,msg)
 d EvalTest(str["lu",1,.fail,msg)
 d EvalTest(str["flute",0,.fail,msg)
 d EvalTest(str["pantie",0,.fail,msg)
 ;
 ; Test follows operator
 s msg=" - String follows failed."
 d EvalTest("ABD"]"ABC",1,.fail,msg)
 d EvalTest("ABCD"]"ABC",1,.fail,msg)
 d EvalTest(10]1,1,.fail,msg)
 d EvalTest(2]1,1,.fail,msg)
 d EvalTest(10]2,0,.fail,msg)
 ;
 ; Test sorts-after operator
 s msg=" - String sorts-after failed."
 d EvalTest("ABD"]]"ABC",1,.fail,msg)
 d EvalTest("ABCD"]]"ABC",1,.fail,msg)
 d EvalTest("ABCD"]]1,1,.fail,msg)
 d EvalTest("ABC"]]2,1,.fail,msg)
 d EvalTest(10]]1,1,.fail,msg)
 d EvalTest(2]]1,1,.fail,msg)
 d EvalTest(10]]2,1,.fail,msg)
 ;
 ; Test concatenation operator
 s msg=" - Concatenation failed."
 d EvalTest(9_3,"93",.fail,msg)
 d EvalTest("First"_"Second","FirstSecond",.fail,msg)
 d EvalTest("First"_" Second","First Second",.fail,msg)
 d EvalTest("First"_" "_"Second","First Second",.fail,msg)
 ;
 d ReportResults(fail)
 q +fail
 ;
 ;**************************
 ;* Intrinsic Tests
 ;**************************
TestIntrinsic() ;
 n val,msg,fail,var
 w !,"Testing intrinsic functions..."
 ;
 ; Test $ASCII function
 s msg=" - $ASCII intrinsic failed."
 d EvalTest($A(""),-1,.fail,msg)
 d EvalTest($A("+"),43,.fail,msg)
 d EvalTest($A("R"),82,.fail,msg)
 d EvalTest($A("„"),8222,.fail,msg)
 ;
 ; Test $CHAR function
 s msg=" - $CHAR intrinsic failed."
 d EvalTest($C(67),"C",.fail,msg)
 d EvalTest($C(104),"h",.fail,msg)
 d EvalTest($C(7912),"Ứ",.fail,msg)
 d EvalTest($C(67,104,114,105,115),"Chris",.fail,msg)
 ;
 ; Test $DATA function
 s msg=" - $DATA intrinsic failed."
 s var=1,var("name","first")="Tyrion",var("name","last")="Lannister"
 d EvalTest($D(notdef),0,.fail,msg)
 d EvalTest($D(var("notdef")),0,.fail,msg)
 d EvalTest($D(var("name","first")),1,.fail,msg)
 d EvalTest($D(var("name")),10,.fail,msg)
 d EvalTest($D(var),11,.fail,msg)
 ;
 ; Test $EXTRACT function
 s msg=" - $EXTRACT intrinsic failed."
 d EvalTest($E(""),"",.fail,msg)
 d EvalTest($E("Chris"),"C",.fail,msg)
 d EvalTest($E("Chris",-1),"",.fail,msg)
 d EvalTest($E("Chris",43),"",.fail,msg)
 d EvalTest($E("Chris",1),"C",.fail,msg)
 d EvalTest($E("Chris",3),"r",.fail,msg)
 d EvalTest($E("Chris",5,3),"",.fail,msg)
 d EvalTest($E("Chris",3,3),"r",.fail,msg)
 d EvalTest($E("Chris",2,4),"hri",.fail,msg)
 d EvalTest($E("Chris",2,10),"hris",.fail,msg)
 ;
 ; Test $FIND function
 s msg=" - $FIND intrinsic failed."
 d EvalTest($F("MUMPy is fun",""),1,.fail,msg)
 d EvalTest($F("MUMPy is fun","is"),9,.fail,msg)
 d EvalTest($F("MUMPy is so fun. It is very powerful.","is",9),23,.fail,msg)
 d EvalTest($F("MUMPy is so fun. It is very powerful.","is",24),0,.fail,msg)
 ;
 ; Test $JUSTIFY function
 s msg=" - $JUSTIFY intrinsic failed."
 d EvalTest($J("MUMPy",3),"MUMPy",.fail,msg)
 d EvalTest($J("MUMPy",8),"   MUMPy",.fail,msg)
 d EvalTest($J(1/128,0,10),"0.0078125000",.fail,msg)          ; #TODO: Remove the leading 0 from the return
 d EvalTest($J(1/128,15,10),"   0.0078125000",.fail,msg)
 ;
 ; Test $LENGTH function
 s MSG=" - $LENGTH intrinsic failed."
 d EvalTest($L(""),0,.fail,msg)
 d EvalTest($L("I like MUMPy and Python."),24,.fail,msg)
 d EvalTest($L("",","),0,.fail,msg)
 d EvalTest($L("String 1, String 2, and String 3",","),2,.fail,msg)
 d EvalTest($L("Test string",";"),0,.fail,msg)
 ;
 ; #TODO: Test $NAME function
 ;
 ; Test $ORDER function
 s msg=" - $ORDER intrinsic failed."
 s var("age")=12
 s var("height")=58
 s var("name")="Chris"
 d EvalTest($O(var("")),"age",.fail,msg)
 d EvalTest($O(var("age")),"height",.fail,msg)
 d EvalTest($O(var("height")),"name",.fail,msg)
 d EvalTest($O(var("height"),-1),"age",.fail,msg)
 d EvalTest($O(var(""),-1),"name",.fail,msg)
 d EvalTest($O(var("age"),-1),"",.fail,msg)
 k var
 ;
 ; Test $PIECE function
 s msg=" - $PIECE instrinsic failed."
 d EvalTest($P("",","),"",.fail,msg)
 d EvalTest($P("Arya",""),"",.fail,msg)
 d EvalTest($P("Arya",","),"Arya",.fail,msg)
 d EvalTest($P("Arya,Jon,Sansa",","),"Arya",.fail,msg)
 d EvalTest($P("Arya,Jon,Sansa",",",2),"Jon",.fail,msg)
 d EvalTest($P("Arya,Jon,Sansa",",",4),"",.fail,msg)
 ; #TODO: Finish the $PIECE function tests
 ;
 ; Test $REVERSE function
 s msg=" - $REVERSE instrinsic failed."
 d EvalTest($RE(""),"",.fail,msg)
 d EvalTest($RE("Chris"),"sirhC",.fail,msg)
 ;
 ; Test $SELECT function
 s msg=" - $SELECT instrinsic failed."
 d EvalTest($S(1:"first",1:"second"),"first",.fail,msg)
 d EvalTest($S("":"first",1:"second"),"second",.fail,msg)
 d EvalTest($S("":"first",0:"second",1:"third"),"third",.fail,msg)
 d EvalTest($S("":"first",0:"second",3:"third",1:"fourth"),"third",.fail,msg)
 ;
 ; Test $TRANSLATE function
 s msg=" - $TRANSLATE intrinsic failed."
 d EvalTest($TR("",""),"",.fail,msg)
 d EvalTest($TR("Chris",""),"Chris",.fail,msg)
 d EvalTest($TR("Chris","ri"),"Chs",.fail,msg)
 d EvalTest($TR("Chris","ri","el"),"Chels",.fail,msg)
 d EvalTest($TR("Chris","ri","e"),"Ches",.fail,msg)
 d EvalTest($TR("Chris","","el"),"Chris",.fail,msg)
 ;
 d ReportResults(fail)
 q +fail
 ;
 ;**************************
 ;* IF/ELSE Tests
 ;**************************
TestIfElse() ;
 n val,fail,msg
 w !,"Testing IF/ELSE..."
 ;
 ; Verify that ELSE doesn't fire initially ($T is true on env start)
 e  s fail=fail+1 d AddMessage(.msg," - $T not properly set.")
 ;
 ; Verify that ELSE doesn't fire if $T is true
 i 1 s val=0
 e  s fail=fail+1 d AddMessage(.msg," - ELSE failed.")
 ;
 ; Verify that an argumentless IF will succeed since $T is still true
 i  s val=1
 i val'=1 s fail=fail+1 d AddMessage(.msg," - Argumentless IF failed.")
 ;
 ; Verify that ELSE does fire if $T is false
 i 0 s fail=fail+1 d AddMessage(.msg," - IF failed.")
 e  s val=0
 ;
 d ReportResults(fail,msg)
 q +fail
 ;
 ;**************************
 ;* Expression Reduction Tests
 ;**************************
 ; In M, programmers can write multiple command arguments on one line.
 ; MUMPy reads in the entire set of arguments before executing any single
 ; set of commands. As a result, we had an issue where the value of
 ; expressions containing variables was evaluated prior to that variable
 ; being set in an earlier `SET` argument, thus resulting in the wrong
 ; value for the subsequent evaluation. We began using deferred/lazy
 ; evaluation to remedy that problem, but now we're going to test for it :)
 ;
 ; Ex. `set x=1,x=x+1` should result with `x` equalling `2`, but before
 ;     we moved to deferred evaluation, a `write x` would show `1`.
TestExpressionReduction() ;
 n val,fail
 w !,"Testing deferred expression evaluation..."
 ;
 ; Test with basic addition
 s val=1,val=val+1,val=val+1,val=val+1
 s:(val'=4) fail=fail+1
 ;
 ; Test with some string operations
 s val="Chris",val=val_" Rink",val=val_" Jr."
 s:(val'="Chris Rink Jr.") fail=fail+1
 ;
 ; Test with intrinsic function
 s val=$P("str1,str2",","),val=val_","_$P("str1,str2",",",2),val=val_",str3"
 s:(val'="str1,str2,str3") fail=fail+1
 ;
 ; Test with an extrinsic
 s val=10,val=val-$$testExpr(3),val=val*$$testExpr(3)
 s:(val'=21) fail=fail+1
 ;
 d ReportResults(fail)
 q +fail
testExpr(arg) ;
 q arg
 ;
 ;**************************
 ;* Assumed Variable Test
 ;**************************
 ; M allows programmers to assume variables from lower stack levels and
 ; make modifications to those values which persist once the higher stack
 ; frame is popped.
TestAssumedVars() ;
 n val,fail,%
 w !,"Testing assumed variables..."
 ;
 s val=1
 d testAssumedSub()
 d testAssumedSub()
 s %=$$testAssumedFunc()
 s %=$$testAssumedFunc()
 ;
 s:(val'=5) fail=fail+1
 d ReportResults(fail)
 q +fail
testAssumedSub() ;
 s val=val+1
 q
testAssumedFunc() ;
 s val=val+1
 q 1
 ;
 ;**************************
 ;* GOTO Test
 ;**************************
TestGoto() ;
 w !,"Testing GOTO functionality..."
 g testGoto
 d ReportResults(1)
 q 1
testGoto ;
 d ReportResults(0)
 q 0
 ;
 ;**************************
 ;* GOTO Looping Test
 ;**************************
TestGotoLoop() ;
 n val,fail
 w !,"Testing GOTO loops..."
 ;
 s val=1
tglStart ;
 s val=val+1
 g:(val<100) tglStart
 ;
 s:(val'=100) fail=fail+1
 d ReportResults(fail)
 q +fail
 ;
 ;**************************
 ;* FOR Looping Test
 ;**************************
TestForLoops() ;
 n val,var,nxt,fail,ln
 w !,"Testing FOR loops..."
 ;
 ; For loop with only a list of values
 s val=""
 f ln="Once ","upon ","a ","time!" s val=val_ln
 s msg=" - Concatenating a string with values"
 d EvalTest(val,"Once upon a time!",.fail,msg)
 ;
 ; For loop with an increment
 s val=0
 f ln=1:1 q:(ln>10)  s val=val+ln
 s msg=" - Adding values together with increment"
 d EvalTest(val,55,.fail,msg)
 ;
 ; For loop with a decrement
 s val=55
 f ln=10:-1 q:(ln<1)  s val=val-ln
 s msg=" - Subtracting from value with decrement"
 d EvalTest(val,0,.fail,msg)
 ;
 ; Fully bounded for loop
 s val=0
 f ln=1:1:10 s val=val+ln
 s msg=" - Adding up a value in a fully-bounded loop"
 d EvalTest(val,55,.fail,msg)
 ;
 ; Fully bounded for loop with decrement
 s val=0
 f ln=10:-1:1 s val=val+ln
 s msg=" - Adding up a value in a fully-bounded loop with decrement"
 d EvalTest(val,55,.fail,msg)
 ;
 ; Fully bounded for loop with extras
 s val=0
 f ln=1:1:10,15,"horse",20 s val=val+ln
 s msg=" - Adding up a value in a fully-bounded loop with extras"
 d EvalTest(val,90,.fail,msg)
 ;
 ; Argumentless for loop
 s val=0
 f  q:(val'<10)  s val=val+1
 s msg=" - Adding up a value in an argumentless for"
 d EvalTest(val,10,.fail,msg)
 ;
 ; Argumentless for loop with $ORDER
 s msg=" - Adding up a value in an argumentless for with $ORDER"
 s nxt=""
 s val=0
 f ln=1:1:10 s var(ln)=ln
 f  s nxt=$O(var(nxt)) q:(nxt="")  s val=val+var(nxt)
 d EvalTest(val,55,.fail,msg)
 ;
 d ReportResults(fail)
 q +fail
 ;
 ;**************************
 ;* Socket Device test
 ;**************************
TestSockets() ;
 n fail
 s fail=$$socketServer("localhost",60002,10,20)
 d ReportResults(fail)
 q +fail
 ;
 ; Socket server -> will job off a client to connect back to itself
socketServer(host,port,bytes,timeout) ;
 n ln,dev,addr,input,gen,conn,res
 w !,"Testing socket objects..."
 s res=1
 ;
 ; Set some reasonable defaults
 s:$S(+port<1:1,+port>65535:1,1:0) port=8080
 s:$S(+bytes<1:1,+bytes>4096:1,1:0) bytes=10
 s:$S(+timeout<1:1,+timeout>3600:1,1:0) timeout=10
 ;
 ; Open the server device
 s dev="HTTP-Listen"
 s addr=$$makeInterface(port)
 o dev:("listen"=addr)
 ;
 ; Generate some random characters for the client to send
 f ln=1:1:bytes s gen=gen_$C($R(93)+33)
 ;
 ; Spawn the client job and report it's PID
 s conn=$$makeHostname(host,port)
 j socketClient(conn,gen)
 ;
 ; Begin listening for the client to communicate back
 u dev
 r input#bytes:timeout
 ;
 ; Check that input is equal to GEN
 i gen=input s res=0
 ;
 ; Close the server socket
 c dev
 q +res
 ;
 ; Client socket
socketClient(dest,gen) ;
 n serv
 ;
 ; Open the client socket
 s serv="HTTP-Connect"
 o serv:("connect"=dest)
 u serv
 ;
 ; Write the gen input back to the server
 w gen
 ;
 ; Close the connection
 c serv
 q
 ;
 ; Make a full hostname
makeHostname(host,port) ;
 q host_":"_+port
 ;
makeInterface(port) ;
 q ":"_+port
 q