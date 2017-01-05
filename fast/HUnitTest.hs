module HUnitTest where

import Test.HUnit
import FastParser
import FastAST
import SimpleParse

t_name = parseEof name
--Positive tests
test_name_1 = TestCase $ assertEqual "name 1" ([("a", "")]) (t_name "a")
test_name_2 = TestCase $ assertEqual "name 2" ([("ab", "")]) (t_name "ab")
test_name_3 = TestCase $ assertEqual "name 3" ([("P", "")]) (t_name "P")
test_name_4 = TestCase $ assertEqual "name 4" ([("PAS_D", "")]) (t_name "PAS_D")

--negative tests
test_name_5 = TestCase $ assertEqual "name 5" ([]) (t_name "1ASD")
test_name_6 = TestCase $ assertEqual "name 6" ([]) (t_name "-1")
test_name_7 = TestCase $ assertEqual "name 7" ([]) (t_name "1P")
test_name_8 = TestCase $ assertEqual "name 8" ([]) (t_name "1_1")

t_string = parseEof str
--positive tests
test_string_9 = TestCase $ assertEqual "String 1" ([("12", "")]) (t_string "\"12\"")
test_string_10 = TestCase $ assertEqual "String 2" ([("ab", "")]) (t_string "\"ab\"")
test_string_11 = TestCase $ assertEqual "String 3" ([("PAsd", "")]) (t_string "\"PAsd\"")
test_string_12 = TestCase $ assertEqual "String 4" ([("1_2", "")]) (t_string "\"1_2\"")

--negative tests
test_string_13 = TestCase $ assertEqual "String 5" ([]) (t_string "\"1A\"SD\"")
test_string_14 = TestCase $ assertEqual "String 6" ([]) (t_string "\"-1\"\"")
test_string_15 = TestCase $ assertEqual "String 7" ([]) (t_string "\"1\"P\"")
test_string_16 = TestCase $ assertEqual "String 8" ([]) (t_string "\"1_\"1\"")

t_integer = parseEof integer
--positive tests
test_integer_17 = TestCase $ assertEqual "integer 1" ([(12, "")]) (t_integer "12")
test_integer_18 = TestCase $ assertEqual "integer 2" ([(1212, "")]) (t_integer "1212")
test_integer_19 = TestCase $ assertEqual "integer 3" ([(22312, "")]) (t_integer "22312")
test_integer_20 = TestCase $ assertEqual "integer 4" ([(123113, "")]) (t_integer "123113")

--negative tests
test_integer_21 = TestCase $ assertEqual "integer 5" ([]) (t_string "aasd")
test_integer_22 = TestCase $ assertEqual "integer 6" ([]) (t_string "_@31")
test_integer_23 = TestCase $ assertEqual "integer 7" ([]) (t_string "a_2")
test_integer_24 = TestCase $ assertEqual "integer 8" ([]) (t_string "A_232")

t_pattern = parseEof pattern
--Positive tests
test_pattern_25 = TestCase $ assertEqual "pattern 1" ([(ConstInt 12,"")]) (t_pattern "12")
test_pattern_26 = TestCase $ assertEqual "pattern 2" ([(AnyValue "asd","")]) (t_pattern "asd")
test_pattern_27 = TestCase $ assertEqual "pattern 3" ([(ConstString "asd","")]) (t_pattern "\"asd\"")
test_pattern_28 = TestCase $ assertEqual "pattern 4" ([(TermPattern "asd" ["a","a"],"")]) (t_pattern "asd(a,a)")
test_pattern_29 = TestCase $ assertEqual "pattern 5" ([(TermPattern "asd" ["a","a","a"],"")]) (t_pattern "asd(a,a,a)")
test_pattern_30 = TestCase $ assertEqual "pattern 6" ([(TermPattern "asd" ["a","a","asd","asd"],"")]) (t_pattern "asd(a,a,asd,asd)")

--negative tests
test_pattern_31 = TestCase $ assertEqual "pattern 7" ([]) (t_pattern "\"a\"sd\"")
test_pattern_32 = TestCase $ assertEqual "pattern 8" ([]) (t_pattern "asd(=)")
test_pattern_33 = TestCase $ assertEqual "pattern 9" ([]) (t_pattern "\"as=\"d\"")
test_pattern_34 = TestCase $ assertEqual "pattern 10" ([]) (t_pattern "asd(1,1)")

t_cases = parseEof cases'
--positive tests
test_cases_35 = TestCase $ assertEqual "cases 1" ([([(AnyValue "a",[ReadVar "a",ReadVar "a"])],"")]) (t_cases "a->{a;a;}")
test_cases_36 = TestCase $ assertEqual "cases 2" ([([(AnyValue "a",[IntConst 1,StringConst "1"])],"")]) (t_cases  "a->{1;\"1\";}")

--negative tests
test_cases_37 = TestCase $ assertEqual "cases 3" ([]) (t_cases "a")
test_cases_38 = TestCase $ assertEqual "cases 4" ([]) (t_cases "{a;a;}")


---Exprs

t_expr = parseEof expr

test_expr_39 = TestCase $ assertEqual "expr 01" ([(IntConst 1,"")]) (t_expr "1")
test_expr_40 = TestCase $ assertEqual "expr 02" ([(ReadVar "a","")]) (t_expr "a")
test_expr_41 = TestCase $ assertEqual "expr 03" ([(StringConst "1","")]) (t_expr "\"1\"")
test_expr_42 = TestCase $ assertEqual "expr 04" ([(ReadVar "Peter","")]) (t_expr "Peter")

test_expr_43 = TestCase $ assertEqual "expr 04" ([(Self,"")]) (t_expr "self")
test_expr_44 = TestCase $ assertEqual "expr 05" ([(Return (ReadVar "a"),"")]) (t_expr "return a")
test_expr_45 = TestCase $ assertEqual "expr 06" ([(Return (IntConst 12),"")]) (t_expr "return 12")
test_expr_46 = TestCase $ assertEqual "expr 07" ([(Return (Plus (IntConst 1) (IntConst 1)),"")]) (t_expr "return 1+1")
test_expr_47 = TestCase $ assertEqual "expr 08" ([(SetField "a" (Plus (IntConst 1) (IntConst 1)),""),(Plus (SetField "a" (IntConst 1)) (IntConst 1),"")]) (t_expr "set self.a= 1+1")
test_expr_48 = TestCase $ assertEqual "expr 09" ([(SetVar "name" (Plus (IntConst 1) (IntConst 1)),""),(Plus (SetVar "name" (IntConst 1)) (IntConst 1),"")]) (t_expr "set name = 1+1")

test_expr_49 = TestCase $ assertEqual "expr 10" ([(Match (ReadVar "n") [(ConstInt 0,[IntConst 1]),(AnyValue "x",[Times (ReadVar "n") (CallMethod Self "fact" [Minus (ReadVar "n") (IntConst 1)])])],"")]) (t_expr "match n{0-> {1;}x -> {n*self.fact(n-1);}}")
test_expr_50 = TestCase $ assertEqual "expr 11" ([(SendMessage (ReadVar "a") (ReadVar "a"),"")]) (t_expr "send (a,a)")
test_expr_51 = TestCase $ assertEqual "expr 12" ([(New "a" [ReadVar "a",ReadVar "a"],"")]) (t_expr "new a (a,a)")
test_expr_52 = TestCase $ assertEqual "expr 13" ([(ReadVar "asd","")]) (t_expr "(asd)")



-- negative tests
test_expr_53 = TestCase $ assertEqual "expr 14" ([]) (t_expr "\"a\"sd\"")
test_expr_54 = TestCase $ assertEqual "expr 15" ([]) (t_expr "asd(=)")

--Addition and Associativity
test_expr_55 = TestCase $ assertEqual "expr 16" ([(Plus(IntConst 1)(IntConst 1), "")]) (t_expr "1+1")
test_expr_56 = TestCase $ assertEqual "expr 17" ([(Plus(IntConst 1)(IntConst 2), "")]) (t_expr "1+2")

--Left Associativity and higher precedence

test_expr_57 = TestCase $ assertEqual "expr 18" ([(Plus(IntConst 1)(Times(IntConst 1)(IntConst 1)), "")]) (t_expr "1+1*1")
test_expr_58 = TestCase $ assertEqual "expr 19" ([(Plus(Times(IntConst 1)(IntConst 1))(IntConst 1), "")]) (t_expr "1*1+1")
test_expr_59 = TestCase $ assertEqual "expr 20" ([(Plus(DividedBy(IntConst 1)(IntConst 1))(IntConst 1), "")]) (t_expr "1/1+1")
test_expr_60 = TestCase $ assertEqual "expr 21" ([(CallMethod (ReadVar "as") "asd" [ReadVar "a",ReadVar "a"],"")]) (t_expr "as.asd(a,a)")
test_expr_61 = TestCase $ assertEqual "expr 22" ([(Plus (IntConst 1) (CallMethod (IntConst 1) "asd" [ReadVar "a",ReadVar "a"]),"")]) (t_expr "1+1.asd(a,a)")
test_expr_62 = TestCase $ assertEqual "expr 23" ([(Plus (IntConst 1) (CallMethod (IntConst 1) "Pas" [ReadVar "a",ReadVar "a"]),"")]) (t_expr "1+1.Pas(a,a)")


-- recvdecl 
t_recvDecl = parseEof recvDecl
-- positive
test_recvDecl_63 = TestCase $ assertEqual "recvdecl 1" ([(Just (ReceiveDecl {receiveParam = "BA", receiveBody = [IntConst 7]}),"")]) (t_recvDecl "receive (BA) {7;}")
test_recvDecl_64 = TestCase $ assertEqual "recvdecl 2" ([(Just (ReceiveDecl {receiveParam = "Psda", receiveBody = [IntConst 7,IntConst 3]}),"")]) (t_recvDecl "receive (Psda) {7;3;}")
test_recvDecl_65 = TestCase $ assertEqual "recvdecl 3" ([(Just (ReceiveDecl {receiveParam = "a", receiveBody = [Return (Plus (ReadVar "a") (Times (ReadVar "b") (IntConst 2)))]}),"")]) (t_recvDecl "receive (a) {return a+b*2;}")
test_recvDecl_67 = TestCase $ assertEqual "recvdecl 5" ([(Just (ReceiveDecl {receiveParam = "a", receiveBody = [Return (IntConst 3)]}),"")]) (t_recvDecl "receive (a) {return 3 ;}")

-- negative

test_recvDecl_66 = TestCase $ assertEqual "recvdecl 4" ([]) (t_recvDecl "(a) return c+d")
test_recvDecl_68 = TestCase $ assertEqual "recvdecl 6" ([]) (t_recvDecl "(a) return asd")


--NamedMethodDecl
t_namedMethodDecl = parseEof namedMethodDecl
--positive
test_namedMethodDecl_69 = TestCase $ assertEqual "nameMethodDecls 1" ([(NamedMethodDecl "asds" (MethodDecl {methodParameters = ["pere","pere"], methodBody = [Return (IntConst 2323)]}),"")]) (t_namedMethodDecl "asds(pere,pere){return 2323;}")
test_namedMethodDecl_70 = TestCase $ assertEqual "namedMedhodDecls 2" ([(NamedMethodDecl "andere" (MethodDecl {methodParameters = ["pere","pere"], methodBody = [Return (Times (ReadVar "aregs") (ReadVar "betrgs"))]}),"")]) (t_namedMethodDecl "andere(pere,pere){return aregs*betrgs;}")
test_namedMethodDecl_71 = TestCase $ assertEqual "namedMethodDecls 3" ([(NamedMethodDecl "sending" (MethodDecl {methodParameters = ["as1","asd2"], methodBody = [SendMessage (ReadVar "asd") (ReadVar "asd")]}),"")]) (t_namedMethodDecl "sending(as1,asd2){send (asd,asd);}")

-- negative
test_namedMethodDecl_72 = TestCase $ assertEqual "namedMethodDecl 4" ([]) (t_namedMethodDecl "3asd (arasd,aasd){send (ada,aad);}")
test_namedMethodDecl_73 = TestCase $ assertEqual "namedMethodDecl 5" ([]) (t_namedMethodDecl "asds (asrg,argasd,){send (aasdrg,argsd);}")
test_namedMethodDecl_74 = TestCase $ assertEqual "namedMethodDecl 6" ([]) (t_namedMethodDecl "asda (1,2,){send (first,second);;}")

---constructordecl
t_constructorDecl = parseEof constructorDecl
-- positive
test_constructorDecl_75 = TestCase $ assertEqual "constructorDecl" ([(Just (MethodDecl {methodParameters = ["first","second"], methodBody = [Plus (IntConst 1) (IntConst 2)]}),"")]) (t_constructorDecl "new (first,second){1+2;}" )
test_constructorDecl_76 = TestCase $ assertEqual "constructorDecl" ([(Just (MethodDecl {methodParameters = ["first","second"], methodBody = [Return (Plus (ReadVar "a1") (ReadVar "a2"))]}),"")]) (t_constructorDecl "new (first,second) {return a1+a2;}")
test_constructorDecl_77 = TestCase $ assertEqual "constructorDecl" ([(Just (MethodDecl {methodParameters = ["b1","b2"], methodBody = [Return (SendMessage (Plus (ReadVar "a") (ReadVar "b")) (Minus (ReadVar "first") (ReadVar "second")))]}),"")]) (t_constructorDecl "new (b1,b2) {return send (a+b, first-second);}")

-- negative
test_constructorDecl_78 = TestCase $ assertEqual "constructorDecl" ([]) (t_constructorDecl "new (ga1,a2) {return send (aa1+aa, aa-a]]);}")
test_constructorDecl_79 = TestCase $ assertEqual "constructorDecl" ([]) (t_constructorDecl "new (ga1,a2) {retasdurn send (aa1+aa, aa-a]sd]);}")
test_constructorDecl_80 = TestCase $ assertEqual "constructorDecl" ([]) (t_constructorDecl "newa (ga1,a2) {return send (aa1+aa, aa-a]]);}")

--classdecls
-- positive
t_classDecls = parseEof program
test_classDecls_81 = TestCase $ assertEqual "classDecls" ([([ClassDecl {className = "p1", classConstructor = Just (MethodDecl {methodParameters = ["andrew","peter"], methodBody = [ReadVar "friend"]}), classMethods = [NamedMethodDecl "m" (MethodDecl {methodParameters = ["first","second"], methodBody = [Plus (ReadVar "attempt") (ReadVar "ap")]})], classReceive = Just (ReceiveDecl {receiveParam = "point", receiveBody = [Return (IntConst 342)]})}],"")]) (t_classDecls  "class p1{new (andrew,peter){friend;}m(first,second){attempt+ap;}receive(point){return 342;}}")
-- negative
test_classDecls_82 = TestCase $ assertEqual "classDecls" ([([ClassDecl {className = "p", classConstructor = Just (MethodDecl {methodParameters = ["first","second"], methodBody = [ReadVar "first"]}), classMethods = [NamedMethodDecl "m" (MethodDecl {methodParameters = ["ander","frederik"], methodBody = [Plus (ReadVar "aas") (ReadVar "aasd")]})], classReceive = Just (ReceiveDecl {receiveParam = "a", receiveBody = [Return (IntConst 1)]})}],"")]) (t_classDecls "class p{new (first,second){first;}m(ander,frederik){aas+aasd;} receive(a){return 1;}}")


--program tests..Yes it parsed  
t_program = parseEof program
test_program_83 = TestCase $ assertEqual "program" ([([ClassDecl {className = "s", classConstructor = Just (MethodDecl {methodParameters = ["Pasd1","Parterg"], methodBody = [ReadVar "first"]}), classMethods = [NamedMethodDecl "m" (MethodDecl {methodParameters = ["as","aa2"], methodBody = [Plus (ReadVar "anns") (ReadVar "jonh")]})], classReceive = Just (ReceiveDecl {receiveParam = "apa", receiveBody = [Return (ReadVar "asd")]})}],"")]) (t_program "class s{new (Pasd1,Parterg){first;}m(as,aa2){anns+jonh;}receive(apa){return asd;}}")
test_program_84 = TestCase $ assertEqual "program" ([([ClassDecl {className = "s", classConstructor = Just (MethodDecl {methodParameters = ["first","second"], methodBody = [ReadVar "andre"]}), classMethods = [NamedMethodDecl "m" (MethodDecl {methodParameters = ["er","per"], methodBody = [Plus (ReadVar "variable1") (ReadVar "vaiablea2")]})], classReceive = Just (ReceiveDecl {receiveParam = "adphone", receiveBody = [Return (IntConst 34233)]})}],"")]) (t_program  "class s{new (first,second){andre;}m(er,per){variable1+vaiablea2;} receive(adphone){return 34233;}}")

----------you have reached here and so I guess you have parsed successfully :)....---------



tests = TestList [TestLabel "" test_name_1,
                  TestLabel "" test_name_2,
                  TestLabel "" test_name_3,
                  TestLabel "" test_name_4,
                  TestLabel "" test_name_5,
                  TestLabel "" test_name_6,
                  TestLabel "" test_name_7,
                  TestLabel "" test_name_8,
                  TestLabel "" test_string_9,
                  TestLabel "" test_string_10,
                  TestLabel "" test_string_11,
                  TestLabel "" test_string_12,
                  TestLabel "" test_string_13,
                  TestLabel "" test_string_14,
                  TestLabel "" test_string_15,
                  TestLabel "" test_string_16,
                  TestLabel "" test_integer_17,
                  TestLabel "" test_integer_18,
                  TestLabel "" test_integer_19,
                  TestLabel "" test_integer_20,
                  TestLabel "" test_integer_21,
                  TestLabel "" test_integer_22,
                  TestLabel "" test_integer_23,
                  TestLabel "" test_integer_24,
                  TestLabel "" test_pattern_25,
                  TestLabel "" test_pattern_26,
                  TestLabel "" test_pattern_27,
                  TestLabel "" test_pattern_28,
                  TestLabel "" test_pattern_29,
                  TestLabel "" test_pattern_30,
                  TestLabel "" test_pattern_31,
                  TestLabel "" test_pattern_32,
                  TestLabel "" test_pattern_34,
                  TestLabel "" test_cases_35,
                  TestLabel "" test_cases_36,
                  TestLabel "" test_cases_37,
                  TestLabel "" test_cases_38,
                  TestLabel "" test_expr_39,
                  TestLabel "" test_expr_40,
                  TestLabel "" test_expr_41,
                  TestLabel "" test_expr_42,
                  TestLabel "" test_expr_43,
                  TestLabel "" test_expr_44,
                  TestLabel "" test_expr_45,
                  TestLabel "" test_expr_46,
                  TestLabel "" test_expr_47,
                  TestLabel "" test_expr_48,
                  TestLabel "" test_expr_49,
                  TestLabel "" test_expr_50,
                  TestLabel "" test_expr_51,
                  TestLabel "" test_expr_52,
                  TestLabel "" test_expr_53,
                  TestLabel "" test_expr_54,
                  TestLabel "" test_expr_55,
                  TestLabel "" test_expr_56,
                  TestLabel "" test_expr_57,
                  TestLabel "" test_expr_58,
                  TestLabel "" test_expr_60,
                  TestLabel "" test_expr_61,
                  TestLabel "" test_expr_62,
                  TestLabel "" test_recvDecl_63,
                  TestLabel "" test_recvDecl_64,
                  TestLabel "" test_recvDecl_65,
                  TestLabel "" test_recvDecl_66,
                  TestLabel "" test_recvDecl_67,
                  TestLabel "" test_recvDecl_68,
                  TestLabel "" test_namedMethodDecl_69,
                  TestLabel "" test_namedMethodDecl_70,
                  TestLabel "" test_namedMethodDecl_71,
                  TestLabel "" test_namedMethodDecl_72,
                  TestLabel "" test_namedMethodDecl_73,
                  TestLabel "" test_namedMethodDecl_74,
                  TestLabel "" test_constructorDecl_75,
                  TestLabel "" test_constructorDecl_76,
                  TestLabel "" test_constructorDecl_77,
                  TestLabel "" test_constructorDecl_78,
                  TestLabel "" test_constructorDecl_79,
                  TestLabel "" test_constructorDecl_80,
                  TestLabel "" test_classDecls_81,
                  TestLabel "" test_classDecls_82,
                  TestLabel "" test_program_83,
                  TestLabel "" test_program_84
                  
                  
--                  TestLabel "" test_expr_63

				  
                  
 ]				  
				  
run = runTestTT tests