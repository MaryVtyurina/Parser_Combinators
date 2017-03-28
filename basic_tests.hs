import Parse_list

-- tests item
test_item1 = parse item "lala" == [('l',((1,2),"ala"))]
test_item2 = parse item ""    -- exception
----
--tests onside
test_onside1 = onside (1,5) (3,4) == onside (1,2) (1,3)
test_onside2 = onside (1,2) (3,4) == False
----
--tests newstate
test_newstate1 = newstate ((1,2), "\nlala") == ((2,0),"lala")
test_newstate2 = newstate ((1,2), "lala") == ((1,3),"ala")
----
--tests first
test_first1 = parse (first spaces) "   sss" == [((),((1,4),"sss"))]
-- *Main> parse spaces "   sss" -- without first
-- [((),((2,3),"sss")),((),((2,2)," sss")),((),((2,1),"  sss"))]
----
-- tests +++
test_1 = parse (lower +++ digit) "ll34" == [('l',((1,2),"l34"))]
test_2 = parse (lower +++ digit) "3str" == [('3',((1,2),"str"))]

----

-- tests string
test_string1 = parse (string "str") "string" == [("str",((1,4),"ing"))]
test_string2 = parse (string "str") "lalala" == []
----
-- tests char
test_char1 = parse (char 'c') "cat" == [('c',((1,2),"at"))]
test_char2 = parse (char 'c') "dog" == []
----
-- tests
test_zero = parse zero "xxx"
----
-- tests digit
test_digit1 = parse digit "342" == [('3',((1,2),"42"))]
test_digit2 = parse digit "lalala" == []
----
-- tests symbol
test_symbol1 = parse (symbol "23") "1234" == []
test_symbol2 = parse (symbol "123") "1234" == [("123",((1,4),"4"))]
----
-- tests lower & upper
test_lower1 = parse lower "abc" == [('a',((1,2),"bc"))]
test_lower2 = parse lower "1abc" == []

test_upper1 = parse upper "ABC" == [('A',((1,2),"BC"))]
test_upper2 = parse upper "abc" == []
----
-- tests letter & alphanum
test_letter1 = parse letter "Abc" == [('A',((1,2),"bc"))]
test_letter2 = parse letter "abc" == [('a',((1,2),"bc"))]

test_alphanum1 = parse alphanum "123" == [('1',((1,2),"23"))]
test_alphanum2 = parse alphanum "abc" == [('a',((1,2),"bc"))]

----
-- tests bracket
test_br1 = parse (bracket (char '[') alphanum (char ']')) "[a]" == [('a',((1,4),""))]
----
-- tests
----
-- tests
----
-- tests
----
-- tests
----
-- tests
----
