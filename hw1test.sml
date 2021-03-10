(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)


val test1 = is_older ((1,2,3),(2,3,4)) = true

val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"

val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3

val test9 = what_month 70 = 3

val test10 = month_range (31, 34) = [1,2,2,2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)

val test12 = is_older((1,2,25),(3,5,26)) = true
val test13 = is_older((1,2,3),(2,3,4)) = true
val test14 = is_older((3,5,26),(1,2,25)) = false
val test15 = is_older((1,2,25),(1,12,29)) = true
val test16 = is_older((1,2,25),(3,2,28)) = true
val test17 = is_older((1,2,25),(1,2,27)) = true
val test18 = is_older((1,2,3),(5,2,3)) = true

val test19 = oldest([(1,2,3),(5,2,3),(7,2,3),(3,2,3)]) = SOME (1,2,3)
val test20 = oldest([(5,5,2),(5,10,2),(5,2,2),(5,12,2)]) = SOME (5,2,2)
val test21 = oldest([(5,12,15),(5,12,10),(5,12,1)]) = SOME (5,12,1)
