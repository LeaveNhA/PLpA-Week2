(* Programming Languages, Part A, Week 2, Homework *)

(* Write a function is_older that takes two dates and evaluates to true or false. It evaluates to true if the first argument is a date that comes before the second argument.
(If the two dates are the same, the result is false.) *)

(* Helpers *)
fun checkInTheList(l, n) =
    case l of
        (h::tail) => (h = n) orelse checkInTheList(tail, n)
      | nil => false

fun first_element (h::tail) = h
  | first_element nil = raise Empty

fun get_nth_prime(elements : 'a list, nth : int, index : int, default : 'a) =
    case elements of
        (h::tail) => if (index = nth) then h else get_nth_prime(tail, nth, index + 1, default)
      | (nil) => default

fun format_date_string(month, day, year)=
    month ^ " " ^ Int.toString(day) ^ ", " ^ Int.toString(year)

val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];

fun range_prime 0 = [ 0 ]
  | range_prime x = x :: range_prime (x - 1)

fun reverse nil = nil
  | reverse (h::t) = reverse t @ [ h ]

val range =
    (reverse o range_prime)

fun inc x = x + 1
(* ----------------------*)

fun is_older(date1 : int*int*int, date2 : int*int*int) =
    ((#1 date1 ) - (#1 date2) + (#2 date1) - (#2 date2) + (#3 date1) - (#3 date2)) > 0

fun dates_in_month(dates : (int * int * int) list, month : int) =
    List.filter (fn ((_, m, _) : (int * int * int)) => m = month) dates

fun dates_in_months(dates : (int * int * int) list, months : int list) =
    List.filter (fn ((_, m, _) : (int * int * int)) => checkInTheList(months, m)) dates

(* So, you wanna talk about function composition? -Yeah, me too! *)
fun number_in_month(dates : (int * int * int) list, month : int) =
    (List.length o dates_in_month)(dates, month)

fun number_in_months(dates : (int * int * int) list, months : int list) =
    (List.length o dates_in_months)(dates, months)

fun get_nth(elements : 'a list, nth : int) =
    get_nth_prime(elements, nth, 1, first_element(elements))

fun date_to_string(date : int * int * int) =
    format_date_string(get_nth(months, #2 date), #3 date, #1 date)

fun number_before_reaching_sum(sum : int, numbers : int list) =
    1 + List.length (List.filter (fn i => i < sum)
                                 (map ((foldl (op+) 0) o (List.filter (fn n => n > 0)) o (map (fn i => get_nth_prime(numbers, i, 1, ~1))) o (map (inc)) o range)
                                      (map (fn x => x + 1) (range(List.length numbers - 1)))))
