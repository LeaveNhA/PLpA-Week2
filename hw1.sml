(* Programming Languages, Part A, Week 2, Homework *)

(* Helpers *)
fun dec n =
    n - 1

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

fun date_of_day(date: int * int * int) =
    (#1 date) * 365 + (#2 date) * 30 + (#3 date)
(* ----------------------*)

fun is_older(date1 : int*int*int, date2 : int*int*int) =
    date_of_day(date1) < date_of_day(date2)

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
    List.length (List.filter (fn i => i < sum andalso i <> ~1)
                             (map ((foldl (op+) 0) o
                                   (List.filter (fn n => n > 0)) o
                                   (map (fn i => get_nth_prime(numbers, i, 1, 0))) o
                                   (map (inc)) o
                                   range)
                                  (range(List.length numbers - 1))))

fun what_month(day : int) =
    number_before_reaching_sum(day, [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]) + 1

fun month_range(day1 : int, day2 : int) =
    if day2 < day1 then []
    else (map (what_month) o map (fn i => i + day1) o range)(day2 - day1)

val ifFn =
 fn expression1 =>
    fn expression2 =>
       fn condition =>
          if condition then expression1 else expression2

fun oldest(dates : (int * int * int) list) =
    if null dates then NONE
    else SOME (foldl (fn (day1, day2) => ifFn day1 day2 (is_older(day1, day2)))
                     (hd dates)
                     (tl dates))

(* Since they are capable of working with duplicated values in months, they can be replaced with challanges *)
val dates_in_months_challenge = dates_in_months
val number_in_months_challenge = number_in_months
