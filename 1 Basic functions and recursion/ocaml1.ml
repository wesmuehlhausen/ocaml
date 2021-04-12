(*
Wesley Muehlhausen
File: ocaml1.ml
Date: Spring 2021
Desc: An implementation of 10 basic functions used to learn ocaml some with recursion
*)

(* 1. minimum of two numbers  *)
let my_min x y =
        if (x > y) then y
        else x;;

(* 2. Median of three numbers *)
let my_median x y z = 
	if ((x < y && y < z)|| (x > y && y > z)) then y
        else if ((y < x && x < z)|| (y > x && x > z))then x
        else if ((x < z && z < y)|| (x > z && z > y))then z
	else if (x = y) then x
	else if (x = z) then x
	else if (z = y) then z
	else x;;

(* 3. Triangle Area *)
let my_triangle_area base height = (base *. height) /. 2.0;;

(* 4. Circle Area *)
let my_circle_area radius = 
	let pi = 3.14 in (pi *. radius *. radius);;

(* 5. Midpoint *)
let my_midpoint (x1, y1) (x2, y2) = 
	(((x2 +. x1) /. 2.0), ((y1 +. y2) /. 2.0));;

(* 6. My Manhatton Distance *)
let my_manhattan_distance (x1, y1) (x2, y2) = 
	(abs_float (x1 -. x2)) +. (abs_float (y1 -. y2));;

(* 7. euclidean distance  *)
let my_euclidean_distance (x1, y1) (x2, y2) = 
	sqrt (((x2 -. x1) *. (x2 -. x1)) +. ((y2 -. y1) *. (y2 -. y1)));;

(* 8. Range Sum *)

let rec my_range_sum v1 v2 = 
	if(v1 > v2) then 0
	else if (v1 = v2) then v1
	else (v1 + my_range_sum (v1+1) v2)

(* 9. Greatest Common Divisor *)
let rec my_gcd x y = 
	if x = y then x
	else if x > y then 
		if (x mod y) = 0 then y
		else my_gcd (x mod y) y
	else 
		if (y mod x) = 0 then x
		else my_gcd x (y mod x);;

(* 10. Mutually Recursive *)

let rec even x = 
	if (x < 0) then false 
	else if (x = 0) then true
	else odd (x - 1)
and odd x = 
	if (x < 0) then false 
	else if (x = 1) then true
	else even (x - 1);;

 (* Testing *)

let assert_equal v1 v2 msg = 
	let cond = v1 = v2 in
	assert (if not cond then print_endline ("TEST FAILED: " ^ msg) ; cond)
;;

(* Question 1: my_min tests *)
assert_equal 1 (my_min 1 2) "1 = my_min 1 2";;
assert_equal 1 (my_min 2 1) "1 = my_min 2 1";;
assert_equal 1 (my_min 1 1) "1 = my_min 1 1";;

(* Question 2: my_median tests *)
assert_equal 2 (my_median 1 2 3) "2 = my_median 1 2 3";;
assert_equal 2 (my_median 2 1 3) "2 = my_median 2 1 3";;
assert_equal 3 (my_median 3 3 2) "3 = my_median 3 3 2";;

(* Question 3: my_triangle_area tests *)
assert_equal 8.0 (my_triangle_area 4.0 4.0) "8 = my_triangle_area 4 4";;

(* Question 4: my_circle_area tests *)
assert_equal 78.5 (my_circle_area 5.) "78.5 = my_circle_area radius 5.";;
assert_equal 200.96 (my_circle_area 8.) "200.96 = my_circle_area radius 8.";;

(* Question 5: my_midpoint tests *)
assert_equal (2., 3.) (my_midpoint (1., 1.) (3., 5.)) "(2., 3.) = my_midpoint (1., 1.) (3., 5.)";;
assert_equal (0., 3.) (my_midpoint (8.0, 1.0) (-8.0, 5.0)) "(0., 3.) = my_midpoint (8.0, 1.0) (-8.0, 5.0)";;

(* Question 6: my_manhattan_distance tests *)
assert_equal 5. (my_manhattan_distance (1., 1.) (3., 4.)) "5. = my_manhattan_distance (1.,1.) (3.,4.)";;
assert_equal 5. (my_manhattan_distance (3., 1.) (1., 4.)) "5. = my_manhattan_distance (3.,1.) (1.,4.)";;
assert_equal 137. (my_manhattan_distance (10.0, -91.0) (13.0, 43.0)) "137. = my_manhattan_distance (10.0, -91.0) (13.0, 43.0)";;

(* Question 7: my_euclidean_distance tests *)
assert_equal 5. (my_euclidean_distance (1.,1.) (4.,5.)) "5. = my_euclidean_distance (1.,1.) (4.,5.)";;
assert_equal 27. (my_euclidean_distance (12.0, -23.0) (12.0, 4.0)) "27. = my_euclidean_distance (12.0, -23.0) (12.0, 4.0)";;

(* Question 8: my_range_sum tests *)
assert_equal 0 (my_range_sum 2 1) "0 = my_range_sum 2 1";;
assert_equal 153495 (my_range_sum 216 594) "153495 = my_range_sum 216 594";;

(* Question 9: my_gcd tests *)
assert_equal 5 (my_gcd 5 5) "5 = my_gcd 5 5";;
assert_equal 6 (my_gcd 12 30) "6 = my_gcd 12 30";;
assert_equal 54 (my_gcd 216 594) "54 = my_gcd 216 594";;

(* Question 10: my_min tests *)
assert_equal true (even 0) "true = even 0";;
assert_equal true (odd 1) "true = odd 1";;
assert_equal false (even 27) "false = even 27";;
assert_equal true (odd 27) "true = odd 27";;
assert_equal true (even 14) "true = even 14";;
assert_equal false (odd 14) "false = odd 14";;

