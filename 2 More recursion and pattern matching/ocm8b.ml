(*
Wesley Muehlhausen
File: ocm8b.ml
Date: Spring 2021
Desc: An implementation of 10 basic recursive functions used to learn ocaml. This part uses pattern matching
*)

(* Helper Functions *)

let rec length xs =
    match xs with
    | [] -> 0
    | _::t -> 1 + length t
;;

let head xs = 
    match xs with 
    | [] -> failwith "Empty List"
    | h::_ -> h
;;

let tail xs =
    match xs with
    | [] -> failwith "Empty List"
    | _::t -> t
;;



(* 1. Reverse *)
let rec my_rev x = 
    match x with 
    | _ when length x > 0 -> my_rev (tail x) @ [head x] 
    | _ -> []
;;

(* 2. Last*)
let rec my_last x = 
    match x with
    | _ when length x = 0 -> failwith "Empty List"
    | _ when length x = 1 -> head x
    | _ -> my_last (tail x)
;;

(* 3. Init *)
let rec my_init x = 
    match x with
    | _ when (length x = 0) -> failwith "Empty List"
    | _ when (length x > 1) -> head x :: my_init (tail x)
    | _  -> []
;;

(* 4. Mem *)
let rec my_mem x v = 
    match x with
    | _ when length x = 0 -> false
    | _ when length x = v -> true
    | _ -> false || my_mem (tail x) v
;;

(* 5. Replace *)
let rec my_replace x lst = 
    match lst with 
    | _ when (((length lst) > 0) && ((head lst) = (fst x))) -> [snd x] @ my_replace (fst x, snd x) (tail lst)
    | _ when (((length lst) > 0) && ((head lst) != (fst x))) -> [head lst] @ my_replace (fst x, snd x) (tail lst)
    | _ -> []
;;

(* 6. Replace all *)
let rec my_replace_all x1 x2 = 
    match x1 with
    | _ when (length x1 > 0) -> my_replace_all (tail x1) (my_replace (head x1) (x2))
    | _ -> x2
;;

(* 7. Elem Sum *)
let rec my_elem_sum x lst = 
    match lst with
    | _ when ((length lst > 0) && (head lst = x)) -> x + (my_elem_sum x (tail lst))
    | _ when ((length lst > 0) && (head lst != x)) -> 0 + (my_elem_sum x (tail lst))
    | _ -> 0
;;

(* 8. My Rem Dups *)
let rec my_rem_dups lst = 
    match lst with
    | _ when ((length lst > 0) && ((my_elem_sum (head lst) lst) = (head lst))) -> [head lst] @ (my_rem_dups (tail lst))
    | _ when ((length lst > 0) && ((my_elem_sum (head lst) lst) != (head lst))) -> [] @ (my_rem_dups (tail lst))
    | _ -> []
;;

(* 9. My Min *)
let my_min lst = 
    match lst with 
    | [] -> failwith "Empty List"
    | _ ->  let rec get_min min lst = 
                match lst with 
                | _ when (length lst = 0) -> min
                | _ when (head lst < min) -> get_min (head lst) (tail lst)
                | _ -> get_min min (tail lst)
                in get_min (head lst) (lst)
;;

(**TESTING**)
let assert_equal v1 v2 msg =
    let cond = v1 = v2 in
    assert (if not cond then print_endline ("TEST FAILED: " ^ msg) ; cond)
;;

(* Test 1*)
assert_equal [9;0;2;1;0] (my_rev [0;1;2;0;9]) "[9;0;2;1;0] = my_rev [0;1;2;0;9]";;
assert_equal [9;8;7;6;5;4;3;2;1] (my_rev [1;2;3;4;5;6;7;8;9]) "[9;8;7;6;5;4;3;2;1] = my_rev [1;2;3;4;5;6;7;8;9]";;

(* Test 2 *)
assert_equal 4 (my_last [3;3;3;4]) "4 = my_last [3;3;3;4]";;
assert_equal 4 (my_last [3;2;8;5;3;3;3;4]) "4 = my_last [3;2;8;5;3;3;3;4]";;

(* Test 3 *)
assert_equal [3;3;3] (my_init [3;3;3;4]) "[3;3;3] = my_init [3;3;3;4]";;
assert_equal [3;2;8;5;3;3;3] (my_init [3;2;8;5;3;3;3;4]) "[3;2;8;5;3;3;3] = my_init [3;2;8;5;3;3;3;4]";;

(* Test 4 *)
assert_equal true (my_mem [1;2;3] 3) "true = my_mem [1;2;3] 3";;
assert_equal false (my_mem [3;2;8;5;3;3;3;4] 69) "false = my_mem [3;2;8;5;3;3;3;4] 69 ";;

(* Test 5 *)
assert_equal [9;2;3;4] (my_replace (1,9) [1;2;3;4]) "[9;2;3;4] = my_replace (1,9) [1;2;3;4]";;
assert_equal [10;1;2;99;4;6] (my_replace (3, 99) [10;1;2;3;4;6]) "[10;1;2;99;4;6] = my_replace (3, 99)) [10;1;2;3;4;6]";;

(* Test 6 *)
assert_equal [9;2;8;4] (my_replace_all [(1,9); (3, 8)] [1;2;3;4]) "[9;2;8;4] = my_replace_all [(1, 9); (3, 8)] [1;2;3;4]";;
assert_equal [99;9;2;8;4;13] (my_replace_all [(1, 9); (3, 8); (6, 13); (10, 99)] [10;1;2;3;4;6]) "[99;9;2;8;4;13] = my_replace_all [(1, 9); (3, 8); (6, 13); (10, 99)] [10;1;2;3;4;6]";;

(* Test 7 *)
assert_equal 64 (my_elem_sum 8 [8;8;8;8;8;8;8;8]) "64 = (my_elem_sum 8 [8;8;8;8;8;8;8;8])";;
assert_equal 32 (my_elem_sum 8 [8;23;8;54;8;12;8;19]) "32 = (my_elem_sum 8 [8;23;8;54;8;12;8;19])";;

(* Test 8 *)
assert_equal [1] (my_rem_dups [1;1;1;1;1;1;1;1;1]) "[1] = my_rem_dups [1;1;1;1;1;1;1;1;1]";;
assert_equal [1;2;3;4;5] (my_rem_dups [2;1;2;3;3;4;5]) "[1;2;3;4;5] = my_rem_dups [2;1;2;3;3;4;5]";;

(* Test 9 *)
assert_equal 1 (my_min [7; 1; 9; 12; 10]) "1 = my_min [7; 1; 9; 12; 10]";;
assert_equal 3 (my_min [24; 13; 3; 10; 43; 65; 43]) "3 = my_min [24; 13; 3; 10; 43; 65; 43]";;

(* Test 10 *)