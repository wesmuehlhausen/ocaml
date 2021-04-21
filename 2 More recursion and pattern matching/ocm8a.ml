(*
Wesley Muehlhausen
File: ocm8a.ml
Date: Spring 2021
Desc: An implementation of 10 basic recursive functions used to learn ocaml. This part does not use pattern matching
*)

(* 1. Reverse *)
let rec my_rev x = 
    if (List.length x > 0) then my_rev (List.tl x) @ [List.hd x]
    else []
;;

(* 2. Last *)
let rec my_last x = 
    if (List.length x = 0) then failwith "Empty List"
    else if (List.length x = 1) then (List.hd x)
    else my_last (List.tl x)
;;

(* 3. Init *)
let rec my_init x = 
    if (List.length x = 0) then failwith "Empty List"
    else if (List.length x > 1) then List.hd x :: my_init (List.tl x) 
    else []
;;

(* 4. Mem *)
let rec my_mem x v = 
    if (List.length x = 0) then false
    else if (List.hd x = v) then true 
    else false || my_mem (List.tl x) v
;;

(* 5. replace *)
let rec my_replace x lst = 
    if (List.length lst > 0) then 
        if List.hd lst = fst x then [snd x] @ my_replace (fst x, snd x) (List.tl lst)
        else [List.hd lst] @ my_replace (fst x, snd x) (List.tl lst)
    else []
;;

(* 6. Replace all *)
let rec my_replace_all x1 x2 = 
    if (List.length x1 > 0) then my_replace_all (List.tl x1) (my_replace (List.hd x1) (x2))
    else x2
;;

(* 7. Elem Sum *)
let rec my_elem_sum x lst = 
    if (List.length lst > 0) then 
        if (x = List.hd lst) then x + (my_elem_sum x (List.tl lst))
        else 0 + my_elem_sum x (List.tl lst)
    else 0
;;

(* 8. My Rem Dups *)
let rec my_rem_dups lst = 
    if (List.length lst > 0) then
        if (my_elem_sum (List.hd lst) lst) = (List.hd lst) 
            then [List.hd lst] @ (my_rem_dups (List.tl lst))
        else [] @ (my_rem_dups (List.tl lst))
    else []
;;

(* 9. My Min *)
let my_min lst = 
    if (List.length lst = 0) then failwith "Empty List"
    else 
        let rec get_min min lst = 
            if (List.length lst = 0) then min
            else if (List.hd lst < min) then get_min (List.hd lst) (List.tl lst)
            else get_min (min) (List.tl lst)
        in get_min (List.hd lst) (lst)
;;
    
(*  10. Merge Sort *)

(**TESTING**)
let assert_equal v1 v2 msg =
    let cond = v1 = v2 in
    assert (if not cond then print_endline ("TEST FAILED: " ^ msg) ; cond)
;;

(* Test 1 *)
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