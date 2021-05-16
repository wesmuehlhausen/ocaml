(* ----------------------------------------------------------------------
 * Name: Wesley Muehlhausen
 * File: kv.ml
 * Date: 4/20/2021
 * Desc: an implementation of a basic key-value pair
         collection algebraic data type: (below)
----------------------------------------------------- *)

(* A key-value collection as a linked list. Allows duplicate keys. *)
type ('a,'b) kvlist = Node of 'a * 'b * ('a, 'b) kvlist
                     | Nil ;;

(* FUNCTION DEFINITIONS  *)


(* 1: INSERT 'a -> 'b -> ('a,'b) kvlist -> ('a,'b) kvlist *)
let insert key v lst =
  match lst with
  | Nil -> Node(key, v, Nil)
  | _ -> Node(key,v, lst)
;;

(* 2:  REMOVE 'a -> ('a,'b) kvlist -> ('a,'b) kvlist) *)
let rec remove key lst =
  match lst with
  | Node (keyp, _ , lstp) when keyp = key -> remove(key)(lstp)
  | Node (keyp, valp, lstp) -> Node(keyp, valp, remove(key)(lstp))
  | Nil -> Nil
;;

(* 3:  SIZE ('a,'b) kvlist -> int *)
let rec size lst =
  match lst with
  | Nil -> 0
  | Node(_,_,lstp) ->  size (lstp) + 1
;;

(* 4:  HAS_KEY 'a -> ('a, 'b) kvlist -> 'a list *)
let rec has_key key lst =
  match lst with
  | Nil -> false
  | Node (keyp,v,lstp) when key=keyp -> true
  | Node (_ , _ ,lstp) -> has_key (key) (lstp)
;;

(* 5:  KEYS ('a,'b) kvlist -> 'a list *)
let rec keys lst =
  match lst with
  | Nil -> []
  | Node(keyp,_,lstp) -> keyp :: keys(lstp)
;;

(* 6:  VALUES ('a,'b) kvlist -> 'b list *)
let rec values lst =
  match lst with
  | Nil -> []
  | Node(_,valp,lstp) -> valp :: values (lstp)
;;

(* 7:  KEY_VALUES 'a -> ('a,'b) kvlist -> 'b list *)
let rec key_values key lst =
  match lst with
  | Nil -> []
  | Node(keyp , valp , lstp) when  key=keyp -> valp :: key_values (key) (lstp)
  | Node( _ , _ , lstp) -> key_values(key)(lstp)
;;

(* 8:  COMBINE ('a,'b) kvlist -> ('a,'b) kvlist -> ('a,'b) kvlist *)
let rec combine lst x =
  match lst with
  | Nil -> x
  | Node(keyp, valp, lstp) -> combine (lstp) (insert (keyp) (valp) (x))
;;

(* 9:  INVERT ('a,'b) kvlist -> ('b,'a) kvlist *)
let rec invert lst =
  match lst with
  | Nil -> Nil
  | Node(keyp, valp, lstp) -> Node(valp, keyp, invert (lstp))
;;

(* 10: GROUP ('a,'b) kvlist -> ('a,'b list) kvlist *)
let rec group lst =
  match lst with
  | Nil -> Nil
  | Node(keyp, valp, lstp) -> Node(keyp, (key_values(keyp)(lst)), group(remove(keyp)(lstp)))
;;

(* 11: KV_MAP ('a -> 'b -> ('c, 'd)) -> ('a, 'b) kvlist -> ('c, 'd) kvlist *)
let rec kv_map m lst =
  match lst with
  | Nil -> Nil
  | Node(keyp, valp, lstp) -> Node( (fst) (m (keyp) (valp) ), (snd) (m (keyp) (valp) ), kv_map (m) (lstp))
;;

(* 12: KV_FILTER ('a -> 'b -> bool) -> ('a, 'b) kvlist -> ('a, 'b) kvlist *)
let rec kv_filter m lst =
  match lst with
  | Nil -> Nil
  | Node(keyp, valp, lstp) when m(keyp)(valp) -> Node(keyp,valp,kv_filter(m)(lstp))
  | Node( _ , _ , lstp) -> kv_filter(m)(lstp)
;;

(* 13: JOIN ('a, 'b) kvlist -> ('a, 'b) kvlist -> ('a, 'b list) kvlist *)
let join lst ms =
  let rec unnest lst =
    match lst with
    | Nil -> Nil
    | Node(keyp, valp, lstp) -> Node(keyp, List.flatten (valp), unnest (lstp))

  in unnest(group(combine(group(lst))(group(ms))))
;;

(* 14: COUNT_KEYS_BY_VAL int -> ('a, 'b) kvlist -> ('b, int) kvlist 
let rec count_keys_by_val m lst =
  let ms = kv_filter (fun a b -> b >= m) (lst) in
  match ms with
  | Nil -> Nil
  (*not ready yet*)
;;*)



(* ************************************************************************** *)
(* TESTING *)

let assert_equal v1 v2 msg =
  let cond = v1 = v2 in
  assert (if not cond then print_endline ("TEST FAILED: " ^ msg) else print_endline("TEST PASSED: " ^ msg) ; cond)
;;

(* test data  *)
let c1 = Nil ;;
let c2 = Node ('a', 1, Nil) ;;
let c3 = Node ('b', 2, Node('c', 3, Nil)) ;;
let c4 = Node ('a', 1, c3) ;;

(* Map helper *)
let map_helper k v =
  let new_k = Char.code (k) - 96
  and new_v = v < 5
  in (new_k, new_v)
;;

(* Filter helper *)
let filter_helper k v = (k == 'a') || (v < 5) ;;

(*  insert tests *)
assert_equal (Node('a',1,Nil)) (insert ('a') (1) (Nil)) ("Node('a',1,Nil) = insert ('a')(1)(Nil)") ;;
assert_equal (Node('a',1,Node('b',2,Node('c',3,Nil)))) (insert ('a') (1) (c3)) ("Node('a', 1, Node('b', 2, Node('c',3,Nil)))) = insert ('a') (1) (c3)") ;;

(*  remove tests *)
assert_equal (Nil) (remove ('a')(Nil)) ("Nil = remove 'a' Nil") ;;
assert_equal (Nil) (remove ('a')(Node('a',1,Nil))) ("Nil = remove ('a')(Node('a', 1, Nil))") ;;
assert_equal (Nil) (remove ('a')(Node('a',1,Node('a',2,Nil)))) ("Nil = remove ('a')(Node('a',1,Node('a',2,Nil)))") ;;
assert_equal (Node('b',1,Nil)) (remove ('a')(Node('b',1,Nil))) ("Node('b', 1, Nil) = remove ('a')(Node('b', 1, Nil))") ;;

(*  size tests *)
assert_equal (0) (size Nil) ("0 = size Nil") ;;
assert_equal (1) (size (Node('a',1,Nil))) ("1 = size Node('a',1,Nil)") ;;
assert_equal (5) (size (Node('a',1,Node('b',2,Node('c',3,Node('d',4,Node('e',5,Nil))))))) ("5 = size (Node('a', 1, Node('b', 2, Node('c', 3, Node('d', 4, Node('e', 5, Nil))))))") ;;

(*  has_key tests *)
assert_equal false (has_key ('a') (Nil)) ("false = has_key ('a') (Nil)") ;;
assert_equal true (has_key ('e') (Node('a',1,Node('b',2,Node('c',3,Node('d',4,Node('e',5,Nil))))))) ("true = has_key ('e') (Node('a',1,Node('b',2,Node('c',3,Node('d',4,Node('e',5,Nil)))))))") ;;

(*  keys tests *)
assert_equal [] (keys (Nil)) ("[] = keys (Nil)") ;;
assert_equal ['b';'a'] (keys (Node('b',2,Node('a',1,Nil)))) ("['b';'a'] = keys (Node('b',2,Node('a',1,Nil)))") ;;
assert_equal ['a';'b';'c';'d';'e'] (keys (Node('a',1,Node('b',2,Node('c',3,Node('d',4,Node('e',5,Nil))))))) ("['a';'b';'c';'d';'e'] = keys (Node('a', 1, Node('b', 2, Node('c', 3, Node('d', 4, Node('e', 5, Nil))))))") ;;

(*  values tests *)
assert_equal [] (values (Nil)) ("[] = values (Nil)") ;;
assert_equal [2;1] (values (Node('b',2,Node('a',1,Nil)))) ("[2;1] = values (Node('b',2,Node('a',1,Nil)))") ;;
assert_equal [1;2;3;4;5] (values (Node('a',1,Node('b',2,Node('c',3,Node('d',4,Node('e',5,Nil))))))) ("[1;2;3;4;5] = values (Node('a', 1, Node('b', 2, Node('c', 3, Node('d', 4, Node('e', 5, Nil))))))") ;;

(*  key_values tests *)
assert_equal [] (key_values('a')(Nil)) ("[] = key_values('a')(Nil)") ;;
assert_equal [1] (key_values('a')(Node('a',1,Nil))) ("[1] = key_values('a')(Node('a',1,Nil))") ;;
assert_equal [1;2;3] (key_values('a')(Node('a',1,Node('a',2,Node('a',3,Nil))))) ("[1;2;3] = key_values('a')(Node('a', 1, Node('a', 2, Node('a', 3, Nil))))") ;;
assert_equal [1;1;1] (key_values('a')(Node('a',1,Node('a',1,Node('a',1,Nil))))) ("[1;1;1] = key_values('a')(Node('a', 1, Node('a', 1, Node('a', 1, Nil))))") ;;
assert_equal [] (key_values('d')(Node('a',1,Node('b',2,Node('c',3,Nil))))) ("[] = key_values('d')(Node('a', 1, Node('b', 2, Node('c', 3, Nil))))") ;;

(*  combine tests *)
assert_equal (Node('a',1,Nil)) (combine (Node('a',1,Nil))(Nil)) ("Node('a', 1, Nil) = combine (Node('a', 1, Nil))(Nil)") ;;
assert_equal (Node('a',1,Nil)) (combine (Nil)(Node('a',1,Nil))) ("Node('a', 1, Nil) = combine (Nil)(Node('a', 1, Nil))") ;;
assert_equal (Node('a',1,Node('b',2,Nil))) (combine (Node('a',1,Nil)) (Node('b',2,Nil))) ("Node('a',1,Node('b',2,Nil)) = combine (Node('a',1,Nil)) (Node('b',2,Nil))") ;;

(*   invert tests *)
assert_equal (Node(1,'a',Nil)) (invert (Node('a',1,Nil))) ("Node(1, 'a', Nil) = invert (Node('a', 1, Nil))") ;;
assert_equal (Node(1,'a',Node(2,'b',Node(3,'c',Nil)))) (invert (Node('a',1,Node('b',2,Node('c',3,Nil))))) ("Node(1, 'a', Node(2, 'b', Node(3, 'c', Nil))) = invert (Node('a', 1, Node('b', 2, Node('c', 3, Nil))))") ;;

(*  group tests *)
assert_equal (Node('a',[1;2],Nil)) (group (Node('a',1,Node('a',2,Nil)))) ("Node('a', [1;2], Nil) = group (Node('a', 1, Node('a', 2, Nil)))") ;
assert_equal (Node('a',[1],Node('b',[2],Nil))) (group (Node('a',1,Node('b',2,Nil)))) ("Node('a',[1],Node('b',[2],Nil))) = group (Node('a',1,Node('b',2,Nil)))") ;;
assert_equal (Node('a',[1],Node('b',[2;3],Nil))) (group (Node('a',1,Node('b',2,Node('b',3,Nil))))) ("Node('a',[1],Node('b',[2;3],Nil))) = group (Node('a',1,Node('b',2,Node('b',3,Nil))))") ;;
assert_equal (Node('a',[1;2;3;4;5],Nil)) (group (Node('a',1,Node('a',2,Node('a',3,Node('a',4,Node('a',5,Nil))))))) ("Node('a', [1;2;3;4;5], Nil) = group (Node('a',1,Node('a',2,Node('a',3,Node('a',4,Node('a',5,Nil))))))") ;;

(*  kv_map tests *)
assert_equal (Node(1, true, Nil)) (kv_map(map_helper)(Node('a',1,Nil))) ("Node(1, true, Nil) = kv_map(map_helper)(Node('a',1,Nil))") ;;
assert_equal (Node(1, true, Node(2, false, Node(6, true, Nil)))) (kv_map(map_helper)(Node('a',1,Node('b', 10, Node('f', 3, Nil))))) ("Node(1, true, Node(2, false, Node(6, true, Nil))) = kv_map(map_helper)(Node('a',1,Node('b', 10, Node('f', 3, Nil)))))") ;;

(*  kv_filter tests *)
assert_equal (Node('a', 5, Nil)) (kv_filter (filter_helper)(Node('a', 5, Nil))) ("Node('a', 5, Nil) = kv_filter (filter_helper)(Node('a', 5, Nil))") ;;
assert_equal (Nil) (kv_filter (filter_helper)(Node('b', 5, Nil))) ("Nil = kv_filter (filter_helper)(Node('b', 5, Nil))") ;;

(*  join tests *)
assert_equal (Nil) (join (Nil)(Nil)) ("Nil = join (Nil)(Nil)") ;;
assert_equal (Node('a',[1],Nil)) (join (Node('a',1,Nil))(Nil)) ("Node('a', 1, Nil) = join (Node('a', 1, Nil)(Nil))") ;;
assert_equal (Node('a',[1],Nil)) (join (Nil)(Node('a',1,Nil))) ("Node('a', 1, Nil) = join (Nil)(Node('a', 1, Nil))") ;;
assert_equal (Node('a',[1;2],Nil)) (join (Node('a',1,Nil))(Node('a',2,Nil))) ("Node('a', [1;2], Nil) = join (Node('a', 1, Nil))(Node('a', 2, Nil))") ;;
assert_equal (Node('a',[1],Node('b',[2],Nil))) (join (Node('a',1,Nil))(Node('b',2,Nil))) ("Node('a', [1], Node('b', [2], Nil)) = join (Node('a', 1, Nil))(Node('b', 2, Nil))") ;;
assert_equal (Node('a',[1],Node('b',[2;3],Nil))) (join (Node('a',1,Nil))(Node('b',2,Node('b',3,Nil)))) ("Node('a', [1], Node('b', [2;3], Nil)) = join (Node('a', 1, Nil))(Node('b', 2, Node('b', 3, Nil)))") ;;
assert_equal (Node('a',[1;4],Node('b',[2;3],Nil))) (join (Node('a',1,Nil))(Node('b',2,Node('b',3,Node('a',4,Nil))))) ("Node('a', [1;4], Node('b', [2;3], Nil)) = join (Node('a', 1, Nil))(Node('b', 2, Node('b', 3, Node('a', 4, Nil))))") ;;








