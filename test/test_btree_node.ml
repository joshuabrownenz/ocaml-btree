open OUnit2
open Btree_node

let find_key_in_the_middle _ =
  let node = create_node ~compare [|1; 3; 5; 7; 9|] [||] true in
  assert_equal (find_key_index 5 node) 2

let find_key_at_the_beginning _ =
  let node = create_node ~compare [|1; 3; 5; 7; 9|] [||] true in
  assert_equal (find_key_index 1 node) 0

let find_key_at_the_end _ =
let node = create_node  ~compare [|1; 3; 5; 7; 9|] [||] true in
assert_equal (find_key_index 9 node) 4

let find_key_not_in_the_node_smaller_than_all_keys _ =
let node = create_node  ~compare [|3; 5; 7; 9|] [||] true in
assert_equal (find_key_index 1 node) 0 

let find_key_not_in_the_node_between_keys _ =
let node = create_node  ~compare [|1; 3; 5; 7|] [||] true in
assert_equal (find_key_index 4 node) 2

let find_key_not_in_the_node_larger_than_all_keys _ =
let node = create_node  ~compare [|1; 3; 5; 7|] [||] true in
assert_equal (find_key_index 10 node) 4 

let find_key_in_a_string_node _ =
let node = create_node  ~compare:String.compare [|"a"; "b"; "c"; "d"; "e"|] [||] true in
assert_equal (find_key_index "b" node) 1

let create_btree _ = 
  let leaf_node_1 = create_node ~compare [|1; 4|] [||] true in
  let leaf_node_2 = create_node ~compare [|9|] [||] true in
  let leaf_node_3 = create_node ~compare [|11; 12|] [||] true in
  let leaf_node_4 = create_node ~compare [|13|] [||] true in
  let leaf_node_5 = create_node ~compare [|16|] [||] true in
  let leaf_node_6 = create_node ~compare [|30|] [||] true in
  let leaf_node_7 = create_node ~compare [|38; 42|] [||] true in
  let mid_node_1 = create_node ~compare [|9; 11|] [|leaf_node_1; leaf_node_2; leaf_node_3|] false in
  let mid_node_2 = create_node ~compare [|16|] [|leaf_node_4; leaf_node_5|] false in
  let mid_node_3 = create_node ~compare [|38|] [|leaf_node_6; leaf_node_7|] false in
  create_node ~compare [|13; 30|] [|mid_node_1; mid_node_2; mid_node_3|] false

let test_key_exists_on_btree _ =
  let btree = create_btree () in
  assert_equal (key_exists 1 btree) true;
  assert_equal (key_exists 4 btree) true;
  assert_equal (key_exists 9 btree) true;
  assert_equal (key_exists 11 btree) true;
  assert_equal (key_exists 12 btree) true;
  assert_equal (key_exists 13 btree) true;
  assert_equal (key_exists 16 btree) true;
  assert_equal (key_exists 30 btree) true;
  assert_equal (key_exists 38 btree) true;
  assert_equal (key_exists 42 btree) true;
  assert_equal (key_exists 2 btree) false;
  assert_equal (key_exists 3 btree) false;
  assert_equal (key_exists 5 btree) false;
  assert_equal (key_exists 10 btree) false;
  assert_equal (key_exists 14 btree) false;
  assert_equal (key_exists 15 btree) false;
  assert_equal (key_exists 17 btree) false;
  assert_equal (key_exists 29 btree) false;
  assert_equal (key_exists 31 btree) false;
  assert_equal (key_exists 37 btree) false;
  assert_equal (key_exists 39 btree) false;
  assert_equal (key_exists 41 btree) false;
  assert_equal (key_exists 43 btree) false

let () =
  run_test_tt_main
    ("btree_node_tests" >:::
       [
          "find key in the middle" >:: find_key_in_the_middle;
          "find key at the beginning" >:: find_key_at_the_beginning;
          "find key at the end" >:: find_key_at_the_end;
          "find key not in the node smaller than all keys" >:: find_key_not_in_the_node_smaller_than_all_keys;
          "find key not in the node between keys" >:: find_key_not_in_the_node_between_keys;
          "find key not in the node larger than all keys" >:: find_key_not_in_the_node_larger_than_all_keys;
          "find key in a string node" >:: find_key_in_a_string_node;
          "test key exists on btree" >:: test_key_exists_on_btree
       ])