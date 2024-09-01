open Base

type 'a btree_node = {
  keys: 'a array;
  children: 'a btree_node array;
  is_leaf: bool; 
}
let create_node keys children is_leaf = { keys; children; is_leaf }

let find ~compare key node = 
  let rec binarysearch key array min max = 
    if min > max then min
    else 
      (* +1 for int ceil *)
      let mid = (min + max + 1) / 2 in
      let mid_key = array.(mid) in
      let cmp_result = compare key mid_key in
        match cmp_result with
        | 0 -> mid
        | -1 -> binarysearch key array min (mid - 1)
        | 1 -> binarysearch key array (mid + 1) max
        | _ -> failwith "Invalid comparison result"
  in
  binarysearch key node.keys 0 (Array.length node.keys - 1) 

let%test "find key in the middle" =
let node = create_node [|1; 3; 5; 7; 9|] [||] true in
find ~compare 5 node = 2

 let%test "find key atthe beginning" =
let node = create_node [|1; 3; 5; 7; 9|] [||] true in
find ~compare 1 node = 0

let%test "find key at the end" =
let node = create_node [|1; 3; 5; 7; 9|] [||] true in
find ~compare 9 node = 4

let%test "find key not in the node (smaller than all keys)" =
let node = create_node [|3; 5; 7; 9|] [||] true in
find ~compare 1 node = 0 

let%test "find key not in the node (between keys)" =
let node = create_node [|1; 3; 5; 7|] [||] true in
find ~compare 4 node = 2

let%test "find key not in the node (larger than all keys)" =
let node = create_node [|1; 3; 5; 7|] [||] true in
find ~compare 10 node = 4 

let%test "find key in a string node" =
let node = create_node [|"a"; "b"; "c"; "d"; "e"|] [||] true in
find ~compare:String.compare "b" node = 1