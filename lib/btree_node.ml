open Base

type 'a btree_node = {
  keys: 'a array;
  children: 'a btree_node array;
  is_leaf: bool; 
  compare : 'a -> 'a -> int;
}

let create_node ~compare keys children is_leaf = { keys; children; is_leaf; compare}

let find_key_index key node = 
  let rec binarysearch key array min max = 
    if min > max then min
    else 
      (* +1 for int ceil *)
      let mid = (min + max + 1) / 2 in
      let mid_key = array.(mid) in
      let cmp_result = node.compare key mid_key in
        match cmp_result with
        | 0 -> mid
        | -1 -> binarysearch key array min (mid - 1)
        | 1 -> binarysearch key array (mid + 1) max
        | _ -> failwith "Invalid comparison result"
  in
  binarysearch key node.keys 0 (Array.length node.keys - 1) 
  

let rec key_exists key node = 
  let index = find_key_index key node in
      if index > Array.length node.keys then false
      else if node.compare node.keys.(index) key = 0 then true 
      else if node.is_leaf then false
      else let child = node.children.(index) in
        key_exists key child