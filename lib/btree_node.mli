open Base

(** A compareable value used a the key for a btree *)
type 'a btree_node = {
  keys: 'a array;
  children: 'a btree_node array;
  is_leaf: bool; 
  compare : 'a -> 'a -> int;
}

(**  *)
val create_node : compare:('a -> 'a -> int) -> 'a array -> 'a btree_node array -> bool -> 'a btree_node

(** This returns a value -1 to the length of array - 1 *)
val find_key_index : 'a -> 'a btree_node -> int  

(**  *)
val key_exists : 'a -> 'a btree_node -> bool