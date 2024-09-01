open Base

(** A compareable value used a the key for a btree *)
type 'a btree_node = {
  keys: 'a array;
  children: 'a btree_node array;
  is_leaf: bool; 
}

(**  *)
val create_node : 'a array -> 'a btree_node array -> bool -> 'a btree_node

(**  *)
val find : compare:('a -> 'a -> int) -> 'a -> 'a btree_node -> int  