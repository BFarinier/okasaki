module type S =
sig

  type 'a rlist

  exception Empty
  exception Index

  val empty : 'a rlist
  val is_empty : 'a rlist -> bool

  val cons : 'a -> 'a rlist -> 'a rlist
  val head : 'a rlist -> 'a
  val tail : 'a rlist -> 'a rlist

  val lookup : 'a rlist -> int -> 'a
  val update : 'a rlist -> int -> 'a -> 'a rlist

end


module RList : S =
struct

  type leaf
  type 'a node = ('a * 'a)
  type (_,_) tree =
    | Leaf : 'a -> ('a, leaf) tree
    | Node : ('a,'b) tree * ('a,'b) tree -> ('a,'b node) tree

  type ('a,'b) tlist =
    | Null
    | Zero of ('a,'b node) tlist
    | One of ('a,'b) tree * ('a,'b node) tlist
  type 'a rlist = ('a,leaf) tlist

  exception Empty
  exception Index


  let empty = Null

  let is_empty rlist = (rlist = Null)


  let rec insert_tree : type a  b. (a,b) tree -> (a,b) tlist -> (a,b) tlist =
    fun x tl -> match tl with
      | Null -> One (x, Null)
      | Zero l -> One (x, l)
      | One (t, l) ->
        let t = insert_tree (Node (x, t)) l in
        Zero t

  let cons x rl = insert_tree (Leaf x) rl

  let rec borrow_tree : type a b. (a,b) tlist -> ((a,b) tree * (a,b) tlist) =
    fun tl -> match tl with
      | Null -> raise Empty
      | Zero l ->
        let (Node (t1, t2), l) =  borrow_tree l in
        (t1, One (t2, l))
      | One (t, l) ->
        match l with
        | Null -> (t, Null)
        | _ -> (t, Zero l)

  let head : type a. a rlist -> a = fun rl ->
    let (Leaf x) =  fst (borrow_tree rl) in x

  let tail rl = snd (borrow_tree rl)


  let rec lookup_tree : type a b. (a,b) tree -> int -> int -> a =
    fun t pos len -> match t with
      | Leaf x ->
        if pos = 0 then x
        else raise Index
      | Node (t1, t2) ->
        if pos < len / 2 then lookup_tree t1 pos (len / 2)
        else lookup_tree t2 (pos - len / 2) (len / 2)

  let rec lookup_list : type a b. (a,b) tlist -> int -> int -> a =
    fun tl pos len -> match tl with
      | Null -> raise Index
      | Zero l -> lookup_list l pos (2 * len)
      | One (t, l) ->
        if pos < len then lookup_tree t pos len
        else lookup_list l (pos - len) (2 * len)

  let lookup rl pos = lookup_list rl pos 1


  let rec update_tree : type a b. (a,b) tree -> int -> a -> int -> (a,b) tree =
    fun t pos y len -> match t with
      | Leaf x ->
        if pos = 0 then (Leaf y)
        else raise Index
      | Node (t1, t2) ->
        if pos < len / 2 then Node (update_tree t1 pos y (len / 2), t2)
        else Node (t1, update_tree t2 (pos - len / 2) y (len / 2))

  let rec update_list : type a b. (a,b) tlist -> int -> a -> int -> (a,b) tlist =
    fun tl pos y len -> match tl with
      | Null -> raise Index
      | Zero l -> Zero (update_list l pos y (2 * len))
      | One (t, l) ->
        if pos < len then One (update_tree t pos y len, l)
        else One (t, update_list l (pos - len) y (2 * len))

  let update rl pos y = update_list rl pos y 1

end
