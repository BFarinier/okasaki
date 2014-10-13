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
end


module RList : S =
struct

	type leaf
	type 'a node = ('a * 'a)
	type (_,_) tree = Leaf : 'a -> ('a, leaf) tree | Node : ('a,'b) tree * ('a,'b) tree -> ('a,'b node) tree

	type ('a,'b) digit = Zero | One of ('a,'b) tree
	type ('a,'b) tlist = Null | Cons of ('a,'b) digit * ('a,'b node) tlist
	type 'a rlist = ('a,leaf) tlist

	exception Empty
	exception Index


	let empty = Null

	let is_empty rlist = (rlist = Null)


	let rec insert_tree : type a  b. (a,b) tree -> (a,b) tlist -> (a,b) tlist =
	fun x tl -> match tl with
	| Null -> Cons (One x, Null)
	| Cons (d, l) ->
		match d with
		| Zero -> Cons (One x, l)
		| One t ->
			let t = insert_tree (Node (x, t)) l in
			Cons (Zero, t)

	let cons x rl = insert_tree (Leaf x) rl

	let rec borrow_tree : type a b. (a,b) tlist -> ((a,b) tree * (a,b) tlist) =
	fun tl -> match tl with
	| Null -> raise Empty
	| Cons (d, l) ->
		match d with
		| Zero ->
			let (Node (t1, t2), l) =  borrow_tree l in
			(t1, Cons (One t2, l))
		| One t ->
			match l with
			| Null -> (t, Null)
			| _ -> (t, Cons (Zero, l))

	let head : type a. a rlist -> a = fun rl ->
		let (Leaf x) =  fst (borrow_tree rl) in x

	let tail rl = snd (borrow_tree rl)


	let rec lookup_tree : type a b. (a,b) tree -> int -> int -> a =
	fun t pos len -> match t with
	| Leaf x ->
		if pos = 0 then x
		else raise Index
	| Node (t1, t2) ->
		let len = len / 2 in
		if pos < len then lookup_tree t1 pos len
		else lookup_tree t2 (pos - len) len

	let rec lookup_list : type a b. (a,b) tlist -> int -> int -> a =
	fun tl pos len -> match tl with
	| Null -> raise Index
	| Cons (d, l) ->
		match d with
		| Zero -> lookup_list l pos (2 * len)
		| One t ->
			if pos < len then lookup_tree t pos len
			else lookup_list l (pos - len) (2 * len)

	let lookup rl pos = lookup_list rl pos 1

end
