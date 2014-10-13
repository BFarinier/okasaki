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

	let is_empty l = (l = Null)


	let rec insert_tree : type a  b. (a,b) tree -> (a,b) tlist -> (a,b) tlist =
	fun x rlist -> match rlist with
	| Null -> Cons (One x, Null)
	| Cons (d, l) ->
		match d with
		| Zero -> Cons (One x, l)
		| One t ->
			let t = insert_tree (Node (x, t)) l in
			Cons (Zero, t)

	let cons x l = insert_tree (Leaf x) l

	let rec borrow_tree : type a b. (a,b) tlist -> ((a,b) tree * (a,b) tlist) = function
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

	let head : type a. a rlist -> a = fun l ->
		let (Leaf l) =  fst (borrow_tree l) in l

	let tail l = snd (borrow_tree l)

end
