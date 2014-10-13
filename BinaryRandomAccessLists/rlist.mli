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


module RList : S
