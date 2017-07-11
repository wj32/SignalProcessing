namespace SignalProcessing

module Heap =
  type T<'a when 'a : comparison>

  val create : unit -> 'a T
  val createWithCapacity : capacity : int -> 'a T

  val add : 'a T -> value : 'a -> unit
  val remove : 'a T -> 'a
  val tryRemove : 'a T -> 'a option
  val replace : 'a T -> value : 'a -> 'a
  val tryReplace : 'a T -> value : 'a -> 'a option
  val clear : 'a T -> unit

  val count : 'a T -> int
  val isEmpty : 'a T -> bool
  val top : 'a T -> 'a
  val tryTop : 'a T -> 'a option

  val toArray : 'a T -> 'a array
