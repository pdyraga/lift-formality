package net.liftweb
package common

// WHOA NELLY
// Slightly redefined HLists (we want this in Lift). This allows HLists to be
// covariant in their parameters, which makes things work properly.

object HListies {

  /**
   * The trait that defines HLists
   */
  sealed trait HList

  implicit final class HListMethods[ListSoFar <: HList](hlist: ListSoFar) extends AnyRef {
    // HNil here refers to our type alias below, even in the pattern match, so
    // we declare a local version of the singleton to properly do pattern
    // matching. Some bizarre Scala compiler thing, but it works and it's very
    // localized, so I'm down.
    private[this] val LocalHnil = HNil

    def :+:[T](v: T): :+:[T, ListSoFar] = {
      HListies.:+:(v, hlist)
    }

    def length: Int = {
      hlist match {
        case LocalHnil =>
          0
        case head :+: rest =>
          1 + rest.length
      }
    }
  }

  /**
   * The last element of an HList
   */
  case object HNil extends HList {
    override def toString = "HNil"
  }

  type HNil = HNil.type

  /**
   * The HList cons cell
   */
  final case class :+:[+H, +T <: HList](head: H, tail: T) extends HList {
    override def toString = head + " :+: " + tail
  }

  //type :+:[+H, +T <: HList] = HCons[H, T]

  /*object :+: {
    def unapply[H, T <: HList](in: HCons[H, T]): Option[(H, T)] = Some(in.head, in.tail)
  }*/

}
