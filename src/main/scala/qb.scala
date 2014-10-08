package com.todesking.qb

object QueryInterpolation {
  implicit class StringContextExt(c:StringContext) {
    def table(args:Any*) = new NamedRelations(toS(args))
    def col(args:Any*) = Column[Any](toS(args))

    private def toS(args:Any*):String =
      c.parts.zipAll(args, "", "").mkString("")
  }
}

object QB {
  def builder() = new Builder()
}

// Set of rows
class Relations {
  def as(newName:String) = new NamedRelations(newName)
  def where(cond:RelationsFilter) = new FilteredRelations(this, cond)
  def exists(rel:Relations) = new FilteredRelations(this, Exists(rel))
  def exists() = Exists(this)
  def select(cols:Column[_]*) = new Relations
  def prod(rhs:Relations) = new ProdRelations(this, rhs)
}

object Relations {
  def empty() = new Relations
}

class NamedRelations(name:String) extends Relations

class FilteredRelations(rel:Relations, condition:RelationsFilter) extends Relations

class ProdRelations(lhs:Relations, rhs:Relations) extends Relations

// Condition for filtering relations
sealed abstract class RelationsFilter
case class Like[A](col:Column[A], pat:String) extends RelationsFilter
case class Eq[A](col:Column[A], value:Any) extends RelationsFilter
case class Exists[A](rel:Relations) extends RelationsFilter
case class In[A](col:Column[A], rel:Relations) extends RelationsFilter

case class Column[A](name:String) {
  def like(pat:String) = Like(this, pat)
  def eq(value:Any) = Eq(this, value)
  def in(rel:Relations) = In(this, rel)
}

class Builder {
  import scala.collection.mutable.ArrayBuffer

  val from = new OptionAwareBuffer[Relations]
  val where = new OptionAwareBuffer[RelationsFilter]

  def toRelations():Relations = {
    where.foldLeft(
      from.foldLeft(Relations.empty()) {(r, f) => r prod f }
    ) {(r, w) => r where(w) }
  }
}

class OptionAwareBuffer[A] extends scala.collection.mutable.ArrayBuffer[A] {
  def +=(value:Option[A]):this.type = {
    value.map { v => this += v }
    this
  }
}
