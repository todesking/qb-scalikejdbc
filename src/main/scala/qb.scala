package com.todesking.qb

object QueryInterpolation {
  implicit class StringContextExt(c:StringContext) {
    def table(args:Any*) = new IdRefRelations(toS(args))
    def col(args:Any*) = Column[Any](toS(args))

    private def toS(args:Seq[Any]):String = {
      c.parts.zipAll(args, "", "").map{case (a, b) => a + b}.mkString("")
    }
  }
}

abstract class SqlDiarect {
  def buildQuery(rel:Relations):SqlData
}

object Sql extends SqlDiarect {
  override def buildQuery(rel:Relations):SqlData = {
    QB.optimize(rel) match {
      case Relations.zero => SqlData("SELECT 1 WHERE 1 = 0")
      case Relations.one => SqlData("SELECT 1 as __dummy__")
      case ProjectRelations(IdRefRelations(name), cols) =>
        SqlData(s"SELECT ${cols.map(_.name).mkString(", ")} FROM ${name}")
      case ProjectRelations(rel, cols) =>
        SqlData(s"SELECT ${cols.map(_.name).mkString(", ")} FROM ") + buildQuery(rel).closed
      case r@(IdRefRelations(_)|NamedRelations(_, _)) =>
        SqlData("SELECT * FROM ") + buildFromPart(r)
      case FilteredRelations(rel, cond) =>
        SqlData("SELECT * FROM ") + buildFromPart(rel) + SqlData(" WHERE ") + createWhere(cond)
      case ProdRelations(lhs, rhs) =>
        SqlData("SELECT * FROM ") + buildFromPart(lhs).closed + SqlData(", ") + buildFromPart(rhs).closed
    }
  }

  def buildFromPart(rel:Relations):SqlData = rel match {
    case NamedRelations(name, rel) =>
      buildFromPart(rel) + SqlData(s" AS ${name}")
    case IdRefRelations(name) =>
      SqlData(name)
    case r =>
      buildQuery(r).closed
  }

  def createWhere(cond:RelationsFilter):SqlData = {
    cond match {
      case Like(col, pat) => SqlData(s"${col.name} LIKE ") + createValue(pat)
      case Eq(col, value) => SqlData(s"${col.name} = ") + createValue(value)
      case Exists(rel) => SqlData("EXISTS ") + buildQuery(rel)
      case In(col, rel) => SqlData(s"${col.name} IN ") + buildQuery(rel)
      case And(lhs, rhs) => SqlData("(") + createWhere(lhs) + SqlData(" AND ") + createWhere(rhs) + SqlData(")")
    }
  }

  def createValue(value:ValueRef):SqlData = value match {
    case ConstantValue(v) => SqlData("?", Seq(v))
    case ColumnValue(col) => SqlData(s"${col.name}")
  }

}

case class SqlData(sql:String, parameters:Seq[Any] = Seq.empty) {
  def +(rhs:SqlData):SqlData = SqlData(
    this.sql + rhs.sql, this.parameters ++ rhs.parameters)
  def closed() = SqlData(s"(${sql})", parameters)
}

object QB {
  def builder() = new Builder()
  def optimize(rel:Relations):Relations = {
    val o1 = optimize1(optimizeSubTree(rel))
    if(o1 == rel) o1
    else optimize(o1)
  }
  def optimizeSubTree(rel:Relations):Relations = rel match {
    case FilteredRelations(r, c) => FilteredRelations(optimize(r), c)
    case r => r
  }
  def optimize1(rel:Relations):Relations = rel match {
    case FilteredRelations(FilteredRelations(base, cond1), cond2) =>
      FilteredRelations(base, cond1 and cond2)
    case FilteredRelations(ProjectRelations(r, cols), cond) =>
      ProjectRelations(FilteredRelations(r, cond), cols)
    case ProdRelations(Relations.one, r) => r
    case ProdRelations(r, Relations.one) => r
    case ProdRelations(Relations.zero, r) => Relations.zero
    case ProdRelations(r, Relations.zero) => Relations.zero
    case r => r
  }
}

// Set of rows
sealed abstract class Relations {
  def as(newName:String) = new NamedRelations(newName, this)
  def where(cond:RelationsFilter) = new FilteredRelations(this, cond)
  def exists(rel:Relations) = new FilteredRelations(this, Exists(rel))
  def exists() = Exists(this)
  def select(cols:Column[_]*) = new ProjectRelations(this, cols)
  def prod(rhs:Relations) = new ProdRelations(this, rhs)
}

object Relations {
  case object one extends Relations
  case object zero extends Relations
}

case class ProjectRelations(rel:Relations, cols:Seq[Column[_]]) extends Relations
case class IdRefRelations(name:String) extends Relations
case class NamedRelations(name:String, rel:Relations) extends Relations
case class FilteredRelations(rel:Relations, condition:RelationsFilter) extends Relations
case class ProdRelations(lhs:Relations, rhs:Relations) extends Relations

sealed abstract class ValueRef
case class ConstantValue(value:Any) extends ValueRef
case class ColumnValue[A](value:Column[A]) extends ValueRef

// Condition for filtering relations
sealed abstract class RelationsFilter {
  def and(rhs:RelationsFilter) = And(this, rhs)
}
case class Like[A](col:Column[A], pat:ValueRef) extends RelationsFilter
case class Eq[A](col:Column[A], value:ValueRef) extends RelationsFilter
case class Exists[A](rel:Relations) extends RelationsFilter
case class In[A](col:Column[A], rel:Relations) extends RelationsFilter
case class And(lhs:RelationsFilter, rhs:RelationsFilter) extends RelationsFilter

case class Column[A](name:String) {
  def like(pat:String) = Like(this, ConstantValue(pat))
  def eq(value:Any) = Eq(this, ConstantValue(value))
  def eq(value:Column[_]) = Eq(this, ColumnValue(value))
  def in(rel:Relations) = In(this, rel)
}

class Builder {
  import scala.collection.mutable.ArrayBuffer

  val from = new OptionAwareBuffer[Relations]
  val where = new OptionAwareBuffer[RelationsFilter]

  def toRelations():Relations = {
    where.foldLeft(
      from.foldLeft[Relations](Relations.one) {(r, f) => r prod f }
    ) {(r, w) => r where(w) }
  }
}

class OptionAwareBuffer[A] extends scala.collection.mutable.ArrayBuffer[A] {
  def +=(value:Option[A]):this.type = {
    value.map { v => this += v }
    this
  }
}
