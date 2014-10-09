package com.todesking.qb

import org.specs2.mutable._

class QBTest extends Specification {
  "QB" should {
    "build real-world query" in {
      import scala.language.reflectiveCalls
      val data = new AnyRef {
        val name = Some("foo")
        val status:Option[Int] = None
        val categoryId = 1
      }

      import com.todesking.qb.QueryInterpolation._

      var query = QB.builder()

      query.from += table"items" as "i"

      query.where += data.name map {name => col"name" like s"%${name}%"}
      query.where += data.status map { status => col"status" eq status}

      query.where +=
        table"categories" as "c" where(col"c.id" eq col"i.category_id") exists()
      query.where +=
        col"category_id" in(table"categories" select(col"id"))
      query.where += col"category_id" eq data.categoryId

      val statement = Sql.buildQuery(query.toRelations)
      println(statement.sql)
      statement.parameters === Seq("%foo%", 1)

      pending
    }

    "optimize nested relations to flatten form" in {
      import com.todesking.qb.QueryInterpolation._
      def filter(rel:Relations, cond:RelationsFilter) = FilteredRelations(rel, cond)
      val foo = table"foo"
      val c1 = col"c1"
      val c2 = col"c2"
      val c3 = col"c3"

      QB.optimize(foo) === foo

      QB.optimize(filter(filter(foo, c1 eq 1), c2 eq "bar")) === filter(foo, (c1 eq 1) and (c2 eq "bar"))

      QB.optimize(filter(filter(filter(foo, c1 eq 1), c2 eq 2), c3 eq 3)) === filter(foo, (c1 eq 1) and (c2 eq 2) and (c3 eq 3))

      QB.optimize(table"t1" select(col"name") where(col"id" eq 1)) === (table"t1" where(col"id" eq 1) select(col"name"))

      QB.optimize(Relations.one prod Relations.one) === Relations.one
      QB.optimize(Relations.zero prod Relations.one) == Relations.zero
      QB.optimize(Relations.zero prod Relations.zero) === Relations.zero
      QB.optimize(Relations.one prod foo) === foo
      QB.optimize(foo prod Relations.one) === foo
    }

    "build sql query" in {
      import com.todesking.qb.QueryInterpolation._
      Sql.buildQuery(table"foo") === SqlData("SELECT * FROM foo")

      Sql.buildQuery(table"foo" select(col"id")) === SqlData("SELECT id FROM foo")

      Sql.buildQuery(table"foo" as "bar") === SqlData("SELECT * FROM foo AS bar")

      Sql.buildQuery(table"foo" as "bar" where(col"a" eq 1) select(col"id")) === SqlData("SELECT id FROM foo AS bar WHERE a = ?", Seq(1))

      Sql.buildQuery(table"foo" prod table"bar") === SqlData("SELECT * FROM foo, bar")

      Sql.buildQuery(table"foo" prod table"bar" where(col"id" eq 1) select(col"a")) === SqlData("SELECT a FROM foo, bar WHERE id = ?", Seq(1))
    }
  }
}
