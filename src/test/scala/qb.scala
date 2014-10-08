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

      val statement = QB.createSqlDataFrom(query.toRelations)
      println(statement.sql)
      statement.parameters === Seq("%foo%", 1)

      pending
    }
  }
}
