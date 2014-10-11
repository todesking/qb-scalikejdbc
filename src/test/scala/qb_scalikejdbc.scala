package com.todesking.qb

import org.specs2.mutable._
import org.specs2.execute.AsResult

import _root_.scalikejdbc._, SQLInterpolation._

class ScalikeJdbcIntegrationTest extends Specification {
  import com.todesking.qb.scalikejdbc._
  import com.todesking.qb.QueryInterpolation._

  Class.forName("org.h2.Driver")
  ConnectionPool.singleton("jdbc:h2:mem:test", "sa", "")

  implicit val session = AutoSession

  trait ctx extends Around {
    override def around[T:AsResult](t: =>T) = {
      sql"create table person(id integer not null primary key, name varchar(255) not null)".update().apply()
      try { AsResult(t) } finally { sql"drop table person".update().apply() }
    }
  }


  case class Person(id:Int, name:String)
  object Person extends SQLSyntaxSupport[Person]

  "QB with Scalikejdbc" should {
    "integrate column names" in new ctx {
      val p = Person.syntax("p")

      col(p.id) === col"p.id"
    }
  }
}
