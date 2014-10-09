# QB: Generic query builder

Goal: Framework independent query builder

Current target: Scalikejdbc

```scala
import com.todesking.qb
import com.todesking.qb.QueryInterpolation._

qb.Sql.buildQuery(
  table"users"
    .where(col"age" eq 20)
    .where(col"name" like "Alice%")
    .where(col"id" in (table"owners" select(col"user_id")))
)

=> SqlData("SELECT * FROM users WHERE ((age = ? AND name LIKE ?) AND id IN (SELECT user_id FROM owners))",List(20, "Alice%"))
```
