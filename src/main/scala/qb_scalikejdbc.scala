package com.todesking.qb

import scala.language.implicitConversions

import _root_.scalikejdbc.interpolation.SQLSyntax

package object scalikejdbc {
  private type SQLSyntaxSupport[A] = _root_.scalikejdbc.SQLSyntaxSupportFeature#SQLSyntaxSupport[A]

  implicit def SQLSyntax2Column(s:SQLSyntax):Column[Any] = Column(s.value)
  def col(raw:SQLSyntax):Column[Any] = Column(raw.value)
  def table(raw:SQLSyntaxSupport[_]) = IdRefRelations(raw.tableNameWithSchema)
}
