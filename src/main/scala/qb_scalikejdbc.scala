package com.todesking.qb

import scala.language.implicitConversions

import _root_.scalikejdbc.interpolation.SQLSyntax

package object scalikejdbc {
  private type SSSF = _root_.scalikejdbc.SQLSyntaxSupportFeature

  implicit def SQLSyntax2Column(s:SQLSyntax):Column[Any] = Column(s.value)
  def col(raw:SQLSyntax):Column[Any] = Column(raw.value)
  def table(raw:SSSF#SQLSyntaxSupport[_]) = IdRefRelations(raw.tableNameWithSchema)
  def table(raw:SSSF#QuerySQLSyntaxProvider[_, _]) = IdRefRelations(raw.tableAliasName)
}
