package com.todesking.qb

import scala.language.implicitConversions

import _root_.scalikejdbc.interpolation.SQLSyntax

package object scalikejdbc {
  implicit def SQLSyntax2Column(s:SQLSyntax):Column[Any] = Column(s.value)
  def col(raw:SQLSyntax):Column[Any] = Column(raw.value)
}
