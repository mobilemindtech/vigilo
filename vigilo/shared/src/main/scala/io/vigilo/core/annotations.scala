package io.vigilo.core

import java.time.Instant
import scala.annotation.StaticAnnotation

trait Relation:
  def valid: Boolean

sealed trait validation extends StaticAnnotation

// validate null, None, empty string, zero number and rel.valid
case class required() extends validation

case class string(blank: Boolean = false, empty: Boolean = false, min: Int = 0, max: Int = 0) extends validation

// work with Relation or Option[Relation]
case class rel() extends validation

// work with String size or number or Option[String | number]
case class number(min: Int = 0, max: Int = 0) extends validation

// work with String or Option[String]
case class email() extends validation

// work with String or Option[String]
case class regexp(pattern: String) extends validation

// Work with any value
case class choice(group: String, min: Int = 1, max: Int = 1) extends validation

// Work with any value
case class options(values: Seq[Any]) extends validation

case class list(min: Int = 0, max: Int = 0) extends validation

case class date(min: Instant | Null = null, max: Instant | Null = null, pattern: String, time: Boolean = false)
    extends validation
