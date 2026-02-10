package io.vigilo.core

import io.vigilo.ValidatorMessages
import io.vigilo.core.macros.FieldInfo

import java.time.Instant
import java.time.format.DateTimeFormatter

object validators:

  type VType = ValidatorMessages ?=> Either[Seq[String], Unit]

  trait FieldValidator[T]:
    def validate(fieldName: String, validations: Seq[validation], value: T): VType

  object FieldValidator:
    def apply[A](using fv: FieldValidator[A]): FieldValidator[A] = fv

  private type VNumber = Short | Int | Long | Float | Double

  extension (n: VNumber)
    private infix def gt(i: VNumber): Boolean = (n, i) match
      case (x: Short, y: Short)   => x > y
      case (x: Int, y: Int)       => x > y
      case (x: Long, y: Long)     => x > y
      case (x: Float, y: Float)   => x > y
      case (x: Double, y: Double) => x > y
      case _                      => false

    private infix def lt(i: VNumber): Boolean = (n, i) match
      case (x: Short, y: Short)   => x < y
      case (x: Int, y: Int)       => x < y
      case (x: Long, y: Long)     => x < y
      case (x: Float, y: Float)   => x < y
      case (x: Double, y: Double) => x < y
      case _                      => false

    private infix def eq(i: VNumber): Boolean = (n, i) match
      case (x: Short, y: Short)   => x == y
      case (x: Int, y: Int)       => x == y
      case (x: Long, y: Long)     => x == y
      case (x: Float, y: Float)   => x == y
      case (x: Double, y: Double) => x == y
      case _                      => false

  def validateNumber(fieldName: String, value: VNumber, validations: Seq[validation])(using
    msg: ValidatorMessages
  ): Either[Seq[String], Unit] =
    def createStringMsg(key: String, args: Any*) =
      msg.message(s"validation.number.$key", msg.message(fieldName) +: args*)

    val results =
      validations
        .foldRight(Seq.empty[String]) { (v, acc) =>
          v match
            case number(min, max) =>
              (min, max) match
                case (0, 0)                                                     => acc
                case (x, 0) if value lt x                                       => acc :+ createStringMsg("min", min)
                case (0, x) if value gt x                                       => acc :+ createStringMsg("max", max)
                case (x, y) if x > 0 && y > 0 && ((value lt x) || (value gt y)) =>
                  acc :+ createStringMsg("size", min, max)
                case _                                                          => acc
            case required()       =>
              if value eq 0 then acc :+ createStringMsg("required") else acc
            case _                => acc
        }
    if results.nonEmpty
    then Left(results)
    else Right(())

  given StringValidator: FieldValidator[String] =
    new FieldValidator[String]:
      override def validate(fieldName: String, validations: Seq[validation], value: String): VType =
        val msg                                      = summon[ValidatorMessages]
        def createStringMsg(key: String, args: Any*) =
          msg.message(s"validation.$key", msg.message(fieldName) +: args*)

        val results =
          validations
            .foldRight(Seq.empty[String]) { (v, acc) =>
              v match
                case string(blank, empty, min, max) =>
                  (blank, empty) match
                    case (false, _) if value.isBlank =>
                      acc :+ createStringMsg("string.blank")
                    case (_, false) if value.isEmpty =>
                      acc :+ createStringMsg("string.blank")
                    case _                           =>
                      (min, max) match
                        case (0, 0)                                                             => acc
                        case (x, 0) if value.length < x                                         =>
                          acc :+ createStringMsg("string.min", x)
                        case (0, x) if value.length > x                                         =>
                          acc :+ createStringMsg("string.max", x)
                        case (x, y) if x > 0 && y > 0 && (value.length < x || value.length > y) =>
                          acc :+ createStringMsg("string.size", x, y)
                        case _                                                                  => acc
                case required()                     =>
                  value match
                    case null           => acc :+ createStringMsg("required")
                    case x if x.isEmpty => acc :+ createStringMsg("required")
                    case _              => acc
                case email()                        =>
                  value match
                    case null => acc :+ createStringMsg("email")
                    case s    =>
                      val emailRegex =
                        """^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r
                      if emailRegex.findFirstMatchIn(s).isDefined
                      then acc
                      else acc :+ createStringMsg("email")
                case regexp(pattern)                =>
                  value match
                    case null => acc :+ createStringMsg("regex")
                    case s    =>
                      if pattern.r.findFirstMatchIn(s).isDefined
                      then acc
                      else acc :+ createStringMsg("regex")
                case _                              => acc
            }
        if results.nonEmpty
        then Left(results)
        else Right(())

  given ShortValidator: FieldValidator[Short] =
    new FieldValidator[Short]:
      override def validate(fieldName: String, validations: Seq[validation], value: Short): VType =
        validateNumber(fieldName, value, validations)

  given IntValidator: FieldValidator[Int] =
    new FieldValidator[Int]:
      override def validate(fieldName: String, validations: Seq[validation], value: Int): VType =
        validateNumber(fieldName, value, validations)

  given LongValidator: FieldValidator[Long] =
    new FieldValidator[Long]:
      def validate(fieldName: String, validations: Seq[validation], value: Long): VType =
        validateNumber(fieldName, value, validations)

  given FloatValidator: FieldValidator[Float] =
    new FieldValidator[Float]:
      def validate(fieldName: String, validations: Seq[validation], value: Float): VType =
        validateNumber(fieldName, value, validations)

  given DoubleValidator: FieldValidator[Double] =
    new FieldValidator[Double]:
      def validate(fieldName: String, validations: Seq[validation], value: Double): VType =
        validateNumber(fieldName, value, validations)

  given RelationValidator: [A <: Relation] => FieldValidator[A] =
    new FieldValidator[A]:
      def validate(fieldName: String, validations: Seq[validation], value: A): VType =
        val msg     = summon[ValidatorMessages]
        val results =
          validations
            .foldRight(Seq.empty[String]) { (v, acc) =>
              v match
                case rel() =>
                  if value == null || !value.valid
                  then acc :+ msg.message("validation.rel", msg.message(fieldName))
                  else acc
                case _     => acc
            }

        if results.nonEmpty
        then Left(results)
        else Right(())

  given InstantValidator: FieldValidator[Instant] =
    new FieldValidator[Instant]:
      def validate(fieldName: String, validations: Seq[validation], value: Instant): VType =
        val msg     = summon[ValidatorMessages]
        val results =
          validations
            .foldRight(Seq.empty[String]) { (v, acc) =>
              v match
                case date(min = null, max = null)                                                => acc
                case date(min = min, max = null, pattern = pattern) if min != null               =>
                  if value.isBefore(min)
                  then
                    acc :+ msg.message(
                      "validation.date.min",
                      msg.message(fieldName),
                      DateTimeFormatter.ofPattern(pattern).format(min)
                    )
                  else acc
                case date(min = null, max = max, pattern = pattern) if max != null               =>
                  if value.isAfter(max)
                  then
                    acc :+ msg.message(
                      "validation.date.max",
                      msg.message(fieldName),
                      DateTimeFormatter.ofPattern(pattern).format(max)
                    )
                  else acc
                case date(min = min, max = max, pattern = pattern) if min != null && max != null =>
                  if value.isBefore(min)
                  then
                    acc :+ msg.message(
                      "validation.date.min",
                      msg.message(fieldName),
                      DateTimeFormatter.ofPattern(pattern).format(min)
                    )
                  else if value.isAfter(max)
                  then
                    acc :+ msg.message(
                      "validation.date.max",
                      msg.message(fieldName),
                      DateTimeFormatter.ofPattern(pattern).format(max)
                    )
                  else acc
                case _                                                                           => acc
            }

        if results.nonEmpty
        then Left(results)
        else Right(())

  given OptionValidator: [A: FieldValidator] => FieldValidator[Option[A]] =
    new FieldValidator[Option[A]]:
      def validate(fieldName: String, validations: Seq[validation], value: Option[A]): VType =
        val msg     = summon[ValidatorMessages]
        val results =
          validations
            .foldRight(Seq.empty[String]) { (v, acc) =>
              v match
                case required() =>
                  value match
                    case None | null => acc :+ msg.message("validation.required", msg.message(fieldName))
                    case Some(t)     =>
                      FieldValidator[A].validate(fieldName, validations, t) match
                        case Left(seq) => acc ++ seq
                        case _         => acc
                case _          => acc
            }
        if results.nonEmpty
        then Left(results)
        else Right(())

  given IterableValidator: [A, Col <: Iterable[A]] => FieldValidator[Col] =
    new FieldValidator[Col]:
      def validate(fieldName: String, validations: Seq[validation], value: Col): VType =
        val msg     = summon[ValidatorMessages]
        val results =
          validations
            .foldRight(Seq.empty[String]) { (v, acc) =>
              v match
                case required()                                                                     =>
                  value match
                    case it if it.isEmpty => acc :+ msg.message("validation.required", msg.message(fieldName))
                    case _                => acc
                case options(opts)                                                                  =>
                  if value.exists(x => !opts.contains(x))
                  then acc :+ msg.message("validation.options", msg.message(fieldName), opts)
                  else acc
                case list(min, 0) if value.size < min                                               =>
                  acc :+ msg.message("validation.list.min", msg.message(fieldName), min)
                case list(0, max) if value.size > max                                               =>
                  acc :+ msg.message("validation.list.max", msg.message(fieldName), max)
                case list(min, max) if min > 0 && max > 0 && (value.size < min || value.size > max) =>
                  acc :+ msg.message("validation.list.size", msg.message(fieldName), min, max)
                case _                                                                              => acc
            }

        if results.nonEmpty
        then Left(results)
        else Right(())

  private[vigilo] def optionsValidator(
    fields: Seq[FieldInfo],
    values: Seq[(String, Any)]
  ): ValidatorMessages ?=> Either[Map[String, String], Unit] =
    val msg         = summon[ValidatorMessages]
    val optionsAnno =
      fields
        .filterNot { fd =>
          fd.annotations.exists {
            case _: list => true
            case _       => false
          }
        }
        .filter { fd =>
          fd.annotations.exists {
            case _: options => true
            case _          => false
          }
        }
        .collect { it =>
          val first = it.annotations.collectFirst { case opt: options => opt }.get
          (fieldName = it.name, opts = first.values)
        }

    val results =
      optionsAnno.foldRight(Map.empty[String, String]) { (v, acc) =>
        val fieldName = v.fieldName
        val opts      = v.opts
        values.find(_._1 == fieldName) match
          case Some(s) =>
            if opts.contains(s._2)
            then acc
            else acc + (fieldName -> msg.message("validation.values", fieldName, opts))
          case None    =>
            acc
      }

    if results.nonEmpty
    then Left(results)
    else Right(())

  private[vigilo] def choicesValidator(
    fields: Seq[FieldInfo],
    values: Seq[(String, Any)]
  ): ValidatorMessages ?=> Either[Map[String, String], Unit] =
    val msg = summon[ValidatorMessages]

    val choicesAnno =
      fields
        .filter { fd =>
          fd.annotations.exists {
            case _: choice => true
            case _         => false
          }
        }
        .collect { it =>
          val first = it.annotations.map(_.asInstanceOf[choice]).head
          (group = first.group, fieldName = it.name, min = first.min, max = first.max)
        }
        .groupBy(_.group)

    val results =
      choicesAnno.foldRight(Map.empty[String, String]) { (v, acc) =>
        val choice = v._2.head

        val names = v._2.map(_.fieldName)
        val count = values
          .filter(t => names.contains(t._1))
          .map(_._2)
          .count {
            case Some(_) => true
            case _       => false
          }

        choice match
          case (min = 0, max = 0)                                               => acc
          case (min = x, max = 0) if count < x                                  =>
            acc ++ names.map(s => s -> msg.message("validation.choices.min", s, x, names)).toMap
          case (min = 0, max = x) if count > x                                  =>
            acc ++ names.map(s => s -> msg.message("validation.choices.max", s, x, names)).toMap
          case (min = x, max = y) if x > 0 && y > 0 && (count < x || count > y) =>
            acc ++ names.map(s => s -> msg.message("validation.choices.size", s, x, y, names)).toMap
          case _                                                                => acc
      }

    if results.nonEmpty
    then Left(results)
    else Right(())
