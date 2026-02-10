package io

import io.vigilo.core.macros.*
import io.vigilo.core.validators.*

import scala.compiletime.*
import scala.deriving.Mirror

package object vigilo:

  export io.vigilo.core.{choice, email, list, number, options, regexp, rel, required, string, validation, Relation}

  export Validator.{validate, validateWithIgnoreFields}

  export io.vigilo.core.validators.FieldValidator

  trait ValidatorMessages:
    def message(code: String, args: Any*): String

  trait Validator[T]:
    def validate(value: T): ValidatorMessages ?=> Result
    def validateWithIgnoreFields(value: T, fields: String*): ValidatorMessages ?=> Result

  case class Result(messages: Map[String, Seq[String]]):
    def isValid: Boolean = messages.isEmpty

  object Validator:

    private def findAnnotations(fields: Seq[FieldInfo], fieldName: String) =
      fields
        .find(_.name == fieldName)
        .map(_.annotations)
        .getOrElse(Nil)

    extension [T](value: T)(using validator: Validator[T])
      def validate: ValidatorMessages ?=> Result                                  = validator.validate(value)
      def validateWithIgnoreFields(fields: String*): ValidatorMessages ?=> Result =
        validator.validateWithIgnoreFields(value, fields*)

    inline def apply[A](using validator: Validator[A]): Validator[A] = validator

    private def validateOthers(value: Any, fields: Seq[FieldInfo]): ValidatorMessages ?=> Map[String, String] =
      val product = value.asInstanceOf[Product]
      val values  =
        product.productElementNames.zipWithIndex.map { case (name, i) =>
          (name, product.productElement(i))
        }.toSeq

      val vchoices =
        choicesValidator(fields, values) match
          case Right(()) => Map.empty
          case Left(map) => map
      val voptions =
        optionsValidator(fields, values) match
          case Right(()) => Map.empty
          case Left(map) => map

      vchoices ++ voptions

    inline given derived: [A] => (m: Mirror.Of[A], msg: ValidatorMessages) => Validator[A] =

      val fields = extractValidationsFields[A]
      type Mets  = m.MirroredElemTypes
      type Mels  = m.MirroredElemLabels
      type Label = m.MirroredLabel

      inline m match
        case p: Mirror.ProductOf[A] =>
          new Validator[A]:
            override def validate(value: A): ValidatorMessages ?=> Result =

              val othersResults =
                validateOthers(value, fields).map { (k, v) =>
                  k -> Seq(v)
                }

              val results = runValidations[A, Mets, Mels](value.asInstanceOf[Product], fields)

              Result(results ++ othersResults)

            override def validateWithIgnoreFields(value: A, ignoreFields: String*): ValidatorMessages ?=> Result =

              val othersResults =
                validateOthers(value, fields).map { (k, v) =>
                  k -> Seq(v)
                }

              val results = runValidations[A, Mets, Mels](
                value.asInstanceOf[Product],
                fields.filter(x => !ignoreFields.contains(x.name))
              )

              Result(results ++ othersResults)

        case _ => throw new Exception(s"can't get product of type")

    private inline def runValidations[A, Mets, Mels](
      product: Product,
      fields: Seq[FieldInfo],
      i: Int = 0,
      acc: Map[String, Seq[String]] = Map.empty
    ): ValidatorMessages ?=> Map[String, Seq[String]] =

      inline (erasedValue[Mets], erasedValue[Mels]) match
        // base case
        case _: (EmptyTuple, EmptyTuple) => acc

        case _: (met *: metsTail, mel *: melsTail) =>
          val fieldName = constValue[mel & String]

          val validations = findAnnotations(fields, fieldName)
          val newAcc      =
            inline if validations.isEmpty
            then acc
            else
              summonFrom {
                case validator: FieldValidator[met] =>
                  val value = product.productElement(i).asInstanceOf[met]
                  validator.validate(fieldName, validations, value) match
                    case Right(())     => acc
                    case Left(results) => acc + (fieldName -> results)
                case _                              => acc
              }

          runValidations[A, metsTail, melsTail](product, fields, i + 1, newAcc)
