package io.vigilo.core

import scala.quoted.*

object macros:

  case class FieldInfo(name: String, annotations: Seq[validation])

  inline def extractValidationsFields[T]: Seq[FieldInfo] = ${ extractValidationsFieldsImpl[T] }

  def extractValidationsFieldsImpl[T: Type](using Quotes): Expr[Seq[FieldInfo]] = {
    import quotes.reflect.*

    val targetSym    = TypeRepr.of[T].typeSymbol
    val baseAnnoType = TypeRepr.of[validation]

    def getAnnotations[A <: validation](using Type[A]): Seq[Expr[(String, Seq[A])]] = {

      val entries = targetSym.primaryConstructor.paramSymss.flatten
        .map { field =>
          val fieldName        = Expr(field.name)
          // Filtra apenas anotações que estendem BaseAnnotation
          val fieldAnnotations = field.annotations.filter { anno =>
            anno.tpe <:< baseAnnoType
          }
          // Converte as anotações encontradas para uma expressão de lista
          val annotationExprs  = fieldAnnotations.map { anno =>
            // Para cada anotação, criamos uma expressão para instanciá-la
            val annoTerm = anno.asExpr.asInstanceOf[Expr[A]]
            annoTerm
          }
          val annoListExpr     = Expr.ofSeq(annotationExprs)
          // Cria a entrada do map (nome do campo -> lista de anotações)
          '{ ($fieldName, $annoListExpr) }
        }
      entries.toSeq
    }

    val values = getAnnotations[validation].map: expr =>
      '{ FieldInfo($expr._1, $expr._2) }

    Expr.ofSeq(values)
  }
