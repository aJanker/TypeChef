package de.fosd.typechef.parser.c

import de.fosd.typechef.conditional._
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory}
import org.kiama.rewriting.Rewriter._

trait ConditionalNavigation {

    // return the parent variability node; Opt or Conditional
    def parentVNode(e: AST, env: ASTEnv): Either[Conditional[_], Opt[_]] = {
        val eparent = env.parent(e)
        eparent match {
            case o: Opt[_] => Right(o)
            case o: One[_] => Left(o)
            case a: AST    => parentVNode(a, env)
            case _         => null
        }
    }

    def parentOpt(e: Product, env: ASTEnv): Opt[_] = {
        val eparent = env.parent(e)
        eparent match {
            case o: Opt[_] => o
            case c: Conditional[_] => c.toOptList.head
            case a: AST => parentOpt(a, env)
            case _ => null
        }
    }

    def prevOpt(e: Opt[_], env: ASTEnv): Opt[_] = {
        val eprev = env.previous(e)
        eprev match {
            case o: Opt[_] => o
            case _ => null
        }
    }

    def nextOpt(e: Opt[_], env: ASTEnv): Opt[_] = {
        val enext = env.next(e)
        enext match {
            case o: Opt[_] => o
            case _ => null
        }
    }

    // check recursively for any nodes that have an annotation != True
    def isVariable(e: Product): Boolean = {
        var res = false
        val variable = manytd(query[Product] {
            case Opt(f, _) => if (f != FeatureExprFactory.False && f != FeatureExprFactory.True) res = true
            case x => res = res
        })

        variable(e)
        res
    }

    def filterAllOptElems(e: Product): List[Opt[_]] = {
        def filterAllOptElemsHelper(a: Any): List[Opt[_]] = {
            a match {
                case o@Opt(_, entry) => List(o) ++ (if (entry.isInstanceOf[Product]) entry.asInstanceOf[Product].productIterator.toList.flatMap(filterAllOptElemsHelper)
                else List())
                case l: List[_] => l.flatMap(filterAllOptElemsHelper(_))
                case x: Product => x.productIterator.toList.flatMap(filterAllOptElemsHelper(_))
                case _ => List()
            }
        }
        filterAllOptElemsHelper(e)
    }

    def filterAllFeatureExpr(e: Product): List[FeatureExpr] = {
        def filterAllFeatureExprHelper(a: Any): List[FeatureExpr] = {
            a match {
                case Opt(feature, entry) => List(feature) ++ (if (entry.isInstanceOf[Product]) entry.asInstanceOf[Product].productIterator.toList.flatMap(filterAllFeatureExprHelper)
                else List())
                case Choice(feature, thenBranch, elseBranch) => List(feature, feature.not()) ++
                        thenBranch.asInstanceOf[Product].productIterator.toList.flatMap(filterAllFeatureExprHelper) ++
                        elseBranch.asInstanceOf[Product].productIterator.toList.flatMap(filterAllFeatureExprHelper)
                case l: List[_] => l.flatMap(filterAllFeatureExprHelper)
                case x: Product => x.productIterator.toList.flatMap(filterAllFeatureExprHelper)
                case _ => List()
            }
        }
        filterAllFeatureExprHelper(e)
    }

    def filterAllSingleFeatureExpr(e: Product): List[String] = {
        def filterAllSingleFeatureExprHelper(a: Any): List[String] = {
            a match {
                case Opt(feature, entry) => feature.collectDistinctFeatures.toList ++ (if (entry.isInstanceOf[Product]) entry.asInstanceOf[Product].productIterator.toList.flatMap(filterAllSingleFeatureExprHelper).distinct
                else List())
                case Choice(feature, thenBranch, elseBranch) => feature.collectDistinctFeatures.toList ++ feature.not().collectDistinctFeatures.toList ++
                    thenBranch.asInstanceOf[Product].productIterator.toList.flatMap(filterAllSingleFeatureExprHelper) ++
                    elseBranch.asInstanceOf[Product].productIterator.toList.flatMap(filterAllSingleFeatureExprHelper).distinct
                case l: List[_] => l.flatMap(filterAllSingleFeatureExprHelper).distinct
                case x: Product => x.productIterator.toList.flatMap(filterAllSingleFeatureExprHelper).distinct
                case _ => List()
            }
        }
        filterAllSingleFeatureExprHelper(e)
    }

    // return all Opt and One elements
    def filterAllVariableElems(e: Product): List[Product] = {
        var res: List[Product] = List()
        val filter = manytd(query[Product] {
            case o: Opt[_] => res ::= o
            case o: One[_] => res ::= o
        })

        filter(e)
        res
    }
}
