package de.fosd.typechef.crewrite

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.parser.c._

// implements a simple analysis that checks whether a case statement associated with a statement
// terminates under all conditions with a break statement
// https://www.securecoding.cert.org/confluence/display/seccode/MSC17-C.+Finish+every+set+of+statements+associated+with+a+case+label+with+a+break+statement
// MSC17-C
class CaseTermination(env: ASTEnv) extends IntraCFG {
    def isTerminating(c: CaseStatement): Boolean = {
        // get all successor elements of the case statement
        // and filter out other case statements, as fall through (case after case)
        // is allowed in this analysis
        val wlist: List[Opt[AST]] = succ(c, env).filterNot({
            case Opt(_, c@CaseStatement(_)) => true
            case _ => false
        })

        // determine switch to make sure we do not leave the successor element
        val switch = findPriorASTElem[SwitchStatement](c, env)

        // store already visited  statements to avoid endless recursion
        val visited: List[AST] = List()

        // determine starting from the case statement that all successor elements will finally
        // come through a break statement
        def isTermination(succs: List[Opt[AST]], visited: List[AST]) : Boolean = {
            if (succs.isEmpty) true
            else succs.head match {
                    case Opt(_, _: BreakStatement) => isTermination(succs.tail, visited)
                    case Opt(_, _: CaseStatement) =>  false
                    case Opt(_, _: DefaultStatement) =>  false
                    case Opt(_, s) => {}
                        if (!isPartOf(s, switch))  false
                        else if (!visited.exists(s.eq)) isTermination(succs.tail ++ succ(s, env), s :: visited)
                        else isTermination(succs.tail, visited)
            }
        }

        isTermination(wlist, visited)
    }
}
