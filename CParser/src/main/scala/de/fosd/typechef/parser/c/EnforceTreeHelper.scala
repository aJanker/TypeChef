package de.fosd.typechef.parser.c


/**
 * preparation and checks for downstream tools
 * which require a tree structure
 *
 * we use the product interface of the elements here works both for
 * case classes Opt and AST elements, which derive product directly
 */
trait EnforceTreeHelper {
    def prepareAST[T <: Product](ast: T): T = ASTRewriter.prepareAST(ast)
    def removeDeadNodes[T <: Product](ast: T, env: ASTEnv): T = ASTRewriter.removeDeadNodes(ast, env)
    def rewriteInfiniteForLoops[T <: Product](ast: T): T = ASTRewriter.rewriteInfiniteForLoops(ast)
}
