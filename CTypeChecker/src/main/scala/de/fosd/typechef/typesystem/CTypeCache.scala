package de.fosd.typechef.typesystem

import de.fosd.typechef.conditional.Conditional
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.parser.c.{AST, Expr, FunctionDef}


trait CTypeCache extends CTypeSystemInterface {
    private val cacheExpr: java.util.IdentityHashMap[Expr, Conditional[CType]] = new java.util.IdentityHashMap()
    private val cacheFun: java.util.IdentityHashMap[FunctionDef, Conditional[CType]] = new java.util.IdentityHashMap()

    override protected def typedExpr(expr: Expr, ctype: Conditional[CType], featureExpr: FeatureExpr, env: Env) {
        cacheExpr.put(expr, ctype)
        super.typedExpr(expr, ctype, featureExpr, env)
    }
    override protected def typedFunction(fun: FunctionDef, ctype: Conditional[CType], featureExpr: FeatureExpr) {
        cacheFun.put(fun, ctype)
        super.typedFunction(fun, ctype, featureExpr)
    }

    def lookupExprType(expr: Expr): Conditional[CType] = cacheExpr.get(expr)
    def lookupFunType(fun: FunctionDef): Conditional[CType] = cacheFun.get(fun)

}


trait CEnvCache extends CTypeSystemInterface {
    private val cache: java.util.IdentityHashMap[AST, Env] = new java.util.IdentityHashMap()

    override protected def addEnv(ast: AST, env: Env) {
        cache.put(ast, env)
        super.addEnv(ast, env)
    }

    def lookupEnv(ast: AST) = cache.get(ast)
}

