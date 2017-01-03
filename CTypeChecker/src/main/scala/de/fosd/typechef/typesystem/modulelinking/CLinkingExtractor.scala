package de.fosd.typechef.typesystem.modulelinking

import de.fosd.typechef.conditional.{Conditional, Opt}
import de.fosd.typechef.error.Position
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory}
import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem._
import de.fosd.typechef.typesystem.linker._

/**
  * Hook into the typesystem to extract exported and imported names.
  *
  * Implementation is based on the previous work by ck.
  */
trait CLinkingExtractor extends CTypeSystem with CDeclUse with CDeclTyping with CTypeCache with CEnvCache {

    private var exportedFunctions: List[(CSignature, Id)] = List()
    private var importedFunctions: List[(CSignature, Id)] = List()
    private var staticFunctions: List[(CSignature, Id)] = List()

    /**
      * Exported names are variables which are declared with the keyword extern and in the current scope a corresponding definition is found.
      */
    private var exportedNames: List[(CSignature, Id)] = List()

    /**
      * Imported names are variables which are declared with the keyword extern and used in the current scope.
      */
    private var importedNames: List[(CSignature, Id)] = List()

    def getExportedFunctions: List[ExportSignature] = exportedFunctions.flatMap(toExportSignature)
    def getImportedFunctions: List[ImportSignature] = getUncoveredImports(importedFunctions, exportedFunctions ++ staticFunctions).flatMap(toImportSignature)

    def getExportedNames: List[ExportSignature] = exportedNames.flatMap(toExportSignature)
    def getImportedNames: List[ImportSignature] = getUncoveredImports(importedNames, exportedNames).flatMap(toImportSignature)

    def getVarLinkingMap: CLinkMap = new CLinkMap(getExportedNames, getImportedNames)
    def getFDefLinkingMap: CLinkMap = new CLinkMap(getExportedFunctions, getImportedFunctions)

    override def typedDeclaration(decl: Declaration, featureExpr: FeatureExpr, env: Env): Unit = {
        super.typedDeclaration(decl, featureExpr, env)
        decl.init.foreach(init => {
            val deadCondition = env.isDeadCode
            val initCondition = init.condition
            val identifier = init.entry.declarator.getId
            val ctypes = env.varEnv.lookupType(identifier.name)

            for ((fexpr, ctype) <- ctypes.toList) {
                val localFexpr = fexpr and featureExpr andNot deadCondition
                if (!(ctype.isFunction || ctype.isUnknown || hasSystemSpecificName(identifier.name)) && localFexpr.isSatisfiable())
                    checkForLinkedVariable(env, identifier, ctype, localFexpr)
            }
        })
    }

    /**
      * all nonstatic function definitions are considered as exports
      *
      * actually the behavior of "extern inline" is slightly complicated, see also
      * http://stackoverflow.com/questions/216510/extern-inline
      * "extern inline" means neither static nor exported
      */
    override protected def typedFunction(fun: FunctionDef, funType: Conditional[CType], featureExpr: FeatureExpr) {
        super.typedFunction(fun, funType, featureExpr)

        val staticSpec = getStaticCondition(fun.specifiers)
        val externSpec = getSpecifierCondition(fun.specifiers, ExternSpecifier())
        val inlineSpec = getSpecifierCondition(fun.specifiers, InlineSpecifier())

        //exportCondition and staticCondition are disjoint, but may not cover all cases (they are both false for "extern inline")
        val exportCondition = staticSpec.not andNot (externSpec and inlineSpec)
        val staticCondition = staticSpec andNot (externSpec and inlineSpec)

        funType.simplify(featureExpr).vmap(featureExpr, {
            (fexpr, ctype) =>
                if ((fexpr and exportCondition).isSatisfiable())
                    exportedFunctions ::= (CSignature(fun.getName, ctype, fexpr and exportCondition, Seq(fun.declarator.getId.getPositionFrom), getExtraFlags(fun, fexpr and exportCondition)), fun.getId)

                if ((fexpr and staticCondition).isSatisfiable())
                    staticFunctions ::= (CSignature(fun.getName, ctype, fexpr and staticCondition, Seq(fun.declarator.getId.getPositionFrom), getExtraFlags(fun, fexpr and staticCondition)), fun.getId)
        })
    }

    /**
      * Extracts all referenced function names which are imported and all referenced variable names which are export or imported.
      */
    override protected def typedExpr(expr: Expr, ctypes: Conditional[CType], featureExpr: FeatureExpr, env: Env) {
        super.typedExpr(expr, ctypes, featureExpr, env)
        expr match {
            case identifier: Id if !hasSystemSpecificName(identifier.name) => // silently ignore system linked functions for now
                val deadCondition = env.isDeadCode

                for ((fexpr, ctype) <- ctypes.toList) {
                    val localFexpr = fexpr and featureExpr andNot deadCondition
                    if (!ctype.isUnknown && (localFexpr isSatisfiable()))

                        if (ctype.isFunction) // all function declarations without definitions are imports if they are referenced at least once
                            isParameter(identifier.name, env).vmap(localFexpr,
                                (f, e) => if (!e)
                                    importedFunctions ::= (CSignature(identifier.name, ctype, f, Seq(identifier.getPositionFrom), Set()), identifier)
                            )
                        else checkForLinkedVariable(env, identifier, ctype, localFexpr)
                }
            case _ =>
        }
    }

    private def checkForLinkedVariable(env: Env, identifier: Id, ctype: CType, localFexpr: FeatureExpr) = {
        // if a variable is detected by the type system to be linked, we consider a definition (e.g. extern int x; int x = y;) of that variable as export.
        isExportedDefinition(identifier.name, env).vmap(localFexpr,
            (f, e) => if (e)
                exportedNames ::= (CSignature(identifier.name, ctype, f, Seq(identifier.getPositionFrom), Set()), identifier)
        )

        // if a variable is detected by the type system to be linked, we consider the presence of a declaration only of that variable as import.
        isImportedDeclaration(identifier.name, env).vmap(localFexpr,
            (f, e) => if (e)
                importedNames ::= (CSignature(identifier.name, ctype, f, Seq(identifier.getPositionFrom), Set()), identifier)

        )
    }

    /**
      * Checks if a name is linked by the C system platform (i.e. printf)
      */
    private def hasSystemSpecificName(name: String): Boolean =
        name.startsWith("__builtin_") || SystemLinker.allLibs.contains(name)

    /**
      * Checks if a variable name is a part of linking and is defined in the local file scope (-> name gets exported)
      */
    private def isExportedDefinition(name: String, env: Env): Conditional[Boolean] =
        env.varEnv.lookup(name).map(ct => ct._4 == ExternalLinkage && ct._2 == KDefinition)

    /**
      * Checks if a variable name is a part of linking and is only declared in the local file scope (-> name gets imported)
      */
    private def isImportedDeclaration(name: String, env: Env): Conditional[Boolean] =
        env.varEnv.lookup(name).map(ct => ct._4 == ExternalLinkage && ct._2 == KDeclaration)

    /**
      * check that the id refers to a parameter instead of to a function or variable
      *
      * returns true if scope==0
      */
    private def isParameter(name: String, env: Env): Conditional[Boolean] =
        env.varEnv.lookupKind(name).map(_ == KParameter)

    /**
      * remove imports that are covered by exports and/or static names
      *
      * ignore CFlags in importers
      */
    private def getUncoveredImports(imports: List[(CSignature, Id)], exports: List[(CSignature, Id)]): List[(CSignature, Id)] = {
        type T2 = (FeatureExpr, Seq[Position], Set[CFlag], List[Id])
        var importMap = Map[(String, CType), T2]()

        //eliminate duplicates with a map
        for ((imp, id) <- imports) {
            val key = (imp.name, imp.ctype.toValueLinker)
            //toValueLinker needed to remove the distinction between Object or not
            val old = importMap.getOrElse[T2](key, (FeatureExprFactory.False, Seq(), Set(), List()))
            importMap = importMap + (key -> (old._1 or imp.fexpr, old._2 ++ imp.pos, CFlagOps.mergeOnImports(old._3, imp.extraFlags), id :: old._4))
        }

        //eliminate imports that have corresponding exports
        for ((exp, id) <- exports) {
            val key = (exp.name, exp.ctype.toValueLinker)
            if (importMap.contains(key)) {
                val (oldFexpr, oldPos, oldExtras, oldId) = importMap(key)
                val newFexpr = oldFexpr andNot exp.fexpr

                if (newFexpr.isSatisfiable())
                    importMap = importMap + (key -> (newFexpr, oldPos, oldExtras, oldId))
                else
                    importMap = importMap - key
            }
        }

        importMap.iterator.toList flatMap { case (k, v) => v._4.map { id => (CSignature(k._1, k._2, v._1, v._2, v._3), id) } } distinct
    }


    // legacy code from old implementation -> maybe remove?

    private def findAttributes(a: GnuAttributeSpecifier, ctx: FeatureExpr): Seq[(FeatureExpr, AtomicAttribute)] =
        (for (Opt(f, at) <- a.attributeList) yield findAttributes(at, ctx and f)).flatten
    private def findAttributes(a: AttributeSequence, ctx: FeatureExpr): Seq[(FeatureExpr, AtomicAttribute)] =
        (for (Opt(f, at) <- a.attributes) yield findAttributes(at, ctx and f)).flatten
    private def findAttributes(a: Attribute, ctx: FeatureExpr): Seq[(FeatureExpr, AtomicAttribute)] = a match {
        case a: AtomicAttribute => Seq((ctx, a))
        case CompoundAttribute(inner) => (for (Opt(f, at) <- inner) yield findAttributes(at, ctx and f)).flatten
    }

    /**
      * try to recognize __attribute__((weak)) attribute as a flag.
      *
      * the recognition is conservative and does ignore conditional attribute declarations
      * (so a conditionally weak method is always recognized as weak, which may prevent us
      * from detecting some problems, but which will not produce false positives)
      */
    private def getExtraFlags(functionDef: FunctionDef, ctx: FeatureExpr): Set[CFlag] = {
        val flags = for (Opt(f, g@GnuAttributeSpecifier(_)) <- functionDef.specifiers;
                         (f, a) <- findAttributes(g, ctx)
        ) yield
            if (a.n == "weak" && f.isSatisfiable()) Some[CFlag](WeakExport) else None

        flags.filter(_.isDefined).map(_.get).toSet
    }

    private def toExportSignature(export: (CSignature, Id)): Option[ExportSignature] = toSignature(export, ExportSignature.apply)

    private def toImportSignature(importSig: (CSignature, Id)): Option[ImportSignature] = toSignature(importSig, ImportSignature.apply)

    private def toSignature[T <: CLinkingSignature](sig: (CSignature, Id), apply: (Seq[CLinkingName], CSignature) => T): Option[T] = {
        val udm = getUseDeclMap
        val ddm = getDeclDefMap

        if (sig._1.fexpr.isSatisfiable())
            if (udm.containsKey(sig._2)) {
                val declPos = udm.get(sig._2).flatMap(decl => {
                    if (ddm.containsKey(decl)) ddm.get(decl).map(CLinkingName.apply)
                    else Some(CLinkingName.apply(decl))
                })
                Some(apply(declPos, sig._1))
            } else if (ddm.containsKey(sig._2)) {
                val declPos = ddm.get(sig._2).map(CLinkingName.apply)
                Some(apply(declPos, sig._1))
            } else Some(apply(Seq(CLinkingName(sig._2)), sig._1))
        else None
    }
}
