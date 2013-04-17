#!/usr/local/groovy/bin/groovy

/*
 * Copyright 2003-2012 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package groovy2ruby


import org.codehaus.groovy.ast.*
import org.codehaus.groovy.ast.expr.*
import org.codehaus.groovy.ast.stmt.*
import org.codehaus.groovy.classgen.BytecodeExpression
import org.codehaus.groovy.control.CompilePhase
import java.lang.reflect.Modifier
import org.codehaus.groovy.control.CompilationUnit
import org.codehaus.groovy.control.CompilerConfiguration
import org.codehaus.groovy.control.CompilationFailedException
import org.codehaus.groovy.classgen.GeneratorContext
import org.codehaus.groovy.control.SourceUnit
import org.codehaus.groovy.control.CompilationUnit.PrimaryClassNodeOperation
import org.codehaus.groovy.classgen.Verifier

/**
 * This class takes Groovy source code, compiles it to a specific compile phase, and then decompiles it
 * into Ruby code that approximates the Groovy code.
 * 
 * This is adapted from Groovy2Ruby from the Groovy distribution.
 *
 * @author Hamlet D'Arcy
 */
public class Groovy2Ruby  {

    /**
     * Run this class as a script to compile a groovy file and print out the resulting source.
     * @param args
     *      a filename to compile and a CompilePhase to run to
     */
    public static void main(args) {

        if (!args || args.length < 1) {
            println """
Usage: Groovy2Ruby.groovy [filename]
where [filename] is a Groovy script
"""
        } else {
            def file = new File((String) args[0])
            def phase = CompilePhase.fromPhaseNumber(3)
            if (!file.exists()) {
                println "File ${args[0]} cannot be found."
            } else if (phase == null) {
                println "Compile phase ${args[1]} cannot be mapped to a org.codehaus.groovy.control.CompilePhase."
            } else {
                println new Groovy2Ruby().compileToScript(file.text, phase.getPhaseNumber())
            }
        }
    }

    /**
    * This method takes source code, compiles it, then reverses it back to source. 
    * @param script
    *    the source code to be compiled. If invalid, a compile error occurs
    * @param compilePhase
    *    the CompilePhase. Must be an int mapped in @{link CompilePhase}
    * @param classLoader
    *    (optional) the classloader to use. If missing/null then the current is used.    
    *    This parameter enables things like ASTBrowser to invoke this with the correct classpath
    * @param showScriptFreeForm
    *    Whether or not to show the script portion of the source code
    * @param showScriptClass
    *    Whether or not to show the Script class from the source code
    * @returns the source code from the AST state
    */
    public String compileToScript(String script, int compilePhase, ClassLoader classLoader = null, boolean showScriptFreeForm = true, boolean showScriptClass = true) {

        def writer = new StringWriter()
        writer.println "# This file was created using groovy2ruby.\n"

        classLoader = classLoader ?: new GroovyClassLoader(getClass().classLoader)

        def scriptName = "script" + System.currentTimeMillis() + ".groovy"
        GroovyCodeSource codeSource = new GroovyCodeSource(script, scriptName, "/groovy/script")
        CompilationUnit cu = new CompilationUnit(CompilerConfiguration.DEFAULT, codeSource.codeSource, classLoader)
        cu.addPhaseOperation(new AstNodeToScriptVisitor(writer, showScriptFreeForm, showScriptClass), compilePhase)
        cu.addSource(codeSource.getName(), script);
        try {
            cu.compile(compilePhase)
        } catch (CompilationFailedException cfe) {

            writer.println "Unable to produce AST for this phase due to earlier compilation error:"
            cfe.message.eachLine {
                writer.println it
            }
            writer.println "Fix the above error(s) and then press Refresh"
        } catch (Throwable t) {
            writer.println "Unable to produce AST for this phase due to an error:"
            writer.println t.getMessage()
            writer.println "Fix the above error(s) and then press Refresh"
        }
        return writer.toString()
    }
}

/**
 * An adapter from ASTNode tree to source code.
 *
 * @author Hamlet D'Arcy
 */
class AstNodeToScriptVisitor extends PrimaryClassNodeOperation implements GroovyCodeVisitor, GroovyClassVisitor {

    private Writer _out
    Stack<String> classNameStack = new Stack<String>();
    String _indent = ""
    boolean readyToIndent = true
    boolean showScriptFreeForm
    boolean showScriptClass
    boolean scriptHasBeenVisited

    def AstNodeToScriptVisitor(Writer writer, boolean showScriptFreeForm = true, boolean showScriptClass = true) {
        this._out = writer;
        this.showScriptFreeForm = showScriptFreeForm
        this.showScriptClass = showScriptClass
        this.scriptHasBeenVisited = false
    }

    String toSnakeCase(text ) {
        if (text) {
            text.toString().replaceAll( /([A-Z])/, /_$1/ ).toLowerCase().replaceAll( /^_/, '' )
        }
    }

    void call(SourceUnit source, GeneratorContext context, ClassNode classNode) {

        visitPackage(source?.getAST()?.getPackage())

        visitAllImports(source)

        if (showScriptFreeForm && !scriptHasBeenVisited) {
            scriptHasBeenVisited = true
            source?.getAST()?.getStatementBlock()?.visit(this)
        }
        if (showScriptClass || !classNode.isScript()) {
            visitClass classNode
        }
    }

    private def visitAllImports(SourceUnit source) {
        boolean staticImportsPresent = false
        boolean importsPresent = false

        source?.getAST()?.getStaticImports()?.values()?.each {
            visitImport(it)
            staticImportsPresent = true
        }
        source?.getAST()?.getStaticStarImports()?.values()?.each {
            visitImport(it)
            staticImportsPresent = true
        }

        if (staticImportsPresent) {
            printDoubleBreak()
        }

        source?.getAST()?.getImports()?.each {
            visitImport(it)
            importsPresent = true
        }
        source?.getAST()?.getStarImports()?.each {
            visitImport(it)
            importsPresent = true
        }
        if (importsPresent) {
            printDoubleBreak()
        }
    }


    void print(parameter) {
        def output = parameter.toString()

        if (readyToIndent) {
            _out.print _indent
            readyToIndent = false
            while (output.startsWith(' ')) {
                output = output[1..-1]  // trim left
            }
        }
        if (_out.toString().endsWith(' ')) {
            if (output.startsWith(' ')) {
                output = output[1..-1]
            }
        }
        _out.print output
    }

    def println(parameter) {
        throw new UnsupportedOperationException('Wrong API')
    }

    def indented(Closure block) {
        String startingIndent = _indent
        _indent = _indent + "  "
        block()
        _indent = startingIndent
    }

    def printLineBreak() {
        if (!_out.toString().endsWith('\n')) {
            _out.print '\n'
        }
        readyToIndent = true
    }

    def printDoubleBreak() {
        if (_out.toString().endsWith('\n\n')) {
            // do nothing
        } else if (_out.toString().endsWith('\n')) {
            _out.print '\n'
        } else {
            _out.print '\n'
            _out.print '\n'
        }
        readyToIndent = true
    }

    void visitPackage(PackageNode packageNode) {

        // if (packageNode) {

        //     packageNode.annotations?.each {
        //         visitAnnotationNode(it)
        //         printLineBreak()
        //     }

        //     if (packageNode.text.endsWith(".")) {
        //         print packageNode.text[0..-2]
        //     } else {
        //         print packageNode.text
        //     }
        //     printDoubleBreak()
        // }
    }

    void visitImport(ImportNode node) {
        // if (node) {
        //     node.annotations?.each {
        //         visitAnnotationNode(it)
        //         printLineBreak()
        //     }
        //     print node.text
        //     printLineBreak()
        // }
    }

    @Override
    public void visitClass(ClassNode node) {

        classNameStack.push(node.name)

        print "class $node.name"
        // visitGenerics node?.genericsTypes
        boolean first = true
        if (!(node.superClass.name == 'java.lang.Object')) {
            print ' < '
            visitType node.superClass
        }
        printDoubleBreak()

        indented {
            node?.properties?.each { visitProperty(it) }
            printLineBreak()
            node?.fields?.each { visitField(it) }
            printDoubleBreak()
            node?.declaredConstructors?.each { visitConstructor(it) }
            printLineBreak()
            node?.methods?.each { visitMethod(it) }
        }
        indented {
            print "alias"
        }
        print 'end'
        printDoubleBreak()
        classNameStack.pop()
    }

    private void visitGenerics(GenericsType[] generics) {
    }

    @Override
    public void visitConstructor(ConstructorNode node) {
        visitMethod(node)
    }

    private String visitParameters(parameters) {
        boolean first = true

        parameters.each { Parameter it ->
            if (!first) {
                print ', '
            }
            first = false

            print toSnakeCase( it.name)
            if (it.initialExpression && !(it.initialExpression instanceof EmptyExpression)) {
                print ' = '
                it.initialExpression.visit this
            }
        }
    }

    @Override
    public void visitMethod(MethodNode node) {
        if (node.name == '<init>') {
            print "def initialize ("
            visitParameters(node.parameters)
            print ")"
            printLineBreak()
        } else if (node.name == '<clinit>') {
            print '{ ' // will already have 'static' from modifiers
            printLineBreak()
        } else {
            // visitType node.returnType
            print 'def ' + toSnakeCase(node.name)
            if (node.parameters?.size() > 0) {
                print ' ('
                visitParameters(node.parameters)
                print ")"

            }
            printLineBreak()
        }

        indented {
            node?.code?.visit(this)
        }
        printLineBreak()
        print 'end'
        printDoubleBreak()
    }

    private def visitModifiers(int modifiers) {
        if (Modifier.isAbstract(modifiers)) {
            print 'abstract '
        }
        if (Modifier.isFinal(modifiers)) {
            print 'final '
        }
        if (Modifier.isInterface(modifiers)) {
            print 'interface '
        }
        if (Modifier.isNative(modifiers)) {
            print 'native '
        }
        if (Modifier.isPrivate(modifiers)) {
            print 'private '
        }
        if (Modifier.isProtected(modifiers)) {
            print 'protected '
        }
        if (Modifier.isPublic(modifiers)) {
            print 'public '
        }
        if (Modifier.isStatic(modifiers)) {
            print 'static '
        }
        if (Modifier.isSynchronized(modifiers)) {
            print 'synchronized '
        }
        if (Modifier.isTransient(modifiers)) {
            print 'transient '
        }
        if (Modifier.isVolatile(modifiers)) {
            print 'volatile '
        }
    }

    @Override
    public void visitField(FieldNode node) {
        print "attr_accessor :${toSnakeCase(node.name)} "
        // do not print initial expression, as this is executed as part of the constructor, unless on static constant
        Expression exp = node.initialValueExpression
        if (exp instanceof ConstantExpression) exp = Verifier.transformToPrimitiveConstantIfPossible(exp)
        ClassNode type = exp?.type
        if (Modifier.isStatic(node.modifiers) && Modifier.isFinal(node.getModifiers())
                && exp instanceof ConstantExpression
                && type == node.type
                && ClassHelper.isStaticConstantInitializerType(type)) {
            // GROOVY-5150: final constants may be initialized directly
            print " = "
            if (ClassHelper.STRING_TYPE == type) {
                print "'"+node.initialValueExpression.text.replaceAll("'", "\\\\'")+"'"
            } else if (ClassHelper.char_TYPE == type) {
                print "'${node.initialValueExpression.text}'"
            } else {
                print node.initialValueExpression.text
            }
        }
        printLineBreak()
    }

    public void visitAnnotationNode(AnnotationNode node) {
        // No annotatoins in Ruby.
    }

    @Override
    public void visitProperty(PropertyNode node) {
        // is a FieldNode, avoid double dispatch
    }

    @Override
    public void visitBlockStatement(BlockStatement block) {
        block?.statements?.each {
            it.visit(this);
            printLineBreak()
        }
        if (!_out.toString().endsWith('\n')) {
            printLineBreak()
        }
    }

    @Override
    public void visitForLoop(ForStatement statement) {

        print 'for ('
        if (statement?.variable != ForStatement.FOR_LOOP_DUMMY) {
            visitParameters([statement.variable])
            print ' : '
        }

        if (statement?.collectionExpression instanceof ListExpression) {
            statement?.collectionExpression?.visit this
        } else {
            statement?.collectionExpression?.visit this
        }
        print ') {'
        printLineBreak()
        indented {
            statement?.loopBlock?.visit this
        }
        print '}'
        printLineBreak()
    }

    @Override
    public void visitIfElse(IfStatement ifElse) {
        print 'if '
        ifElse?.booleanExpression?.visit this
        // print ') {'
        printLineBreak()
        indented {
            ifElse?.ifBlock?.visit this
        }
        printLineBreak()
        if (ifElse?.elseBlock && !(ifElse.elseBlock instanceof EmptyStatement)) {
            print "else"
            printLineBreak()
            indented {
                ifElse?.elseBlock?.visit this
            }
            printLineBreak()
        }
        print 'end'
        printLineBreak()
    }

    @Override
    public void visitExpressionStatement(ExpressionStatement statement) {
        statement.expression.visit this
    }

    @Override
    public void visitReturnStatement(ReturnStatement statement) {
        printLineBreak()
        print "return "
        statement.getExpression().visit(this);
        printLineBreak()
    }

    @Override
    public void visitSwitch(SwitchStatement statement) {
        print 'case '
        statement?.expression?.visit this
        // print ') {'
        printLineBreak()
        indented {
            statement?.caseStatements?.each {
                visitCaseStatement it
            }
            if (statement?.defaultStatement) {
                print 'else'
                printLineBreak()
                indented {
                    statement?.defaultStatement?.visit this
                }
            }
        }
        print 'end'
        printLineBreak()
    }

    @Override
    public void visitCaseStatement(CaseStatement statement) {
        print 'when '
        statement?.expression?.visit this
        // print ':'
        printLineBreak()
        indented {
            statement?.code?.visit this
        }
    }

    @Override
    public void visitBreakStatement(BreakStatement statement) {
        print 'break'
        printLineBreak()
    }

    @Override
    public void visitContinueStatement(ContinueStatement statement) {
        print 'continue'
        printLineBreak()
    }

    @Override
    public void visitMethodCallExpression(MethodCallExpression expression) {

        Expression objectExp = expression.getObjectExpression()
        Expression method = expression.getMethod()

        // Convert println to private puts calls.
        if (method instanceof ConstantExpression && method.value == 'println') {
            print "puts"
            expression.getArguments().visit(this)
            return
        }

        if (objectExp instanceof VariableExpression) {
            visitVariableExpression(objectExp, false)
        } else {
            objectExp.visit(this);
        }
        if (expression.spreadSafe) {
            print '*'
        }
        if (expression.safe) {
            print '?'
        }
        print '.'
        if (method instanceof ConstantExpression) {
            if (method.value == 'println') {
                method.value = 'puts'
            }
            visitConstantExpression(method, true)
        } else {
            method.visit(this);
        }
        expression.getArguments().visit(this)
    }

    @Override
    public void visitStaticMethodCallExpression(StaticMethodCallExpression expression) {
        print expression?.ownerType?.name + "." + expression?.method
        if (expression?.arguments instanceof VariableExpression || expression?.arguments instanceof MethodCallExpression) {
            print '('
            expression?.arguments?.visit this
            print ')'            
        } else {
            expression?.arguments?.visit this
        }
    }

    @Override
    public void visitConstructorCallExpression(ConstructorCallExpression expression) {
        if (expression?.isSuperCall()) {
            print 'super'
        } else if (expression?.isThisCall()) {
            print 'self'
        } else {
            visitType expression?.type
            print '.new'

        }
        expression?.arguments?.visit this
    }

    @Override
    public void visitBinaryExpression(BinaryExpression expression) {
        expression?.leftExpression?.visit this
        def operationText = expression?.operation?.text
        if (operationText != '[' && operationText != 'instanceof') {
            print " "
        }
        if (operationText == 'instanceof') {
            operationText = '.is_a('
        }
        print "$operationText"
        if (operationText != '[' && operationText != '.is_a(') {
            print " "
        }
        expression.rightExpression.visit this

        if (operationText == '.is_a(') {
            print ')'
        } else if (expression?.operation?.text == '[') {
            print ']'
        }
    }

    @Override
    public void visitPostfixExpression(PostfixExpression expression) {
        print '('
        expression?.expression?.visit this
        print ')'
        print expression?.operation?.text
    }

    @Override
    public void visitPrefixExpression(PrefixExpression expression) {
        print expression?.operation?.text
        print '('
        expression?.expression?.visit this
        print ')'
    }


    @Override
    public void visitClosureExpression(ClosureExpression expression) {
        print ' do '
        if (expression?.parameters) {
            print "|"
            visitParameters(expression?.parameters)
            print '|'
        }
        printLineBreak()
        indented {
            expression?.code?.visit this
        }
        print 'end'
    }

    @Override
    public void visitTupleExpression(TupleExpression expression) {
        print '('
        visitExpressionsAndCommaSeparate(expression?.expressions)
        print ')'
    }

    @Override
    public void visitRangeExpression(RangeExpression expression) {
        print '('
        expression?.from?.visit this
        print '..'
        expression?.to?.visit this
        print ')'
    }

    @Override
    public void visitPropertyExpression(PropertyExpression expression) {
        expression?.objectExpression?.visit this
        if (expression?.spreadSafe) {
            print '*'
        } else if (expression?.isSafe()) {
            print '?'
        }
        print '.'
        if (expression?.property instanceof ConstantExpression) {
            visitConstantExpression(expression?.property, true)
        } else {
            expression?.property?.visit this
        }
    }

    @Override
    public void visitAttributeExpression(AttributeExpression attributeExpression) {
        visitPropertyExpression attributeExpression
    }

    @Override
    public void visitFieldExpression(FieldExpression expression) {
        print toSnakeCase(expression?.field?.name)
    }

    @Override
    public void visitConstantExpression(ConstantExpression expression, boolean unwrapQuotes = false) {
        if (expression.value instanceof String && !unwrapQuotes) {
            // string reverse escaping is very naive
            def escaped = ((String) expression.value).replaceAll('\n', '\\\\n').replaceAll("'", "\\\\'")
            print "'$escaped'"
        } else {
            // print "!!! ${expression.value} !!!"
            print toSnakeCase(expression.value.toString())
        }
    }

    @Override
    public void visitClassExpression(ClassExpression expression) {
        print expression.text
    }

    @Override
    public void visitVariableExpression(VariableExpression expression, boolean spacePad = true) {

        if (expression.name == 'this') {
            print 'self'
        } else {
            print toSnakeCase(expression.name)
        }
        // if (spacePad) {
        //     print ' ' + expression.name + ' '
        // } else {
        //     print expression.name
        // }
    }

    @Override
    public void visitDeclarationExpression(DeclarationExpression expression) {
        // handle multiple assignment expressions
        if (expression?.leftExpression instanceof ArgumentListExpression) {
            print 'def '
            visitArgumentlistExpression expression?.leftExpression, true
            print " $expression.operation.text "
            expression.rightExpression.visit this

            if (expression?.operation?.text == '[') {
                print ']'
            }
        } else {
            // visitType expression?.leftExpression?.type
            visitBinaryExpression expression // is a BinaryExpression
        }
    }

    @Override
    public void visitGStringExpression(GStringExpression expression) {
        print '"'

        def children = []
        children.addAll(expression.strings)
        children.addAll(expression.values)
        children.sort { a, b ->
            a.columnNumber <=> b.columnNumber
        }.each {
            if (it instanceof VariableExpression || it instanceof MethodCallExpression || it instanceof PropertyExpression) {
                print "#{"
                it.visit(this)
                print "}"
            } else if (it instanceof ConstantExpression) {
                print it.value
            } else {
                it.visit(this)
            }
        }
        print '"'
        // print '"' + expression.text + '"'


        // printDoubleBreak()
        // expression.strings.each { sExp ->
        //     print sExp
        //     printLineBreak()
        // }
        // printDoubleBreak()
        // expression.values.each { 
        //     print it
        //     printLineBreak()
        // }
        // printDoubleBreak()

    }

    @Override
    public void visitSpreadExpression(SpreadExpression expression) {
        print '*'
        expression?.expression?.visit this
    }

    @Override
    public void visitNotExpression(NotExpression expression) {
        print '!('
        expression?.expression?.visit this
        print ')'
    }

    @Override
    public void visitUnaryMinusExpression(UnaryMinusExpression expression) {
        print '-('
        expression?.expression?.visit this
        print ')'
    }

    @Override
    public void visitUnaryPlusExpression(UnaryPlusExpression expression) {
        print '+('
        expression?.expression?.visit this
        print ')'
    }

    @Override
    public void visitCastExpression(CastExpression expression) {
        print '(('
        expression?.expression?.visit this
        print ') as '
        visitType(expression?.type)
        print ')'

    }

    /**
     * Prints out the type, safely handling arrays.
     * @param classNode
     *      classnode
     */
    public void visitType(ClassNode classNode) {
        def name = classNode.name
        if (name =~ /^\[+L/ && name.endsWith(";")) {
            int numDimensions = name.indexOf('L')
            print "${classNode.name[(numDimensions + 1)..-2]}" + ('[]' * numDimensions)
        } else {
            print name
        }
        visitGenerics classNode?.genericsTypes
    }

    @Override
    public void visitArgumentlistExpression(ArgumentListExpression expression, boolean showTypes = false) {
        if (expression.expressions?.size() == 0) {
            return
        }

        int count = expression?.expressions?.size()
        int originalCount = count
        def argumentExpressions = expression.expressions
        def lastArgumentIsClosure = argumentExpressions[originalCount - 1] instanceof ClosureExpression

        if (originalCount == 1 && lastArgumentIsClosure) {
            argumentExpressions[0].visit(this)
        } else {
            print '('
            expression.expressions.each {
                if (it instanceof VariableExpression) {
                    visitVariableExpression it, false
                } else if (it instanceof ConstantExpression) {
                    visitConstantExpression it, false
                } else {
                    it.visit this
                }
                count--
                if (count == 1 && lastArgumentIsClosure) {
                    print ") "
                } else if (count == 0 && lastArgumentIsClosure) {
                } else if (count) {
                    print ', '
                } else {
                    print ')'
                }
            }
        }

    }

    @Override
    public void visitBytecodeExpression(BytecodeExpression expression) {
        print "/*BytecodeExpression*/"
        printLineBreak()
    }



    @Override
    public void visitMapExpression(MapExpression expression) {
        print '{'
        if (expression?.mapEntryExpressions?.size() == 0) {
        } else {
            visitExpressionsAndCommaSeparate(expression?.mapEntryExpressions)
        }
        print '}'
    }

    @Override
    public void visitMapEntryExpression(MapEntryExpression expression) {
        if (expression?.keyExpression instanceof SpreadMapExpression) {
            print '*'            // is this correct? 
        } else {
            // expression?.keyExpression?.visit this
            print "'" + toSnakeCase(expression?.keyExpression?.value.toString()) + "'"
        }
        print ' => '
        expression?.valueExpression?.visit this
    }

    @Override
    public void visitListExpression(ListExpression expression) {
        print '['
        visitExpressionsAndCommaSeparate(expression?.expressions)
        print ']'
    }

    @Override
    public void visitTryCatchFinally(TryCatchStatement statement) {
        print 'begin'
        printLineBreak()
        indented {
            statement?.tryStatement?.visit this
        }
        printLineBreak()
        printLineBreak()
        statement?.catchStatements?.each { CatchStatement catchStatement ->
            visitCatchStatement(catchStatement)
        }
        print 'ensure'
        printLineBreak()
        indented {
            statement?.finallyStatement?.visit this
        }
        print 'end'
        printLineBreak()
    }

    @Override
    public void visitThrowStatement(ThrowStatement statement) {
        print 'raise '
        statement?.expression?.visit this
        printLineBreak()
    }

    @Override
    public void visitSynchronizedStatement(SynchronizedStatement statement) {
        print 'synchronized ('
        statement?.expression?.visit this
        print ') {'
        printLineBreak()
        indented {
            statement?.code?.visit this
        }
        print '}'
    }

    @Override
    public void visitTernaryExpression(TernaryExpression expression) {
        expression?.booleanExpression?.visit this
        print ' ? '
        expression?.trueExpression?.visit this
        print ' : '
        expression?.falseExpression?.visit this
    }

    @Override
    public void visitShortTernaryExpression(ElvisOperatorExpression expression) {
        visitTernaryExpression(expression)
    }

    @Override
    public void visitBooleanExpression(BooleanExpression expression) {
        expression?.expression?.visit this
    }

    @Override
    public void visitWhileLoop(WhileStatement statement) {
        print 'while ('
        statement?.booleanExpression?.visit this
        print ') {'
        printLineBreak()
        indented {
            statement?.loopBlock?.visit this
        }
        printLineBreak()
        print '}'
        printLineBreak()
    }

    @Override
    public void visitDoWhileLoop(DoWhileStatement statement) {
        print 'do {'
        printLineBreak()
        indented {
            statement?.loopBlock?.visit this
        }
        print '} while ('
        statement?.booleanExpression?.visit this
        print ')'
        printLineBreak()
    }

    @Override
    public void visitCatchStatement(CatchStatement statement) {
        print 'rescue '
        visitParameters([statement.variable])
        // print ') {'
        printLineBreak()
        indented {
            statement.code?.visit this
        }
        // print '} '
        printLineBreak()
    }

    @Override
    public void visitBitwiseNegationExpression(BitwiseNegationExpression expression) {
        print '~('
        expression?.expression?.visit this
        print ') '
    }


    @Override
    public void visitAssertStatement(AssertStatement statement) {
        print 'assert '
        statement?.booleanExpression?.visit this
        print ' : '
        statement?.messageExpression?.visit this
    }

    @Override
    public void visitClosureListExpression(ClosureListExpression expression) {
        boolean first = true
        expression?.expressions?.each {
            if (!first) {
                print ';'
            }
            first = false
            it.visit this
        }
    }

    @Override
    public void visitMethodPointerExpression(MethodPointerExpression expression) {
        expression?.expression?.visit this
        print '.&'
        expression?.methodName?.visit this
    }

    @Override
    public void visitArrayExpression(ArrayExpression expression) {
        print 'new '
        visitType expression?.elementType
        print '['
        visitExpressionsAndCommaSeparate(expression?.sizeExpression)
        print ']'
    }

    private void visitExpressionsAndCommaSeparate(List<? super Expression> expressions) {
        boolean first = true
        expressions?.each {
            if (!first) {
                print ', '
            }
            first = false
            it.visit this
        }
    }

    @Override
    public void visitSpreadMapExpression(SpreadMapExpression expression) {
        print '*:'
        expression?.expression?.visit this
    }
}
