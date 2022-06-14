package pt.up.fe.comp.jmm.ollir;

import pt.up.fe.comp.jmm.analysis.JmmSemanticsResult;
import pt.up.fe.comp.jmm.analysis.table.Symbol;
import pt.up.fe.comp.jmm.analysis.table.SymbolTable;
import pt.up.fe.comp.jmm.analysis.table.Type;
import pt.up.fe.comp.jmm.ast.AstNode;
import pt.up.fe.comp.jmm.ast.JmmNode;
import pt.up.fe.comp.jmm.ast.visitors.AJmmVisitor;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class OllirGenerator extends AJmmVisitor<Integer, Integer> {
    private final StringBuilder code;
    private final SymbolTable symbolTable;
    private final Map<String, String> config;
    private String methodSignature = null;
    private Type varType;
    private boolean isAssign = false;
    private boolean wasField = false;

    private String simpleExpression;
    private Integer tempVarCounter = 0;
    private Integer labelCounter = 0;
    private static final Type INT_TYPE = new Type("int", false);
    private static final Type BOOL_TYPE = new Type("bool", false);
    private static final Type VOID_TYPE = new Type("void", false);

    public OllirGenerator(JmmSemanticsResult semanticsResult) {
        this.code = new StringBuilder();
        this.symbolTable = semanticsResult.getSymbolTable();
        this.config = semanticsResult.getConfig();

        addVisit(AstNode.PROGRAM, this::programVisit);
        addVisit(AstNode.CLASS_DECL, this::classDeclVisit);
        addVisit(AstNode.METHOD_DECL, this::methodDeclVisit);
        addVisit(AstNode.ASSIGN, this::assignVisit);
        addVisit(AstNode.ID, this::idVisit);
        addVisit(AstNode.INT, this::intVisit);
        addVisit(AstNode.TRUE, this::boolVisit);
        addVisit(AstNode.FALSE, this::boolVisit);
        addVisit(AstNode.NOT, this::notVisit);
        addVisit(AstNode.NEW, this::newVisit);
        addVisit(AstNode.ACCESS, this::accessVisit);
        addVisit(AstNode.ARRAY_ACCESS, this::arrayAccessVisit);
        addVisit(AstNode.BLOCK, this::blockVisit);
        addVisit(AstNode.RETURN_STATEMENT, this::returnVisit);
        addVisit(AstNode.ADD, this::opVisit);
        addVisit(AstNode.SUBTRACT, this::opVisit);
        addVisit(AstNode.MULTIPLY, this::opVisit);
        addVisit(AstNode.DIVIDE, this::opVisit);
        addVisit(AstNode.LOWER, this::opVisit);
        addVisit(AstNode.AND, this::opVisit);
        addVisit(AstNode.IF, this::ifVisit);
        addVisit(AstNode.WHILE, this::whileVisit);
    }

    public String getCode() {
        return code.toString();
    }

    private String getLabel() {
        return "Label" + labelCounter++;
    }

    private String getTemp() {
        return "t" + tempVarCounter++;
    }

    private boolean isTerminal(String kind) {
        return kind.equals(AstNode.ID.toString()) ||
                kind.equals(AstNode.TRUE.toString()) ||
                kind.equals(AstNode.FALSE.toString()) ||
                kind.equals(AstNode.INT.toString());
    }

    private void visitAndCreateTemp(JmmNode expressionNode) {
        visitAndCreateTemp(expressionNode, false, false);
    }

    private void visitAndCreateTemp(JmmNode expressionNode, boolean integerTemp, boolean argTemp) {
        visit(expressionNode);

        if (!isTerminal(expressionNode.getKind()) ||
                integerTemp && expressionNode.getKind().equals(AstNode.INT.toString()) ||
                argTemp && !simpleExpression.isEmpty() && simpleExpression.charAt(0) == '$') {
            String temp = getTemp();
            code.append(temp)
                    .append(".")
                    .append(OllirUtils.getCode(varType))
                    .append(" :=.")
                    .append(OllirUtils.getCode(varType))
                    .append(" ")
                    .append(simpleExpression)
                    .append(";\n");

            simpleExpression = temp + "." + OllirUtils.getCode(varType);
        }
    }

    private Integer programVisit(JmmNode program, Integer dummy) {
        for (String importString : symbolTable.getImports()) {
            code.append("import ").append(importString).append(";\n");
        }

        for (var child : program.getChildren()) {
            visit(child);
        }
        return 0;
    }

    private Integer classDeclVisit(JmmNode classDecl, Integer dummy) {
        code.append("public ").append(symbolTable.getClassName());
        String superClass = symbolTable.getSuper();
        if (!superClass.isEmpty()) {
            code.append(" extends ").append(superClass);
        }

        code.append(" {\n");

        List<Symbol> fields = symbolTable.getFields();


        for (Symbol field : fields) {
            code.append(".field private ")
                    .append(OllirUtils.getCode(field))
                    .append(";\n");
        }

        JmmNode classBody = null;
        for (JmmNode child : classDecl.getChildren()) {
            if (child.getKind().equals("ClassBody")) {
                classBody = child;
                break;
            }
        }

        assert classBody != null;
        for (JmmNode child : classBody.getChildren()) {
            visit(child);
        }

        code.append("}\n");
        return 0;
    }

    private Integer methodDeclVisit(JmmNode methodDecl, Integer dummy) {
        code.append(".method public ");
        List<JmmNode> stmts;
        if (methodDecl.getJmmChild(0).getKind().equals("Main")) {
            methodSignature = "main";
            stmts = methodDecl.getJmmChild(0).getJmmChild(1).getChildren();
            code.append("static ");
        } else {
            methodSignature = methodDecl.getJmmChild(0).getJmmChild(1).getJmmChild(0).get("name");
            stmts = methodDecl.getJmmChild(0).getJmmChild(3).getChildren();
        }

        code.append(methodSignature).append("(");

        List<Symbol> params = symbolTable.getParameters(methodSignature);

        String paramCode = params.stream()
                .map(OllirUtils::getCode)
                .collect(Collectors.joining(", "));

        code.append(paramCode)
                .append(").")
                .append(OllirUtils.getCode(symbolTable.getReturnType(methodSignature)))
                .append(" {\n");

        for (var stmt : stmts) {
            if (stmt.getKind().equals(AstNode.VAR.toString())) continue;
            visit(stmt);
            if (!simpleExpression.isEmpty()) {
                code.append(simpleExpression);
                code.append(";\n");
            }
        }

        if (methodSignature.equals("main")) {
            code.append("ret.V;\n");
        }

        code.append("}\n");

        return 0;
    }

    private Integer assignVisit(JmmNode assignNode, Integer dummy) {

        isAssign = true;
        visit(assignNode.getJmmChild(0));
        String left = simpleExpression;
        Type type = varType;
        isAssign = false;
        if (wasField) {
            code.append(simpleExpression);
            simpleExpression = "";
            wasField = false;
            return 0;
        }
        simpleExpression = "";

        visit(assignNode.getJmmChild(1));
        String right = simpleExpression;
        simpleExpression = "";

        code.append(left)
                .append(" :=.")
                .append(OllirUtils.getOllirType(type.getName()))
                .append(" ")
                .append(right)
                .append(";\n");


        return 0;
    }

    private Integer idVisit(JmmNode idNode, Integer dummy) {
        final StringBuilder terminalCode = new StringBuilder();

        Symbol symbol;

        List<Symbol> variables = symbolTable.getLocalVariables(methodSignature);
        for (Symbol variable : variables) {
            if (variable.getName().equals(idNode.get("name"))) {
                symbol = variable;
                terminalCode.append(OllirUtils.getCode(symbol));
                varType = symbol.getType();
                simpleExpression = terminalCode.toString();
                return 0;
            }
        }

        List<Symbol> parameters = symbolTable.getParameters(methodSignature);
        for (int param_idx = 0; param_idx < parameters.size(); param_idx++) {
            if (parameters.get(param_idx).getName().equals(idNode.get("name"))) {
                symbol = parameters.get(param_idx);
                terminalCode.append("$")
                        .append(param_idx + 1)
                        .append(".")
                        .append(OllirUtils.getCode(symbol));
                varType = symbol.getType();
                simpleExpression = terminalCode.toString();
                return 0;
            }
        }

        List<Symbol> fields = symbolTable.getFields();
        for (Symbol field : fields) {
            if (field.getName().equals(idNode.get("name"))) {
                symbol = field;
                varType = symbol.getType();
                if (isAssign) {
                    wasField = true;
                    JmmNode valueNode = idNode.getJmmParent().getJmmChild(1);
                    visitAndCreateTemp(valueNode);
                    terminalCode.append("putfield(this, ")
                            .append(OllirUtils.getCode(symbol))
                            .append(", ")
                            .append(simpleExpression)
                            .append(").V;\n");

                    simpleExpression = terminalCode.toString();
                    return 0;
                }
                String temp = getTemp();
                code.append(temp)
                        .append(".")
                        .append(OllirUtils.getCode(symbol.getType()))
                        .append(" :=.")
                        .append(OllirUtils.getCode(symbol.getType()))
                        .append(" getfield(this, ")
                        .append(OllirUtils.getCode(symbol))
                        .append(").")
                        .append(OllirUtils.getCode(symbol.getType()))
                        .append(";\n");

                simpleExpression = temp + "." + OllirUtils.getCode(symbol.getType());
                return 0;
            }
        }
        return 0;
    }

    private Integer intVisit(JmmNode intNode, Integer dummy) {
        final StringBuilder terminalCode = new StringBuilder();

        terminalCode.append(intNode.get("value"))
                .append(".")
                .append(OllirUtils.getCode(INT_TYPE));

        varType = INT_TYPE;
        simpleExpression = terminalCode.toString();
        return 0;
    }

    private Integer boolVisit(JmmNode boolNode, Integer dummy) {
        final StringBuilder terminalCode = new StringBuilder();

        if (boolNode.getKind().equals(AstNode.TRUE.toString())) terminalCode.append("1.");
        else if (boolNode.getKind().equals(AstNode.FALSE.toString())) terminalCode.append("0.");

        terminalCode.append(OllirUtils.getCode(BOOL_TYPE));

        varType = BOOL_TYPE;
        simpleExpression = terminalCode.toString();
        return 0;
    }

    private Integer notVisit(JmmNode notNode, Integer dummy) {
        JmmNode expressionNode = notNode.getJmmChild(0);

        visitAndCreateTemp(expressionNode);
        simpleExpression = "!.bool " + simpleExpression;
        return 0;
    }

    private Integer newVisit(JmmNode newNode, Integer dummy) {

        if (newNode.getJmmChild(0).getKind().equals(AstNode.INT_TYPE.toString())) {
            visit(newNode.getJmmChild(1));

            if (!(newNode.getJmmChild(1).getKind().equals(AstNode.ID.toString()) || newNode.getJmmChild(1).getKind().equals(AstNode.INT.toString()))) {
                final String temp = getTemp();

                code.append(temp)
                        .append(".")
                        .append(OllirUtils.getCode(INT_TYPE))
                        .append(" :=.")
                        .append(OllirUtils.getCode(INT_TYPE))
                        .append(" ")
                        .append(simpleExpression)
                        .append(";\n");

                simpleExpression = temp;
            }

            final StringBuilder terminalCode = new StringBuilder();

            terminalCode.append("new(array, ")
                    .append(simpleExpression)
                    .append(").")
                    .append(OllirUtils.getCode(INT_TYPE));

            simpleExpression = terminalCode.toString();
            return 0;
        }

        final StringBuilder terminalCode = new StringBuilder();

        String typeName = newNode.getJmmChild(0).get("name");

        terminalCode.append("new(")
                .append(typeName)
                .append(").")
                .append(typeName)
                .append(";\ninvokespecial(");

        visit(newNode.getJmmParent().getJmmChild(0));
        terminalCode.append(simpleExpression)
                .append(", \"<init>\").V");

        simpleExpression = terminalCode.toString();

        return 0;
    }

    private Integer accessVisit(JmmNode accessNode, Integer dummy) {
        JmmNode chained = accessNode.getJmmChild(1);

        if (chained.getJmmChild(0).getKind().equals(AstNode.LENGTH.toString())) {
            visit(accessNode.getJmmChild(0));
            final StringBuilder terminalCode = new StringBuilder();

            terminalCode.append("arraylength(")
                    .append(simpleExpression)
                    .append(").")
                    .append(OllirUtils.getCode(INT_TYPE));

            varType = INT_TYPE;
            simpleExpression = terminalCode.toString();

            return 0;
        }

        final StringBuilder terminalCode = new StringBuilder();

        List<String> argsExpr = new ArrayList<>();
        JmmNode args = chained.getJmmChild(1);
        for (JmmNode arg : args.getChildren()) {
            visitAndCreateTemp(arg);
            argsExpr.add(simpleExpression);
        }

        simpleExpression = "";
        Type prevVarType = varType;

        boolean isThis = false;
        if (accessNode.getJmmChild(0).getKind().equals(AstNode.THIS.toString())) {
            varType = new Type(symbolTable.getClassName(), false);
            simpleExpression = "this";
            isThis = true;
        } else {
            visitAndCreateTemp(accessNode.getJmmChild(0), false, true);
        }

        // Check if it is a static method invocation from an import
        if (!isThis && simpleExpression.isEmpty()) {
            List<String> imports = symbolTable.getImports();
            for (String importName : imports) {
                if (importName.equals(accessNode.getJmmChild(0).get("name"))) {
                    terminalCode.append("invokestatic(")
                            .append(importName)
                            .append(", \"")
                            .append(chained.getJmmChild(0).get("name"))
                            .append("\"");

                    for (String arg : argsExpr) {
                        terminalCode.append(", ");
                        terminalCode.append(arg);
                    }

                    terminalCode.append(").");

                    if (isAssign) {
                        terminalCode.append(varType);
                    } else {
                        terminalCode.append("V");
                    }

                    varType = VOID_TYPE;
                    simpleExpression = terminalCode.toString();

                    return 0;
                }
            }
        }

        if (varType.getName().equals(symbolTable.getClassName())) {
            varType = symbolTable.getReturnType(chained.getJmmChild(0).get("name"));
        } else if (accessNode.getJmmParent().getKind().equals(AstNode.ASSIGN.toString())) {
            varType = prevVarType;
        } else {
            varType = VOID_TYPE;
        }

        terminalCode.append("invokevirtual(")
                .append(simpleExpression)
                .append(", \"")
                .append(chained.getJmmChild(0).get("name"))
                .append("\"");

        for (String arg : argsExpr) {
            terminalCode.append(", ");
            terminalCode.append(arg);
        }

        terminalCode.append(").")
                .append(OllirUtils.getCode(varType));

        simpleExpression = terminalCode.toString();

        return 0;
    }

    private Integer arrayAccessVisit(JmmNode arrayAccessNode, Integer dummy) {
        final StringBuilder terminalCode = new StringBuilder();

        JmmNode idNode = arrayAccessNode.getJmmChild(0);

        Symbol symbol;

        List<Symbol> variables = symbolTable.getLocalVariables(methodSignature);
        for (Symbol variable : variables) {
            if (variable.getName().equals(idNode.get("name")) && variable.getType().isArray()) {
                JmmNode indexNode = arrayAccessNode.getJmmChild(1).getJmmChild(0);
                visitAndCreateTemp(indexNode, true, false);
                symbol = variable;
                terminalCode.append(symbol.getName())
                        .append("[")
                        .append(simpleExpression)
                        .append("].")
                        .append(OllirUtils.getOllirType(symbol.getType().getName()));
                varType = new Type(symbol.getType().getName(), false);

                simpleExpression = terminalCode.toString();
                return 0;
            }
        }

        List<Symbol> parameters = symbolTable.getParameters(methodSignature);
        for (int param_idx = 0; param_idx < parameters.size(); param_idx++) {
            if (parameters.get(param_idx).getName().equals(idNode.get("name")) && parameters.get(param_idx).getType().isArray()) {
                JmmNode indexNode = arrayAccessNode.getJmmChild(1).getJmmChild(0);
                visitAndCreateTemp(indexNode, true, false);
                symbol = parameters.get(param_idx);
                terminalCode.append("$")
                        .append(param_idx + 1)
                        .append(".")
                        .append(symbol.getName())
                        .append("[")
                        .append(simpleExpression)
                        .append("].")
                        .append(OllirUtils.getCode(symbol.getType()));
                varType = new Type(symbol.getType().getName(), false);

                simpleExpression = terminalCode.toString();
                return 0;
            }
        }

        List<Symbol> fields = symbolTable.getFields();
        for (Symbol field : fields) {
            if (field.getName().equals(idNode.get("name")) && field.getType().isArray()) {
                JmmNode indexNode = arrayAccessNode.getJmmChild(1).getJmmChild(0);
                visitAndCreateTemp(indexNode, true, false);
                symbol = field;
                terminalCode.append(symbol.getName())
                        .append("[")
                        .append(simpleExpression)
                        .append("].")
                        .append(OllirUtils.getOllirType(symbol.getType().getName()));
                varType = new Type(symbol.getType().getName(), false);

                simpleExpression = terminalCode.toString();
                return 0;
            }
        }

        return 0;
    }

    private Integer blockVisit(JmmNode blockNode, Integer dummy) {
        for (JmmNode stmt : blockNode.getChildren()) {
            visit(stmt);
            if (!simpleExpression.isEmpty()) {
                code.append(simpleExpression);
                code.append(";\n");
            }
        }
        simpleExpression = "";
        return 0;
    }

    private Integer returnVisit(JmmNode retNode, Integer dummy) {
        final StringBuilder terminalCode = new StringBuilder();

        JmmNode expressionNode = retNode.getJmmChild(0);
        visitAndCreateTemp(expressionNode);

        terminalCode.append("ret.")
                .append(OllirUtils.getCode(symbolTable.getReturnType(methodSignature)))
                .append(" ")
                .append(simpleExpression);

        simpleExpression = terminalCode.toString();
        return 0;
    }

    private Integer opVisit(JmmNode opNode, Integer dummy) {
        final StringBuilder terminalCode = new StringBuilder();

        String op = "";
        Type type = INT_TYPE;

        if (opNode.getKind().equals(AstNode.ADD.toString())) {
            op = "+";
        } else if (opNode.getKind().equals(AstNode.SUBTRACT.toString())) {
            op = "-";
        } else if (opNode.getKind().equals(AstNode.MULTIPLY.toString())) {
            op = "*";
        } else if (opNode.getKind().equals(AstNode.DIVIDE.toString())) {
            op = "/";
        } else if (opNode.getKind().equals(AstNode.LOWER.toString())) {
            op = "<";
            type = BOOL_TYPE;
        } else if (opNode.getKind().equals(AstNode.AND.toString())) {
            op = "&&";
            type = BOOL_TYPE;
        }

        visit(opNode.getJmmChild(0));
        String leftExpr = simpleExpression;
        Type leftType = varType;
        visit(opNode.getJmmChild(1));
        String rightExpr = simpleExpression;
        Type rightType = varType;

        if (!(opNode.getJmmChild(0).getKind().equals(AstNode.INT.toString())
                || opNode.getJmmChild(0).getKind().equals(AstNode.TRUE.toString())
                || opNode.getJmmChild(0).getKind().equals(AstNode.FALSE.toString())
                || opNode.getJmmChild(0).getKind().equals(AstNode.ID.toString()))) {
            String temp = getTemp();

            code.append(temp)
                    .append(".")
                    .append(OllirUtils.getCode(leftType))
                    .append(" :=.")
                    .append(OllirUtils.getCode(leftType))
                    .append(" ")
                    .append(leftExpr)
                    .append(";\n");

            leftExpr = temp + "." + OllirUtils.getCode(leftType);
        }
        if (!(opNode.getJmmChild(1).getKind().equals(AstNode.INT.toString())
                || opNode.getJmmChild(1).getKind().equals(AstNode.TRUE.toString())
                || opNode.getJmmChild(1).getKind().equals(AstNode.FALSE.toString())
                || opNode.getJmmChild(1).getKind().equals(AstNode.ID.toString()))) {
            String temp = getTemp();

            code.append(temp)
                    .append(".")
                    .append(OllirUtils.getCode(rightType))
                    .append(" :=.")
                    .append(OllirUtils.getCode(rightType))
                    .append(" ")
                    .append(rightExpr)
                    .append(";\n");

            rightExpr = temp + "." + OllirUtils.getCode(rightType);
        }

        terminalCode.append(leftExpr)
                .append(" ")
                .append(op)
                .append(".")
                .append(OllirUtils.getCode(type))
                .append(" ")
                .append(rightExpr);

        varType = type;
        simpleExpression = terminalCode.toString();
        return 0;
    }

    private Integer ifVisit(JmmNode ifNode, Integer dummy) {
        String thenLabel = getLabel();
        String endLabel = getLabel();

        // Condition
        visit(ifNode.getJmmChild(0).getJmmChild(0));

        code.append("if (")
                .append(simpleExpression)
                .append(") goto ")
                .append(thenLabel)
                .append(";\n");

        // Else block
        visit(ifNode.getJmmChild(2).getJmmChild(0));

        if (!simpleExpression.isEmpty()) {
            code.append(simpleExpression);
            code.append(";\n");
        }

        code.append("goto ")
                .append(endLabel)
                .append(";\n")
                .append(thenLabel)
                .append(":\n");

        // Then block
        visit(ifNode.getJmmChild(1));

        if (!simpleExpression.isEmpty()) {
            code.append(simpleExpression);
            code.append(";\n");
        }

        code.append(endLabel)
                .append(":\n");

        simpleExpression = "";

        return 0;
    }

    private Integer whileVisit(JmmNode whileNode, Integer dummy) {
        String loopLabel = getLabel();
        String endLoopLabel = getLabel();

        code.append(loopLabel)
                .append(":\n");

        // Condition
        // Check optimization flag
        if (config.containsKey("optimize") && config.get("optimize").equals("true")) {
            visitAndCreateTemp(whileNode.getJmmChild(0).getJmmChild(0));
            code.append("if (!.bool ")
                    .append(simpleExpression)
                    .append(") goto ")
                    .append(endLoopLabel)
                    .append(";\n");
        } else {
            String bodyLabel = getLabel();
            visit(whileNode.getJmmChild(0).getJmmChild(0));
            code.append("if (")
                    .append(simpleExpression)
                    .append(") goto ")
                    .append(bodyLabel)
                    .append(";\n")
                    .append("goto ")
                    .append(endLoopLabel)
                    .append(";\n")
                    .append(bodyLabel)
                    .append(":\n");
        }

        visit(whileNode.getJmmChild(1));

        if (!simpleExpression.isEmpty()) {
            code.append(simpleExpression);
            code.append(";\n");
        }

        code.append("goto ")
                .append(loopLabel)
                .append(";\n")
                .append(endLoopLabel)
                .append(":\n");

        simpleExpression = "";

        return 0;
    }
}
