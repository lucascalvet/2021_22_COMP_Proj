package pt.up.fe.comp.jmm.ollir;

import pt.up.fe.comp.jmm.analysis.table.Symbol;
import pt.up.fe.comp.jmm.analysis.table.SymbolTable;
import pt.up.fe.comp.jmm.analysis.table.Type;
import pt.up.fe.comp.jmm.ast.AstNode;
import pt.up.fe.comp.jmm.ast.JmmNode;
import pt.up.fe.comp.jmm.ast.visitors.AJmmVisitor;

import java.util.List;
import java.util.stream.Collectors;

public class OllirGenerator extends AJmmVisitor<Integer, Integer> {
    private final StringBuilder code;
    private final SymbolTable symbolTable;
    private String methodSignature = null;
    private Type varType;
    private Integer tempVarCounter = 0;
    private Integer labelCounter = 0;

    public OllirGenerator(SymbolTable symbolTable) {
        this.code = new StringBuilder();
        this.symbolTable = symbolTable;

        addVisit(AstNode.PROGRAM, this::programVisit);
        addVisit(AstNode.CLASS_DECL, this::classDeclVisit);
        addVisit(AstNode.METHOD_DECL, this::methodDeclVisit);
        addVisit(AstNode.ASSIGN, this::assignVisit);
        addVisit(AstNode.ID, this::idVisit);
        addVisit(AstNode.INT, this::intVisit);
        addVisit(AstNode.TRUE, this::trueVisit);
        addVisit(AstNode.FALSE, this::falseVisit);
        addVisit(AstNode.NEW, this::newVisit);
        addVisit(AstNode.ACCESS, this::accessVisit);
        addVisit(AstNode.ARRAY_ACCESS, this::arrayAccessVisit);
        addVisit(AstNode.BLOCK, this::blockVisit);
        addVisit(AstNode.RETURN_STATEMENT, this::returnVisit);
        addVisit(AstNode.ADD, this::opVisit);
        addVisit(AstNode.SUBTRACT, this::opVisit);
        addVisit(AstNode.MULTIPLY, this::opVisit);
        addVisit(AstNode.DIVIDE, this::opVisit);
        addVisit(AstNode.IF, this::ifVisit);
        addVisit(AstNode.WHILE, this::whileVisit);
    }

    public String getCode() {
        return code.toString();
    }

    private String getLabel() {
        return "Label" + labelCounter++;
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
        if (superClass != null) {
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
            code.append(";\n");
        }

        code.append("}\n");

        return 0;
    }

    private Integer assignVisit(JmmNode assignNode, Integer dummy) {
        visit(assignNode.getJmmChild(0));

        code.append(" :=.")
                .append(OllirUtils.getOllirType(varType.getName()))
                .append(" ");

        visit(assignNode.getJmmChild(1));

        return 0;
    }

    private Integer idVisit(JmmNode idNode, Integer dummy) {
        List<Symbol> parameters = symbolTable.getParameters(methodSignature);
        List<Symbol> variables = symbolTable.getLocalVariables(methodSignature);

        Symbol symbol;

        for (Symbol variable : variables) {
            if (variable.getName().equals(idNode.get("name"))) {
                symbol = variable;
                code.append(OllirUtils.getCode(symbol));
                varType = symbol.getType();
                return 0;
            }
        }

        for (int param_idx = 0; param_idx < parameters.size(); param_idx++) {
            if (parameters.get(param_idx).getName().equals(idNode.get("name"))) {
                symbol = parameters.get(param_idx);
                code.append("$")
                        .append(param_idx + 1)
                        .append(".")
                        .append(OllirUtils.getCode(symbol));
                varType = symbol.getType();
                return 0;
            }
        }

        return 0;
    }

    private Integer intVisit(JmmNode intNode, Integer dummy) {
        Type type = new Type("int", false);

        code.append(intNode.get("value"))
                .append(".")
                .append(OllirUtils.getCode(type));

        return 0;
    }

    private Integer trueVisit(JmmNode trueNode, Integer dummy) {
        code.append("1.bool");
        return 0;
    }

    private Integer falseVisit(JmmNode falseNode, Integer dummy) {
        code.append("0.bool");
        return 0;
    }

    private Integer newVisit(JmmNode newNode, Integer dummy) {
        if (newNode.getJmmChild(0).getKind().equals(AstNode.INT_TYPE.toString())) {
            Type type = new Type("int", true);
            code.append("new(array, ");
            visit(newNode.getJmmChild(1));
            code.append(").")
                    .append(OllirUtils.getCode(type));
            return 0;
        }

        String typeName = newNode.getJmmChild(0).get("name");

        code.append("new(")
                .append(typeName)
                .append(").")
                .append(typeName)
                .append(";\ninvokespecial(");

        visit(newNode.getJmmParent().getJmmChild(0));

        code.append(", \"<init>\").V");

        return 0;
    }

    private Integer accessVisit(JmmNode accessNode, Integer dummy) {
        JmmNode chained = accessNode.getJmmChild(1);

        if (chained.getJmmChild(0).getKind().equals(AstNode.LENGTH.toString())) {
            code.append("arraylength(");
            visit(accessNode.getJmmChild(0));
            Type type = new Type("int", false);
            code.append(").")
                    .append(OllirUtils.getCode(type));

            return 0;
        }

        //TODO: Detect if Id is from an import, and call invokestatic.

        code.append("invokevirtual(");

        visit(accessNode.getJmmChild(0));

        code.append(", \"")
                .append(chained.getJmmChild(0).get("name"))
                .append("\"");

        JmmNode args = chained.getJmmChild(1);

        for (JmmNode arg : args.getChildren()) {
            code.append(", ");
            visit(arg);
        }

        code.append(")");

        return 0;
    }

    private Integer arrayAccessVisit(JmmNode arrayAccessNode, Integer dummy) {
        JmmNode idNode = arrayAccessNode.getJmmChild(0);

        List<Symbol> parameters = symbolTable.getParameters(methodSignature);
        List<Symbol> variables = symbolTable.getLocalVariables(methodSignature);

        Symbol symbol;

        for (Symbol variable : variables) {
            if (variable.getName().equals(idNode.get("name")) && variable.getType().isArray()) {
                symbol = variable;
                code.append(symbol.getName())
                        .append("[");
                visit(arrayAccessNode.getJmmChild(1).getJmmChild(0));
                code.append("].")
                        .append(OllirUtils.getOllirType(symbol.getType().getName()));
                varType = symbol.getType();
                return 0;
            }
        }

        for (int param_idx = 0; param_idx < parameters.size(); param_idx++) {
            if (parameters.get(param_idx).getName().equals(idNode.get("name")) && parameters.get(param_idx).getType().isArray()) {
                symbol = parameters.get(param_idx);
                code.append("$")
                        .append(param_idx + 1)
                        .append(".")
                        .append(symbol.getName())
                        .append("[");
                visit(arrayAccessNode.getJmmChild(1).getJmmChild(0));
                code.append("].")
                        .append(OllirUtils.getCode(symbol.getType()));
                varType = symbol.getType();
                return 0;
            }
        }

        return 0;
    }

    private Integer blockVisit(JmmNode blockNode, Integer dummy) {
        for (JmmNode stmt : blockNode.getChildren()) {
            visit(stmt);
            code.append(";\n");
        }

        return 0;
    }

    private Integer returnVisit(JmmNode retNode, Integer dummy) {
        code.append("ret.")
                .append(OllirUtils.getCode(symbolTable.getReturnType(methodSignature)))
                .append(" ");

        visit(retNode.getJmmChild(0));

        return 0;
    }

    private Integer opVisit(JmmNode opNode, Integer dummy) {
        Type type = new Type("int", false);

        visit(opNode.getJmmChild(0));

        code.append(" ");

        if (opNode.getKind().equals(AstNode.ADD.toString())) {
            code.append("+");
        } else if (opNode.getKind().equals(AstNode.SUBTRACT.toString())) {
            code.append("-");
        } else if (opNode.getKind().equals(AstNode.MULTIPLY.toString())) {
            code.append("*");
        } else if (opNode.getKind().equals(AstNode.DIVIDE.toString())) {
            code.append("/");
        }

        code.append(".")
                .append(OllirUtils.getCode(type))
                .append(" ");

        visit(opNode.getJmmChild(1));

        return 0;
    }

    private Integer ifVisit(JmmNode ifNode, Integer dummy) {
        String thenLabel = getLabel();
        String endLabel = getLabel();

        code.append("if (");
        // Condition
        visit(ifNode.getJmmChild(0));
        code.append(") goto ")
                .append(thenLabel)
                .append(";\n");

        // Else block
        visit(ifNode.getJmmChild(2));

        code.append("goto ")
                .append(endLabel)
                .append(";\n")
                .append(thenLabel)
                .append(":\n");

        // Then block
        visit(ifNode.getJmmChild(1));

        code.append(endLabel)
                .append(":\n");

        return 0;
    }

    private Integer whileVisit(JmmNode whileNode, Integer dummy) {
        String loopLabel = getLabel();
        String bodyLabel = getLabel();
        String endLoopLabel = getLabel();

        code.append(loopLabel)
                .append(":\n");

        //TODO: "if" here
        code.append("IF...")
                .append("goto ")
                .append(bodyLabel)
                .append(";\n");

        code.append("goto ")
                .append(endLoopLabel)
                .append(";\n")
                .append(bodyLabel)
                .append(":\n");

        visit(whileNode.getJmmChild(1));

        code.append("goto ")
                .append(loopLabel)
                .append(";\n")
                .append(endLoopLabel)
                .append(":\n");

        return 0;
    }
}
