package pt.up.fe.comp.jmm.ollir;

import pt.up.fe.comp.Int;
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
    private String methodSignature;
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
        addVisit(AstNode.NEW, this::newVisit);
        addVisit(AstNode.ACCESS, this::accessVisit);
        addVisit(AstNode.RETURN_STATEMENT, this::returnVisit);
        addVisit(AstNode.ADD, this::opVisit);
        addVisit(AstNode.SUBTRACT, this::opVisit);
        addVisit(AstNode.MULTIPLY, this::opVisit);
        addVisit(AstNode.DIVIDE, this::opVisit);
    }

    public String getCode() {
        return code.toString();
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

    private Integer assignVisit(JmmNode assign, Integer dummy) {
        visit(assign.getJmmChild(0));

        code.append(" :=.")
                .append(OllirUtils.getCode(varType))
                .append(" ");

        visit(assign.getJmmChild(1));

        return 0;
    }

    private Integer idVisit(JmmNode id, Integer dummy) {
        List<Symbol> parameters = symbolTable.getParameters(methodSignature);
        List<Symbol> variables = symbolTable.getLocalVariables(methodSignature);

        Symbol symbol;

        for (Symbol variable : variables) {
            if (variable.getName().equals(id.get("name"))) {
                symbol = variable;
                code.append(OllirUtils.getCode(symbol));
                varType = symbol.getType();
                return 0;
            }
        }

        for (int param_idx = 0; param_idx < parameters.size(); param_idx++) {
            if (parameters.get(param_idx).getName().equals(id.get("name"))) {
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

    private Integer newVisit(JmmNode newNode, Integer dummy) {
        String typeName = newNode.getJmmChild(0).get("name");

        code.append("new(")
                .append(typeName)
                .append(").")
                .append(typeName)
                .append(";\n")
                .append("invokespecial(");

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
}
