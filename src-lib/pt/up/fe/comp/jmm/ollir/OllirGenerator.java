package pt.up.fe.comp.jmm.ollir;

import pt.up.fe.comp.jmm.analysis.table.Symbol;
import pt.up.fe.comp.jmm.analysis.table.SymbolTable;
import pt.up.fe.comp.jmm.ast.AstNode;
import pt.up.fe.comp.jmm.ast.JmmNode;
import pt.up.fe.comp.jmm.ast.visitors.AJmmVisitor;

import java.util.List;
import java.util.stream.Collectors;

public class OllirGenerator extends AJmmVisitor<Integer, Integer> {
    private final StringBuilder code;
    private final SymbolTable symbolTable;

    public OllirGenerator(SymbolTable symbolTable) {
        this.code = new StringBuilder();
        this.symbolTable = symbolTable;

        addVisit(AstNode.PROGRAM, this::programVisit);
        addVisit(AstNode.CLASS_DECL, this::classDeclVisit);
        addVisit(AstNode.METHOD_DECL, this::methodDeclVisit);
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
        String methodSignature;
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
            visit(stmt);
        }

        code.append("}\n");

        return 0;
    }
}
