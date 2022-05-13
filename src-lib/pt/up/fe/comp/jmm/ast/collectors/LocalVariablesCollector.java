package pt.up.fe.comp.jmm.ast.collectors;

import pt.up.fe.comp.jmm.analysis.table.Symbol;
import pt.up.fe.comp.jmm.analysis.table.Type;
import pt.up.fe.comp.jmm.ast.AstNode;
import pt.up.fe.comp.jmm.ast.JmmNode;

import java.util.ArrayList;
import java.util.List;

public class LocalVariablesCollector extends Collector {
    private final List<Symbol> local_vars;
    private String signature;

    public LocalVariablesCollector(String methodSignature) {
        this.visits = 0;
        this.local_vars = new ArrayList<>();
        this.signature = methodSignature;
        addVisit(AstNode.PROGRAM, this::visitDefault);
        addVisit(AstNode.CLASS_DECL, this::visitDefault);
        addVisit(AstNode.CLASS_BODY, this::visitClassBody);
        addVisit(AstNode.METHOD_DECL, this::visitDefault);
        if (this.signature.equals("main")){
            addVisit(AstNode.MAIN, this::visitDefault);
            addVisit(AstNode.MAIN_BODY, this::visitDefault);
        }
        else{
            addVisit(AstNode.FUNCTION, this::visitFunction);
            addVisit(AstNode.BODY, this::visitDefault);
        }
        addVisit(AstNode.VAR, this::visitVariable);
        setDefaultVisit((node, imports) -> ++visits);
    }

    public List<Symbol> getLocalVariables() {
        return this.local_vars;
    }

    private Integer visitClassBody(JmmNode node, Boolean dummy) {
        for (var child : node.getChildren()) {
            if(child.getKind().equals(AstNode.METHOD_DECL.toString())){
                visit(child, true);
            }
        }
        return ++visits;
    }

    private Integer visitFunction(JmmNode node, Boolean dummy) {
        for (var child : node.getChildren()) {
            if (child.getKind().equals(AstNode.FUNC_NAME.toString())) {
                if (!child.getJmmChild(0).get("name").equals(this.signature)) {
                    break;
                }
            }
            visit(child, true);
        }
        return ++visits;
    }

    private Integer visitVariable(JmmNode variable, Boolean dummy) {
        Type t = new Type("int", false);
        String n = "var";
        for (var child : variable.getChildren()) {
            if (child.getKind().equals(AstNode.TYPE.toString())){
                switch(child.get("type")){
                    case "custom":
                        t = new Type(child.getJmmChild(0).get("name"), false);
                        break;
                    case "int array":
                        t = new Type("int", true);
                        break;
                    default:
                        t = new Type(child.get("type"), false);
                        break;
                }
            }
            if (child.getKind().equals(AstNode.ID.toString())){
                n = child.get("name");
            }
        }
        this.local_vars.add(new Symbol(t, n));
        return ++visits;
    }
}

