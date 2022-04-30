package pt.up.fe.comp.jmm.ast;

import pt.up.fe.comp.jmm.analysis.table.Symbol;
import pt.up.fe.comp.jmm.analysis.table.Type;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class LocalVariablesCollector extends AJmmVisitor<Boolean, Integer> {
    private int visits;
    private final List<Symbol> local_vars;
    private String signature;

    public LocalVariablesCollector(String methodSignature) {
        this.visits = 0;
        this.local_vars = new ArrayList<>();
        this.signature = methodSignature;
        addVisit("Program", this::visitDefault);
        addVisit("ClassDecl", this::visitDefault);
        addVisit("MethodDecl", this::visitDefault);
        addVisit("Function", this::visitFunction);
        addVisit("Body", this::visitDefault);
        addVisit("Var", this::visitVariable);
        setDefaultVisit((node, imports) -> ++visits);
    }

    public List<Symbol> getLocalVariables() {
        return this.local_vars;
    }

    private Integer visitDefault(JmmNode node, Boolean dummy) {
        for (var child : node.getChildren()) {
            visit(child, true);
        }
        return ++visits;
    }

    private Integer visitFunction(JmmNode node, Boolean dummy) {
        for (var child : node.getChildren()) {
            if (child.getKind().equals("FuncName")) {
                if (!child.getChildren().get(0).get("name").equals(this.signature.substring(0, this.signature.indexOf('(')))) {
                    break;
                }
            }
            visit(child, true);
        }
        return ++visits;
    }

    private Integer visitVariable(JmmNode variable, Boolean dummy) {
        if (variable.getJmmParent().getKind() != "Body"){
            return ++visits;
        }
        Type t = new Type("int", false);
        String n = "var";
        for (var child : variable.getChildren()) {
            if (child.getKind().equals("Type")){
                switch(child.get("type")){
                    case "custom":
                        t = new Type(child.getChildren().get(0).get("name"), false);
                        break;
                    case "int array":
                        t = new Type("int", true);
                        break;
                    default:
                        t = new Type(child.get("type"), false);
                        break;
                }
            }
            if (child.getKind().equals("Id")){
                n = child.get("name");
            }
        }
        this.local_vars.add(new Symbol(t, n));
        return ++visits;
    }

    private String getCustomVarType(JmmNode custom_var){
        return custom_var.getChildren().get(0).get("name");
    }
}

