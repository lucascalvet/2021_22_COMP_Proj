package pt.up.fe.comp.jmm.ast;

import pt.up.fe.comp.jmm.analysis.table.Symbol;
import pt.up.fe.comp.jmm.analysis.table.Type;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class ReturnTypeCollector extends AJmmVisitor<Boolean, Integer> {
    private int visits;
    private Type return_type;
    private String signature;

    public ReturnTypeCollector(String methodSignature) {
        this.visits = 0;
        this.return_type = new Type("", false);
        this.signature = methodSignature;
        addVisit("Program", this::visitDefault);
        addVisit("ClassDecl", this::visitDefault);
        addVisit("MethodDecl", this::visitDefault);
        addVisit("Function", this::visitFunction);
        setDefaultVisit((node, imports) -> ++visits);
    }

    public Type getReturnType() {
        return this.return_type;
    }

    private Integer visitDefault(JmmNode node, Boolean dummy) {
        for (var child : node.getChildren()) {
            visit(child, true);
        }
        return ++visits;
    }

    private Integer visitFunction(JmmNode func, Boolean dummy) {
        String rtype = "";
        for (var child : func.getChildren()) {
            if (child.getKind().equals("FuncReturn")) {
                rtype = child.getChildren().get(0).get("type");
            }
            if (child.getKind().equals("FuncName")){
                if (child.getChildren().get(0).get("name").equals(this.signature.substring(0, this.signature.indexOf('(')))){
                    if(rtype.equals("int array")){
                        this.return_type = new Type("int", true);
                    }
                    else this.return_type = new Type(rtype, false);
                    break;
                }
            }
        }
        return ++visits;
    }

}