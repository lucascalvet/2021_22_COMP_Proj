package pt.up.fe.comp.jmm.ast;

import pt.up.fe.comp.jmm.analysis.table.Symbol;
import pt.up.fe.comp.jmm.analysis.table.Type;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class FieldsCollector extends Collector {
    private final List<Symbol> fields;

    public FieldsCollector() {
        this.visits = 0;
        this.fields = new ArrayList<>();
        addVisit("Program", this::visitDefault);
        addVisit("ClassDecl", this::visitDefault);
        addVisit("ClassBody", this::visitDefault);
        addVisit("Var", this::visitVariable);
        setDefaultVisit((node, imports) -> ++visits);
    }

    public List<Symbol> getFields() {
        return this.fields;
    }

    private Integer visitVariable(JmmNode variable, Boolean dummy) {
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
        this.fields.add(new Symbol(t, n));
        return ++visits;
    }

    private String getCustomVarType(JmmNode custom_var){
        return custom_var.getChildren().get(0).get("name");
    }
}

