package pt.up.fe.comp.jmm.ast;

import pt.up.fe.comp.jmm.analysis.table.Symbol;
import pt.up.fe.comp.jmm.analysis.table.Type;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class MethodCollector extends Collector {
    private final List<String> methods;

    public MethodCollector() {
        this.visits = 0;
        this.methods = new ArrayList<>();
        addVisit("Program", this::visitDefault);
        addVisit("ClassDecl", this::visitDefault);
        addVisit("ClassBody", this::visitDefault);
        addVisit("MethodDecl", this::visitDefault);
        addVisit("Function", this::visitFunction);
        setDefaultVisit((node, imports) -> ++visits);
    }

    public List<String> getMethods() {
        return this.methods;
    }

    private Integer visitFunction(JmmNode func, Boolean dummy) {
        String kind = "";
        String name = "";
        List<String> args = new ArrayList<>();
        String method = "";
        for (var child : func.getChildren()) {
            if (child.getKind().equals("FuncReturn")){
                kind = child.getChildren().get(0).get("type");
            }
            if (child.getKind().equals("FuncName")){
                name = child.getChildren().get(0).get("name");
            }
            if (child.getKind().equals("FuncArgs")){
                args = visitFunctionArgs(child);
            }
        }
        //method = kind + " " + name + "(";
        method = name + "(";
        Boolean first = true;
        for (String arg : args){
            if (first){
                method += arg;
                first = false;
            }
            else{
                method += ", " + arg;
            }
        }
        method += ")";
        this.methods.add(method);
        return ++visits;
    }

    private List <String> visitFunctionArgs(JmmNode node){
        List <String> args = new ArrayList<>();
        String t = "";
        for (var child : node.getChildren()) {
            if (child.getKind().equals("Type")){
                t = child.get("type");
                if (t.equals("int array")){
                    t = "int[]";
                }
            }
            else{
                if (child.getKind().equals("Id")){
                    args.add(t + " " + child.get("name"));
                }
            }
        }
        return args;
    }
}

