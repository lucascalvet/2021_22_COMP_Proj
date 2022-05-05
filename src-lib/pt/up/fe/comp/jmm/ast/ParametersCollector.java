package pt.up.fe.comp.jmm.ast;

import pt.up.fe.comp.jmm.analysis.table.Symbol;
import pt.up.fe.comp.jmm.analysis.table.Type;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class ParametersCollector extends Collector {
    private List<Symbol> parameters;
    private String signature;

    public ParametersCollector(String methodSignature) {
        this.visits = 0;
        this.parameters = new ArrayList<>();
        this.signature = methodSignature;
        addVisit("Program", this::visitDefault);
        addVisit("ClassDecl", this::visitDefault);
        addVisit("ClassBody", this::visitDefault);
        addVisit("MethodDecl", this::visitDefault);
        addVisit("Function", this::visitFunction);
        setDefaultVisit((node, imports) -> ++visits);
    }

    public List<Symbol> getParameters() {
        return this.parameters;
    }

    private Integer visitFunction(JmmNode func, Boolean dummy) {
        Boolean check = false;
        List<Symbol> args = new ArrayList<>();
        for (var child : func.getChildren()) {
            if (child.getKind().equals("FuncName")){
                if(child.getChildren().get(0).get("name").equals(this.signature.substring(0, this.signature.indexOf('(')))){
                    check = true;
                }
                else break;
            }
            if (check && child.getKind().equals("FuncArgs")){
                this.parameters = visitFunctionArgs(child);
                break;
            }
        }

        return ++visits;
    }

    private List <Symbol> visitFunctionArgs(JmmNode node){
        List <Symbol> params = new ArrayList<>();
        Type t = new Type("", false);
        for (var child : node.getChildren()) {
            if (child.getKind().equals("Type")){
                String ts = child.get("type");
                if (ts.equals("int array")){
                    t = new Type("int", true);
                }
                else{
                    t = new Type(ts, false);
                }
            }
            else{
                if (child.getKind().equals("Id")){
                    params.add(new Symbol(t, child.get("name")));
                }
            }
        }
        return params;
    }
}

