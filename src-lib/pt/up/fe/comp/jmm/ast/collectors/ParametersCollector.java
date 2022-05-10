package pt.up.fe.comp.jmm.ast.collectors;

import pt.up.fe.comp.jmm.analysis.table.Symbol;
import pt.up.fe.comp.jmm.analysis.table.Type;
import pt.up.fe.comp.jmm.ast.AstNode;
import pt.up.fe.comp.jmm.ast.JmmNode;

import java.util.ArrayList;
import java.util.List;

public class ParametersCollector extends Collector {
    private List<Symbol> parameters;
    private String signature;

    public ParametersCollector(String methodSignature) {
        this.visits = 0;
        this.parameters = new ArrayList<>();
        this.signature = methodSignature;
        addVisit(AstNode.PROGRAM, this::visitDefault);
        addVisit(AstNode.CLASS_DECL, this::visitDefault);
        addVisit(AstNode.CLASS_BODY, this::visitDefault);
        addVisit(AstNode.METHOD_DECL, this::visitDefault);
        //if (this.signature.substring(0, this.signature.indexOf('(')).equals("main")){
        if (this.signature.equals("main")){
            addVisit(AstNode.MAIN, this::visitDefault);
            addVisit(AstNode.MAIN_HEADER, this::visitDefault);
            addVisit(AstNode.MAIN_ARGS, this::visitMainArgs);
        }
        else{
            addVisit(AstNode.FUNCTION, this::visitFunction);
        }
        setDefaultVisit((node, imports) -> ++visits);
    }

    public List<Symbol> getParameters() {
        return this.parameters;
    }

    private Integer visitFunction(JmmNode func, Boolean dummy) {
        Boolean check = false;
        List<Symbol> args = new ArrayList<>();
        for (var child : func.getChildren()) {
            if (child.getKind().equals(AstNode.FUNC_NAME.toString())){
                //if(child.getChildren().get(0).get("name").equals(this.signature.substring(0, this.signature.indexOf('(')))){
                if(child.getChildren().get(0).get("name").equals(this.signature)){
                    check = true;
                }
                else break;
            }
            if (check && child.getKind().equals(AstNode.FUNC_ARGS.toString())){
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
            if (child.getKind().equals(AstNode.TYPE.toString())){
                String ts = child.get("type");
                if (ts.equals("int array")){
                    t = new Type("int", true);
                }
                else{
                    t = new Type(ts, false);
                }
            }
            else{
                if (child.getKind().equals(AstNode.ID.toString())){
                    params.add(new Symbol(t, child.get("name")));
                }
            }
        }
        return params;
    }

    private Integer visitMainArgs(JmmNode node, Boolean dummy){
        for (var child : node.getChildren()) {
            if (child.getKind().equals(AstNode.ID.toString())){
                this.parameters.add(new Symbol(new Type("String", true), child.get("name")));
            }
        }
        return ++visits;
    }
}

