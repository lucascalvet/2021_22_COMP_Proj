package pt.up.fe.comp.jmm.ast.collectors;

import pt.up.fe.comp.jmm.ast.AstNode;
import pt.up.fe.comp.jmm.ast.JmmNode;

import java.util.ArrayList;
import java.util.List;

public class MethodCollector extends Collector {
    private final List<String> methods;

    public MethodCollector() {
        this.visits = 0;
        this.methods = new ArrayList<>();
        addVisit(AstNode.PROGRAM, this::visitDefault);
        addVisit(AstNode.CLASS_DECL, this::visitDefault);
        addVisit(AstNode.CLASS_BODY, this::visitDefault);
        addVisit(AstNode.METHOD_DECL, this::visitDefault);
        addVisit(AstNode.FUNCTION, this::visitFunction);
        addVisit(AstNode.MAIN, this::visitDefault);
        addVisit(AstNode.MAIN_HEADER, this::visitMain);
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
            /*
            if (child.getKind().equals(AstNode.FUNC_RETURN.toString())){
                kind = child.getChildren().get(0).get("type");
            }
            */
            if (child.getKind().equals(AstNode.FUNC_NAME.toString())){
                name = child.getChildren().get(0).get("name");
            }
            /*
            if (child.getKind().equals(AstNode.FUNC_ARGS.toString())){
                args = visitFunctionArgs(child);
            }
            */
        }
        //method = kind + " " + name + "(";
        //method = name + "(";
        method = name;
        /*
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
        */
        this.methods.add(method);
        return ++visits;
    }

    /*
    private List <String> visitFunctionArgs(JmmNode node){
        List <String> args = new ArrayList<>();
        String t = "";
        for (var child : node.getChildren()) {
            if (child.getKind().equals(AstNode.TYPE.toString())){
                t = child.get("type");
                if (t.equals("int array")){
                    t = "int[]";
                }
            }
            else{
                if (child.getKind().equals(AstNode.ID.toString())){
                    args.add(t + " " + child.get("name"));
                }
            }
        }
        return args;
    }

    private Integer visitMainArgs(JmmNode node, Boolean dummy){
        for (var child : node.getChildren()) {
            if (child.getKind().equals(AstNode.ID.toString())){
                this.methods.add("main(String[] " + child.get("name") + ")");
            }
        }
        return ++visits;
    }

    */

    private Integer visitMain(JmmNode main, Boolean dummy){
        this.methods.add("main");
        return ++visits;
    }
}

