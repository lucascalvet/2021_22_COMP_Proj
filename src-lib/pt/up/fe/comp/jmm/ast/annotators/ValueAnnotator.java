package pt.up.fe.comp.jmm.ast.annotators;

import pt.up.fe.comp.BaseNode;
import pt.up.fe.comp.jmm.ast.AstNode;
import pt.up.fe.comp.jmm.ast.JmmNode;
import pt.up.fe.comp.jmm.ast.JmmNodeImpl;
import pt.up.fe.comp.jmm.ast.visitors.AJmmVisitor;
import pt.up.fe.comp.jmm.ast.visitors.PreorderJmmVisitor;

import java.util.HashMap;
import java.util.Map;

public class ValueAnnotator extends PreorderJmmVisitor<Boolean, Boolean> {
    Map<String, String> vars;
    public ValueAnnotator(){
        this.vars = new HashMap<>();
        addVisit(AstNode.WHILE, this::visitWhile);
        addVisit(AstNode.BLOCK, this::visitBlock);
        addVisit(AstNode.BODY, this::visitBody);
        addVisit(AstNode.MAIN_BODY, this::visitBody);
        addVisit(AstNode.ASSIGN, this::annotateAssign);
        addVisit(AstNode.CONDITION, this::annotateCondition);
        setDefaultVisit(this::visitDefault);
    }

    private Boolean visitDefault(JmmNode node, Boolean dummy) {
        if(node.getJmmParent() != null && (node.getJmmParent().equals(AstNode.WHILE.toString()) || (node.getJmmParent().equals(AstNode.BLOCK.toString()) && node.getJmmParent().getJmmParent().equals(AstNode.WHILE.toString())))){
            return false;
        }
        for(var child : node.getChildren()){
            if(child.getKind().equals(AstNode.ID.toString())){
                changeId(child);
            }
        }
        return true;
    }

    private String getValue(JmmNode node){
        if(node.getAttributes().contains("value")){
            return node.get("value");
        }
        String kind = node.getKind();
        if(kind.equals(AstNode.TRUE.toString())){
            return "true";
        }
        if(kind.equals(AstNode.FALSE.toString())){
            return "false";
        }
        if(kind.equals(AstNode.ID.toString())){
            if(vars.containsKey(node.get("name"))){
                return vars.get(node.get("name"));
            }
            return "";
        }
        final int size = node.getChildren().size();
        if(size < 1){
            return "";
        }
        final String v1 = getValue(node.getJmmChild(0));
        if(v1.equals("")){
            return "";
        }
        if(kind.equals(AstNode.NOT.toString())){
            if(v1.equals("false")){
                return "true";
            }
            else if (v1.equals("true")){
                return "false";
            }
            return "";
        }
        if(size < 2){
            return "";
        }
        final String v2 = getValue(node.getJmmChild(1));
        if(v2.equals("")){
            return "";
        }
        if(kind.equals(AstNode.AND.toString())){
            if(v1.equals("true") && v2.equals("true")){
                return "true";
            }
            else if (v1.equals("false") || v2.equals("false")){
                return "false";
            }
            return "";
        }

        final int vint1, vint2;
        try{
            vint1 = Integer.parseInt(v1);
            vint2 = Integer.parseInt(v2);
        }
        catch (Exception e){
            return "";
        }

        if(kind.equals(AstNode.LOWER.toString())){
            if(vint1 < vint2){
                return "true";
            }
            return "false";
        }
        if(kind.equals(AstNode.ADD.toString())){
            return Integer.toString(vint1 + vint2);
        }
        if(kind.equals(AstNode.SUBTRACT.toString())){
            return Integer.toString(vint1 - vint2);
        }
        if(kind.equals(AstNode.MULTIPLY.toString())){
            return Integer.toString(vint1 * vint2);
        }
        if(kind.equals(AstNode.DIVIDE.toString())){
            return Integer.toString(vint1 / vint2);
        }
        return "";
    }

    private Boolean visitBlock(JmmNode node, Boolean dummy) {
        if(node.getJmmParent().getKind().equals(AstNode.WHILE.toString())){
            for (var child : node.getChildren()) {
                if(child.getKind().equals(AstNode.ASSIGN.toString())){
                    annotateAssign(child, true);
                }
            }
        }
        return true;
    }

    private Boolean visitWhile(JmmNode node, Boolean dummy) {
        for (var child : node.getChildren()) {
            if(child.getKind().equals(AstNode.ASSIGN.toString())){
                annotateAssign(child, true);
            }
        }
        return true;
    }

    private Boolean visitBody(JmmNode node, Boolean dummy) {
        this.vars = new HashMap<>();
        return visitDefault(node, dummy);
    }

    private Boolean changeId(JmmNode node) {
        if(!node.getJmmParent().getKind().equals(AstNode.ASSIGN.toString()) || node.getIndexOfSelf() != 0){
            if(node.getAttributes().contains("name") && vars.containsKey(node.get("name"))){
                final String value = vars.get(node.get("name"));
                if(value.equals("true")){
                    var newNode = new JmmNodeImpl(AstNode.TRUE.toString());
                    for(String at : node.getAttributes()){
                        newNode.put(at, node.get(at));
                    }
                    node.getJmmParent().setChild(newNode, node.getIndexOfSelf());
                }
                else if (value.equals("false")){
                    var newNode = new JmmNodeImpl(AstNode.FALSE.toString());
                    for(String at : node.getAttributes()){
                        newNode.put(at, node.get(at));
                    }
                    node.getJmmParent().setChild(newNode, node.getIndexOfSelf());
                }
                else{
                    var newNode = new JmmNodeImpl(AstNode.INT.toString());
                    for(String at : node.getAttributes()){
                        newNode.put(at, node.get(at));
                    }
                    newNode.put("value", value);
                    node.getJmmParent().setChild(newNode, node.getIndexOfSelf());
                }
                return false;
            }
        }
        return true;
    }

    private Boolean annotateAssign(JmmNode assign, Boolean dummy){
        if(assign.getJmmParent().getKind().equals(AstNode.BLOCK.toString()) || assign.getJmmParent().getKind().equals(AstNode.IF.toString()) || assign.getJmmParent().getKind().equals(AstNode.ELSE.toString()) || assign.getJmmParent().getKind().equals(AstNode.WHILE.toString())){
            if(assign.getJmmChild(0).getAttributes().contains("name")){
                vars.remove(assign.getJmmChild(0).get("name"));
            }
            return true;
        }
        final String value = getValue(assign.getJmmChild(1));
        if(value.equals("")){
            if(assign.getJmmChild(0).getAttributes().contains("name")){
                vars.remove(assign.getJmmChild(0).get("name"));
            }
            return false;
        }

        if(assign.getJmmChild(0).getAttributes().contains("name")){
            vars.put(assign.getJmmChild(0).get("name"), value);
        }

        return true;
    }

    private Boolean annotateCondition(JmmNode condition, Boolean dummy){
        final String value = getValue(condition.getJmmChild(0));
        if(value.equals("")){
            return false;
        }

        condition.put("value", value);

        return true;
    }

}

