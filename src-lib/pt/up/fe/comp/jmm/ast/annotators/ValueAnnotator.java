package pt.up.fe.comp.jmm.ast.annotators;

import pt.up.fe.comp.BaseNode;
import pt.up.fe.comp.jmm.ast.AstNode;
import pt.up.fe.comp.jmm.ast.JmmNode;
import pt.up.fe.comp.jmm.ast.visitors.AJmmVisitor;
import pt.up.fe.comp.jmm.ast.visitors.PreorderJmmVisitor;

import java.util.HashMap;
import java.util.Map;

public class ValueAnnotator extends PreorderJmmVisitor<Boolean, Boolean> {
    Map<String, String> vars;
    public ValueAnnotator(){
        vars = new HashMap<>();
        addVisit(AstNode.MAIN_BODY, this::visitBody);
        addVisit(AstNode.BODY, this::visitBody);
        addVisit(AstNode.BLOCK, this::visitBlock);
        addVisit(AstNode.CONDITION, this::annotateCondition);
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

    private Boolean visitBody(JmmNode node, Boolean dummy) {
        for (var child : node.getChildren()) {
            if(child.getKind().equals(AstNode.ASSIGN.toString())){
                annotateAssign(child, false);
            }
        }
        return true;
    }

    private Boolean visitBlock(JmmNode node, Boolean dummy) {
        for (var child : node.getChildren()) {
            if(child.getKind().equals(AstNode.ASSIGN.toString())){
                annotateAssign(child, true);
            }
        }
        return true;
    }

    private Boolean annotateAssign(JmmNode assign, Boolean erase){
        if(erase){
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

