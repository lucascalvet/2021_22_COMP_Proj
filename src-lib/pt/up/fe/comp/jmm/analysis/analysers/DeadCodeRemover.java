package pt.up.fe.comp.jmm.analysis.analysers;

import pt.up.fe.comp.jmm.analysis.SemanticAnalyser;
import pt.up.fe.comp.jmm.analysis.table.SymbolTable;
import pt.up.fe.comp.jmm.ast.AstNode;
import pt.up.fe.comp.jmm.ast.JmmNode;
import pt.up.fe.comp.jmm.ast.annotators.ValueAnnotator;
import pt.up.fe.comp.jmm.ast.visitors.PreorderJmmVisitor;
import pt.up.fe.comp.jmm.report.Report;
import pt.up.fe.comp.jmm.report.ReportType;
import pt.up.fe.comp.jmm.report.Stage;

import java.util.*;
import java.util.stream.Collectors;

public class DeadCodeRemover extends PreorderJmmVisitor<Boolean, Boolean> {
    private boolean changeless;
    private Map<String, List<JmmNode>> declaredVars;
    private Map<String, JmmNode> imports;

    public DeadCodeRemover(){
        this.changeless = true;
        this.declaredVars = new HashMap();
        this.imports = new HashMap();
        addVisit(AstNode.IMPORT, this::visitImport);
        addVisit(AstNode.VAR, this::visitVar);
        addVisit(AstNode.ASSIGN, this::visitAssign);
        addVisit(AstNode.ID, this::visitId);
        addVisit(AstNode.MAIN_BODY, this::visitBody);
        addVisit(AstNode.BODY, this::visitBody);
        addVisit(AstNode.BLOCK, this::removeDeadCode);
    }

    private Boolean clearUnusedVars(){
        if(this.declaredVars.isEmpty()){
            return true;
        }
        for(String key : this.declaredVars.keySet()){
            final var nodeList = this.declaredVars.get(key);
            for(var node : nodeList){
                if(node.getKind().equals(AstNode.ASSIGN.toString()) && node.getJmmChild(1).getKind().equals(AstNode.ACCESS.toString())){
                    node.getJmmParent().setChild(node.getJmmChild(1), node.getIndexOfSelf());
                }
                else{
                    node.getJmmParent().removeJmmChild(node);
                }
            }
        }
        this.declaredVars = new HashMap();
        return false;
    }

    private Boolean clearUnusedImports(){
        if(this.imports.isEmpty()){
            return true;
        }
        for(String key : this.imports.keySet()){
            final var node = this.imports.get(key);
            node.getJmmParent().removeJmmChild(node);
        }
        this.imports = new HashMap();
        return false;
    }

    private Boolean visitImport(JmmNode node, Boolean dummy){
        var packageNode = node.getJmmChild(0);
        var importString = packageNode.getChildren().stream()
                .map(id -> id.get("name"))
                .collect(Collectors.joining("."));

        this.imports.put(importString, node);
        return true;
    }

    private Boolean visitBody(JmmNode node, Boolean dummy){
        clearUnusedVars();
        return removeDeadCode(node, dummy);
    }

    private Boolean visitVar(JmmNode node, Boolean dummy){
        if(!node.getJmmParent().getKind().equals(AstNode.CLASS_BODY.toString())){
            final var nodeList = new ArrayList<JmmNode>();
            nodeList.add(node);
            this.declaredVars.put(node.getJmmChild(1).get("name"), nodeList);
            return true;
        }
        return false;
    }

    private Boolean visitAssign(JmmNode node, Boolean dummy){
        if(node.getJmmChild(0).getAttributes().contains("name") && declaredVars.containsKey(node.getJmmChild(0).get("name"))){
            List<JmmNode> nodeList = this.declaredVars.get(node.getJmmChild(0).get("name"));
            nodeList.add(node);
            this.declaredVars.put(node.getJmmChild(0).get("name"), nodeList);
            return true;
        }
        return false;
    }

    private Boolean visitId(JmmNode node, Boolean dummy){
        if(node.getAttributes().contains("name")){
            final String nodeName = node.get("name");
            if(this.declaredVars.containsKey(nodeName)){
                if(node.getJmmParent().getKind().equals(AstNode.VAR.toString()) || (node.getJmmParent().getKind().equals(AstNode.ASSIGN.toString()) && node.getIndexOfSelf() == 0)){
                    return false;
                }
                this.declaredVars.remove(nodeName);
                return true;
            }
            if(this.imports.containsKey(nodeName)){
                if(node.getJmmParent().getKind().equals(AstNode.IMPORT.toString()) || node.getJmmParent().getKind().equals(AstNode.PACKAGE.toString())){
                    return false;
                }
                this.imports.remove(nodeName);
                return true;
            }
        }
        return false;
    }

    private Boolean removeDeadCode(JmmNode node, Boolean dummy) {
        for (var child : node.getChildren()) {
            final String kind = child.getKind();
            if(kind.equals(AstNode.WHILE.toString())){
                final var condition = child.getJmmChild(0);
                if(condition.getAttributes().contains("value") && condition.get("value").equals("false")){
                    //Remove While
                    System.out.println("DEADCODE Removed While on line: " + child.get("line"));
                    node.removeJmmChild(child);
                    changeless = false;
                }
            }
            else if(kind.equals(AstNode.IF.toString())){
                final var condition = child.getJmmChild(0);
                if(condition.getAttributes().contains("value")) {
                    if(condition.get("value").equals("false")) {
                        //Remove If
                        System.out.println("DEADCODE Removed If on line: " + child.get("line"));
                        int index = child.getIndexOfSelf();
                        final var block = child.getJmmChild(2).getJmmChild(0);
                        if(block.getKind().equals(AstNode.BLOCK.toString())){
                            for(var block_child : block.getChildren()){
                                //block_child.removeParent();
                                node.add(block_child, index);
                                index += 1;
                            }
                        }
                        else{
                            node.add(block, index);
                        }

                        node.removeJmmChild(child);
                        changeless = false;
                    }
                    else if(condition.get("value").equals("true")){
                        //Remove Else
                        System.out.println("DEADCODE Removed Else on line: " + child.get("line"));
                        int index = child.getIndexOfSelf();
                        final var block = child.getJmmChild(1);
                        if(block.getKind().equals(AstNode.BLOCK.toString())){
                            for(var block_child : block.getChildren()){
                                //block_child.removeParent();
                                node.add(block_child, index);
                                index += 1;
                            }
                        }
                        else{
                            node.add(block, index);
                        }
                        node.removeJmmChild(child);
                        changeless = false;
                    }
                }
            }
        }
        return true;
    }

    public boolean isAllRemoved(){
        if(this.changeless){
            clearUnusedImports();
            return clearUnusedVars();
        }
        return false;
    }

}
