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

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class DeadCodeRemover extends PreorderJmmVisitor<Boolean, Boolean> {
    private boolean changeless;

    public DeadCodeRemover(){
        addVisit(AstNode.MAIN_BODY, this::removeDeadCode);
        addVisit(AstNode.BODY, this::removeDeadCode);
        addVisit(AstNode.BLOCK, this::removeDeadCode);
        this.changeless = true;
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
                        for(var block_child : block.getChildren()){
                            //block_child.removeParent();
                            node.add(block_child, index);
                            index += 1;
                        }
                        node.removeJmmChild(child);
                        changeless = false;
                    }
                    else if(condition.get("value").equals("true")){
                        //Remove Else
                        System.out.println("DEADCODE Removed Else on line: " + child.get("line"));
                        int index = child.getIndexOfSelf();
                        final var block = child.getJmmChild(1);
                        for(var block_child : block.getChildren()){
                            //block_child.removeParent();
                            node.add(block_child, index);
                            index += 1;
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
        return this.changeless;
    }

}
