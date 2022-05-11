package pt.up.fe.comp.jmm.analysis.analysers;

import pt.up.fe.comp.jmm.analysis.SemanticAnalyser;
import pt.up.fe.comp.jmm.analysis.table.Symbol;
import pt.up.fe.comp.jmm.analysis.table.SymbolTable;
import pt.up.fe.comp.jmm.analysis.table.Type;
import pt.up.fe.comp.jmm.ast.AstNode;
import pt.up.fe.comp.jmm.ast.JmmNode;
import pt.up.fe.comp.jmm.ast.visitors.PreorderJmmVisitor;
import pt.up.fe.comp.jmm.report.Report;
import pt.up.fe.comp.jmm.report.ReportType;
import pt.up.fe.comp.jmm.report.Stage;

import java.util.ArrayList;
import java.util.List;

public class TypeVerifier extends Verifier{

    public TypeVerifier(JmmNode root, SymbolTable symbolTable) {
        super(root, symbolTable);
        addVisit(AstNode.METHOD_DECL, this::visitMethodDeclaration);
        addVisit(AstNode.RETURN_STATEMENT, this::visitReturnStatement);
        addVisit(AstNode.CONDITION, this::visitCondition);
        addVisit(AstNode.ASSIGN, this::visitAssign);
    }

    private Boolean visitReturnStatement(JmmNode ret_statement, Boolean dummy){
        for(var child : ret_statement.getChildren()){
            this.getExpressionType(child);
        }
        return true;
    }

    private Boolean visitMethodDeclaration(JmmNode method, Boolean dummy){
        String name = "";
        for(var child : method.getChildren()){
            if(child.getKind().equals(AstNode.FUNC_NAME.toString())){
                name = child.get("name");
            }
            if(name != "" && child.getKind().equals(AstNode.BODY.toString())){
                for(var grandchild : child.getChildren()){
                    if(grandchild.getKind().equals(AstNode.FUNC_RETURN.toString())){
                        var rt = this.getExpressionType(grandchild.getJmmChild(0));
                        if (!rt.equals(symbolTable.getReturnType(name))){
                            this.addReport(new Report(ReportType.ERROR, Stage.SEMANTIC, Integer.valueOf(grandchild.get("line")), Integer.valueOf(grandchild.get("col")), "Return type must be of the type " + symbolTable.getReturnType(name).toString() + " but it instead is " + rt.toString()));
                            return false;
                        }
                    }
                }
            }
        }
        return true;
    }

    private Boolean visitAssign(JmmNode assign, Boolean dummy){
        if(assign.getChildren().size() == 2){
            Type t1 = this.getExpressionType(assign.getJmmChild(0));
            //System.out.println("T1: " + t1.toString());
            Type t2 = this.getExpressionType(assign.getJmmChild(1));
            //System.out.println("T2: " + t2.toString());
            if (!t1.equals(t2)){
                if((!symbolTable.getImports().contains(t1.getName()) || !symbolTable.getImports().contains(t2.getName())) && (!symbolTable.getClassName().equals(t1.getName()) || !symbolTable.getSuper().equals(t2.getName()))){
                    this.addReport(new Report(ReportType.ERROR, Stage.SEMANTIC, Integer.valueOf(assign.get("line")), Integer.valueOf(assign.get("col")), "Assignment types " + t1.toString() + " and " + t2.toString() + " don't match!"));
                    return false;
                }
            }
        }
        return true;
    }

    private Boolean visitCondition(JmmNode condition, Boolean dummy){
        var child = condition.getJmmChild(0);
        //System.out.println("DEBUG: " + child.getKind());
        var child_type = this.getExpressionType(child);
        //System.out.println("DEBUG2: " + child_type.toString());
        if(!child_type.equals(new Type("boolean", false))){
            this.addReport(new Report(ReportType.ERROR, Stage.SEMANTIC, Integer.valueOf(child.get("line")), Integer.valueOf(child.get("col")), "Condition " + child.toString() + " should be a bool, instead it's of the type " + child_type.toString()));
            return false;
        }
        return true;
    }

}
