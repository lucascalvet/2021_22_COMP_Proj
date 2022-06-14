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
import java.util.HashMap;
import java.util.List;

public class TypeVerifier extends Verifier{

    public TypeVerifier(JmmNode root, SymbolTable symbolTable) {
        super(root, symbolTable);
        addVisit(AstNode.FUNCTION, this::visitFunction);
        addVisit(AstNode.MAIN, this::visitMain);
        addVisit(AstNode.RETURN_STATEMENT, this::visitReturnStatement);
        addVisit(AstNode.CONDITION, this::visitCondition);
        addVisit(AstNode.ASSIGN, this::visitAssign);
    }

    private Boolean visitMain(JmmNode main, Boolean dummy){
        this.scope = "main";
        return true;
    }

    private Boolean visitReturnStatement(JmmNode ret_statement, Boolean dummy){
        var child = ret_statement.getJmmChild(0);
        var child_type = this.getExpressionType(child);
        String methodSignature = ret_statement.getJmmParent().getJmmParent().getJmmChild(1).getJmmChild(0).get("name");
        if(!symbolTable.getReturnType(methodSignature).getName().equals("") && !symbolTable.getReturnType(methodSignature).equals(child_type)){
            if(!child_type.getName().equals("") || !isValidExternal(child)){
                this.addReport(new Report(ReportType.ERROR, Stage.SEMANTIC, Integer.valueOf(ret_statement.get("line")), Integer.valueOf(ret_statement.get("col")), "Return statement type must be of the type " + symbolTable.getReturnType(methodSignature) + " but it instead is " + child_type));
                return false;
            }
        }
        return true;
    }

    private Boolean visitFunction(JmmNode func, Boolean dummy){
        String name =  func.getJmmChild(1).getJmmChild(0).get("name");
        this.scope = name;
        var body = func.getJmmChild(3);
        for(var child : body.getChildren()){
            if(child.getKind().equals(AstNode.FUNC_RETURN.toString())){
                var rt = this.getExpressionType(child.getJmmChild(0));
                if (!rt.equals(symbolTable.getReturnType(name))){
                    this.addReport(new Report(ReportType.ERROR, Stage.SEMANTIC, Integer.valueOf(child.get("line")), Integer.valueOf(child.get("col")), "Return type must be of the type " + symbolTable.getReturnType(name).toString() + " but it instead is " + rt.toString()));
                    return false;
                }
            }
        }
        return true;
    }

    private Boolean visitAssign(JmmNode assign, Boolean dummy){
        if(assign.getChildren().size() == 2){
            Type t1 = this.getExpressionType(assign.getJmmChild(0));
            Type t2 = this.getExpressionType(assign.getJmmChild(1));
            if (!t1.equals(t2)){
                if((!symbolTable.getImports().contains(t1.getName()) || !symbolTable.getImports().contains(t2.getName())) && (!symbolTable.getClassName().equals(t2.getName()) || !symbolTable.getSuper().equals(t1.getName()))){
                    if(!t2.getName().equals("") || !isValidExternal(assign.getJmmChild(1))){
                        this.addReport(new Report(ReportType.ERROR, Stage.SEMANTIC, Integer.valueOf(assign.get("line")), Integer.valueOf(assign.get("col")), "Assignment types " + t1.toString() + " and " + t2.toString() + " don't match!"));
                        return false;
                    }
                    /*
                    else if (t2.getName().equals("") && isValidExternal(assign.getJmmChild(1))){
                        assign.getJmmChild(1).put("type", t1.toString());
                    }
                     */
                }
            }
        }
        return true;
    }

    private Boolean visitCondition(JmmNode condition, Boolean dummy){
        var child = condition.getJmmChild(0);
        var child_type = this.getExpressionType(child);
        if(!child_type.equals(new Type("boolean", false))){
            if(!child_type.getName().equals("") || !isValidExternal(child)) {
                this.addReport(new Report(ReportType.ERROR, Stage.SEMANTIC, Integer.valueOf(child.get("line")), Integer.valueOf(child.get("col")), "Condition in '" + condition.getJmmParent().getKind() + "' should be a bool, instead it's of the type " + child_type.toString()));
                return false;
            }
            /*
            else if (child_type.getName().equals("") && isValidExternal(child)){
                child.put("type", "boolean");
            }
             */
        }
        return true;
    }

}
