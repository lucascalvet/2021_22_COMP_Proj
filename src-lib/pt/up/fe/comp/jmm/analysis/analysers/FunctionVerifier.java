package pt.up.fe.comp.jmm.analysis.analysers;

import pt.up.fe.comp.jmm.analysis.SemanticAnalyser;
import pt.up.fe.comp.jmm.analysis.table.SymbolTable;
import pt.up.fe.comp.jmm.ast.AstNode;
import pt.up.fe.comp.jmm.ast.JmmNode;
import pt.up.fe.comp.jmm.ast.collectors.Collector;
import pt.up.fe.comp.jmm.ast.visitors.PreorderJmmVisitor;
import pt.up.fe.comp.jmm.report.Report;
import pt.up.fe.comp.jmm.report.ReportType;
import pt.up.fe.comp.jmm.report.Stage;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class FunctionVerifier extends Verifier{

    public FunctionVerifier(JmmNode root, SymbolTable symbolTable) {
        super(root, symbolTable);
        addVisit(AstNode.ACCESS, this::visitAccess);
        addVisit(AstNode.CHAINED, this::visitChained);
    }

    private Boolean visitAccess(JmmNode access, Boolean dummy){
        var child = access.getJmmChild(0);
        if(access.getJmmChild(1).getJmmChild(0).getKind().equals(AstNode.LENGTH.toString())){
            if(!child.getKind().equals(AstNode.ID.toString()) || !this.getVar(child.get("name")).getType().isArray()){
                this.addReport(new Report(ReportType.ERROR, Stage.SEMANTIC, Integer.valueOf(child.get("line")), Integer.valueOf(child.get("col")), "Length call should be done to an array"));
            }
        }
        else{
            if(child.getKind().equals(AstNode.ID.toString())){
                String name = child.get("name");
                Boolean current_class = false;
                if(symbolTable.getClassName().equals(name)){
                    current_class = true;
                }
                else if(!symbolTable.getClassName().equals(name) && !symbolTable.getImports().contains(name) && !symbolTable.getSuper().equals(name)){
                    String class_name = this.getVar(name).getType().getName();
                    if(symbolTable.getClassName().equals(class_name)){
                        current_class = true;
                    }
                    else if(!symbolTable.getClassName().equals(class_name) && !symbolTable.getImports().contains(class_name) && !symbolTable.getSuper().equals(class_name)){
                        this.addReport(new Report(ReportType.ERROR, Stage.SEMANTIC, Integer.valueOf(child.get("line")), Integer.valueOf(child.get("col")), "Couldn't find class named '" + name + "'"));
                        return false;
                    }
                }
                if(current_class && symbolTable.getSuper().equals("")){
                    var acess_method = access.getJmmChild(1).getJmmChild(0);
                    if(!symbolTable.getMethods().contains(acess_method.get("name"))){
                        this.addReport(new Report(ReportType.ERROR, Stage.SEMANTIC, Integer.valueOf(acess_method.get("line")), Integer.valueOf(acess_method.get("col")), "Couldn't find method named '" + acess_method.get("name") + "'"));
                        return false;
                    }
                }
            }
        }
        return true;
    }

    private Boolean visitChained(JmmNode chained, Boolean dummy){
        for(var child : chained.getChildren()) {
            String name = "";
            if (child.getKind().equals(AstNode.ID.toString())) {
                name = child.get("name");
                /*
                if (!symbolTable.getMethods().contains(name) && symbolTable.getSuper().equals("")) {
                    this.reports.add(new Report(ReportType.ERROR, Stage.SEMANTIC, Integer.valueOf(child.get("line")), Integer.valueOf(child.get("col")), "Couldn't find method named '" + name + "' and class " + symbolTable.getClassName() + " doesn't have a super class"));
                    return false;
                }
                */
            }
            else if (name != "" && child.getKind().equals(AstNode.ARGS.toString())){
                var params = symbolTable.getParameters(name);
                if(params.size() == child.getChildren().size()){
                    for (int i = 0; i < params.size(); i++){
                        var grandchild = child.getJmmChild(i);
                        var rparam = this.getExpressionType(grandchild);
                        var tparam = params.get(i).getType();
                        if(!rparam.equals(tparam)){
                            this.addReport(new Report(ReportType.ERROR, Stage.SEMANTIC, Integer.valueOf(grandchild.get("line")), Integer.valueOf(grandchild.get("col")), "Argument number " + i + " of method " + name + " should be of type " + tparam.toString() + " instead it's " + rparam.toString()));
                            return false;
                        }
                    }
                }
                else{
                    this.addReport(new Report(ReportType.ERROR, Stage.SEMANTIC, Integer.valueOf(child.get("line")), Integer.valueOf(child.get("col")), "Wrong number of arguments, " + child.getChildren().size() + " instead of " + params.size()));
                    return false;
                }
            }
        }
        return true;
    }

}
