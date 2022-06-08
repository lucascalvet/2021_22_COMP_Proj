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
        addVisit(AstNode.FUNCTION, this::visitFunction);
        addVisit(AstNode.MAIN, this::visitMain);
        addVisit(AstNode.ACCESS, this::visitAccess);
        addVisit(AstNode.CHAINED, this::visitChained);
    }

    private Boolean visitFunction(JmmNode func, Boolean dummy){
        this.scope = func.getJmmChild(1).getJmmChild(0).get("name");
        return true;
    }

    private Boolean visitMain(JmmNode main, Boolean dummy){
        this.scope = "main";
        return true;
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
                    if(this.getVar(name) == null){
                        this.addReport(new Report(ReportType.ERROR, Stage.SEMANTIC, Integer.valueOf(child.get("line")), Integer.valueOf(child.get("col")), "Couldn't find class named '" + name + "'"));
                        return false;
                    }
                    else{
                        String class_name = this.getVar(name).getType().getName();
                        if(symbolTable.getClassName().equals(class_name)){
                            current_class = true;
                        }
                        else if(!symbolTable.getClassName().equals(class_name) && !symbolTable.getImports().contains(class_name) && !symbolTable.getSuper().equals(class_name)){
                            this.addReport(new Report(ReportType.ERROR, Stage.SEMANTIC, Integer.valueOf(child.get("line")), Integer.valueOf(child.get("col")), "Couldn't find class named '" + name + "'"));
                            return false;
                        }
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
        var first_child = chained.getJmmChild(0);
        if(!first_child.getKind().equals(AstNode.LENGTH.toString())){
            String name = first_child.get("name");
            if(symbolTable.getMethods().contains(name)){
                var args = chained.getJmmChild(1);
                var params = symbolTable.getParameters(name);
                if(params.size() == args.getChildren().size()){
                    for (int i = 0; i < params.size(); i++){
                        var arg = args.getJmmChild(i);
                        var rparam = this.getExpressionType(arg);
                        var tparam = params.get(i).getType();
                        if(!rparam.equals(tparam)){
                            this.addReport(new Report(ReportType.ERROR, Stage.SEMANTIC, Integer.valueOf(arg.get("line")), Integer.valueOf(arg.get("col")), "Argument number " + (i+1) + " of method " + name + " should be of type " + tparam.toString() + " instead it's " + rparam.toString()));
                            return false;
                        }
                    }
                }
                else{
                    this.addReport(new Report(ReportType.ERROR, Stage.SEMANTIC, Integer.valueOf(args.get("line")), Integer.valueOf(args.get("col")), "Wrong number of arguments, " + args.getChildren().size() + " instead of " + params.size()));
                    return false;
                }
            }
        }
        return true;
    }

}
