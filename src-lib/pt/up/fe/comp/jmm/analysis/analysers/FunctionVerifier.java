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

public class FunctionVerifier extends PreorderJmmVisitor<Boolean, Boolean> implements SemanticAnalyser{

    private final SymbolTable symbolTable;
    private final JmmNode root;
    private List<Report> reports = new ArrayList<Report>();

    public FunctionVerifier(JmmNode root, SymbolTable symbolTable) {
        this.root = root;
        this.symbolTable = symbolTable;
        this.reports = new ArrayList<Report>();
        addVisit(AstNode.ACCESS, this::visitAccess);
        addVisit(AstNode.CHAINED, this::visitChained);

    }

    private Boolean visitAccess(JmmNode access, Boolean dummy){
        for(var child : access.getChildren()){
            if(child.getKind().equals(AstNode.ID.toString())){
                String name = child.get("name");
                if(!symbolTable.getClassName().equals(name) && !symbolTable.getImports().contains(name) && !symbolTable.getSuper().equals(name)){
                    this.reports.add(new Report(ReportType.ERROR, Stage.SEMANTIC, Integer.valueOf(child.get("line")), Integer.valueOf(child.get("col")), "Couldn't find class named '" + name + "'"));
                    return false;
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
                    var tv = new TypeVerifier(root, symbolTable);
                    for (int i = 0; i < params.size(); i++){
                        var grandchild = child.getJmmChild(i);
                        var rparam = tv.getExpressionType(grandchild);
                        var tparam = params.get(i).getType();
                        if(!rparam.equals(tparam)){
                            this.reports.add(new Report(ReportType.ERROR, Stage.SEMANTIC, Integer.valueOf(grandchild.get("line")), Integer.valueOf(grandchild.get("col")), "Argument number " + i + " of method " + name + " should be of type " + tparam.toString() + " instead it's " + rparam.toString()));
                            return false;
                        }
                    }
                }
                else{
                    this.reports.add(new Report(ReportType.ERROR, Stage.SEMANTIC, Integer.valueOf(child.get("line")), Integer.valueOf(child.get("col")), "Wrong number of arguments, " + child.getChildren().size() + " instead of " + params.size()));
                    return false;
                }
            }
        }
        return true;
    }

    @Override
    public List<Report> getReports() {
        this.visit(root);
        return this.reports;
    }
}
