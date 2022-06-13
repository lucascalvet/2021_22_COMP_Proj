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

public abstract class Verifier extends PreorderJmmVisitor<Boolean, Boolean> implements SemanticAnalyser {
    protected final SymbolTable symbolTable;
    private final JmmNode root;
    private List<Report> reports;
    protected String scope;

    public Verifier(JmmNode root, SymbolTable symbolTable){
        this.root = root;
        this.scope = "";
        this.symbolTable = symbolTable;
        this.reports = new ArrayList<>();
    }

    protected boolean isValidExternal(JmmNode access){

        if(!access.getKind().equals(AstNode.ACCESS.toString()) || (!access.getJmmChild(0).getKind().equals(AstNode.THIS.toString()) && !access.getJmmChild(0).getAttributes().contains("name"))){
            return false;
        }
        String accessIdName = symbolTable.getClassName();
        if(!access.getJmmChild(0).getKind().equals(AstNode.THIS.toString())){
            accessIdName = access.getJmmChild(0).get("name");
        }
        String className = accessIdName;
        if(getVar(accessIdName) != null){
            className = getVar(accessIdName).getType().getName();
        }
        return ((symbolTable.getClassName().equals(className) && !symbolTable.getSuper().equals("")) || symbolTable.getSuper().equals(className) || symbolTable.getImports().contains(className));
    }

    protected boolean isField(String name){
        if(!this.scope.equals("")){
            for(var local_variable : symbolTable.getLocalVariables(this.scope)){
                if(local_variable.getName().equals(name)){
                    return false;
                }
            }
            for(var param : symbolTable.getParameters(this.scope)){
                if(param.getName().equals(name)){
                    return false;
                }
            }
        }
        for(var field : symbolTable.getFields()){
            if(field.getName().equals(name)){
                return true;
            }
        }
        return false;
    }

    protected Symbol getVar(String name){
        if(!this.scope.equals("")){
            for(var local_variable : symbolTable.getLocalVariables(this.scope)){
                if(local_variable.getName().equals(name)){
                    return local_variable;
                }
            }
            for(var param : symbolTable.getParameters(this.scope)){
                if(param.getName().equals(name)){
                    return param;
                }
            }
        }
        for(var field : symbolTable.getFields()){
            if(field.getName().equals(name)){
                return field;
            }
        }
        return null;
    }

    protected Type getExpressionType(JmmNode expr){
        var kind = expr.getKind();
        var expected_type = new Type("int", false);
        var return_type = new Type("int", false);
        if(AstNode.getTerminalNodes().contains(kind)){
            if(kind.equals(AstNode.ID.toString()) || kind.equals(AstNode.THIS.toString())){
                if(kind.equals(AstNode.ID.toString())){
                    Symbol id_var = this.getVar(expr.get("name"));
                    if(id_var == null){
                        this.addReport(new Report(ReportType.ERROR, Stage.SEMANTIC, Integer.valueOf(expr.get("line")), Integer.valueOf(expr.get("col")), "Variable '" + expr.get("name") + "' isn't declared"));
                        return expected_type;
                    }
                    return id_var.getType();
                }
                else{
                    return new Type(symbolTable.getClassName(), false);
                    //return new Type("this", false);
                }
            }
            if(kind.equals(AstNode.TRUE.toString()) || kind.equals(AstNode.FALSE.toString())){
                return new Type("boolean", false);
            }
            if(kind.equals(AstNode.INT.toString())){
                return new Type("int", false);
            }
        }
        if(kind.equals(AstNode.ACCESS.toString())){
            if(expr.getJmmChild(1).getJmmChild(0).getKind().equals(AstNode.LENGTH.toString())){
                return new Type("int", false);
            }
            else{
                Type function_type = symbolTable.getReturnType(expr.getJmmChild(1).getJmmChild(0).get("name"));
                return function_type;
            }
        }
        if(kind.equals(AstNode.ARRAY_ACCESS.toString())){
            var arr = expr.getJmmChild(0);
            var arr_name = arr.get("name");
            var index = expr.getJmmChild(1);
            if(!this.getExpressionType(index).equals(new Type("int", false))){
                this.addReport(new Report(ReportType.ERROR, Stage.SEMANTIC, Integer.valueOf(index.get("line")), Integer.valueOf(index.get("col")), "Array index must be an integer"));
            }
            if(this.getVar(arr_name) == null){
                this.addReport(new Report(ReportType.ERROR, Stage.SEMANTIC, Integer.valueOf(arr.get("line")), Integer.valueOf(arr.get("col")), "Array '" + arr_name + "' doesn't exist"));
                return new Type("int", false);
            }
            else if (!this.getVar(arr_name).getType().isArray()){
                this.addReport(new Report(ReportType.ERROR, Stage.SEMANTIC, Integer.valueOf(arr.get("line")), Integer.valueOf(arr.get("col")), "To access '" + arr_name + "' it must be an array, instead it's of the type " + this.getVar(arr_name).getType().toString()));
            }

            return new Type(this.getVar(arr_name).getType().getName(), false);
        }
        if(kind.equals(AstNode.NEW.toString())){
            if(expr.getChildren().size() == 2){
                return new Type("int", true);
            }
            else{
                return new Type(expr.getJmmChild(0).get("name"), false);
            }
        }
        if(kind.equals(AstNode.LOWER.toString())){
            expected_type = new Type("int", false);
            return_type = new Type("boolean", false);
        }
        if(AstNode.getAritmeticOp().contains(kind)){
            expected_type = new Type("int", false);
            return_type = new Type("int", false);
        }
        if(AstNode.getBooleanOp().contains(kind)){
            expected_type = new Type("boolean", false);
            return_type = new Type("boolean", false);
        }
        for(var child : expr.getChildren()){
            Type child_type = this.getExpressionType(child);
            if (child_type.isArray()){
                this.addReport(new Report(ReportType.ERROR, Stage.SEMANTIC, Integer.valueOf(child.get("line")), Integer.valueOf(child.get("col")), "Arrays can't be used in arithmetic expressions"));
            }
            if (!child_type.equals(expected_type)){
                this.addReport(new Report(ReportType.ERROR, Stage.SEMANTIC, Integer.valueOf(child.get("line")), Integer.valueOf(child.get("col")), "Operation of type " + expr.getKind() + " expects operands of type " + expected_type.toString() + " but found " + child_type.toString()));
            }
        }
        return return_type;
    }

    @Override
    public List<Report> getReports() {
        this.visit(root);
        return this.reports;
    }

    protected void addReport(Report report){
        this.reports.add(report);
    }
}
