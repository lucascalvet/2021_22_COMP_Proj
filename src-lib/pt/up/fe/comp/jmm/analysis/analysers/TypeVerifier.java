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

public class TypeVerifier extends PreorderJmmVisitor<Boolean, Boolean> implements SemanticAnalyser{

    private final SymbolTable symbolTable;
    private final JmmNode root;
    private List<Report> reports = new ArrayList<Report>();
    private final List<Symbol> vars;

    public TypeVerifier(JmmNode root, SymbolTable symbolTable) {
        this.root = root;
        this.symbolTable = symbolTable;
        this.reports = new ArrayList<Report>();
        this.vars = symbolTable.getFields();
        for(var methodSignature : symbolTable.getMethods()){
            this.vars.addAll(symbolTable.getParameters(methodSignature));
            this.vars.addAll(symbolTable.getLocalVariables(methodSignature));
        }
        addVisit(AstNode.METHOD_DECL, this::visitMethodDeclaration);
        addVisit(AstNode.CONDITION, this::visitCondition);
        addVisit(AstNode.ASSIGN, this::visitAssign);
    }

    private Symbol getVar(String name){
        for(var variable : this.vars){
            if(variable.getName().equals(name)){
                return variable;
            }
        }
        return null;
    }

    protected Type getExpressionType(JmmNode expr){
        var kind = expr.getKind();
        System.out.println("GET: " + expr.getKind());
        var expected_type = new Type("int", false);
        var return_type = new Type("int", false);
        if(AstNode.getTerminalNodes().contains(kind)){
            if(kind.equals(AstNode.ID.toString()) || kind.equals(AstNode.THIS.toString())){
                if(kind.equals(AstNode.ID.toString())){
                    System.out.println("GETT: " + expr.get("name"));
                    Symbol id_var = this.getVar(expr.get("name"));
                    if(id_var == null){
                        System.out.println("GETTY: " + expr.get("name"));
                        this.reports.add(new Report(ReportType.ERROR, Stage.SEMANTIC, Integer.valueOf(expr.get("line")), Integer.valueOf(expr.get("col")), "Variable " + expr.get("name") + " isn't declared"));
                    }
                    System.out.println("GETT: " + expr.get("name"));
                    return id_var.getType();
                }
                else{
                    return new Type("this", false);
                }
            }
            if(kind.equals(AstNode.TRUE.toString()) || kind.equals(AstNode.FALSE.toString())){
                return new Type("boolean", false);
            }
            if(kind.equals(AstNode.INT.toString())){
                return new Type("int", false);
            }
        }
        if(kind.equals(AstNode.ARRAY_ACCESS.toString())){
            var arr = expr.getJmmChild(0);
            var arr_name = arr.get("name");
            var index = expr.getJmmChild(1);
            if(this.getVar(arr_name) == null){
                this.reports.add(new Report(ReportType.ERROR, Stage.SEMANTIC, Integer.valueOf(arr.get("line")), Integer.valueOf(arr.get("col")), "Array " + arr_name + " doesn't exist"));
            }
            else if (!this.getVar(arr_name).getType().isArray()){
                this.reports.add(new Report(ReportType.ERROR, Stage.SEMANTIC, Integer.valueOf(arr.get("line")), Integer.valueOf(arr.get("col")), "To access " + arr_name + " it must be an array, instead it's of the type " + this.getVar(arr_name).getType().toString()));
            }
            if(!this.getExpressionType(index).equals(new Type("int", false))){
                this.reports.add(new Report(ReportType.ERROR, Stage.SEMANTIC, Integer.valueOf(index.get("line")), Integer.valueOf(index.get("col")), "Array index must be an integer"));
            }
            return new Type("int", false);
        }
        if(kind.equals(AstNode.LOWER)){
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
                this.reports.add(new Report(ReportType.ERROR, Stage.SEMANTIC, Integer.valueOf(child.get("line")), Integer.valueOf(child.get("col")), "Arrays can't be used in arithmetic expressions"));
            }
            if (!child_type.equals(expected_type)){
                this.reports.add(new Report(ReportType.ERROR, Stage.SEMANTIC, Integer.valueOf(child.get("line")), Integer.valueOf(child.get("col")), "Operation of type " + expr.getKind() + " expects operands of type " + expected_type.toString() + " but found " + child_type.toString()));
            }
        }
        return return_type;
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
                            this.reports.add(new Report(ReportType.ERROR, Stage.SEMANTIC, Integer.valueOf(grandchild.get("line")), Integer.valueOf(grandchild.get("col")), "Return type must be of the type " + symbolTable.getReturnType(name).toString() + " but it instead is " + rt.toString()));
                            return false;
                        }
                    }
                }
            }
        }
        return true;
    }

    private Boolean visitAssign(JmmNode assign, Boolean dummy){
        System.out.println("Ass: " + assign.toString());
        System.out.println("Asss: " + assign.getJmmChild(0));
        if(assign.getChildren().size() == 2){
            Type t1 = this.getExpressionType(assign.getJmmChild(0));
            System.out.println("T1: " + t1.toString());
            Type t2 = this.getExpressionType(assign.getJmmChild(1));
            System.out.println("T2: " + t2.toString());
            if (!t1.equals(t2)){
                this.reports.add(new Report(ReportType.ERROR, Stage.SEMANTIC, Integer.valueOf(assign.get("line")), Integer.valueOf(assign.get("col")), "Assignment types " + t1.toString() + " and " + t2.toString() + " don't match!"));
                return false;
            }
        }
        return true;
    }

    private Boolean visitCondition(JmmNode condition, Boolean dummy){
        var child = condition.getJmmChild(0);
        var child_type = this.getExpressionType(child);
        if(!child_type.equals(new Type("boolean", false))){
            this.reports.add(new Report(ReportType.ERROR, Stage.SEMANTIC, Integer.valueOf(child.get("line")), Integer.valueOf(child.get("col")), "Condition " + child.toString() + " should be a bool, instead it's of the type " + child_type.toString()));
            return false;
        }
        return true;
    }

    @Override
    public List<Report> getReports() {
        this.visit(root);
        return this.reports;
    }
}
