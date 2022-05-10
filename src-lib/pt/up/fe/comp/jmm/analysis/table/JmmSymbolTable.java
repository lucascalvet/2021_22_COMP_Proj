package pt.up.fe.comp.jmm.analysis.table;

import pt.up.fe.comp.jmm.ast.*;
import pt.up.fe.comp.jmm.ast.collectors.*;
import pt.up.fe.comp.jmm.report.Report;

import java.util.ArrayList;
import java.util.List;

public class JmmSymbolTable implements SymbolTable{

    private final JmmNode root;
    private List<Report> reports;

    public JmmSymbolTable(JmmNode root) {
        this.root = root;
        this.reports = new ArrayList<>();
    }

    public List<Report> getReports() {
        if(this.reports.isEmpty()){
            this.reports.addAll(new ImportCollector().getReports());
            this.reports.addAll(new ClassNameCollector().getReports());
            this.reports.addAll(new SuperCollector().getReports());
            this.reports.addAll(new FieldsCollector().getReports());
            this.reports.addAll(new MethodCollector().getReports());
            for(var meth: getMethods()){
                this.reports.addAll(new ReturnTypeCollector(meth).getReports());
                this.reports.addAll(new ParametersCollector(meth).getReports());
                this.reports.addAll(new LocalVariablesCollector(meth).getReports());
            }
        }

        return this.reports;
    }

    @Override
    public List<String> getImports() {
        var importCollector = new ImportCollector();
        importCollector.visit(this.root, true);
        return importCollector.getImports();
    }

    @Override
    public String getClassName() {
        var classNameCollector = new ClassNameCollector();
        classNameCollector.visit(this.root, true);
        return classNameCollector.getClassName();
    }

    @Override
    public String getSuper() {
        var superCollector = new SuperCollector();
        superCollector.visit(this.root, true);
        return superCollector.getSuperName();
    }

    @Override
    public List<Symbol> getFields() {
        var fieldsCollector = new FieldsCollector();
        fieldsCollector.visit(this.root, true);
        return fieldsCollector.getFields();
    }

    @Override
    public List<String> getMethods() {
        var methodCollector = new MethodCollector();
        methodCollector.visit(this.root, true);
        return methodCollector.getMethods();
    }

    @Override
    public Type getReturnType(String methodSignature) {
        var returnTypeCollector = new ReturnTypeCollector(methodSignature);
        returnTypeCollector.visit(this.root, true);
        return returnTypeCollector.getReturnType();
    }

    @Override
    public List<Symbol> getParameters(String methodSignature) {
        var parametersCollector = new ParametersCollector(methodSignature);
        parametersCollector.visit(this.root, true);
        return parametersCollector.getParameters();
    }

    @Override
    public List<Symbol> getLocalVariables(String methodSignature) {
        var localVariablesCollector = new LocalVariablesCollector(methodSignature);
        localVariablesCollector.visit(this.root, true);
        return localVariablesCollector.getLocalVariables();
    }
}
