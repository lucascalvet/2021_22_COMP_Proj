package pt.up.fe.comp.jmm.analysis.table;

import pt.up.fe.comp.jmm.ast.*;
import pt.up.fe.comp.jmm.ast.collectors.*;
import pt.up.fe.comp.jmm.report.Report;
import pt.up.fe.comp.jmm.report.ReportType;
import pt.up.fe.comp.jmm.report.Stage;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class JmmSymbolTable implements SymbolTable{

    private final JmmNode root;
    private List<Report> reports;
    private final List<String> imports;
    private final String class_name;
    private final String super_class;
    private final List<Symbol> fields;
    private final List<String> methods;
    private final Map<String, Type> return_type;
    private final Map<String, List<Symbol>> params;
    private final Map<String, List<Symbol>> local_vars;


    public JmmSymbolTable(JmmNode root) {
        this.root = root;
        this.reports = new ArrayList<>();

        this.return_type = new HashMap<String,Type>();
        this.params = new HashMap<String,List<Symbol>>();
        this.local_vars = new HashMap<String,List<Symbol>>();

        var importCollector = new ImportCollector();
        importCollector.visit(this.root, true);
        this.reports.addAll(importCollector.getReports());
        this.imports = importCollector.getImports();

        var classNameCollector = new ClassNameCollector();
        classNameCollector.visit(this.root, true);
        this.reports.addAll(classNameCollector.getReports());
        this.class_name = classNameCollector.getClassName();

        var superCollector = new SuperCollector();
        superCollector.visit(this.root, true);
        this.reports.addAll(superCollector.getReports());
        this.super_class = superCollector.getSuperName();

        var fieldsCollector = new FieldsCollector();
        fieldsCollector.visit(this.root, true);
        this.reports.addAll(fieldsCollector.getReports());
        this.fields = fieldsCollector.getFields();

        var methodCollector = new MethodCollector();
        methodCollector.visit(this.root, true);
        this.reports.addAll(methodCollector.getReports());
        this.methods = methodCollector.getMethods();

        for(var methodSignature: getMethods()) {
            var returnTypeCollector = new ReturnTypeCollector(methodSignature);
            returnTypeCollector.visit(this.root, true);
            this.reports.addAll(returnTypeCollector.getReports());
            this.return_type.put(methodSignature, returnTypeCollector.getReturnType());

            var parametersCollector = new ParametersCollector(methodSignature);
            parametersCollector.visit(this.root, true);
            this.reports.addAll(parametersCollector.getReports());
            this.params.put(methodSignature, parametersCollector.getParameters());

            var localVariablesCollector = new LocalVariablesCollector(methodSignature);
            localVariablesCollector.visit(this.root, true);
            this.reports.addAll(localVariablesCollector.getReports());
            this.local_vars.put(methodSignature, localVariablesCollector.getLocalVariables());
        }
    }

    public List<Report> getReports() {
        return this.reports;
    }

    @Override
    public List<String> getImports() {
        return this.imports;
    }

    @Override
    public String getClassName() {
        return this.class_name;
    }

    @Override
    public String getSuper() {
        return this.super_class;
    }

    @Override
    public List<Symbol> getFields() {
        return this.fields;
    }

    @Override
    public List<String> getMethods() {
        return this.methods;
    }

    @Override
    public Type getReturnType(String methodSignature) {
        if(!this.return_type.containsKey(methodSignature)){
            var returnTypeCollector = new ReturnTypeCollector(methodSignature);
            returnTypeCollector.visit(this.root, true);
            this.reports.addAll(returnTypeCollector.getReports());
            return returnTypeCollector.getReturnType();
        }
        else{
            return this.return_type.get(methodSignature);
        }
    }

    @Override
    public List<Symbol> getParameters(String methodSignature) {
        if(!this.params.containsKey(methodSignature)){
            var parametersCollector = new ParametersCollector(methodSignature);
            parametersCollector.visit(this.root, true);
            this.reports.addAll(parametersCollector.getReports());
            return parametersCollector.getParameters();
        }
        else{
            return this.params.get(methodSignature);
        }
    }

    @Override
    public List<Symbol> getLocalVariables(String methodSignature) {
        if(!this.local_vars.containsKey(methodSignature)){
            var localVariablesCollector = new LocalVariablesCollector(methodSignature);
            localVariablesCollector.visit(this.root, true);
            this.reports.addAll(localVariablesCollector.getReports());
            return localVariablesCollector.getLocalVariables();
        }
        else{
            return this.local_vars.get(methodSignature);
        }
    }
}
