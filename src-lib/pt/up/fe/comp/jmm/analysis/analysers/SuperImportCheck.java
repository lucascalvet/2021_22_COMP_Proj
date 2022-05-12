package pt.up.fe.comp.jmm.analysis.analysers;

import pt.up.fe.comp.jmm.analysis.SemanticAnalyser;
import pt.up.fe.comp.jmm.analysis.table.SymbolTable;
import pt.up.fe.comp.jmm.report.Report;
import pt.up.fe.comp.jmm.report.ReportType;
import pt.up.fe.comp.jmm.report.Stage;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class SuperImportCheck implements SemanticAnalyser {

    private final SymbolTable symbolTable;

    public SuperImportCheck(SymbolTable symbolTable){
        this.symbolTable = symbolTable;
    }

    @Override
    public List<Report> getReports(){
        if(!symbolTable.getSuper().equals("") && !symbolTable.getImports().contains(symbolTable.getSuper())){
            return Arrays.asList(new Report(ReportType.ERROR, Stage.SEMANTIC, -1, -1, "Super Class '" + symbolTable.getSuper() + "' is not imported"));
        }
        return Collections.emptyList();
    }

}
