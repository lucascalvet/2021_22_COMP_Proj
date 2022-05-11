package pt.up.fe.comp;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import pt.up.fe.comp.jmm.analysis.JmmAnalysis;
import pt.up.fe.comp.jmm.analysis.JmmSemanticsResult;
import pt.up.fe.comp.jmm.analysis.SemanticAnalyser;
import pt.up.fe.comp.jmm.analysis.analysers.FunctionVerifier;
import pt.up.fe.comp.jmm.analysis.analysers.SingleMainMethodCheck;
import pt.up.fe.comp.jmm.analysis.analysers.SuperImportCheck;
import pt.up.fe.comp.jmm.analysis.analysers.TypeVerifier;
import pt.up.fe.comp.jmm.analysis.table.JmmSymbolTable;
import pt.up.fe.comp.jmm.analysis.table.SymbolTable;
import pt.up.fe.comp.jmm.parser.JmmParserResult;
import pt.up.fe.comp.jmm.report.Report;

public class JmmAnalyser implements JmmAnalysis {

    @Override
    public JmmSemanticsResult semanticAnalysis(JmmParserResult parserResult) {
        JmmSymbolTable symbolTable = new JmmSymbolTable(parserResult.getRootNode());
        List <Report> reports = symbolTable.getReports();
        List <SemanticAnalyser> analysers = Arrays.asList(new SingleMainMethodCheck(symbolTable), new FunctionVerifier(parserResult.getRootNode(), symbolTable), new TypeVerifier(parserResult.getRootNode(), symbolTable), new SuperImportCheck(symbolTable));

        for(var analyser : analysers){
            //System.out.println(analyser);
            //System.out.println(analyser.getReports());

            reports.addAll(analyser.getReports());
        }

        return new JmmSemanticsResult(parserResult, symbolTable, reports);
    }

}
