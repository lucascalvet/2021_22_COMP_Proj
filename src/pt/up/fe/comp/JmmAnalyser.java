package pt.up.fe.comp;

import java.util.Collections;
import java.util.List;

import pt.up.fe.comp.jmm.analysis.JmmAnalysis;
import pt.up.fe.comp.jmm.analysis.JmmSemanticsResult;
import pt.up.fe.comp.jmm.analysis.table.JmmSymbolTable;
import pt.up.fe.comp.jmm.analysis.table.SymbolTable;
import pt.up.fe.comp.jmm.parser.JmmParserResult;
import pt.up.fe.comp.jmm.report.Report;

public class JmmAnalyser implements JmmAnalysis {

    @Override
    public JmmSemanticsResult semanticAnalysis(JmmParserResult parserResult) {
        JmmSymbolTable symbolTable = new JmmSymbolTable(parserResult.getRootNode());
        List <Report> reports = symbolTable.getReports();

        return new JmmSemanticsResult(parserResult, symbolTable, Collections.emptyList());
    }

}
