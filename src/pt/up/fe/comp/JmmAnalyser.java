package pt.up.fe.comp;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import pt.up.fe.comp.jmm.analysis.JmmAnalysis;
import pt.up.fe.comp.jmm.analysis.JmmSemanticsResult;
import pt.up.fe.comp.jmm.analysis.SemanticAnalyser;
import pt.up.fe.comp.jmm.analysis.analysers.*;
import pt.up.fe.comp.jmm.analysis.table.JmmSymbolTable;
import pt.up.fe.comp.jmm.analysis.table.SymbolTable;
import pt.up.fe.comp.jmm.ast.annotators.ValueAnnotator;
import pt.up.fe.comp.jmm.parser.JmmParserResult;
import pt.up.fe.comp.jmm.report.Report;

public class JmmAnalyser implements JmmAnalysis {

    @Override
    public JmmSemanticsResult semanticAnalysis(JmmParserResult parserResult) {

        if(parserResult.getConfig().containsKey("optimize") && parserResult.getConfig().get("optimize").equals("true")){
            boolean allRemoved = false;
            int counter = 1;
            while(!allRemoved){
                new ValueAnnotator(true).visit(parserResult.getRootNode());
                DeadCodeRemover deadCodeRemover = new DeadCodeRemover();
                deadCodeRemover.visit(parserResult.getRootNode());
                allRemoved = deadCodeRemover.isAllRemoved();
                if(parserResult.getConfig().containsKey("debug") && parserResult.getConfig().get("debug").equals("true")) {
                    System.out.println("DEADCODE Removal Iteration " + counter);
                    System.out.println(parserResult.getRootNode().toTree());
                }
                counter += 1;
            }
        }

        JmmSymbolTable symbolTable = new JmmSymbolTable(parserResult.getRootNode());
        List <Report> reports = symbolTable.getReports();
        List <SemanticAnalyser> analysers = Arrays.asList(new FunctionVerifier(parserResult.getRootNode(), symbolTable), new TypeVerifier(parserResult.getRootNode(), symbolTable), new SuperImportCheck(symbolTable), new BlockVerifier(parserResult.getRootNode(), symbolTable));

        for(var analyser : analysers){
            if(parserResult.getConfig().containsKey("debug") && parserResult.getConfig().get("debug").equals("true")) {
                System.out.println(analyser.toString());
            }

            reports.addAll(analyser.getReports());
        }

        if(parserResult.getConfig().containsKey("debug") && parserResult.getConfig().get("debug").equals("true")) {
            System.out.println(parserResult.getRootNode().toTree());
        }

        return new JmmSemanticsResult(parserResult, symbolTable, reports);
    }

}
