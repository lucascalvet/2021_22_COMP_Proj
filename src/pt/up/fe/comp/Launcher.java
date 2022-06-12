package pt.up.fe.comp;

import java.io.File;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import pt.up.fe.comp.jasmin.JasminResult;
import pt.up.fe.comp.jmm.parser.JmmParserResult;
import pt.up.fe.comp.jmm.analysis.JmmSemanticsResult;
import pt.up.fe.comp.jmm.ollir.OllirResult;
import pt.up.fe.specs.util.SpecsIo;
import pt.up.fe.specs.util.SpecsLogs;
import pt.up.fe.specs.util.SpecsSystem;
import pt.up.fe.comp.jmm.report.Report; //adictional import for printing reports

public class Launcher {

    public static void main(String[] args) {
        SpecsSystem.programStandardInit();

        SpecsLogs.info("Executing with args: " + Arrays.toString(args));

        // read the input code
        if (args.length != 1) {
            throw new RuntimeException("Expected a single argument, a path to an existing input file.");
        }
        File inputFile = new File(args[0]);
        if (!inputFile.isFile()) {
            throw new RuntimeException("Expected a path to an existing input file, got '" + args[0] + "'.");
        }
        String input = SpecsIo.read(inputFile);

        // Create config
        Map<String, String> config = new HashMap<>();
        config.put("inputFile", args[0]);
        config.put("optimize", "false");
        config.put("registerAllocation", "-1");
        config.put("debug", "false");

        // Instantiate JmmParser
        SimpleParser parser = new SimpleParser();

        // Parse stage
        JmmParserResult parserResult = parser.parse(input, config);

        // Check if there are parsing errors
        TestUtils.noErrors(parserResult.getReports());

        // Instantiate JmmAnalysis
        JmmAnalyser analyser = new JmmAnalyser();

        // Analysis stage
        JmmSemanticsResult analysisResult = analyser.semanticAnalysis(parserResult);

        // Check if there are parsing errors
        TestUtils.noErrors(analysisResult.getReports());

        // Instantiate JmmOptimizer
        JmmOptimizer optimizer = new JmmOptimizer();

        // Optimization stage
        JmmSemanticsResult optimizationResult = optimizer.optimize(analysisResult);

        OllirResult ollirCode = optimizer.toOllir(optimizationResult);

        // Check if there are parsing errors
        TestUtils.noErrors(optimizationResult.getReports());

        // Instantiate jasminBackend
        JasminBackendClass jasminBackend = new JasminBackendClass();

        // Jasmin Backend stage
        JasminResult jasminResult = jasminBackend.toJasmin(ollirCode);

        // Check if there are parsing errors
        TestUtils.noErrors(jasminResult.getReports());

        // ... add remaining stages

        System.out.println("\n-----------------------------------------------------------------------------------------------------------");
        System.out.println(" Warnings (Reports): ");

        System.out.println("   Parser reports: ");
        for (Report rep : parserResult.getReports()) {
            System.out.println("    " + rep);
        }

        System.out.println("   Analysis reports: ");
        for (Report rep : analysisResult.getReports()) {
            System.out.println("    " + rep);
        }

        System.out.println("   Optimization reports: ");
        for (Report rep : optimizationResult.getReports()) {
            System.out.println("    " + rep);
        }

        System.out.println("   Jasmin reports: ");
        for (Report rep : jasminResult.getReports()) {
            System.out.println("    " + rep);
        }

        System.out.println("\n");

        System.out.println("\n-----------------------------------------------------------------------------------------------------------");
        System.out.println(" Running... \n");

        jasminResult.compile();
        jasminResult.run();
    }

}
