package pt.up.fe.comp;

import java.io.File;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import pt.up.fe.comp.jmm.jasmin.JasminResult;
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
        if (args.length > 5) {
            throw new RuntimeException("Expected at most 5 arguments, a path to an existing input file and at most 3 optional flags (with a value for -r).");
        }

        File inputFile = new File(args[0]);
        if (!inputFile.isFile()) {
            throw new RuntimeException("Expected a path to an existing input file, got '" + args[0] + "'.");
        }
        String input = SpecsIo.read(inputFile);

        // get the flags
        Map<String, Integer> flags = new HashMap<>();
        flags.put("-r", -1); //Default value
        String optimize = "false";
        String debug = "false";

        if(args.length != 1) {
            for (int i = 1; i < args.length; i++) {
                if (args[i].equals("-r")) {
                    if(args.length <= i+1){
                        throw new RuntimeException("Missing value for -r optimization! Please try again with a valid input.");
                    }
                    flags.put("-r", Integer.valueOf(args[i + 1]));
                    i++;
                } else if (args[i].equals("-o")) {
                    optimize = "true";
                } else if (args[i].equals("-d")) {
                    debug = "true";
                } else {
                    throw new RuntimeException(args[i] + " is not a valid optimization! Please try again only adding value ones.");
                }
            }

            if (flags.get("-r") == -1 && optimize.equals("false") && debug.equals("false")) {
                throw new RuntimeException("Not a valid optimization! Please try again with a different one.");
            }
        }

        System.out.println("Optimizations: -r? " + String.valueOf(flags.get("-r")) + "; -o? " + optimize + "; -d? " + debug);
        System.out.println();

        // Create config
        Map<String, String> config = new HashMap<>();
        config.put("inputFile", args[0]);
        config.put("optimize", optimize);
        config.put("registerAllocation", String.valueOf(flags.get("-r")));
        config.put("debug", debug);

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
