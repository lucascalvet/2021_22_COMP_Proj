package pt.up.fe.comp.cpf;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.junit.Test;

import pt.up.fe.comp.CpUtils;
import pt.up.fe.comp.TestUtils;
import pt.up.fe.comp.jmm.jasmin.JasminResult;
import pt.up.fe.comp.jmm.ollir.OllirResult;
import pt.up.fe.specs.util.SpecsIo;
import pt.up.fe.specs.util.SpecsStrings;
import pt.up.fe.specs.util.utilities.LineStream;

import static org.junit.Assert.*;

public class ourTests {

    private static boolean USE_OLLIR_EXPERIMENTAL = false;

    public static void enableOllirInputs() {
        USE_OLLIR_EXPERIMENTAL = true;
    }

    public static boolean useOllirInputs() {
        return USE_OLLIR_EXPERIMENTAL;
    }

    static JasminResult getJasminResult(String filename) {
        if (USE_OLLIR_EXPERIMENTAL) {
            filename = SpecsIo.removeExtension(filename) + ".ollir";
            return TestUtils.backend(new OllirResult(SpecsIo.getResource("fixtures/public/cpf/4_jasmin/" + filename),
                    Collections.emptyMap()));
        }

        return TestUtils.backend(SpecsIo.getResource("fixtures/public/cpf/4_jasmin/" + filename));
    }


    /*checks if the access to the elements of array is correct*/
    @Test
    public void section5_Arrays_Store_Array() {
        CpUtils.runJasmin(getJasminResult("arrays/sec5_store_ourtest.jmm"),
                "Result: 1\nResult: 2\nResult: 3\nResult: 4\nResult: 5");

    }

    @Test
    public void semantic_UndeclaredVar() {
        var semantics = TestUtils.analyse(SpecsIo.getResource("fixtures/public/cpf/2_semantic_analysis/lookup/UndeclaredVar.jmm"));
        TestUtils.mustFail(semantics.getReports());
    }

    @Test
    public void deadcode_UnusedImports() {
        Map<String, String> config = new HashMap<>();
        config.put("optimize", "true");
        var semantics = TestUtils.analyse(SpecsIo.getResource("fixtures/public/cpf/2_semantic_analysis/import/UnusedImports.jmm"));
        var semanticsOptimized = TestUtils.analyse(SpecsIo.getResource("fixtures/public/cpf/2_semantic_analysis/import/UnusedImports.jmm"), config);
        TestUtils.noErrors(semanticsOptimized.getReports());
        assertNotEquals("Expected symbol table to change with -o flag\n", semantics.getSymbolTable(), semanticsOptimized.getSymbolTable());
    }

    @Test
    public void deadcode_IfDeletion() {
        Map<String, String> config = new HashMap<>();
        config.put("optimize", "true");
        var semantics = TestUtils.analyse(SpecsIo.getResource("fixtures/public/cpf/4_jasmin/control_flow/IfDeletion.jmm"));
        var semanticsOptimized = TestUtils.analyse(SpecsIo.getResource("fixtures/public/cpf/4_jasmin/control_flow/IfDeletion.jmm"), config);
        TestUtils.noErrors(semanticsOptimized.getReports());
        assertNotEquals("Expected symbol table to change with -o flag\n", semantics.getSymbolTable(), semanticsOptimized.getSymbolTable());
    }

    @Test
    public void deadcode_WhileDeletion() {
        Map<String, String> config = new HashMap<>();
        config.put("optimize", "true");
        var semantics = TestUtils.analyse(SpecsIo.getResource("fixtures/public/cpf/4_jasmin/control_flow/WhileDeletion.jmm"));
        var semanticsOptimized = TestUtils.analyse(SpecsIo.getResource("fixtures/public/cpf/4_jasmin/control_flow/WhileDeletion.jmm"), config);
        TestUtils.noErrors(semanticsOptimized.getReports());
        assertNotEquals("Expected symbol table to change with -o flag\n", semantics.getSymbolTable(), semanticsOptimized.getSymbolTable());
    }

    @Test
    public void deadcode_UnusedVarsDeletion() {
        Map<String, String> config = new HashMap<>();
        config.put("optimize", "true");
        var semantics = TestUtils.analyse(SpecsIo.getResource("fixtures/public/cpf/4_jasmin/control_flow/UnusedVars.jmm"));
        var semanticsOptimized = TestUtils.analyse(SpecsIo.getResource("fixtures/public/cpf/4_jasmin/control_flow/UnusedVars.jmm"), config);
        TestUtils.noErrors(semanticsOptimized.getReports());
        assertNotEquals("Expected symbol table to change with -o flag\n", semantics.getSymbolTable(), semanticsOptimized.getSymbolTable());
    }

    @Test
    public void constprop_PropBools() {
        Map<String, String> config = new HashMap<>();
        config.put("optimize", "true");
        var semantics = TestUtils.analyse(SpecsIo.getResource("fixtures/public/cpf/5_optimizations/const_prop/PropBools.jmm"));
        var semanticsOptimized = TestUtils.analyse(SpecsIo.getResource("fixtures/public/cpf/5_optimizations/const_prop/PropBools.jmm"), config);
        TestUtils.noErrors(semanticsOptimized.getReports());
        assertNotEquals("Expected symbol table to change with -o flag\n", semantics.getSymbolTable(), semanticsOptimized.getSymbolTable());
    }

    @Test
    public void constprop_PropInts() {
        Map<String, String> config = new HashMap<>();
        config.put("optimize", "true");
        var semantics = TestUtils.analyse(SpecsIo.getResource("fixtures/public/cpf/5_optimizations/const_prop/PropInts.jmm"));
        var semanticsOptimized = TestUtils.analyse(SpecsIo.getResource("fixtures/public/cpf/5_optimizations/const_prop/PropInts.jmm"), config);
        TestUtils.noErrors(semanticsOptimized.getReports());
        assertNotEquals("Expected symbol table to change with -o flag\n", semantics.getSymbolTable(), semanticsOptimized.getSymbolTable());
    }

    @Test
    public void constprop_PropMix() {
        Map<String, String> config = new HashMap<>();
        config.put("optimize", "true");
        var semantics = TestUtils.analyse(SpecsIo.getResource("fixtures/public/cpf/5_optimizations/const_prop/PropMix.jmm"));
        var semanticsOptimized = TestUtils.analyse(SpecsIo.getResource("fixtures/public/cpf/5_optimizations/const_prop/PropMix.jmm"), config);
        TestUtils.noErrors(semanticsOptimized.getReports());
        assertNotEquals("Expected symbol table to change with -o flag\n", semantics.getSymbolTable(), semanticsOptimized.getSymbolTable());
    }

}
