package pt.up.fe.comp;

import pt.up.fe.comp.jmm.analysis.JmmSemanticsResult;
import pt.up.fe.comp.jmm.analysis.table.JmmSymbolTable;
import pt.up.fe.comp.jmm.ollir.JmmOptimization;
import pt.up.fe.comp.jmm.ollir.OllirGenerator;
import pt.up.fe.comp.jmm.ollir.OllirResult;
import pt.up.fe.comp.optimize.RegisterAllocator;

import java.util.Collections;

public class JmmOptimizer implements JmmOptimization {
    @Override
    public JmmSemanticsResult optimize(JmmSemanticsResult semanticsResult) {
        return JmmOptimization.super.optimize(semanticsResult);
    }

    @Override
    public OllirResult toOllir(JmmSemanticsResult semanticsResult) {
        OllirGenerator ollirGenerator = new OllirGenerator(semanticsResult);
        ollirGenerator.visit(semanticsResult.getRootNode());
        String ollirCode = ollirGenerator.getCode();

        System.out.println("OLLIR CODE:\n" + ollirCode);

        //String optimizations_o = semanticsResult.getConfig().get("optimize");

        return new OllirResult(semanticsResult, ollirCode, Collections.emptyList());
    }

    @Override
    public OllirResult optimize(OllirResult ollirResult) {
        String numMaxReg = ollirResult.getConfig().get("registerAllocation");

        if (numMaxReg != null) {
            int r_registers = Integer.parseInt(numMaxReg);

            if (r_registers != -1) {
                RegisterAllocator regAlloc = new RegisterAllocator(ollirResult, r_registers);

                regAlloc.allocateRegisters(r_registers);
            }
        }
        return ollirResult;
    }
}
