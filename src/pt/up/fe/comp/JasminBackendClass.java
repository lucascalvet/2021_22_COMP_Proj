package pt.up.fe.comp;

import pt.up.fe.comp.jmm.jasmin.JasminBackend;
import pt.up.fe.comp.jmm.jasmin.JasminResult;
import pt.up.fe.comp.jmm.jasmin.OllirToJasmin;
import pt.up.fe.comp.jmm.ollir.OllirResult;
import pt.up.fe.comp.jmm.report.Report;
import pt.up.fe.specs.util.SpecsIo;

import java.util.Collections;
import java.util.List;

public class JasminBackendClass implements JasminBackend {
    @Override
    public JasminResult toJasmin(OllirResult ollirResult){
        String jasminCode = new OllirToJasmin(ollirResult.getOllirClass()).getCode();

        return new JasminResult(ollirResult, jasminCode, ollirResult.getReports());
    }
}
