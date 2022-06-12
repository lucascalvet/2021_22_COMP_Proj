package pt.up.fe.comp;

import pt.up.fe.comp.jmm.jasmin.JasminResult;
import pt.up.fe.comp.jasmin.OllirToJasmin;
import pt.up.fe.comp.jmm.jasmin.JasminBackend;
import pt.up.fe.comp.jmm.ollir.OllirResult;

public class JasminBackendClass implements JasminBackend {
    @Override
    public JasminResult toJasmin(OllirResult ollirResult){
        String jasminCode = new OllirToJasmin(ollirResult.getOllirClass()).getCode();

        return new JasminResult(ollirResult, jasminCode, ollirResult.getReports());
    }
}
