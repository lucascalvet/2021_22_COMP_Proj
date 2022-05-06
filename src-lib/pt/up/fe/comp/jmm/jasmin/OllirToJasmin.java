package pt.up.fe.comp.jmm.jasmin;

import org.specs.comp.ollir.ClassUnit;
import pt.up.fe.specs.util.SpecsIo;

public class OllirToJasmin {

    public String getCode(ClassUnit classUnit){
        return SpecsIo.getResource("fixtures/public/jasmin/HelloWorld.j");
    }
}
