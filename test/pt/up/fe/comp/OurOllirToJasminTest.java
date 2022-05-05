package pt.up.fe.comp;
import static org.junit.Assert.assertEquals;

import org.junit.Test;

import pt.up.fe.comp.TestUtils;
import pt.up.fe.specs.util.SpecsIo;
import pt.up.fe.specs.util.SpecsStrings;

public class OurOllirToJasminTest {

    @Test
    public void test() {
        var jasminResult = TestUtils.backend(SpecsIo.getResource("fixtures/public/HelloWorld.jmm"));
        TestUtils.noErrors(jasminResult);
    }

    @Test
    public void fac(){
        //TODO -> tradução OllirParaJasmin
        String jasminCode = "";

        var output = TestUtils.runJasmin(jasminCode);
        assertEquals("Output\n", SpecsStrings.normalizeFileContents(output));
    }

    @Test
    public void myclass1(){
        String jasminCode = "";

        var output = TestUtils.runJasmin(jasminCode);
        assertEquals("Output\n", SpecsStrings.normalizeFileContents(output));
    }

    @Test
    public void myclass2(){
        String jasminCode = "";

        var output = TestUtils.runJasmin(jasminCode);
        assertEquals("Output\n", SpecsStrings.normalizeFileContents(output));
    }

    @Test
    public void myclass3(){
        String jasminCode = "";

        var output = TestUtils.runJasmin(jasminCode);
        assertEquals("Output\n", SpecsStrings.normalizeFileContents(output));
    }

    @Test
    public void myclass4(){
        String jasminCode = "";

        var output = TestUtils.runJasmin(jasminCode);
        assertEquals("Output\n", SpecsStrings.normalizeFileContents(output));
    }
}
