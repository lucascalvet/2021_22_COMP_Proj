package pt.up.fe.comp;
import static org.junit.Assert.assertEquals;
import static pt.up.fe.specs.util.SpecsIo.getResource;

import org.junit.Test;

import pt.up.fe.comp.TestUtils;
import pt.up.fe.comp.jmm.ollir.OllirResult;
import pt.up.fe.specs.util.SpecsIo;
import pt.up.fe.specs.util.SpecsStrings;

import java.util.Collections;

public class OurOllirToJasminTest {

    @Test
    public void test() {
        OllirResult ollirResult = new OllirResult(SpecsIo.getResource("fixtures/public/ollir/HelloWorld.ollir"), Collections.emptyMap());
        var jasminResult = TestUtils.backend(ollirResult);
        TestUtils.noErrors(jasminResult);
        jasminResult.compile();
        String result = jasminResult.run();
        System.out.println("Jasmin result: "+ result);
    }

    @Test
    public void fac(){
        //TODO -> tradução OllirParaJasmin
        OllirResult ollirResult = new OllirResult(SpecsIo.getResource("fixtures/public/ollir/Fac.ollir"), Collections.emptyMap());

        var jasminResult = TestUtils.backend(ollirResult);

//        TestUtils.noErrors(jasminResult);
//        String result = jasminResult.run();
//        System.out.println("Jasmin result: "+ result);

        //assertEquals("HelloWorld! \n", SpecsStrings.normalizeFileContents(result));
    }

    @Test
    public void myclass1(){

        OllirResult ollirResult = new OllirResult(SpecsIo.getResource("fixtures/public/ollir/myclass1.ollir"), Collections.emptyMap());

        var jasminResult = TestUtils.backend(ollirResult);

//        TestUtils.noErrors(jasminResult);
//        String result = jasminResult.run();
//        System.out.println("Jasmin result: "+ result);

        //assertEquals("HelloWorld! \n", SpecsStrings.normalizeFileContents(result));
    }

    @Test
    public void myclass2(){

        OllirResult ollirResult = new OllirResult(SpecsIo.getResource("fixtures/public/ollir/myclass2.ollir"), Collections.emptyMap());

        var jasminResult = TestUtils.backend(ollirResult);

//        TestUtils.noErrors(jasminResult);
//        String result = jasminResult.run();
//        System.out.println("Jasmin result: "+ result);

        //assertEquals("HelloWorld! \n", SpecsStrings.normalizeFileContents(result));
    }

    @Test
    public void myclass3(){

        OllirResult ollirResult = new OllirResult(SpecsIo.getResource("fixtures/public/ollir/myclass3.ollir"), Collections.emptyMap());

        var jasminResult = TestUtils.backend(ollirResult);

//        TestUtils.noErrors(jasminResult);
//        String result = jasminResult.run();
//        System.out.println("Jasmin result: "+ result);

        //assertEquals("HelloWorld! \n", SpecsStrings.normalizeFileContents(result));
    }

    @Test
    public void myclass4(){

        OllirResult ollirResult = new OllirResult(SpecsIo.getResource("fixtures/public/ollir/myclass4.ollir"), Collections.emptyMap());

        var jasminResult = TestUtils.backend(ollirResult);

//        TestUtils.noErrors(jasminResult);
//        String result = jasminResult.run();
//        System.out.println("Jasmin result: "+ result);

        //assertEquals("HelloWorld! \n", SpecsStrings.normalizeFileContents(result));
    }
}
