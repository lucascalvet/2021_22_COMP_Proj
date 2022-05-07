package pt.up.fe.comp.jmm.jasmin;

import org.specs.comp.ollir.ClassUnit;
import pt.up.fe.specs.util.SpecsIo;

import java.util.Collections;

public class OllirToJasmin {

    private final ClassUnit classUnit;

    public OllirToJasmin(ClassUnit classUnit) {
        this.classUnit = classUnit;
    }

    public String getFullyQualifiedName(String className){
        //secalhar construir um map em que a chave é o last name e o valor é o import já c

        var imports = classUnit.getImports();

        for(var importString : imports) {
            var splittedImports = importString.split("\\.");

            String lastName;
            if(splittedImports.length == 0){
                lastName = importString;
            } else {
                lastName = splittedImports[splittedImports.length - 1];
            }


            if(lastName.equals(className)){
                return importString.replace(".", "/");
            }
        }

        if(!imports.equals(Collections.emptyList())){
            throw new RuntimeException("Could not find import for class " + className);
        }

        return "java/lang/Object";
    }

    public String getCode(){
        var code = new StringBuilder();
        var qualifiedNameSuper = getFullyQualifiedName(classUnit.getSuperClass());

        code.append(".class public ").append(classUnit.getClassName()).append("\n");
        code.append(".super ").append(qualifiedNameSuper).append("\n\n");


        code.append(SpecsIo.getResource("../test/templates/constructor.txt").replace("${SUPER_CLASS}", qualifiedNameSuper));

        var result = code.toString();
        System.out.println("\nJasmin Code: \n" + result);
        return code.toString();
    }
}
