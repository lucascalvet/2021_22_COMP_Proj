package pt.up.fe.comp.jmm.jasmin;

import org.specs.comp.ollir.ClassUnit;

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

        code.append(".class public ").append(classUnit.getClassName()).append("\n");
        code.append(".super ").append(getFullyQualifiedName(classUnit.getSuperClass())).append("\n\n");


        var result = code.toString();
        System.out.println("Jasmin Code: \n" + result);
        return code.toString();
    }
}
