package pt.up.fe.comp.jmm.jasmin;

import org.specs.comp.ollir.ClassUnit;

import java.util.Collections;

public class ConversionUtils {

    private ClassUnit classUnit;

    public ConversionUtils(ClassUnit classUnit) {
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
}
