package pt.up.fe.comp.jmm.jasmin;

import org.specs.comp.ollir.*;
import pt.up.fe.specs.util.exceptions.NotImplementedException;

import java.util.Collections;

public class ConversionUtils {

    private ClassUnit classUnit;

    public ConversionUtils(ClassUnit classUnit) {
        this.classUnit = classUnit;
    }

    public String getFullyQualifiedName(String className){
        //TODO : can be more efficient with hash mao
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

    public String getJasminType(Type type){

        if(type instanceof ArrayType){
            return "[" + getJasminType(((ArrayType) type).getTypeOfElements());
        }

        return getJasminType(type.getTypeOfElement());
    }

    public String getJasminType(ElementType type){
        //TODO: Adicionar os outros tipos!!
        var jasminType = "";
        switch(type) {
            case STRING:
                jasminType = "Ljava/lang/String;";
                break;
            case VOID:
                jasminType = "V";
                break;
            case INT32:
                jasminType = "I";
                break;
            case BOOLEAN:
                jasminType = "Z";
                break;
            case THIS:
                //TODO verificar se está correto
                jasminType = classUnit.getClassName();
                break;
            case CLASS:
                //TODO: check if right
                jasminType = type.getClass().getName();
                break;
            case ARRAYREF:
                break;
            case OBJECTREF:
                //TODO: check if right
                jasminType = type.getClass().getName();
                break;
            default:
                throw new NotImplementedException(type);
        }

        return jasminType;
    }
}
