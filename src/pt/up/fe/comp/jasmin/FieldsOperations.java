package pt.up.fe.comp.jasmin;

import org.specs.comp.ollir.Descriptor;
import org.specs.comp.ollir.Element;
import org.specs.comp.ollir.Operand;
import org.specs.comp.ollir.PutFieldInstruction;

import java.util.HashMap;

public class FieldsOperations {

    public static String getPutFieldCode(PutFieldInstruction instruction, ConversionUtils utils, HashMap<String, Descriptor> scope, StackLocalsCount counters){
        StringBuilder result = new StringBuilder();

        Element classElement = instruction.getFirstOperand();
        Element field = instruction.getSecondOperand();
        Element value = instruction.getThirdOperand();

        result.append(LoadStore.load(classElement, scope, counters));
        result.append(LoadStore.load(value, scope, counters));

        String className = utils.getJasminType(classElement.getType());
        String fieldName = ((Operand) field).getName();
        String type = utils.getJasminType(field.getType());

        result.append("putfield ").append(className).append("/").append(fieldName);
        counters.decStackSize(2);

        result.append(" ").append(type).append("\n");


        return result.toString();
    }

    public static String getGetFieldCode(Element classElement, Element field,
                                         ConversionUtils utils, HashMap<String, Descriptor> scope, StackLocalsCount counters){
        StringBuilder result = new StringBuilder();

        String className = utils.getJasminType(classElement.getType());
        String fieldName = ((Operand) field).getName();
        String fieldType = utils.getJasminType(field.getType());

        result.append(LoadStore.load(classElement, scope, counters));
        result.append("getfield ").append(className).append("/");
        counters.decStackSize(2);

        result.append(fieldName).append(" ").append(fieldType).append("\n");

        return result.toString();
    }

}
