package pt.up.fe.comp.jasmin;

import org.specs.comp.ollir.*;
import pt.up.fe.specs.util.exceptions.NotImplementedException;

import java.util.HashMap;

public class LoadStore {
    public static String load(Element element, HashMap<String, Descriptor> scope){
        StringBuilder result = new StringBuilder();

        ElementType type = element.getType().getTypeOfElement();
        if(element.isLiteral()){
            result.append("ldc ").append(((LiteralElement)element).getLiteral()).append("\n");
        } else {
            if (type == ElementType.INT32 || type == ElementType.STRING || type == ElementType.BOOLEAN){
                int register = scope.get(((Operand)element).getName()).getVirtualReg();
                if (register > 3 || register < 0)
                    result.append("iload ").append(register).append("\n");
                else
                    result.append("iload_").append(register).append("\n");
            } else {
                if(type == ElementType.CLASS || type == ElementType.THIS || type == ElementType.OBJECTREF){
                    int register = scope.get(((Operand)element).getName()).getVirtualReg();
                    if (register > 3 || register < 0)
                        result.append("aload ").append(register).append("\n");
                    else
                        result.append("aload_").append(register).append("\n");

                }
            }
        }
        return result.toString();
    }

    public static String store(Element element, HashMap<String, Descriptor> scope, String rightSide){
        StringBuilder result = new StringBuilder();
        ElementType type = element.getType().getTypeOfElement();

        if(type == ElementType.INT32 || type == ElementType.STRING || type ==  ElementType.BOOLEAN){
            String name = ((Operand)element).getName();
            Descriptor descriptor = scope.get(name);
            int register = descriptor.getVirtualReg();
            if (register > 3 || register < 0)
                result.append("istore ").append(register).append("\n");
            else
                result.append("istore_").append(register).append("\n");
        }
        else if (type == ElementType.OBJECTREF || type == ElementType.ARRAYREF || type == ElementType.THIS){
            int register = scope.get(((Operand)element).getName()).getVirtualReg();
            if (register > 3 || register < 0)
                result.append("astore ").append(register).append("\n");
            else result.append("astore_").append(register).append("\n");
        }
        return result.toString();
    }

    public static String newArray(Element element, HashMap<String, Descriptor> scope){
        StringBuilder result = new StringBuilder();
        //result.append(LoadStore.load(element, scope)).append("\n");

        result.append("newarray " + "int" + "\n");
        return result.toString();
    }

}
