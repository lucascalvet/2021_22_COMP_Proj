package pt.up.fe.comp.jmm.jasmin;

import org.specs.comp.ollir.*;

import java.util.HashMap;

public class StackHandle {
    public String load(Element element, HashMap<String, Descriptor> scope){
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
            }
        }
        return result.toString();
    }

    public String store(Element element, HashMap<String, Descriptor> scope, String rightSide){
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
        return result.toString();
    }

}
