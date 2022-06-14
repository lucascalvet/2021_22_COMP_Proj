package pt.up.fe.comp.jasmin;

import org.specs.comp.ollir.*;
import pt.up.fe.specs.util.exceptions.NotImplementedException;

import java.util.ArrayList;
import java.util.HashMap;

public class LoadStore {
    public static String load(Element element, HashMap<String, Descriptor> scope, StackLocalsCount counters){
        StringBuilder result = new StringBuilder();

        ElementType type = element.getType().getTypeOfElement();
        if(element.isLiteral()){
            result.append(InstructionUtil.iconst(Integer.parseInt(((LiteralElement)element).getLiteral()), counters));
            //result.append("ldc ").append(((LiteralElement)element).getLiteral()).append("\n");
        } else {
            if (type == ElementType.INT32 || type == ElementType.STRING || type == ElementType.BOOLEAN){
                ElementType typeVar = scope.get(((Operand)element).getName()).getVarType().getTypeOfElement();
                if(typeVar == ElementType.ARRAYREF){
                    result.append(loadArray(element, scope, counters));
                }
                else{

                    int register = scope.get(((Operand)element).getName()).getVirtualReg();
                    counters.incLocalSize(register);
                    if (register > 3 || register < 0)
                        result.append("iload ").append(register).append("\n");
                    else
                        result.append("iload_").append(register).append("\n");
                }
            } else {
                if(type == ElementType.CLASS || type == ElementType.THIS || type == ElementType.OBJECTREF || type == ElementType.ARRAYREF){
                    int register = scope.get(((Operand)element).getName()).getVirtualReg();
                    counters.incLocalSize(register);
                    if (register > 3 || register < 0)
                        result.append("aload ").append(register).append("\n");
                    else
                        result.append("aload_").append(register).append("\n");

                }
            }
        }
        return result.toString();
    }

    public static String store(Element element, HashMap<String, Descriptor> scope, String rightSide, StackLocalsCount counters){
        StringBuilder result = new StringBuilder();
        ElementType type = element.getType().getTypeOfElement();

        if(type == ElementType.INT32 || type == ElementType.STRING || type ==  ElementType.BOOLEAN){
            ElementType typeVar = scope.get(((Operand)element).getName()).getVarType().getTypeOfElement();
            if(typeVar == ElementType.ARRAYREF) {
                result.append(storeArray(element, scope, rightSide, counters));
            }
            else {
                String name = ((Operand)element).getName();
                Descriptor descriptor = scope.get(name);
                int register = descriptor.getVirtualReg();
                counters.incLocalSize(register);
                if (register > 3 || register < 0)
                    result.append("istore ").append(register).append("\n");
                else
                    result.append("istore_").append(register).append("\n");
            }
        }
        else if (type == ElementType.OBJECTREF || type == ElementType.ARRAYREF || type == ElementType.THIS){
            int register = scope.get(((Operand)element).getName()).getVirtualReg();
            counters.incLocalSize(register);
            if (register > 3 || register < 0)
                result.append("astore ").append(register).append("\n");
            else result.append("astore_").append(register).append("\n");
        }
        return result.toString();
    }

    public static String newArray(Element element, HashMap<String, Descriptor> scope, StackLocalsCount counters){
        StringBuilder result = new StringBuilder();
        result.append(LoadStore.load(element, scope, counters));
        result.append("newarray " + "int" + "\n");
        return result.toString();
    }

    public static String storeArray(Element element, HashMap<String, Descriptor> scope, String rightSide, StackLocalsCount counters){
        StringBuilder result = new StringBuilder();
        int array = scope.get(((Operand)element).getName()).getVirtualReg();
        counters.incLocalSize(array);
        if (array > 3 || array < 0)
            result.append("aload " + array + "\n");
        else result.append("aload_"+ array + "\n");


        ArrayOperand arrayOperand = (ArrayOperand) element;
        ArrayList<Element> indexOperand = arrayOperand.getIndexOperands();
        Element index = indexOperand.get(0);
        int indexVirtual =  scope.get(((Operand) index).getName()).getVirtualReg();
        counters.incLocalSize(indexVirtual);
        if (indexVirtual > 3 || indexVirtual < 0)
            result.append("iload " + indexVirtual + "\n");
        else result.append("iload_"+ indexVirtual + "\n");
        result.append(rightSide);
        result.append("iastore\n");
        return result.toString();
    }

    public static String loadArray(Element element, HashMap<String, Descriptor> scope, StackLocalsCount counters){
        StringBuilder result = new StringBuilder();
        int array = scope.get(((Operand)element).getName()).getVirtualReg();
        counters.incLocalSize(array);
        if (array > 3 || array < 0)
            result.append("aload " + array + "\n");
        else result.append("aload_"+ array + "\n");


        ArrayOperand arrayOperand = (ArrayOperand) element;
        ArrayList<Element> indexOperand = arrayOperand.getIndexOperands();
        Element index = indexOperand.get(0);
        int indexVirtual =  scope.get(((Operand) index).getName()).getVirtualReg();
        counters.incLocalSize(indexVirtual);

        if (indexVirtual > 3 || indexVirtual < 0)
            result.append("iload " + indexVirtual + "\n");
        else result.append("iload_"+ indexVirtual + "\n");
        result.append("iaload\n");
        return result.toString();
    }

}
