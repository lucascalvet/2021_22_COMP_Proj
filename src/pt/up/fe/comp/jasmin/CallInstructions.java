package pt.up.fe.comp.jasmin;

import org.specs.comp.ollir.*;
import pt.up.fe.specs.util.exceptions.NotImplementedException;

import java.util.ArrayList;
import java.util.HashMap;


public class CallInstructions {

    public static String getCodeInvokeSpecial(CallInstruction instruction, ConversionInstructions converter) {
        ConversionUtils utils = converter.getUtils();
        HashMap<String, Descriptor> scope = converter.getScope();

        StringBuilder result = new StringBuilder();
        ArrayList<Element> parameters = instruction.getListOfOperands();
        String methodName =  ((LiteralElement) instruction.getSecondArg()).getLiteral().replace("\"", "");
        Element classElement = instruction.getFirstArg();
        Type returnType = instruction.getReturnType();

        for (Element param : parameters){
            result.append(LoadStore.load(param, scope));
        }
        result.append("invokespecial ").append(((ClassType) classElement.getType()).getName());
        result.append(".").append(methodName);

        result.append("(");

        for(var operand : parameters){
            result.append(CallInstructions.getArgumentCode(operand, utils));
        }

        result.append(")");
        result.append(utils.getJasminType(returnType)).append("\n");
        if(converter.isAssign()) {
            result.append(LoadStore.store(converter.getLeftSideNew(), scope, converter.getRightSideNew()));
            converter.setAssign(false);
        }
        return result.toString();
    }

    public static String getCodeInvokeStatic(CallInstruction instruction, ConversionInstructions converter) {

        ConversionUtils utils = converter.getUtils();
        HashMap<String, Descriptor> scope = converter.getScope();
        var result = new StringBuilder();

        ArrayList<Element> parameters = instruction.getListOfOperands();
        Type returnType = instruction.getReturnType();
        for (Element param : parameters){
            result.append(LoadStore.load(param, scope));
        }
        result.append("invokestatic ");

        var methodClass = ((Operand) instruction.getFirstArg()).getName();
        result.append(utils.getFullyQualifiedName(methodClass)).append(".");

        String methodName = ((LiteralElement) instruction.getSecondArg()).getLiteral();
        result.append(methodName.replace("\"", ""));

        result.append("(");

        for(var operand : parameters){
            result.append(CallInstructions.getArgumentCode(operand, utils));
        }

        result.append(")");

        result.append(utils.getJasminType(instruction.getReturnType()));
        result.append("\n");

        if(!converter.isAssign() && returnType.getTypeOfElement() != ElementType.VOID){
            result.append("pop\n");
        }

        return result.toString();
    }

    public static String getCodeInvokeVirtual(CallInstruction instruction, ConversionInstructions converter) {
        //return "aload_1\niload_2 \ninvokevirtual GetterAndSetter.setA(I)I \nistore_3 \n" + "aload_1\ninvokevirtual GetterAndSetter.getA()I\nistore_3\n";
        ConversionUtils utils = converter.getUtils();
        HashMap<String, Descriptor> scope = converter.getScope();

        StringBuilder result = new StringBuilder();
        ArrayList<Element> operands = instruction.getListOfOperands();
        Type returnType = instruction.getReturnType();
        Element firstArg = instruction.getFirstArg();

        String className = ((ClassType) firstArg.getType()).getName();
        String methodCall = ((LiteralElement) instruction.getSecondArg()).getLiteral();

        result.append(LoadStore.load(firstArg, scope));

        for(Element operand : operands){
            result.append(LoadStore.load(operand, scope));
        }

        result.append("invokevirtual ").append(className).append(".");
        result.append(methodCall.replace("\"", ""));

        result.append("(");

        for(var operand : operands){
            result.append(CallInstructions.getArgumentCode(operand, utils));
        }

        result.append(")");

        result.append(utils.getJasminType(returnType)).append("\n");


        if(!converter.isAssign() && returnType.getTypeOfElement() != ElementType.VOID){
            result.append("pop\n");

        }
        if (converter.isAssign()){
            result.append(LoadStore.store(converter.getLeftSideNew(), scope, converter.getRightSideNew()));
            converter.setAssign(false);
        }

        return result.toString();
    }

    public static String getCodeNew(CallInstruction instruction, ConversionInstructions converter) {
        HashMap<String, Descriptor> scope = converter.getScope();
        StringBuilder result = new StringBuilder();
        Type returnType = instruction.getReturnType();

        if(returnType.getTypeOfElement() == ElementType.OBJECTREF){
            result.append("new ").append(((ClassType) returnType).getName()).append("\n");
            result.append("dup\n");
        } else{
            Element element;
            if(instruction.getListOfOperands().size() != 0){
                element = instruction.getListOfOperands().get(0);
            } else {
                element = instruction.getFirstArg();
            }
            result.append(LoadStore.newArray(element, converter.getScope()));
            if (converter.isAssign()){
                result.append(LoadStore.store(converter.getLeftSideNew(), scope, converter.getRightSideNew()));
                converter.setAssign(false);
            }
            //result.append("newarray int\n");
            //TODO : new array
            //throw new NotImplementedException("Array new");
        }
        return result.toString();
    }

    public static String getCodeArrayLength(CallInstruction instruction, ConversionInstructions converter) {
        StringBuilder result = new StringBuilder();
        Element arrayLength = instruction.getFirstArg();
        result.append(LoadStore.load(arrayLength, converter.getScope()));
        result.append("arraylength\n");
        if (converter.isAssign())
            result.append(LoadStore.store(converter.getLeftSideNew(), converter.getScope(), converter.getRightSideNew()));

        return result.toString();
    }

    public static String getArgumentCode(Element operand, ConversionUtils utils) {
        StringBuilder result = new StringBuilder();

        result.append(utils.getJasminType(operand.getType()));
        return result.toString();
    }
}
