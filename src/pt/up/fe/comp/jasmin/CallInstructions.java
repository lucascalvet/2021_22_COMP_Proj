package pt.up.fe.comp.jasmin;

import org.specs.comp.ollir.*;
import pt.up.fe.comp.VOID_;

import java.util.ArrayList;
import java.util.HashMap;


public class CallInstructions {

    public static String getCodeInvokeSpecial(CallInstruction instruction, ConversionInstructions converter, StackLocalsCount counters) {
        ConversionUtils utils = converter.getUtils();
        HashMap<String, Descriptor> scope = converter.getScope();

        StringBuilder result = new StringBuilder();
        ArrayList<Element> parameters = instruction.getListOfOperands();

        String methodName =  ((LiteralElement) instruction.getSecondArg()).getLiteral().replace("\"", "");
        Element classElement = instruction.getFirstArg();
        Type returnType = instruction.getReturnType();

        for (Element param : parameters){
            result.append(LoadStore.load(param, scope, counters));
        }
        result.append("invokespecial ").append(((ClassType) classElement.getType()).getName());

        int parametersSize = parameters.size();
        counters.decStackSize(parametersSize);
        if(returnType.getTypeOfElement() != ElementType.VOID){
            counters.incStackSize(1);
        }

        result.append(".").append(methodName);

        result.append("(");

        for(var operand : parameters){
            result.append(CallInstructions.getArgumentCode(operand, utils));
        }

        result.append(")");
        result.append(utils.getJasminType(returnType)).append("\n");
        return result.toString();
    }

    public static String getCodeInvokeStatic(CallInstruction instruction, ConversionInstructions converter, StackLocalsCount counters) {

        ConversionUtils utils = converter.getUtils();
        HashMap<String, Descriptor> scope = converter.getScope();
        var result = new StringBuilder();

        ArrayList<Element> parameters = instruction.getListOfOperands();
        Type returnType = instruction.getReturnType();
        for (Element param : parameters){
            result.append(LoadStore.load(param, scope, counters));
        }
        result.append("invokestatic ");

        int parametersSize = parameters.size();
        counters.decStackSize(parametersSize);
        if(returnType.getTypeOfElement() != ElementType.VOID){
            counters.incStackSize(1);
        }

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
            counters.decStackSize(1);
        }

        return result.toString();
    }

    public static String getCodeInvokeVirtual(CallInstruction instruction, ConversionInstructions converter, StackLocalsCount counters) {
        ConversionUtils utils = converter.getUtils();
        HashMap<String, Descriptor> scope = converter.getScope();

        StringBuilder result = new StringBuilder();
        ArrayList<Element> operands = instruction.getListOfOperands();
        Type returnType = instruction.getReturnType();
        Element firstArg = instruction.getFirstArg();

        String className = ((ClassType) firstArg.getType()).getName();
        String methodCall = ((LiteralElement) instruction.getSecondArg()).getLiteral();

        result.append(LoadStore.load(firstArg, scope, counters));

        for(Element operand : operands){
            result.append(LoadStore.load(operand, scope, counters));
        }

        result.append("invokevirtual ").append(className).append(".");
        int parametersSize = operands.size();
        counters.decStackSize(parametersSize);
        if(returnType.getTypeOfElement() != ElementType.VOID){
            counters.incStackSize(1);
        }

        result.append(methodCall.replace("\"", ""));

        result.append("(");

        for(var operand : operands){
            result.append(CallInstructions.getArgumentCode(operand, utils));
        }

        result.append(")");

        result.append(utils.getJasminType(returnType)).append("\n");


        if(!converter.isAssign() && returnType.getTypeOfElement() != ElementType.VOID){
            result.append("pop\n");
            counters.decStackSize(1);
        }

        return result.toString();
    }

    public static String getCodeNew(CallInstruction instruction, ConversionInstructions converter, StackLocalsCount counters) {
        HashMap<String, Descriptor> scope = converter.getScope();
        StringBuilder result = new StringBuilder();
        Type returnType = instruction.getReturnType();

        if(returnType.getTypeOfElement() == ElementType.OBJECTREF){
            result.append("new ").append(((ClassType) returnType).getName()).append("\n");
            counters.incStackSize(1);

            result.append("dup\n");
            counters.incStackSize(1);
        } else{
            Element element;
            if(instruction.getListOfOperands().size() != 0){
                element = instruction.getListOfOperands().get(0);
            } else {
                element = instruction.getFirstArg();
            }
            result.append(LoadStore.newArray(element, scope, counters));
        }
        return result.toString();
    }

    public static String getCodeArrayLength(CallInstruction instruction, ConversionInstructions converter, StackLocalsCount counters) {
        StringBuilder result = new StringBuilder();
        Element arrayLength = instruction.getFirstArg();
        result.append(LoadStore.load(arrayLength, converter.getScope(), counters));
        result.append("arraylength\n");
        return result.toString();
    }

    public static String getArgumentCode(Element operand, ConversionUtils utils) {
        StringBuilder result = new StringBuilder();

        result.append(utils.getJasminType(operand.getType()));
        return result.toString();
    }
}
