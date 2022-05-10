package pt.up.fe.comp.jmm.jasmin;

import org.specs.comp.ollir.*;
import pt.up.fe.specs.util.classmap.FunctionClassMap;
import pt.up.fe.specs.util.exceptions.NotImplementedException;

import java.awt.*;
import java.util.HashMap;

public class ConversionInstructions {
    private final ClassUnit classUnit;
    private final ConversionUtils utils;
    private final FunctionClassMap<Instruction, String> instructionMap;
    private HashMap<String, Descriptor> scope;
    private StackHandle stackHandle;

    public ConversionInstructions(ClassUnit classUnit) {
        this.classUnit = classUnit;
        this.utils = new ConversionUtils(classUnit);
        this.instructionMap = new FunctionClassMap<>();
        instructionMap.put(CallInstruction.class,this::getCode);
        instructionMap.put(AssignInstruction.class,this::getCode);
        instructionMap.put(ReturnInstruction.class, this::getCode);
        this.scope = new HashMap<>();
        this.stackHandle = new StackHandle();
    }

    public void updateScope(HashMap newScope){
        this.scope = newScope;
    }

    public String getCode(Instruction instruction){
        return instructionMap.apply(instruction);
    }

    public String getCode(CallInstruction instruction){

        switch(instruction.getInvocationType()){
            //TODO : ver as outras invocações
            case invokestatic:
                return getCodeInvokeStatic(instruction);
            default:
                return ""; //throw new NotImplementedException(instruction.getInvocationType());
        }
    }

    public String getCode(AssignInstruction instruction){
        Instruction rightSide = instruction.getRhs();
        Element leftSide = instruction.getDest();

        return "";
    }

    //return
    public String getCode(ReturnInstruction instruction){
        StringBuilder result = new StringBuilder();
        if(!instruction.hasReturnValue()) {
            result.append("return").append("\n");
        } else{
            Element operand = instruction.getOperand();
            if(operand.isLiteral()){
                result.append(stackHandle.load(operand, scope));
                result.append("ireturn").append("\n");

            } else {
                System.out.println("problems from return");
            }

        }
        return result.toString();
    }

    private String getCodeInvokeStatic(CallInstruction instruction) {
        var code = new StringBuilder();
        code.append("invokestatic ");

        //Operandos, FirstArg(classe), SecondArgs (nomemétodo), ReturnType

        var methodClass = ((Operand) instruction.getFirstArg()).getName();

        code.append(utils.getFullyQualifiedName(methodClass));
        code.append("/");
        code.append((((LiteralElement) instruction.getSecondArg()).getLiteral()).replace("\"", ""));
        code.append("(");

        for(var operand : instruction.getListOfOperands()){
            getArgumentCode(operand);
        }

        code.append(")");
        code.append(utils.getJasminType(instruction.getReturnType()));
        code.append("\n");

        return code.toString();
    }

    private void getArgumentCode(Element operand) {
        throw new NotImplementedException(this);
    }


}
