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
        instructionMap.put(PutFieldInstruction.class, this::getCode);

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
        StringBuilder result = new StringBuilder();
        Instruction rightSide = instruction.getRhs();
        Element leftSide = instruction.getDest();

        //handling right side
        StringBuilder right = new StringBuilder();
        InstructionType type = rightSide.getInstType();
        switch (type){
            case NOPER:
                Element single = ((SingleOpInstruction) rightSide).getSingleOperand();
                right.append(stackHandle.load(single, scope));
                result.append(stackHandle.load(single, scope));
                break;
            case GETFIELD:
                Element classElement = ((GetFieldInstruction) rightSide).getFirstOperand();
                Element field = ((GetFieldInstruction) rightSide).getSecondOperand();

                String className = utils.getJasminType(classElement.getType());
                String fieldName = ((Operand) field).getName();
                String fieldType = utils.getJasminType(field.getType());

                result.append(stackHandle.load(classElement, scope));
                result.append("getfield ").append(className).append("/");
                result.append(fieldName).append(" ").append(fieldType).append("\n");

                break;
            default:
                throw new NotImplementedException(this);
        }
        result.append(stackHandle.store(leftSide, scope, right.toString()));
        return result.toString();
    }

    //return
    public String getCode(ReturnInstruction instruction){
        StringBuilder result = new StringBuilder();
        if(!instruction.hasReturnValue()) {
            result.append("return").append("\n");
        } else{
            Element operand = instruction.getOperand();
            result.append(stackHandle.load(operand, scope));
            ElementType type = instruction.getOperand().getType().getTypeOfElement();
            if (type == ElementType.INT32 || type == ElementType.BOOLEAN){
                result.append("ireturn").append("\n");
            } else {
                result.append("areturn").append("\n");
            }

        }
        return result.toString();
    }

    public String getCode(PutFieldInstruction instruction){
        StringBuilder result = new StringBuilder();

        Element classElement = instruction.getFirstOperand();
        Element field = instruction.getSecondOperand();
        Element value = instruction.getThirdOperand();

        result.append(stackHandle.load(classElement, scope));
        result.append(stackHandle.load(value, scope));

        String className = utils.getJasminType(classElement.getType());
        String fieldName = ((Operand) field).getName();
        String type = utils.getJasminType(field.getType());

        result.append("putfield ").append(className).append("/").append(fieldName);
        result.append(" ").append(type).append("\n");


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
