package pt.up.fe.comp.jasmin;

import org.specs.comp.ollir.*;
import org.specs.comp.ollir.Type;
import pt.up.fe.specs.util.classmap.FunctionClassMap;
import pt.up.fe.specs.util.exceptions.NotImplementedException;

import java.util.ArrayList;
import java.util.HashMap;

import static pt.up.fe.comp.jasmin.CallInstructions.getCodeNew;


public class ConversionInstructions {
    private final ClassUnit classUnit;

    public ConversionUtils getUtils() {
        return utils;
    }

    public HashMap<String, Descriptor> getScope() {
        return scope;
    }

    private final ConversionUtils utils;
    private final FunctionClassMap<Instruction, String> instructionMap;
    private HashMap<String, Descriptor> scope;

    private boolean assign = false;

    public Element getLeftSideNew() {
        return leftSideNew;
    }

    public String getRightSideNew() {
        return rightSideNew;
    }

    public void setLeftSideNew(Element leftSideNew) {
        this.leftSideNew = leftSideNew;
    }

    public void setRightSideNew(String rightSideNew) {
        this.rightSideNew = rightSideNew;
    }

    private Element leftSideNew;
    private String rightSideNew;

    public boolean isAssign() {
        return assign;
    }

    public void setAssign(boolean assign) {
        this.assign = assign;
    }

    public ConversionInstructions(ClassUnit classUnit) {
        this.classUnit = classUnit;
        this.utils = new ConversionUtils(classUnit);
        this.instructionMap = new FunctionClassMap<>();
        instructionMap.put(CallInstruction.class,this::getCode);
        instructionMap.put(AssignInstruction.class,this::getCode);
        instructionMap.put(ReturnInstruction.class, this::getCode);
        instructionMap.put(PutFieldInstruction.class, this::getCode);
        instructionMap.put(GotoInstruction.class, this::getCode);
        instructionMap.put(CondBranchInstruction.class, this::getCode);
        this.scope = new HashMap<>();
    }

    public void updateScope(HashMap newScope){
        this.scope = newScope;
    }

    public String getCode(Instruction instruction){
        return instructionMap.apply(instruction);
    }

    public String getCode(GotoInstruction instruction){
        return "goto " + instruction.getLabel() + "\n";
    }

    public String getCode(CondBranchInstruction instruction){
        return ConditionalOperations.getCode(instruction, scope);
    }

    public String getCode(CallInstruction instruction){

        switch(instruction.getInvocationType()){
            case invokestatic:
                return CallInstructions.getCodeInvokeStatic(instruction, this);
            case invokespecial:
                return CallInstructions.getCodeInvokeSpecial(instruction, this);
            case invokevirtual:
                return CallInstructions.getCodeInvokeVirtual(instruction, this);
            case NEW:
                return getCodeNew(instruction);
            default:
                return "";
        }
    }

    public String getCode(AssignInstruction instruction){
        return AssignOperation.getCode(instruction, this);
    }


    //return
    public String getCode(ReturnInstruction instruction){
        StringBuilder result = new StringBuilder();
        if(!instruction.hasReturnValue()) {
            result.append("return").append("\n");
        } else{
            Element operand = instruction.getOperand();
            result.append(LoadStore.load(operand, scope));
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
        return FieldsOperations.getPutFieldCode(instruction, utils, scope);

    }

}
