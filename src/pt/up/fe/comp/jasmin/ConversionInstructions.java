package pt.up.fe.comp.jasmin;

import org.specs.comp.ollir.*;
import org.specs.comp.ollir.Type;
import pt.up.fe.specs.util.classmap.FunctionClassMap;
import pt.up.fe.specs.util.exceptions.NotImplementedException;

import java.util.ArrayList;
import java.util.HashMap;


public class ConversionInstructions {
    private final ClassUnit classUnit;
    private final ConversionUtils utils;
    private final FunctionClassMap<Instruction, String> instructionMap;
    private HashMap<String, Descriptor> scope;

    public boolean isAssign() {
        return assign;
    }

    public void setAssign(boolean assign) {
        this.assign = assign;
    }

    private boolean assign = false;
    private Element leftSideNew;
    private String rightSideNew;

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
            //TODO : ver as outras invocações
            case invokestatic:
                return getCodeInvokeStatic(instruction);
            case invokespecial:
                return getCodeInvokeSpecial(instruction);
            case invokevirtual:
                return getCodeInvokeVirtual(instruction);
            case NEW:
                return getCodeNew(instruction);
            default:
                return ""; //throw new NotImplementedException(instruction.getInvocationType());
        }
    }

    private String getCodeNew(CallInstruction instruction) {
        StringBuilder result = new StringBuilder();
        org.specs.comp.ollir.Type returnType = instruction.getReturnType();

        if(returnType.getTypeOfElement() == ElementType.OBJECTREF){
            result.append("new ").append(((ClassType) returnType).getName()).append("\n");
            result.append("dup\n");
        } else{
            //TODO : new array
            //throw new NotImplementedException(this);
        }
        return result.toString();
    }

    private String getCodeInvokeVirtual(CallInstruction instruction) {
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

        if(!this.assign && returnType.getTypeOfElement() != ElementType.VOID){
            result.append("pop\n");
            this.assign = false;
        }

        return result.toString();
    }

    private String getCodeInvokeSpecial(CallInstruction instruction) {
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
        result.append(LoadStore.store(this.leftSideNew, this.scope, this.rightSideNew));

        return result.toString();
    }

    public String getCode(AssignInstruction instruction){
        this.assign = true;
        StringBuilder result = new StringBuilder();
        Instruction rightSide = instruction.getRhs();
        Element leftSide = instruction.getDest();
        this.leftSideNew = instruction.getDest();

        //handling right side
        StringBuilder right = new StringBuilder();
        InstructionType type = rightSide.getInstType();
        switch (type){
            case NOPER:
                Element single = ((SingleOpInstruction) rightSide).getSingleOperand();
                right.append(LoadStore.load(single, scope));
                result.append(LoadStore.load(single, scope));
                break;
            case GETFIELD:
                Element classElement = ((GetFieldInstruction) rightSide).getFirstOperand();
                Element field = ((GetFieldInstruction) rightSide).getSecondOperand();

                String className = utils.getJasminType(classElement.getType());
                String fieldName = ((Operand) field).getName();
                String fieldType = utils.getJasminType(field.getType());

                result.append(LoadStore.load(classElement, scope));
                result.append("getfield ").append(className).append("/");
                result.append(fieldName).append(" ").append(fieldType).append("\n");

                break;
            case BINARYOPER:
                Element rightElement = ((BinaryOpInstruction) rightSide).getRightOperand();
                Element leftElement = ((BinaryOpInstruction) rightSide).getLeftOperand();
                OperationType operationType = ((BinaryOpInstruction) rightSide).getOperation().getOpType();

                result.append(BinaryOperation.processBinaryOperation(rightElement, leftElement, operationType, scope));
                break;
            case CALL:
                this.rightSideNew = right.toString();
                result.append(getCode((CallInstruction) rightSide));
                return result.toString();

            default:
                throw new NotImplementedException(this);
        }
        this.rightSideNew = right.toString();
        result.append(LoadStore.store(leftSide, scope, rightSideNew));
        this.assign = false;
        return result.toString();
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
        StringBuilder result = new StringBuilder();

        Element classElement = instruction.getFirstOperand();
        Element field = instruction.getSecondOperand();
        Element value = instruction.getThirdOperand();

        result.append(LoadStore.load(classElement, scope));
        result.append(LoadStore.load(value, scope));

        String className = utils.getJasminType(classElement.getType());
        String fieldName = ((Operand) field).getName();
        String type = utils.getJasminType(field.getType());

        result.append("putfield ").append(className).append("/").append(fieldName);
        result.append(" ").append(type).append("\n");


        return result.toString();
    }

    private String getCodeInvokeStatic(CallInstruction instruction) {
        //return CallInstructions.get
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

        if(!this.assign && returnType.getTypeOfElement() != ElementType.VOID){
            result.append("pop\n");
            this.assign = false;
        }

        return result.toString();
    }

}
