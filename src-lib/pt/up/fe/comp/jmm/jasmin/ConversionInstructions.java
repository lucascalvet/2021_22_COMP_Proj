package pt.up.fe.comp.jmm.jasmin;

import org.specs.comp.ollir.*;
import pt.up.fe.specs.util.classmap.FunctionClassMap;
import pt.up.fe.specs.util.exceptions.NotImplementedException;

import java.util.ArrayList;
import java.util.HashMap;

public class ConversionInstructions {
    private final ClassUnit classUnit;
    private final ConversionUtils utils;
    private final FunctionClassMap<Instruction, String> instructionMap;
    private HashMap<String, Descriptor> scope;
    private StackHandle stackHandle;

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
        this.stackHandle = new StackHandle();
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
        Element left = instruction.getOperands().
        return "";
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
        Type returnType = instruction.getReturnType();

        if(returnType.getTypeOfElement() == ElementType.OBJECTREF){
            result.append("new ").append(utils.getJasminType(returnType)).append("\n");
            result.append("dup\n");
        } else{
            throw new NotImplementedException(this);
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

        result.append(stackHandle.load(firstArg, scope));

        for(Element operand : operands){
            result.append(stackHandle.load(operand, scope));
        }

        result.append("invokevirtual ").append(className).append(".");
        result.append(methodCall.replace("\"", ""));

        result.append("(");

        for(var operand : operands){
            result.append(getArgumentCode(operand));
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
            result.append(stackHandle.load(param, scope));
        }
        result.append("invokespecial ").append(((ClassType) classElement.getType()).getName());
        result.append(".").append(methodName);

        result.append("(");

        for(var operand : parameters){
            result.append(getArgumentCode(operand));
        }

        result.append(")");
        result.append(utils.getJasminType(returnType)).append("\n");
        result.append(stackHandle.store(this.leftSideNew, this.scope, this.rightSideNew));

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
            case BINARYOPER:
                Element rightElement = ((BinaryOpInstruction) rightSide).getRightOperand();
                Element leftElement = ((BinaryOpInstruction) rightSide).getLeftOperand();

                String leftInstruction = stackHandle.load(leftElement, scope);
                String rightInstruction = stackHandle.load(rightElement, scope);
                OperationType operationType = ((BinaryOpInstruction) rightSide).getOperation().getOpType();

                if(operationType == OperationType.ADD || operationType == OperationType.DIV || operationType == OperationType.MUL ||
                        operationType == OperationType.SUB){

                    result.append(stackHandle.load(leftElement, scope));
                    result.append(stackHandle.load(rightElement, scope));
                    result.append(stackHandle.getOperation(operationType));
                } else if(operationType == OperationType.ANDB || operationType == OperationType.LTH || operationType == OperationType.NOTB){
                    switch (operationType){
                        case LTH:
                            result.append(lthConversion(leftInstruction, rightInstruction, operationType));
                            break;
                        case ANDB:
                            result.append(andConversion(leftInstruction, rightInstruction, operationType));
                            break;
                        case NOTB:
                            result.append(notConversion(rightInstruction));
                            break;
                        default:
                            throw new NotImplementedException(this);
                    }
                }
                break;
            case CALL:
                this.rightSideNew = right.toString();
                result.append(getCode((CallInstruction) rightSide));
                return result.toString();

            default:
                throw new NotImplementedException(this);
        }
        this.rightSideNew = right.toString();
        result.append(stackHandle.store(leftSide, scope, rightSideNew));
        this.assign = false;
        return result.toString();
    }

    private String notConversion(String rightInstruction) {
        StringBuilder result = new StringBuilder();
        int index = OllirToJasmin.index;

        result.append(rightInstruction);
        String label1 = "IFNE"+ (index*2);
        result.append("ifne ").append(label1+"\n");
        result.append("iconst_1\n");
        String label2 = "IFNE_"+(index*2+1);
        result.append("goto ").append(label2+"\n");
        result.append(label1).append(":\n");
        result.append("iconst_0\n");
        result.append(label2).append(":\n");
        return result.toString();
    }

    private String andConversion(String leftInstruction, String rightInstruction, OperationType operationType) {
        StringBuilder result = new StringBuilder();
        int index = OllirToJasmin.index;
        String label1 = "IFEQ_"+ (index*2);
        String label2 = "IFEQ_"+ (index*2+1);

        result.append(leftInstruction);
        result.append("ifeq ").append(label1).append("\n");
        result.append(rightInstruction);
        result.append("ifeq ").append(label1).append("\n");

        result.append("iconst_1");
        result.append("goto ").append(label2).append("\n");
        result.append(label1).append(":\n");
        result.append("iconst_0");
        result.append(label2).append(":\n");
        return result.toString();
    }

    private String lthConversion(String leftInstruction, String rightInstruction, OperationType operationType) {
        StringBuilder result = new StringBuilder();
        result.append(leftInstruction).append(rightInstruction);
        int index = OllirToJasmin.index;
        String label1 = "IFICMP_"+ (index*2);
        String label2 = "IFICMP_"+ (index*2+1);
        result.append("if_icmplt ").append(label1).append("\n");
        result.append("iconst_0");
        result.append("goto ").append(label2).append("\n");
        result.append(label1).append(":\n");
        result.append("iconst_1");
        result.append(label1).append(":\n");
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
        var result = new StringBuilder();

        ArrayList<Element> parameters = instruction.getListOfOperands();
        Type returnType = instruction.getReturnType();
        for (Element param : parameters){
            result.append(stackHandle.load(param, scope));
        }
        result.append("invokestatic ");

        var methodClass = ((Operand) instruction.getFirstArg()).getName();
        result.append(utils.getFullyQualifiedName(methodClass)).append(".");

        String methodName = ((LiteralElement) instruction.getSecondArg()).getLiteral();
        result.append(methodName.replace("\"", ""));

        result.append("(");

        for(var operand : parameters){
            result.append(getArgumentCode(operand));
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

    private String getArgumentCode(Element operand) {
        StringBuilder result = new StringBuilder();

        result.append(utils.getJasminType(operand.getType()));
        return result.toString();
    }


}
