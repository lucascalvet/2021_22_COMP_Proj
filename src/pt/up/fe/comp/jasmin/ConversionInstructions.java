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
    //private LoadStore stackHandle;

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
        //instructionMap.put()
        this.scope = new HashMap<>();
        //this.stackHandle = new LoadStore();
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
        StringBuilder result = new StringBuilder();
        if(instruction.getOperands().size() == 1) {
            Element op1 = instruction.getOperands().get(0);
            String op11 = LoadStore.load(op1, scope);
            result.append(op11);
            result.append("ifne ");
            result.append(instruction.getLabel()).append("\n");
        } else {

            Element operand1 = instruction.getOperands().get(0);
            Element operand2 = instruction.getOperands().get(1);

            String op1 = LoadStore.load(operand1, scope);
            String op2 = LoadStore.load(operand2, scope);
            Instruction operation = instruction.getCondition();

            BinaryOpInstruction op = (BinaryOpInstruction) operation;
            OperationType type = op.getOperation().getOpType();

            //availar a expressção
            switch (type){
                case LTH:
                    result.append(lthConversion(op1, op2));
                    break;
                case ANDB:
                    result.append(andConversion(op1, op2));
                    break;
                case NOTB:
                    result.append(notConversion(op1));
                    break;
                default:
                    throw new NotImplementedException(this);
            }

            result.append("ifne ");
            result.append(instruction.getLabel()).append("\n");

        }


        return result.toString();
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

                String leftInstruction = LoadStore.load(leftElement, scope);
                String rightInstruction = LoadStore.load(rightElement, scope);
                OperationType operationType = ((BinaryOpInstruction) rightSide).getOperation().getOpType();

                if(operationType == OperationType.ADD || operationType == OperationType.DIV || operationType == OperationType.MUL ||
                        operationType == OperationType.SUB){

                    result.append(LoadStore.load(leftElement, scope));
                    result.append(LoadStore.load(rightElement, scope));
                    result.append(BinaryOps.getOperation(operationType));
                } else if(operationType == OperationType.ANDB || operationType == OperationType.LTH || operationType == OperationType.NOTB){
                    switch (operationType){
                        case LTH:
                            result.append(lthConversion(leftInstruction, rightInstruction));
                            break;
                        case ANDB:
                            result.append(andConversion(leftInstruction, rightInstruction));
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
        result.append(LoadStore.store(leftSide, scope, rightSideNew));
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

    private String andConversion(String leftInstruction, String rightInstruction) {
        StringBuilder result = new StringBuilder();
        int index = OllirToJasmin.index;
        String labelFalse = "$isFalse_"+ (index*2);
        String labelTrue = "$isTrue_"+ (index*2+1);

        //is left 0?
        result.append(leftInstruction);
        result.append("ifeq ").append(labelFalse).append("\n");

        //is right 0?
        result.append(rightInstruction);
        result.append("ifeq ").append(labelFalse).append("\n");

        //if it is true it reached here
        result.append("iconst_1").append("\n");
        result.append("goto ").append(labelTrue ).append("\n"); //jump to the end

        result.append(labelFalse).append(":\n");
        result.append("iconst_0").append("\n");
        result.append(labelTrue ).append(":\n");
        return result.toString();
    }

    private String lthConversion(String leftInstruction, String rightInstruction) {
        StringBuilder result = new StringBuilder();
        result.append(leftInstruction).append(rightInstruction);
        int index = OllirToJasmin.index;

        String label1 = "$IFICMP_"+ (index*2);
        String label2 = "$IFICMP_"+ (index*2+1);

        result.append("if_icmplt ").append(label1).append("\n");
        result.append("iconst_0").append("\n");
        result.append("goto ").append(label2).append("\n");
        result.append(label1).append(":\n");
        result.append("iconst_1").append("\n");
        result.append(label2).append(":\n");
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
