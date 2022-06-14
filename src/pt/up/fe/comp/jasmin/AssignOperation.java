package pt.up.fe.comp.jasmin;

import org.specs.comp.ollir.*;
import pt.up.fe.specs.util.exceptions.NotImplementedException;

import java.util.HashMap;

public class AssignOperation {

    public static String getCode(AssignInstruction instruction, ConversionInstructions converter, StackLocalsCount counters){
        ConversionUtils utils = converter.getUtils();
        HashMap<String, Descriptor> scope = converter.getScope();

        converter.setAssign(true);
        StringBuilder result = new StringBuilder();
        Instruction rightSide = instruction.getRhs();
        Element leftSide = instruction.getDest();

        StringBuilder right = new StringBuilder();
        InstructionType type = rightSide.getInstType();
        switch (type){
            case NOPER:
                Element single = ((SingleOpInstruction) rightSide).getSingleOperand();
                System.out.println("antes do load no noper");
                String load = LoadStore.load(single, scope, counters);
                right.append(load);
                result.append(load);
                break;
            case GETFIELD:
                Element classElement = ((GetFieldInstruction) rightSide).getFirstOperand();
                Element field = ((GetFieldInstruction) rightSide).getSecondOperand();

                result.append(FieldsOperations.getGetFieldCode(classElement, field, utils, scope, counters));
                break;
            case BINARYOPER:
                Element rightElement = ((BinaryOpInstruction) rightSide).getRightOperand();
                Element leftElement = ((BinaryOpInstruction) rightSide).getLeftOperand();
                OperationType operationType = ((BinaryOpInstruction) rightSide).getOperation().getOpType();

                result.append(BinaryOperation.processBinaryOperation(rightElement, leftElement, operationType, scope, counters));
                break;
            case CALL:
                result.append(converter.getCode((CallInstruction) rightSide));
                break;
            case UNARYOPER:
                Element e = ((UnaryOpInstruction) rightSide).getOperand();
                OperationType optype = ((UnaryOpInstruction) rightSide).getOperation().getOpType();
                String loadoper = LoadStore.load(e, scope, counters);
                BooleanOperations.operate(optype, null, loadoper, counters);
                result.append(BooleanOperations.notConversion(loadoper, counters));
                break;
            default:
                throw new NotImplementedException("Problem in assign");
        }
        result.append(LoadStore.store(leftSide, scope, right.toString(), counters));
        converter.setAssign(false);
        return result.toString();
    }
}
