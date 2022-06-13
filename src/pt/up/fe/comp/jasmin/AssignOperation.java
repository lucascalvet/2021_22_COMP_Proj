package pt.up.fe.comp.jasmin;

import org.specs.comp.ollir.*;
import pt.up.fe.specs.util.exceptions.NotImplementedException;

import java.util.HashMap;

public class AssignOperation {

    public static String getCode(AssignInstruction instruction, ConversionInstructions converter){
        ConversionUtils utils = converter.getUtils();
        HashMap<String, Descriptor> scope = converter.getScope();

        converter.setAssign(true);
        StringBuilder result = new StringBuilder();
        Instruction rightSide = instruction.getRhs();
        Element leftSide = instruction.getDest();
        converter.setLeftSideNew(instruction.getDest());

        //handling right side
        InstructionType type = rightSide.getInstType();
        switch (type){
            case NOPER:
                StringBuilder right = new StringBuilder();
                Element single = ((SingleOpInstruction) rightSide).getSingleOperand();
                right.append(LoadStore.load(single, converter.getScope()));
                result.append(LoadStore.load(single, scope));
                result.append(LoadStore.store(leftSide, scope, right.toString()));
                break;
            case GETFIELD:
                Element classElement = ((GetFieldInstruction) rightSide).getFirstOperand();
                Element field = ((GetFieldInstruction) rightSide).getSecondOperand();

                result.append(FieldsOperations.getGetFieldCode(classElement, field, utils, scope));
                result.append(LoadStore.store(leftSide, scope, null));
                break;
            case BINARYOPER:
                Element rightElement = ((BinaryOpInstruction) rightSide).getRightOperand();
                Element leftElement = ((BinaryOpInstruction) rightSide).getLeftOperand();
                OperationType operationType = ((BinaryOpInstruction) rightSide).getOperation().getOpType();

                result.append(BinaryOperation.processBinaryOperation(rightElement, leftElement, operationType, scope));
                result.append(LoadStore.store(leftSide, scope, null));
                break;
            case CALL:
                result.append(converter.getCode((CallInstruction) rightSide));
                return result.toString();

            default:
                throw new NotImplementedException("Problem in assign");
        }
        converter.setAssign(false);
        return result.toString();
    }
}
