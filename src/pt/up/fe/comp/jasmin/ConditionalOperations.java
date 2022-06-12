package pt.up.fe.comp.jasmin;

import org.specs.comp.ollir.*;
import pt.up.fe.specs.util.exceptions.NotImplementedException;

import java.util.HashMap;

import static pt.up.fe.comp.jasmin.BooleanOperations.*;

public class ConditionalOperations {

    public static String getCode(CondBranchInstruction instruction, HashMap<String, Descriptor> scope){
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
                    throw new NotImplementedException("Binary op");
            }

            result.append("ifne ");
            result.append(instruction.getLabel()).append("\n");

        }


        return result.toString();
    }
}
