package pt.up.fe.comp.jasmin;

import org.specs.comp.ollir.*;
import pt.up.fe.specs.util.exceptions.NotImplementedException;

import java.util.HashMap;

public class ConditionalOperations {

    public static String getCode(CondBranchInstruction instruction, HashMap<String, Descriptor> scope, StackLocalsCount counters){
        StringBuilder result = new StringBuilder();
        if(instruction.getOperands().size() == 1) {
            if(instruction.getCondition().getInstType() == InstructionType.NOPER){
                Element op1 = instruction.getOperands().get(0);
                String op11 = LoadStore.load(op1, scope, counters);
                result.append(op11);
                result.append("ifne ");
                result.append(instruction.getLabel()).append("\n");
            } else {
                Element op1 = instruction.getOperands().get(0);
                String op11 = LoadStore.load(op1, scope, counters);
                result.append(BooleanOperations.notConversion(op11));
                result.append("ifne ");
                result.append(instruction.getLabel()).append("\n");

            }
        } else {

            Element operand1 = instruction.getOperands().get(0);
            Element operand2 = instruction.getOperands().get(1);

            String op1 = LoadStore.load(operand1, scope, counters);
            String op2 = LoadStore.load(operand2, scope, counters);
            Instruction operation = instruction.getCondition();

            BinaryOpInstruction op = (BinaryOpInstruction) operation;
            OperationType type = op.getOperation().getOpType();

            result.append(BooleanOperations.operate(type, op1, op2));

            result.append("ifne ");
            result.append(instruction.getLabel()).append("\n");

        }


        return result.toString();
    }
}
