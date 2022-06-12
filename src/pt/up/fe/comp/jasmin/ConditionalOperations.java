package pt.up.fe.comp.jasmin;

import org.specs.comp.ollir.*;

import java.util.HashMap;

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

            result.append(BooleanOperations.operate(type, op1, op2));

            result.append("ifne ");
            result.append(instruction.getLabel()).append("\n");

        }


        return result.toString();
    }
}
