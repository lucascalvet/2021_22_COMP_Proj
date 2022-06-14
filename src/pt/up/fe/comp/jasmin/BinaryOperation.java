package pt.up.fe.comp.jasmin;

import org.specs.comp.ollir.Descriptor;
import org.specs.comp.ollir.Element;
import org.specs.comp.ollir.OperationType;


import java.util.HashMap;

public class BinaryOperation {

    public static String processBinaryOperation(Element rightElement, Element leftElement,
                                                OperationType operationType, HashMap<String, Descriptor> scope, StackLocalsCount counters){
        StringBuilder result = new StringBuilder();



        if(ArithmeticOps.isArithmeticOp(operationType)){
            result.append(ArithmeticOps.operate(rightElement, leftElement, scope, operationType, counters));
        } else if(BooleanOperations.isBooleanOp(operationType)){
            String leftInstruction = LoadStore.load(leftElement, scope, counters);
            String rightInstruction = LoadStore.load(rightElement, scope, counters);
            result.append(BooleanOperations.operate(operationType, leftInstruction, rightInstruction, counters));

        }
        return result.toString();
    }
}
