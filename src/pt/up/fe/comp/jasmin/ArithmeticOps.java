package pt.up.fe.comp.jasmin;

import org.specs.comp.ollir.Descriptor;
import org.specs.comp.ollir.Element;
import org.specs.comp.ollir.OperationType;
import pt.up.fe.specs.util.exceptions.NotImplementedException;

import java.util.HashMap;

public class ArithmeticOps {

    public static boolean isArithmeticOp(OperationType operationType){
        if(operationType == OperationType.ADD || operationType == OperationType.DIV || operationType == OperationType.MUL ||
                operationType == OperationType.SUB){
            return true;
        }
        return false;
    }

    public static String operate(Element rightElement, Element leftElement,
                                 HashMap<String, Descriptor> scope, OperationType operationType, StackLocalsCount counters){
        StringBuilder result = new StringBuilder();
        result.append(LoadStore.load(leftElement, scope, counters));
        result.append(LoadStore.load(rightElement, scope, counters));
        result.append(ArithmeticOps.getOperation(operationType, counters));
        return result.toString();
    }

    public static String getOperation(OperationType operationType, StackLocalsCount counters) {
        StringBuilder result = new StringBuilder();
        switch (operationType){
            case MUL:
                counters.decStackSize(1);
                result.append("imul \n");
                break;
            case DIV:
                counters.decStackSize(1);
                result.append("idiv \n");
                break;
            case ADD:
                counters.decStackSize(1);
                result.append("iadd \n");
                break;
            case SUB:
                counters.decStackSize(1);
                result.append("isub \n");
                break;
            default:
                throw new NotImplementedException("Operation binary");
        }
        return  result.toString();
    }
}
