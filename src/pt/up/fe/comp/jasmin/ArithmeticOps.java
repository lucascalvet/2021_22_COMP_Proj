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
                                 HashMap<String, Descriptor> scope, OperationType operationType){
        StringBuilder result = new StringBuilder();
        result.append(LoadStore.load(leftElement, scope));
        result.append(LoadStore.load(rightElement, scope));
        result.append(ArithmeticOps.getOperation(operationType));
        return result.toString();
    }

    public static String getOperation(OperationType operationType) {
        StringBuilder result = new StringBuilder();
        switch (operationType){
            case MUL:
                result.append("imul \n");
                break;
            case DIV:
                result.append("idiv \n");
                break;
            case ADD:
                result.append("iadd \n");
                break;
            case SUB:
                result.append("isub \n");
                break;
            default:
                throw new NotImplementedException("Operation binary");
        }
        return  result.toString();
    }
}
