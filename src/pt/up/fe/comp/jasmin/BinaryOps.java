package pt.up.fe.comp.jasmin;

import org.specs.comp.ollir.OperationType;
import pt.up.fe.specs.util.exceptions.NotImplementedException;

public class BinaryOps {

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
