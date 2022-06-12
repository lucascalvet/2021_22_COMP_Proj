package pt.up.fe.comp.jasmin;

import org.specs.comp.ollir.OperationType;
import pt.up.fe.specs.util.exceptions.NotImplementedException;

public class BooleanOperations {

    public static String operate(OperationType type, String op1, String op2){
        StringBuilder result = new StringBuilder();
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
        return result.toString();
    }

    public static String notConversion(String rightInstruction) {
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

    public static String andConversion(String leftInstruction, String rightInstruction) {
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


    public static String lthConversion(String leftInstruction, String rightInstruction) {
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
}
