package pt.up.fe.comp.jasmin;

public class InstructionUtil {
    public static String iconst(int value){
        String valueStr = Integer.toString(value);
        StringBuilder result = new StringBuilder();
        if(value == -1){
            result.append("iconst_m1\n");
        } else if (value <= 5 && value >= -1){
            result.append("iconst_").append(valueStr).append("\n");
        } else if (value <= 127 && value >= -128){
            result.append("bipush ").append(valueStr).append("\n");
        } else if (value <= 32767 && value >= -32768){
            result.append("sipush ").append(valueStr).append("\n");
        } else {
            result.append("ldc ").append(valueStr).append("\n");
        }
        return result.toString();
    }
}
