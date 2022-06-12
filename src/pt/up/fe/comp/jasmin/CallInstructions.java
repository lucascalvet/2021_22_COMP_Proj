package pt.up.fe.comp.jasmin;

import org.specs.comp.ollir.*;


public class CallInstructions {

    /*private static String getCodeInvokeStatic(CallInstruction instruction, HashMap<String, Descriptor> scope,
                                              ConversionUtils utils, boolean isAssign) {
        var result = new StringBuilder();

        ArrayList<Element> parameters = instruction.getListOfOperands();
        Type returnType = instruction.getReturnType();
        for (Element param : parameters){
            result.append(LoadStore.load(param, scope));
        }
        result.append("invokestatic ");

        var methodClass = ((Operand) instruction.getFirstArg()).getName();
        result.append(utils.getFullyQualifiedName(methodClass)).append(".");

        String methodName = ((LiteralElement) instruction.getSecondArg()).getLiteral();
        result.append(methodName.replace("\"", ""));

        result.append("(");

        for(var operand : parameters){
            result.append(getArgumentCode(operand, utils));
        }

        result.append(")");

        result.append(utils.getJasminType(instruction.getReturnType()));
        result.append("\n");

        if(!this.assign && returnType.getTypeOfElement() != ElementType.VOID){
            result.append("pop\n");
            this.assign = false;
        }

        return result.toString();
    }
*/
    public static String getArgumentCode(Element operand, ConversionUtils utils) {
        StringBuilder result = new StringBuilder();

        result.append(utils.getJasminType(operand.getType()));
        return result.toString();
    }
}
