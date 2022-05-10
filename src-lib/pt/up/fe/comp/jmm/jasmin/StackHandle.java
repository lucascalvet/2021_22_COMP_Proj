package pt.up.fe.comp.jmm.jasmin;

import org.specs.comp.ollir.Descriptor;
import org.specs.comp.ollir.Element;
import org.specs.comp.ollir.ElementType;
import org.specs.comp.ollir.LiteralElement;

import java.util.HashMap;

public class StackHandle {
    public String load(Element element, HashMap<String, Descriptor> scope){
        StringBuilder result = new StringBuilder();

        ElementType type = element.getType().getTypeOfElement();
        if(element.isLiteral()){
            result.append("ldc ").append(((LiteralElement)element).getLiteral()).append("\n");
        } else {
            if (type == ElementType.INT32 || type == ElementType.STRING || type == ElementType.BOOLEAN){
                int register = scope.get(element).getVirtualReg();
                if (register > 3 || register < 0)
                    result.append("aload ").append(register).append("\n");
                else
                    result.append("aload_").append(register).append("\n");
            }
        }
        return result.toString();
    }
}
