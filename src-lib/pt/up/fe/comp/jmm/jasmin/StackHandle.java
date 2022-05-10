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
        }
        return result.toString();
    }
}
