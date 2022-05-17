package pt.up.fe.comp.jmm.ast.collectors;

import pt.up.fe.comp.jmm.analysis.table.Type;
import pt.up.fe.comp.jmm.ast.AstNode;
import pt.up.fe.comp.jmm.ast.JmmNode;

public class ReturnTypeCollector extends Collector {
    private Type return_type;
    private String signature;

    public ReturnTypeCollector(String methodSignature) {
        this.visits = 0;
        this.return_type = new Type("", false);
        this.signature = methodSignature;
        if (this.signature.equals("main")){
            this.return_type = new Type("void", false);
        }
        else{
            addVisit(AstNode.PROGRAM, this::visitDefault);
            addVisit(AstNode.CLASS_DECL, this::visitDefault);
            addVisit(AstNode.CLASS_BODY, this::visitDefault);
            addVisit(AstNode.METHOD_DECL, this::visitDefault);
            addVisit(AstNode.FUNCTION, this::visitFunction);
            setDefaultVisit((node, imports) -> ++visits);
        }
    }

    public Type getReturnType() {
        return this.return_type;
    }

    private Integer visitFunction(JmmNode func, Boolean dummy) {
        String rtype = "";
        for (var child : func.getChildren()) {
            if (child.getKind().equals(AstNode.FUNC_RETURN.toString())) {
                rtype = child.getJmmChild(0).get("type");
                if(rtype.equals("custom")){
                    rtype = child.getJmmChild(0).getJmmChild(0).get("name");
                }
            }
            if (child.getKind().equals(AstNode.FUNC_NAME.toString())){
                if (child.getJmmChild(0).get("name").equals(this.signature)){
                    if(rtype.equals("int array")){
                        this.return_type = new Type("int", true);
                    }
                    else this.return_type = new Type(rtype, false);
                    break;
                }
            }
        }
        return ++visits;
    }

}