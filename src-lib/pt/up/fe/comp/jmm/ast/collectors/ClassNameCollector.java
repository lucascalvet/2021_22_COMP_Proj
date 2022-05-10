package pt.up.fe.comp.jmm.ast.collectors;

import pt.up.fe.comp.jmm.ast.AstNode;
import pt.up.fe.comp.jmm.ast.JmmNode;

public class ClassNameCollector extends Collector {
    private String class_name;

    public ClassNameCollector() {
        this.visits = 0;
        this.class_name = "";
        addVisit(AstNode.PROGRAM, this::visitDefault);
        addVisit(AstNode.CLASS_DECL, this::visitClassDecl);
        setDefaultVisit((node, imports) -> ++visits);
    }

    public String getClassName() {
        return this.class_name;
    }

    private Integer visitClassDecl(JmmNode class_decl, Boolean dummy) {
        this.class_name = class_decl.getChildren().get(0).get("name");
        return ++visits;
    }
}
