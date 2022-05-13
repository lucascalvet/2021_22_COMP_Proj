package pt.up.fe.comp.jmm.ast.collectors;

import pt.up.fe.comp.jmm.ast.AstNode;
import pt.up.fe.comp.jmm.ast.JmmNode;

public class SuperCollector extends Collector {
    private String super_name;

    public SuperCollector() {
        this.visits = 0;
        this.super_name = "";
        addVisit(AstNode.PROGRAM, this::visitDefault);
        addVisit(AstNode.CLASS_DECL, this::visitDefault);
        addVisit(AstNode.EXTENDS, this::visitExtends);
        setDefaultVisit((node, imports) -> ++visits);
    }

    public String getSuperName() {
        return this.super_name;
    }

    private Integer visitExtends(JmmNode super_extends, Boolean dummy) {
        this.super_name = super_extends.getJmmChild(0).get("name");
        return ++visits;
    }
}

