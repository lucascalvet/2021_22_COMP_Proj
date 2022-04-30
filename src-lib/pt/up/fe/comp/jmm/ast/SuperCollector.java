package pt.up.fe.comp.jmm.ast;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class SuperCollector extends AJmmVisitor<Boolean, Integer> {
    private int visits;
    private String super_name;

    public SuperCollector() {
        this.visits = 0;
        this.super_name = null;
        addVisit("Program", this::visitDefault);
        addVisit("ClassDecl", this::visitDefault);
        addVisit("Extends", this::visitExtends);
        setDefaultVisit((node, imports) -> ++visits);
    }

    public String getSuperName() {
        return this.super_name;
    }

    private Integer visitDefault(JmmNode node, Boolean dummy) {
        for (var child : node.getChildren()) {
            visit(child, true);
        }
        return ++visits;
    }

    private Integer visitExtends(JmmNode super_extends, Boolean dummy) {
        this.super_name = super_extends.getChildren().get(0).get("name");
        return ++visits;
    }
}

