package pt.up.fe.comp.jmm.ast;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class ClassNameCollector extends AJmmVisitor<Boolean, Integer> {
    private int visits;
    private String class_name;

    public ClassNameCollector() {
        this.visits = 0;
        this.class_name = "";
        addVisit("Program", this::visitDefault);
        addVisit("ClassDecl", this::visitClassDecl);
        setDefaultVisit((node, imports) -> ++visits);
    }

    public String getClassName() {
        return this.class_name;
    }

    private Integer visitDefault(JmmNode node, Boolean dummy) {
        for (var child : node.getChildren()) {
            visit(child, true);
        }
        return ++visits;
    }

    private Integer visitClassDecl(JmmNode class_decl, Boolean dummy) {
        this.class_name = class_decl.getChildren().get(0).get("name");
        return ++visits;
    }
}
