package pt.up.fe.comp.jmm.ast.collectors;

import pt.up.fe.comp.jmm.ast.AstNode;
import pt.up.fe.comp.jmm.ast.JmmNode;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class ImportCollector extends Collector {
    private final List<String> imports;

    public ImportCollector() {
        this.visits = 0;
        this.imports = new ArrayList<>();
        addVisit(AstNode.PROGRAM, this::visitDefault);
        addVisit(AstNode.IMPORT, this::visitDefault);
        addVisit(AstNode.PACKAGE, this::visitPackage);
        setDefaultVisit((node, imports) -> ++visits);
    }

    public List<String> getImports() {
        return this.imports;
    }

    private Integer visitPackage(JmmNode package_node, Boolean dummy) {
        var importString = package_node.getChildren().stream()
                .map(id -> id.get("name"))
                .collect(Collectors.joining("."));

        this.imports.add(importString);
        return ++visits;
    }
}
