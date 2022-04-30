package pt.up.fe.comp.jmm.ast;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class ImportCollector extends AJmmVisitor<Boolean, Integer> {
    private int visits;
    private final List<String> imports;

    public ImportCollector() {
        this.visits = 0;
        this.imports = new ArrayList<>();
        addVisit("Program", this::visitDefault);
        addVisit("Import", this::visitDefault);
        addVisit("Package", this::visitPackage);
        setDefaultVisit((node, imports) -> ++visits);
    }

    public List<String> getImports() {
        return this.imports;
    }

    private Integer visitDefault(JmmNode node, Boolean dummy) {
        for (var child : node.getChildren()) {
            visit(child, true);
        }
        return ++visits;
    }

    private Integer visitPackage(JmmNode package_node, Boolean dummy) {
        var importString = package_node.getChildren().stream()
                .map(id -> id.get("name"))
                .collect(Collectors.joining("."));

        this.imports.add(importString);
        return ++visits;
    }
}
