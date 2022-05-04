package pt.up.fe.comp.jmm.ast;

public abstract class Collector extends AJmmVisitor<Boolean, Integer> {
    protected int visits;

    protected Integer visitDefault(JmmNode node, Boolean dummy) {
        for (var child : node.getChildren()) {
            visit(child, true);
        }
        return ++visits;
    }
}
