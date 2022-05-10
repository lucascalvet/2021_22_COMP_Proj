package pt.up.fe.comp.jmm.ast.collectors;

import pt.up.fe.comp.jmm.ast.visitors.AJmmVisitor;
import pt.up.fe.comp.jmm.ast.JmmNode;
import pt.up.fe.comp.jmm.report.Report;

import java.util.ArrayList;
import java.util.List;

public abstract class Collector extends AJmmVisitor<Boolean, Integer> {
    protected int visits;
    private List<Report> reports = new ArrayList<>();

    protected Integer visitDefault(JmmNode node, Boolean dummy) {
        for (var child : node.getChildren()) {
            visit(child, true);
        }
        return ++visits;
    }

    public List<Report> getReports(){
        return this.reports;
    }
}
