package pt.up.fe.comp.jmm.ast.collectors;

import pt.up.fe.comp.jmm.ast.AstNode;
import pt.up.fe.comp.jmm.ast.JmmNode;
import pt.up.fe.comp.jmm.report.Report;
import pt.up.fe.comp.jmm.report.ReportType;
import pt.up.fe.comp.jmm.report.Stage;

import java.util.ArrayList;
import java.util.List;

public class MethodCollector extends Collector {
    private final List<String> methods;

    public MethodCollector() {
        this.visits = 0;
        this.methods = new ArrayList<>();
        addVisit(AstNode.PROGRAM, this::visitDefault);
        addVisit(AstNode.CLASS_DECL, this::visitDefault);
        addVisit(AstNode.CLASS_BODY, this::visitDefault);
        addVisit(AstNode.METHOD_DECL, this::visitDefault);
        addVisit(AstNode.FUNCTION, this::visitFunction);
        addVisit(AstNode.MAIN, this::visitDefault);
        addVisit(AstNode.MAIN_HEADER, this::visitMain);
        setDefaultVisit((node, imports) -> ++visits);
    }

    public List<String> getMethods() {
        return this.methods;
    }

    private Integer visitFunction(JmmNode func, Boolean dummy) {
        String name = "";
        for (var child : func.getChildren()) {
            if (child.getKind().equals(AstNode.FUNC_NAME.toString())){
                name = child.getChildren().get(0).get("name");
                if(this.methods.contains(name)){
                    this.addReport(new Report(ReportType.ERROR, Stage.SEMANTIC, Integer.valueOf(child.get("line")), Integer.valueOf(child.get("column")), "Found duplicate method with signature '" + name + "'"));
                    return -1;
                }
                break;
            }
        }
        this.methods.add(name);
        return ++visits;
    }


    private Integer visitMain(JmmNode main, Boolean dummy){
        this.methods.add("main");
        return ++visits;
    }
}

