package pt.up.fe.comp.jmm.analysis.analysers;

import pt.up.fe.comp.jmm.analysis.SemanticAnalyser;
import pt.up.fe.comp.jmm.analysis.table.SymbolTable;
import pt.up.fe.comp.jmm.ast.AstNode;
import pt.up.fe.comp.jmm.ast.JmmNode;
import pt.up.fe.comp.jmm.report.Report;
import pt.up.fe.comp.jmm.report.ReportType;
import pt.up.fe.comp.jmm.report.Stage;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class BlockVerifier extends Verifier{

    public BlockVerifier(JmmNode root, SymbolTable symbolTable){
        super(root, symbolTable);
        addVisit(AstNode.BLOCK, this::visitBlock);
    }

    private Boolean visitBlock(JmmNode block, Boolean dummy){
        var parent_kind = block.getJmmParent().getKind();
        if(!parent_kind.equals(AstNode.IF.toString()) && !parent_kind.equals(AstNode.ELSE.toString()) && !parent_kind.equals(AstNode.WHILE.toString())){
            this.addReport(new Report(ReportType.ERROR, Stage.SEMANTIC, Integer.valueOf(block.get("line")), Integer.valueOf(block.get("col")), "Unnecessary Block"));
            return false;
        }
        return true;
    }

}
