package pt.up.fe.comp.jmm.ast;

import pt.up.fe.specs.util.SpecsStrings;

import java.util.Arrays;
import java.util.List;


public enum AstNode {

    PROGRAM,
    IMPORT,
    PACKAGE,
    EXTENDS,
    CLASS_DECL,
    CLASS_BODY,
    VAR,
    METHOD_DECL,
    FUNCTION,
    FUNC_RETURN,
    FUNC_NAME,
    FUNC_ARGS,
    MAIN,
    MAIN_HEADER,
    MAIN_BODY,
    MAIN_ARGS,
    BODY,
    RETURN_STATEMENT,
    BLOCK,
    ASSIGN,
    WHILE,
    IF,
    CONDITION,
    ELSE,
    ACCESS,
    CHAINED,
    TYPE,
    ID,
    INT,
    TRUE,
    FALSE,
    INT_TYPE,
    NEW,
    ARRAY_ACCESS,
    LENGTH,
    ADD,
    SUBTRACT,
    MULTIPLY,
    DIVIDE,
    AND,
    LOWER,
    NOT,
    THIS;

    private final String name;

    private AstNode(){
        this.name = SpecsStrings.toCamelCase(name(), "_", true);
    }

    public static List<String> getConsts(){return Arrays.asList(AstNode.INT.toString(), AstNode.TRUE.toString(), AstNode.FALSE.toString());}
    public static List<String> getBooleanOp(){return Arrays.asList(AstNode.NOT.toString(), AstNode.AND.toString());}
    public static List<String> getAritmeticOp(){return Arrays.asList(AstNode.ADD.toString(), AstNode.SUBTRACT.toString(), AstNode.MULTIPLY.toString(), AstNode.DIVIDE.toString());}
    public static List<String> getTerminalNodes(){return Arrays.asList(AstNode.INT.toString(), AstNode.ID.toString(), AstNode.TRUE.toString() , AstNode.FALSE.toString(), AstNode.THIS.toString());}
    @Override
    public String toString(){
        return this.name;
    }
}
