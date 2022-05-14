package pt.up.fe.comp.jmm.ast;

import pt.up.fe.specs.util.SpecsStrings;


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
    TYPE,
    ID,
    INT,
    TRUE,
    FALSE,
    INT_TYPE,
    NEW,
    ACCESS,
    ARRAY_ACCESS,
    LENGTH,
    ADD,
    SUBTRACT,
    MULTIPLY,
    DIVIDE;

    private final String name;

    private AstNode(){
        this.name = SpecsStrings.toCamelCase(name(), "_", true);
    }

    @Override
    public String toString(){
        return this.name;
    }
}
