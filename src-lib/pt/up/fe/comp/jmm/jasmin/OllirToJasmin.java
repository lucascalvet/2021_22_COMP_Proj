package pt.up.fe.comp.jmm.jasmin;

import org.specs.comp.ollir.*;

import pt.up.fe.specs.util.SpecsIo;
import pt.up.fe.specs.util.classmap.FunctionClassMap;
import pt.up.fe.specs.util.exceptions.NotImplementedException;

import java.util.Collections;
import java.util.stream.Collectors;

public class OllirToJasmin {

    private final ClassUnit classUnit;
    private final FunctionClassMap<Instruction, String> instructionMap;
    private final ConversionUtils utils;

    public OllirToJasmin(ClassUnit classUnit) {
        this.classUnit = classUnit;
        this.utils = new ConversionUtils(this.classUnit);


        this.instructionMap = new FunctionClassMap<>();
        instructionMap.put(CallInstruction.class,this::getCode);
        instructionMap.put(AssignInstruction.class,this::getCode);
        instructionMap.put(ReturnInstruction.class, this::getCode);
    }


    public String getCode(){
        var code = new StringBuilder();
        var qualifiedNameSuper = utils.getFullyQualifiedName(classUnit.getSuperClass());

        code.append(".class public ").append(classUnit.getClassName()).append("\n");
        code.append(".super ").append(qualifiedNameSuper).append("\n\n");


        //FIELDS
        for(var field: classUnit.getFields()){
            code.append(getCode(field));
        }

        code.append(SpecsIo.getResource("../test/templates/constructor.txt").replace("${SUPER_CLASS}", qualifiedNameSuper));
        code.append("\n\n");

        //METHODS
        for(var method : classUnit.getMethods()){
            if(!method.isConstructMethod()) code.append(getCode(method));
        }

        var result = code.toString();
        System.out.println("\nJasmin Code: \n" + result);
        return result;
    }

    private String getCode(Field field) {
        var result = new StringBuilder();

        result.append(".field ");
        var fieldAccessModifier = field.getFieldAccessModifier().name();
        if(!fieldAccessModifier.equals("DEFAULT")){
            result.append(fieldAccessModifier.toLowerCase()).append(" ");
        }

        result.append(field.getFieldName()).append(" ");

        result.append(utils.getJasminType(field.getFieldType())).append("\n");

        return result.toString();
    }

    public String getCode(Method method){
        var code = new StringBuilder();

        //Cabeça do método
        code.append(".method ");

        var methodAccessModifier = method.getMethodAccessModifier().name();
        if(!methodAccessModifier.equals("DEFAULT")){
            code.append(methodAccessModifier.toLowerCase()).append(" ");
        }

        if (method.isStaticMethod()){
            code.append("static ");
        }

        code.append(method.getMethodName()).append("(");

        var methodParamTypes = method.getParams().stream()
                .map(elem -> utils.getJasminType(elem.getType()))
                .collect(Collectors.joining());

        code.append(methodParamTypes).append(")").append(utils.getJasminType(method.getReturnType())).append("\n");

        //Corpo do método
        code.append(".limit stack 99\n");
        code.append(".limit locals 99\n");

        for(var inst : method.getInstructions()){
            code.append(getCode(inst));
        }

        //TODO: Return Statement
        code.append("return\n"); //se for return Void

        //Fecho do método
        code.append(".end method\n\n");

        var result = code.toString();
        return result;
    }




    public String getCode(Instruction instruction){
        return instructionMap.apply(instruction);
    }

    public String getCode(CallInstruction instruction){

        switch(instruction.getInvocationType()){
            //TODO : ver as outras invocações
            case invokestatic:
                return getCodeInvokeStatic(instruction);
            default:
                return ""; //throw new NotImplementedException(instruction.getInvocationType());
        }
    }

    public String getCode(AssignInstruction instruction){

        return "";
    }

    public String getCode(ReturnInstruction instruction){

        return "";
    }

    private String getCodeInvokeStatic(CallInstruction instruction) {
        var code = new StringBuilder();
        code.append("invokestatic ");

        //Operandos, FirstArg(classe), SecondArgs (nomemétodo), ReturnType

        var methodClass = ((Operand) instruction.getFirstArg()).getName();

        code.append(utils.getFullyQualifiedName(methodClass));
        code.append("/");
        code.append((((LiteralElement) instruction.getSecondArg()).getLiteral()).replace("\"", ""));
        code.append("(");

        for(var operand : instruction.getListOfOperands()){
            getArgumentCode(operand);
        }
        
        code.append(")");
        code.append(utils.getJasminType(instruction.getReturnType()));
        code.append("\n");

        return code.toString();
    }

    private void getArgumentCode(Element operand) {
        throw new NotImplementedException(this);
    }


}
