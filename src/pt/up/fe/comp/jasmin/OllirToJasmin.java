package pt.up.fe.comp.jasmin;

import org.specs.comp.ollir.*;

import java.util.HashMap;
import java.util.List;
import java.util.stream.Collectors;

public class OllirToJasmin {

    private final ClassUnit classUnit;
    private final ConversionUtils utils;
    private final ConversionInstructions inst;
    public static int index = 0;

    public OllirToJasmin(ClassUnit classUnit) {
        this.classUnit = classUnit;
        this.utils = new ConversionUtils(this.classUnit);
        this.inst = new ConversionInstructions(classUnit);
    }


    public String getCode(){
        var code = new StringBuilder();
        var qualifiedNameSuper = utils.getFullyQualifiedName(classUnit.getSuperClass());

        //Main class name
        code.append(".class public ").append(classUnit.getClassName()).append("\n");
        //Parent class name
        code.append(".super ").append(qualifiedNameSuper).append("\n\n");


        //Fields
        for(var field: classUnit.getFields()){
            code.append(getCode(field));
        }

        //Constructor
        code.append(buildConstructor(qualifiedNameSuper));

        //Methods
        for(var method : classUnit.getMethods()){
            //Constructor is already done in the above lines
            if(!method.isConstructMethod()) code.append(getCode(method));
        }

        var result = code.toString();
        System.out.println("\nJasmin Code: \n" + result);
        return result;
    }

    private String buildConstructor(String qualifiedNameSuper){
        var result = new StringBuilder();
        result.append(".method public <init>()V\n");
        result.append("aload_0\n");
        result.append("invokenonvirtual ").append(qualifiedNameSuper).append("/<init>()V\n");
        result.append("return\n");
        result.append(".end method");

        result.append("\n\n");
        return result.toString();
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
        method.buildVarTable();
        HashMap<String, Descriptor> scope = method.getVarTable();
        this.inst.updateScope(scope);
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
        code.append(".limit stack ").append(getStackLimit(method)).append("\n");
        code.append(".limit locals ").append(getLocalsLimit(method)).append("\n");

        boolean hasReturn = false;
        index = 0;
        for(var instruction : method.getInstructions()){
            List<String> labels = method.getLabels(instruction);
            for (String label: labels)
                code.append(label).append(":\n");
            if(instruction.getInstType()==InstructionType.RETURN) hasReturn = true;
            code.append(inst.getCode(instruction));
            index++;
        }

        if(!hasReturn)
            code.append("return\n"); //se for return Void

        //Fecho do método
        code.append(".end method\n\n");

        var result = code.toString();
        return result;
    }

    private int getStackLimit(Method method) {
        return 99;
    }

    private int getLocalsLimit(Method method) {
        // Elements in the varTable + this.
        var varTable = method.getVarTable();
        int limit = varTable.size();

        if(!method.getMethodName().equals("main")){
            limit++;
        }

        return limit;
    }

}
