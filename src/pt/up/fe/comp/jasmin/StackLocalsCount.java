package pt.up.fe.comp.jasmin;

import org.specs.comp.ollir.Descriptor;
import org.specs.comp.ollir.Element;
import org.specs.comp.ollir.Method;
import org.specs.comp.ollir.Operand;

import java.util.HashMap;

public class StackLocalsCount {

    private int stackMaxSize;
    private int stackSize;
    private int localsMaxSize;
    private int localsSize;

    public StackLocalsCount() {
        this.stackMaxSize = 0;
        this.stackSize = 0;
        this.localsMaxSize = 0;
        this.localsSize = 0;
    }

    public int getStackMaxSize() {
        return stackMaxSize;
    }

    public int getLocalsMaxSize(Method method, HashMap<String, Descriptor> scope) {
        for(Element element : method.getParams()){
            int local = scope.get(((Operand)element).getName()).getVirtualReg();
            this.incLocalSize(local);
        }
        localsMaxSize++;
        return localsMaxSize;
    }

    public void incLocalSize(int value){
        this.localsSize = value;
        if(localsSize > localsMaxSize){
            localsMaxSize = localsSize;
        }
    }

    public void incStackSize(int value, String instruction){
        this.stackSize += value;
        if(stackSize > stackMaxSize){
            stackMaxSize = stackSize;
        }
        System.out.println("Stacksize evolution " + this.stackSize + " " + instruction);
    }

    public void decStackSize(int value, String instruction){
        this.stackSize -= value;
        System.out.println("Stacksize evolution " + this.stackSize + " " + instruction);
    }


}
