package pt.up.fe.comp.jasmin;

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
        return 99;
        //return stackMaxSize;
    }

    public int getLocalsMaxSize() {
        return 98;
        //return localsMaxSize;
    }

    public void incLocalSize(int value){
        this.localsSize = value;
        if(localsSize > localsMaxSize){
            localsMaxSize = localsSize;
        }
    }

    public void incStackSize(int value){
        this.stackSize += value;
        if(stackSize > stackMaxSize){
            stackMaxSize = stackSize;
        }
    }

    public void decStackSize(int value){
        this.stackSize -= value;
    }


}
