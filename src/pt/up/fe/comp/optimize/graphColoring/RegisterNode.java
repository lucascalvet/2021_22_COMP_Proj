package pt.up.fe.comp.optimize.graphColoring;

import java.util.ArrayList;
import java.util.List;

public class RegisterNode {
    String name;
    List<RegisterNode> varsConnected;
    int assignedRegister;

    public RegisterNode(String name) {
        this.name = name;
        this.varsConnected = new ArrayList<>();
        this.assignedRegister = -1;
    }

    public int getRegister() {
        return assignedRegister;
    }

    public void setRegister(int new_value) {
        this.assignedRegister = new_value;
    }

    public String getVarName() {
        return name;
    }

    public List<RegisterNode> getVarsConnected(){
        return varsConnected;
    }

    public void addConnection(RegisterNode new_edge){
        varsConnected.add(new_edge);
    }

    public void remConnection(RegisterNode old_edge){
        varsConnected.remove(old_edge);
    }

    public boolean isConnected(RegisterNode dest) {
        return varsConnected.contains(dest);
    }
}
