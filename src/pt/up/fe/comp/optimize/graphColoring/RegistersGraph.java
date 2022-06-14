package pt.up.fe.comp.optimize.graphColoring;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class RegistersGraph {
    String methodName;
    HashMap<String, List<String>> varRelations;
    List<RegisterNode> vars;

    public RegistersGraph(String methodName, HashMap<String, List<String>> analysisResult) {
        this.methodName = methodName;
        this.varRelations = analysisResult;
        this.vars = new ArrayList<>();

        initializeGraph();
    }

    private void initializeGraph() {

        //Initialize nodes
        for (var name: varRelations.keySet()){
            vars.add(new RegisterNode(name));
        }

        //Initialize graph edges
        for (RegisterNode node: vars){
            for(String name: varRelations.get(node.getVarName())){
                RegisterNode hasPath = getNode(name);

                if(hasPath != null){
                    node.addConnection(hasPath);
                }
            }
        }
    }

    public List<RegisterNode> getNodes() {
        return vars;
    }

    public RegisterNode getNode(String name){
        RegisterNode ret = null;

        for(int i = 0; i < vars.size(); i++){
            RegisterNode node = vars.get(i);

            if(node.getVarName().equals(name)){
                ret = node;
                break;
            }
        }

        return ret;
    }

    @Override
    public String toString() {
        StringBuilder toPrint = new StringBuilder("varRelation = " + varRelations + "\n vars = \n");

        for(RegisterNode var: vars){
            toPrint.append("  ").append(var.getVarName()).append(" - color: ").append(var.getRegister()).append("\n");
        }

        return toPrint.toString();
    }

    public String getMethodName() {
        return this.methodName;
    }
}
