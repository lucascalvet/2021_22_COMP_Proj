package pt.up.fe.comp.optimize.graphColoring;

import java.sql.Ref;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class GraphColoring {
    RegistersGraph methodGraph;
    int rValue;
    List<RegisterNode> visited;

    public GraphColoring(int numRegisters, RegistersGraph graph) {
        this.methodGraph = graph;
        this.rValue = numRegisters;
        this.visited = new ArrayList<>();

        run();
    }

    private void run() {
        List<RegisterNode> nodes = methodGraph.getNodes();

        for(RegisterNode node : nodes){
            if(node.getRegister() == -1){
                //not colored yet
                int newColor = availableColor(node);

                node.setRegister(newColor);

                for(RegisterNode otherNode: nodes){
                    if(otherNode.getRegister() == -1 && !node.isConnected(otherNode)){
                        otherNode.setRegister(newColor);
                    }
                }
            }
        }
    }

    private int availableColor(RegisterNode node) {
        List<Integer> colorsInUse = new ArrayList<>();
        int colorToUse = -1;

        for (var node_connected : node.getVarsConnected()){
            int color = node_connected.getRegister();
            if(color != -1){
                colorsInUse.add(color);
            }
        }

        //Check what color available for newNode
        for(int r = 1; r < rValue; r++){
            if(!colorsInUse.contains(r)){
                colorToUse = r;
                break;
            }
        }

        if(colorToUse == -1){
            if(colorsInUse.isEmpty()){
                colorToUse = 1;
            } else {
                colorToUse = (Collections.max(colorsInUse)) + 1;
                System.out.println("Not enough Registers defined by -r (" + rValue + ")! - using new max (" + colorToUse + ")");
            }

        }

        return colorToUse;
    }

}
