package pt.up.fe.comp.optimize;

import org.specs.comp.ollir.Descriptor;
import org.specs.comp.ollir.Method;
import pt.up.fe.comp.jmm.ollir.OllirResult;
import pt.up.fe.comp.optimize.graphColoring.GraphColoring;
import pt.up.fe.comp.optimize.graphColoring.RegisterNode;
import pt.up.fe.comp.optimize.graphColoring.RegistersGraph;

import java.util.HashMap;
import java.util.List;

public class RegisterAllocator {
    OllirResult ollirCode;
    int maxNumReg;

    public RegisterAllocator(OllirResult ollir, int numMaxRegisters) {
        this.ollirCode = ollir;
        this.maxNumReg = numMaxRegisters;
    }

//    public int getMinNumReg(){
//        //TODO: Calculate the minimum number of registers needed -> is this mandatory?
//        //if maxNumReg < minNumReg -> send message and do optimization with MinNumReg
//        int minNumReg = 0;
//
//        for (Method method: ollirCode.getOllirClass().getMethods()){
//            //create graph for variables of method
//            //Compute number of colors needed for graph coloring problem
//            DataflowAnalyser dataflowAnalyser = new DataflowAnalyser(method);
//            HashMap<String, List<String>> dataflowAnalysisResult = dataflowAnalyser.getInterferenceResult();
//
//            RegistersGraph methodGraph = new RegistersGraph(dataflowAnalysisResult);
//            GraphColoring graphColored = new GraphColoring(numRegisters, methodGraph);
//            int colorsNeeded = graphColored.getNumColors();
//
//            if (minNumReg < colorsNeeded){
//                minNumReg = colorsNeeded;
//            }
//        }
//
//
//        return minNumReg;
//    }

    public void allocateRegisters(int numRegisters) {
        for (Method method: ollirCode.getOllirClass().getMethods()){
            methodAllocateRegisters(method, numRegisters);
        }
    }

    private void methodAllocateRegisters(Method method, int numRegisters) {
        int numRegsParams = method.getParams().size();

        DataflowAnalyser dataflowAnalyser = new DataflowAnalyser(method);
        HashMap<String, List<String>> dataflowAnalysisResult = dataflowAnalyser.getInterferenceResult();

        RegistersGraph methodGraph = new RegistersGraph(dataflowAnalysisResult);

        GraphColoring graphColored = new GraphColoring(numRegisters, methodGraph);
        System.out.println(methodGraph.toString());

        HashMap<String, Descriptor> scope = method.getVarTable();

        for (RegisterNode node: methodGraph.getNodes()){
            int new_register = numRegsParams + node.getRegister();
            scope.get(node.getVarName()).setVirtualReg(new_register);
        }
    }
}
