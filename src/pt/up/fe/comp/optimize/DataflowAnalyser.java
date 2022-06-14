package pt.up.fe.comp.optimize;

import org.specs.comp.ollir.*;

import java.util.*;

public class DataflowAnalyser {
    Method method;
    int numInstructions;

    List<List<String>> in;
    List<List<String>> out;
    HashMap<Integer, List<String>> use;
    List<List<String>> def;
    HashSet<String> methodVariables;

    List<List<Integer>> succIdxs;

    HashMap<String, List<Integer>> liveness;
    HashMap<String, List<String>> var_interference;

    public DataflowAnalyser(Method toAnalyse) {
        this.method = toAnalyse;
        method.buildCFG(); //Build Control Flow Graph
        this.numInstructions = method.getInstructions().size();
        in = new ArrayList<>();
        out = new ArrayList<>();
        def = new ArrayList<>();
        use = new HashMap<>();
        succIdxs = new ArrayList<>();

        //Initialize Lists
        for(int i = 0; i < numInstructions; i++){
            in.add(new ArrayList<>());
            out.add(new ArrayList<>());
            def.add(new ArrayList<>());
            succIdxs.add(new ArrayList<>());
        }

        this.methodVariables = new HashSet<>();
        this.liveness = new HashMap<>();
        this.var_interference = new HashMap<>();

        run();
    }

    private void run() {
        Node firstSuccessor = method.getBeginNode().getSucc1();
        setupAnalysis(firstSuccessor);

        dataflowAlg();
    }

    private void setupAnalysis(Node succ) {

        if(succ == null ||
            succ.getNodeType() == NodeType.END ||
                !(succIdxs.get(succ.getId() - 1)).isEmpty()){
            return;
        }

        saveDefVariable(succ.getId() - 1);
        saveVariableUse(succ.getId() - 1);
        saveSuccessors(succ);

        setupAnalysis(succ.getSucc1());
        setupAnalysis(succ.getSucc2());

    }

    private void saveSuccessors(Node curr_node) {
        List<Integer> successors = new ArrayList<>();
        int currIdx = curr_node.getId() - 1;

        Node succ1 = curr_node.getSucc1();
        Node succ2 = curr_node.getSucc2();

        if(succ1 != null) successors.add(succ1.getId()-1);
        if(succ2 != null) successors.add(succ2.getId()-1);

        succIdxs.set(currIdx, successors);
    }

    private void saveVariableUse(int instIdx) {
        Instruction inst = method.getInstr(instIdx);
        UsedVariablesGetter usedVarsGetter = new UsedVariablesGetter(inst);

        List<String> toAdd = usedVarsGetter.getUsedVariables();

        if(!toAdd.isEmpty()){
          use.put(instIdx, toAdd);
        }
    }

    private void saveDefVariable(int instIdx) {
        if(method.getInstr(instIdx).getInstType() == InstructionType.ASSIGN){
            //Its a Definition
            AssignInstruction inst = (AssignInstruction) method.getInstr(instIdx);
            Element instDest = inst.getDest();

            if(!(instDest instanceof ArrayOperand)){
                List<String> toAdd = new ArrayList<>();
                toAdd.add( ((Operand) instDest).getName());

                def.add(instIdx, toAdd);
                String varName = ((Operand) instDest).getName();
                methodVariables.add(varName);
            } else {
                def.add(instIdx, new ArrayList<>());
            }
        } else {
            def.add(instIdx, new ArrayList<>());
        }
    }



    public HashMap<String, List<String>> getInterferenceResult() {
        return this.var_interference;
    }

    private void dataflowAlg() {
        List<List<String>> last_in, last_out;
        boolean equalIteration = false;

        while(!equalIteration){
            last_in = list2DClone(in);
            last_out = list2DClone(out);

            //backward problem, so start in the end
            for (int n = numInstructions - 1; n >= 0; n--){
                List<String> newOut = getNewOut(n);
                List<String> newIn = getNewIn(n);

                out.set(n, remMethodParams(newOut));
                in.set(n, remMethodParams(newIn));
            }

            if(list2DEquals(in, last_in) || list2DEquals(out, last_out)){
                equalIteration = true;
            }
        }

        computeLivenessOfVars();
    }

    private List<String> remMethodParams(List<String> toClean) {
        List<String> cleaned = new ArrayList<>(toClean);

        for (Element param: method.getParams()){
            String pName = ((Operand) param).getName();
            cleaned.remove(pName);
        }

        return cleaned;
    }

    private List<String> getNewOut(int idx) {
        HashSet<String> newOut = new HashSet<>(); //for no repetition

        //out[n] = union of the in of all (succ[n])
        for(var idxSucc : succIdxs.get(idx)){
            if(idxSucc == -1) continue; //no successors

            if(in.get(idxSucc) == null){
                in.set(idxSucc, new ArrayList<>());
            }

            newOut.addAll(in.get(idxSucc));
        }

        return new ArrayList<>(newOut);
    }

    private List<String> getNewIn(int idx) {
        HashSet<String> newIn = new HashSet<>(); //for no repetition

        //in[n] = use[n] union (out[n] - def[n])

        //(out[n] - def[n])
        HashSet<String> outMinusDef = new HashSet<>(out.get(idx));
        outMinusDef.removeAll(def.get(idx));

        //use[n] union outMinusDef

        if(use.containsKey(idx))
            newIn.addAll(use.get(idx));

        newIn.addAll(outMinusDef);

        return new ArrayList<>(newIn);
    }

    private void computeLivenessOfVars() {
        // variable is live on an edge if there
        // is a forward path from that edge to a
        // use that does not go through any def of the same variable

        for (String variable: methodVariables){
            List<Integer> varLife = new ArrayList<>();
            int idxInstLastIn = getLastVarIn(variable);
            int idxInstDef = getIdxDef(variable);

            if(idxInstLastIn == -1 && idxInstDef != -1){
                //alive until the end of method
                varLife.add(idxInstDef);
                varLife.add(numInstructions);
            } else {
                //alive until idxLastIn
                varLife.add(idxInstDef);
                varLife.add(idxInstLastIn);
            }

            liveness.put(variable, varLife);
        }

        computeVarInterference();
    }

    private int getIdxDef(String variable) {
        for(int i = 0; i < def.size(); i++){
            List<String> defOfInst = def.get(i);

            if(defOfInst.contains(variable)) return i;
        }

        return 0;
    }

    private int getLastVarIn(String variable) {
        for(int j = in.size() - 1; j >= 0; j--){
            List<String> inOfInst = in.get(j);

            if(inOfInst.contains(variable)) return j;
        }

        return 0;
    }

    private void computeVarInterference() {
        //see which variables are alive at the same time
        for (String varName : methodVariables){
            List<String> conflicts = new ArrayList<>();

            if(liveness.containsKey(varName)) {
                for (String otherVar : liveness.keySet()) {
                    if (!otherVar.equals(varName)) {
                        if(checkConflict(liveness.get(varName), liveness.get(otherVar))){
                            conflicts.add(otherVar);
                        }
                    }
                }
            }

            var_interference.put(varName, conflicts);
        }
    }

    private boolean checkConflict(List<Integer> lifeVar1, List<Integer> lifeVar2) {
        int beginLifeVar1 = lifeVar1.get(0);
        int endLifeVar1 = lifeVar1.get(1);
        int beginLifeVar2 = lifeVar2.get(0);
        int endLifeVar2 = lifeVar2.get(1);

        return  (beginLifeVar1 < beginLifeVar2 && beginLifeVar2 < endLifeVar1) ||
                (beginLifeVar2 < beginLifeVar1 && beginLifeVar1 < endLifeVar2);
    }

    //Deep copy for list
    private List<List<String>> list2DClone(List<List<String>> list2D) {
        List<List<String>> clone = new ArrayList<>();

        for(List<String> values : list2D){
            List<String> clonedLine = new ArrayList<>();

            clonedLine = List.copyOf(values);

            clone.add(clonedLine);
        }


        return clone;
    }


    private boolean list2DEquals(List<List<String>> list1, List<List<String>> list2) {

        if(list1.size() == list2.size()) {

            for (int line = 0; line < list1.size(); line++){
                List<String> lineLst1 = list1.get(line);
                List<String> lineLst2 = list2.get(line);

                if(lineLst1.size() == lineLst2.size()){
                    for (int val = 0; val < lineLst1.size(); val++){
                        if(!lineLst1.get(val).equals(lineLst2.get(val))){
                            return false;
                        }
                    }
                } else {
                    return false;
                }
            }

            return true;
        }

        return false;
    }
}
