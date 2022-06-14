package pt.up.fe.comp.optimize;

import org.specs.comp.ollir.*;

import java.util.ArrayList;
import java.util.List;

public class UsedVariablesGetter {
    Instruction instruction;
    InstructionType instType;

    List<String> usedVars;

    public UsedVariablesGetter(Instruction toAnalyse){
        this.instruction = toAnalyse;
        this.instType = this.instruction.getInstType();

        this.usedVars = new ArrayList<>();
    }

    public List<String> getUsedVariables(){

        if(instType == InstructionType.CALL){
            CallInstruction inst = (CallInstruction) instruction;

            //é estar como parâmetro da call ou ser ele sobre a qual está a ser feita a call
            Element fst_arg = inst.getFirstArg();
            Element snd_arg = inst.getSecondArg();

            if(snd_arg != null){
                if(!snd_arg.isLiteral() || !((LiteralElement) snd_arg).getLiteral().equals("<init>")){

                    //check if fst arg is not a class - because 'this' needs no reg
                    if(fst_arg.getType().getTypeOfElement() != ElementType.CLASS){
                        addToUsedIfNotContains(getElemUses(fst_arg));
                        addToUsedIfNotContains(getElemUses(fst_arg));
                    }

                    for(Element elem : inst.getListOfOperands()){
                        addToUsedIfNotContains(getElemUses(elem));
                    }
                }
            }

        } else if (instType == InstructionType.ASSIGN){
            AssignInstruction inst = (AssignInstruction) instruction;

            //se for assign é estar no rhs ou ser usado do left side
            Instruction rightHS = inst.getRhs();
            Element instDest = inst.getDest();

           if(instDest instanceof ArrayOperand){
               //add elems used by call to array
               addToUsedIfNotContains(getElemUses(instDest));
           }

           //Add used vars from rightHS
            UsedVariablesGetter usedVarsRHS = new UsedVariablesGetter(rightHS);
            addToUsedIfNotContains(usedVarsRHS.getUsedVariables());

        } else if (instType == InstructionType.RETURN){
            ReturnInstruction inst = (ReturnInstruction) instruction;

            if(inst.hasReturnValue())
                usedVars = getElemUses(inst.getOperand());

        } else if (instType == InstructionType.PUTFIELD){
            PutFieldInstruction inst = (PutFieldInstruction) instruction;

            addToUsedIfNotContains(getElemUses(inst.getThirdOperand()));
        } else if (instType == InstructionType.BRANCH) {
            CondBranchInstruction inst = (CondBranchInstruction) instruction;

            for(var elem : inst.getOperands()){
                addToUsedIfNotContains(getElemUses(elem));
            }
        } else if (instType == InstructionType.UNARYOPER){
            UnaryOpInstruction inst = (UnaryOpInstruction) instruction;
            Element operand = inst.getOperand();

            addToUsedIfNotContains(getElemUses(operand));

        } else if (instType == InstructionType.BINARYOPER){
            BinaryOpInstruction inst = (BinaryOpInstruction) instruction;
            OperationType op_type = inst.getOperation().getOpType();
            Element right_op = inst.getRightOperand();

            //if operation is a NOT -> only first element
            if(op_type == OperationType.NOTB){
                    addToUsedIfNotContains(getElemUses(right_op));
            } else {
                //else get both
                Element left_op = inst.getLeftOperand();

                addToUsedIfNotContains(getElemUses(right_op));
                addToUsedIfNotContains(getElemUses(left_op));
            }
        }

        return this.usedVars;
    }

    private void addToUsedIfNotContains(List<String> elemUses) {
        for(String val: elemUses){
            if(!this.usedVars.contains(val)){
                usedVars.add(val);
            }
        }
    }

    private List<String> getElemUses(Element elem) {
        List<String> elemUses = new ArrayList<>();

        if(!elem.isLiteral()){
            if(elem instanceof ArrayOperand){
                Element idxOp = ((ArrayOperand) elem).getIndexOperands().get(0);

                elemUses.add(((Operand) idxOp).getName());
            }
            elemUses.add(((Operand) elem).getName());
        }

        return elemUses;
    }
}
