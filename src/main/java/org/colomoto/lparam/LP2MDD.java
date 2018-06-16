package org.colomoto.lparam;

import java.util.List;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.lparam.core.LogicalParameter;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.operators.MDDBaseOperators;

public class LP2MDD {
	private MDDManager ddmanager;
	private List<NodeInfo> nodeOrder;

	public LP2MDD(LogicalModel m) {
		this.ddmanager = m.getMDDManager();
		this.nodeOrder = m.getComponents();
	}

	public int getBDD(List<LogicalParameter> lParams, int[] mapping) {
		int formulaBDD = 0; // Default value = False
		for (LogicalParameter lp : lParams) {
			formulaBDD = MDDBaseOperators.OR.combine(this.ddmanager, formulaBDD, this.getBDD(lp, mapping));
		}
		return formulaBDD;
	}

	// mapping [index@LP] -> index@Model
	private int getBDD(LogicalParameter lp, int[] mapping) {
		int iBDD = lp.getState(); // Default value = Leaf value
		MDDVariable[] ddVariables = this.ddmanager.getAllVariables();
		ddVariables[0].getNode(children)
		for (int i = 0; i < lp.nVars(); i++) {
			if (lp.isSetAt(i)) {
				iBDD = MDDBaseOperators.AND.combine(ddmanager, iBDD, ddVariables[mapping[i]].getNode(0, 1));
			} else {
				iBDD = MDDBaseOperators.AND.combine(ddmanager, iBDD, ddVariables[mapping[i]].getNode(1, 0));
			}
		}
		return iBDD;
	}
}
