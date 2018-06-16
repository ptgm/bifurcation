package org.colomoto.lparam;

import java.io.File;
import java.util.Collection;
import java.util.List;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.LogicalModelImpl;
import org.colomoto.lparam.core.BifurcationHDPath;
import org.colomoto.lparam.core.DependencyManager;
import org.colomoto.lparam.core.Formula;
import org.colomoto.lparam.core.LogicalParameter;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.operators.MDDBaseOperators;
import org.ginsim.core.graph.GSGraphManager;
import org.ginsim.core.graph.regulatorygraph.RegulatoryGraph;
import org.ginsim.core.graph.regulatorygraph.RegulatoryMultiEdge;
import org.ginsim.core.graph.regulatorygraph.RegulatoryNode;

public class GINML2Bifurcation {
	private RegulatoryGraph g;
	private MDDManager ddmanager;

	public GINML2Bifurcation(File fName) {
		try {
			this.g = (RegulatoryGraph) GSGraphManager.getInstance().open(fName);
			this.ddmanager = this.g.getMDDFactory();
		} catch (Exception e) {
			System.err.println(e.getMessage());
			this.g = null;
		}
	}

	public boolean hasModel() {
		return this.g != null;
	}

	private int getIndex(String nodeID) {
		List<RegulatoryNode> lNodes = g.getNodeOrder();
		for (int i = 0; i < lNodes.size(); i++) {
			if (lNodes.get(i).getId().equals(nodeID))
				return i;
		}
		return -1;
	}

	private int[] getMapping(String nodeID) {
		List<RegulatoryNode> lNodes = g.getNodeOrder();
		// Get RegulatoryNode
		int nodeIdx = this.getIndex(nodeID);
		RegulatoryNode node = lNodes.get(nodeIdx);

		// Get number of regulators (since MDD may miss some non-functional regulators)
		Collection<RegulatoryMultiEdge> regs = g.getIncomingEdges(node);
		int[] mapping = new int[regs.size()];
		int i = 0;
		for (RegulatoryMultiEdge e : regs) {
			String srcID = e.getSource().getId();
			mapping[i++] = this.getIndex(srcID);
		}
		return mapping;
	}

	public BifurcationHDPath getBifurcation(String nodeID) {
		List<RegulatoryNode> lNodes = g.getNodeOrder();
		// Get RegulatoryNode
		int nodeIdx = this.getIndex(nodeID);
		RegulatoryNode node = lNodes.get(nodeIdx);

		// Get number of regulators (since MDD may miss some non-functional regulators)
		Collection<RegulatoryMultiEdge> regs = g.getIncomingEdges(node);
		DependencyManager pdg = new DependencyManager(regs.size());

		// FIXME - get formula from model
		return new BifurcationHDPath(pdg, pdg.createBottomFormula(), node.getMaxValue());
	}

	public LogicalModel getModel(String nodeID, Formula f) {
		int[] mapping = this.getMapping(nodeID);
//		System.out.print("\nRegs mapping: ");
//		for (int i = 0; i < mapping.length; i++) {
//			System.out.print(i + " ");
//		}
//		System.out.println();

		LogicalModel m = this.g.getModel();
		int[] kMDDs = m.getLogicalFunctions();
		int nodeIdx = this.getIndex(nodeID);
//		System.out.println("-old: " + kMDDs[nodeIdx]);
		kMDDs[nodeIdx] = this.formula2BDD(f, mapping);
//		System.out.println("-new: " + kMDDs[nodeIdx]);
//		System.out.println(m.getComponents());
		return new LogicalModelImpl(m.getComponents(), ddmanager, kMDDs);
	}

	private int formula2BDD(Formula f, int[] mapping) {
		int formulaBDD = 0; // Default value = False
		for (LogicalParameter lp : f.getParams()) {
			formulaBDD = MDDBaseOperators.OR.combine(this.ddmanager, formulaBDD, this.lParam2BDD(lp, mapping));
//			System.out.println("  " + lp + "->" + lp.getState() + " f: " + formulaBDD);
		}
		return formulaBDD;
	}

	// mapping [index@LP] -> index@Model
	private int lParam2BDD(LogicalParameter lp, int[] mapping) {
		int iBDD = lp.getState(); // Default value = Leaf value
		MDDVariable[] ddVariables = this.ddmanager.getAllVariables();
		// ddVariables[0].getNode(children);
		for (int i = 0; i < lp.nVars(); i++) {
			int[] children = new int[ddVariables[mapping[i]].nbval];
			if (lp.isSetAt(i)) {
				for (int k = 1; k < children.length; k++) {
					children[k] = 1;
				}
			} else {
				children[0] = 1;
			}
			iBDD = MDDBaseOperators.AND.combine(ddmanager, iBDD, ddVariables[mapping[i]].getNode(children));
		}
		return iBDD;
	}
}
