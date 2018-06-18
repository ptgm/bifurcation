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
import org.ginsim.core.graph.regulatorygraph.RegulatoryEdgeSign;
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

	// private boolean[] getRegSigns(String) {
	// List<Regu>
	// }

	public BifurcationHDPath getBifurcation(String nodeID) {
		List<RegulatoryNode> lNodes = g.getNodeOrder();
		// Get RegulatoryNode
		int nodeIdx = this.getIndex(nodeID);
		RegulatoryNode node = lNodes.get(nodeIdx);

		// Get number of regulators (since MDD may miss some non-functional regulators)
		Collection<RegulatoryMultiEdge> regs = g.getIncomingEdges(node);
		DependencyManager pdg = new DependencyManager(regs.size());

		// FIXME - get actual node's formula from model
		// for now, start from bottom formula
		return new BifurcationHDPath(pdg, pdg.createBottomFormula(), node.getMaxValue());
	}

	public LogicalModel getModel(String nodeID, Formula f) {
		List<RegulatoryNode> lNodes = g.getNodeOrder();
		// Get RegulatoryNode
		int nodeIdx = this.getIndex(nodeID);
		RegulatoryNode node = lNodes.get(nodeIdx);

		// Get number of regulators (since MDD may miss some non-functional regulators)
		Collection<RegulatoryMultiEdge> regs = g.getIncomingEdges(node);
		int[] mapping = new int[regs.size()];
		boolean[] sign = new boolean[regs.size()];
		int i = 0;
		for (RegulatoryMultiEdge e : regs) {
			String srcID = e.getSource().getId();
			mapping[i] = this.getIndex(srcID);
			// Only monotone functions
			sign[i++] = e.getSign().equals(RegulatoryEdgeSign.POSITIVE);
		}

		// System.out.print("\nRegs mapping: ");
		// for (int i = 0; i < mapping.length; i++) {
		// System.out.print(i + " ");
		// }
		// System.out.println();

		LogicalModel m = this.g.getModel().clone();
		int[] kMDDs = m.getLogicalFunctions();
		// System.out.println(nodeID + " @ " + nodeIdx);
		// System.out.println("-old: " + kMDDs[nodeIdx]);
		kMDDs[nodeIdx] = this.formula2BDD(f, mapping, sign);
		// System.out.println("-new: " + kMDDs[nodeIdx]);
		// System.out.println(m.getComponents());
		// for (int i = 0; i < kMDDs.length; i++) {
		// System.out.println(m.getComponents().get(i).getNodeID() + " - " + kMDDs[i]);
		// }
		// return new LogicalModelImpl(m.getComponents(), ddmanager, kMDDs);
		return m;
	}

	private int formula2BDD(Formula f, int[] mapping, boolean[] sign) {
		int formulaBDD = 0; // Default value = False
		for (LogicalParameter lp : f.getParams()) {
			if (lp.getState() == 0)
				continue;
			formulaBDD = MDDBaseOperators.OR.combine(this.ddmanager, formulaBDD, this.lParam2BDD(lp, mapping, sign));
			// System.out.println(" " + lp + "->" + lp.getState() + " f: " + formulaBDD);
		}
		return formulaBDD;
	}

	// mapping [index@LP] -> index@Model
	private int lParam2BDD(LogicalParameter lp, int[] mapping, boolean[] sign) {
		System.out.println("  - " + lp);
		int iBDD = 1; // True
		MDDVariable[] ddVariables = this.ddmanager.getAllVariables();
		for (int i = 0; i < lp.nVars(); i++) {
			System.out.print("  v=" + i);
			int[] children = new int[ddVariables[mapping[i]].nbval];
			// ASSUMPTION:
			// 1. Functions are monotone
			// 2. Leafs are multivalued <- lp state
			if (lp.isSetAt(i) == sign[i]) {
				// Assumes that interaction is active from regulator's [1,max]
				for (int k = 1; k < children.length; k++) {
					children[k] = lp.getState();
				}
				System.out.println("  set");
			} else {
				children[0] = lp.getState();
				System.out.println("  unset");
			}
			iBDD = MDDBaseOperators.AND.combine(ddmanager, iBDD, ddVariables[mapping[i]].getNode(children));
		}
		return iBDD;
	}
}
