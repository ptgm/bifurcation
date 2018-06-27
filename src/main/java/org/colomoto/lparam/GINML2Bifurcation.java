package org.colomoto.lparam;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.lparam.core.BifurcationHDPath;
import org.colomoto.lparam.core.DependencyManager;
import org.colomoto.lparam.core.Formula;
import org.colomoto.lparam.core.LogicalParameter;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.PathSearcher;
import org.colomoto.mddlib.operators.MDDBaseOperators;
import org.ginsim.core.graph.GSGraphManager;
import org.ginsim.core.graph.regulatorygraph.RegulatoryEdgeSign;
import org.ginsim.core.graph.regulatorygraph.RegulatoryGraph;
import org.ginsim.core.graph.regulatorygraph.RegulatoryMultiEdge;
import org.ginsim.core.graph.regulatorygraph.RegulatoryNode;

public class GINML2Bifurcation {
	private Map<RegulatoryNode, Collection<RegulatoryMultiEdge>> regs;
	private LogicalModel m;

	public GINML2Bifurcation(File fName) {
		try {
			RegulatoryGraph g = (RegulatoryGraph) GSGraphManager.getInstance().open(fName);
			this.m = g.getModel();
			this.regs = new HashMap<RegulatoryNode, Collection<RegulatoryMultiEdge>>();
			for (RegulatoryNode node : g.getNodeOrder()) {
				this.regs.put(node, g.getIncomingEdges(node));
			}
		} catch (Exception e) {
			System.err.println(e.getMessage());
			this.m = null;
		}
	}

	public boolean hasModel() {
		return this.m != null;
	}

	// FIXME it does not consider extra components
	private int getIndex(String nodeID) {
		List<NodeInfo> lNodes = this.m.getComponents();
		for (int i = 0; i < lNodes.size(); i++) {
			if (lNodes.get(i).getNodeID().equals(nodeID))
				return i;
		}
		return -1;
	}

	private RegulatoryNode getNode(String nodeID) {
		for (RegulatoryNode n : this.regs.keySet()) {
			if (n.getId().equals(nodeID))
				return n;
		}
		return null;
	}

	public BifurcationHDPath getBifurcation(String nodeID) {
		RegulatoryNode node = this.getNode(nodeID);
		// Get number of regulators (since MDD may miss some non-functional regulators)
		DependencyManager pdg = new DependencyManager(this.regs.get(node).size());

		// FIXME - get actual node's formula from model
		// for now, start from bottom formula
		return new BifurcationHDPath(pdg, pdg.createBottomFormula(), node.getMaxValue());
	}

	public LogicalModel getModel(String nodeID, Formula f) {
		// Get RegulatoryNode
		RegulatoryNode node = this.getNode(nodeID);

		// Get true number of regulators & their signs
		// (since MDD may miss some non-functional regulators)
		Collection<RegulatoryMultiEdge> regs = this.regs.get(node);
		int[] mapping = new int[regs.size()];
		boolean[] sign = new boolean[regs.size()];
		int i = 0;
		for (RegulatoryMultiEdge e : regs) {
			String srcID = e.getSource().getId();
			mapping[i] = this.getIndex(srcID);
			// ASSUMPTION: Only monotone functions
			sign[i++] = e.getSign().equals(RegulatoryEdgeSign.POSITIVE);
		}

		int nodeIdx = this.getIndex(nodeID);
		int[] kMDDs = this.m.getLogicalFunctions();
		int oldValue = kMDDs[nodeIdx];
//		System.out.println("-old MDD: " + oldValue);
		kMDDs[nodeIdx] = this.formula2BDD(f, mapping, sign);
//		System.out.println("-new MDD: " + kMDDs[nodeIdx]);
//		Formula fNew = this.mdd2Formula(kMDDs[nodeIdx], mapping, sign);

//		System.out.println("Formula: " + f + "\t" + (f.equals(fNew)));
		this.m.getMDDManager().free(oldValue);
		return this.m;
	}

	private int formula2BDD(Formula f, int[] mapping, boolean[] sign) {
		int formulaBDD = 0; // Default value = False
		for (LogicalParameter lp : f.getParams()) {
//			System.out.println(lp);
			byte[] signature = new byte[this.m.getMDDManager().getAllVariables().length];
			for (int i = 0; i < signature.length; i++) {
				signature[i] = -1;
			}
			for (int i = 0; i < lp.nVars(); i++) {
//				System.out.println(" - " + i + "/" + lp.nVars() + ": " + lp.isSetAt(i));
				signature[mapping[i]] = (byte) (lp.isSetAt(i) == sign[i] ? 1 : 0);
			}
			formulaBDD = MDDBaseOperators.OR.combine(this.m.getMDDManager(), formulaBDD,
					this.m.getMDDManager().nodeFromState(signature, lp.getState()));
		}
		return formulaBDD;
	}

	private Formula mdd2Formula(int mdd, int[] mapping, boolean[] sign) {
		Set<LogicalParameter> sLPs = new HashSet<LogicalParameter>();
		PathSearcher psearcher = new PathSearcher(this.m.getMDDManager(), 1, 2);
		psearcher.setNode(mdd);
		int[] iaSSPath = psearcher.getPath();
		for (@SuppressWarnings("unused")
		int v : psearcher) {
			LogicalParameter lp = new LogicalParameter(mapping.length);
			for (int i = 0; i < iaSSPath.length; i++) {
				if (iaSSPath[i] < 0) {
					// -1: ignore effect of variable
				} else {
					// reverse mapping
					for (int j = 0; j < mapping.length; j++) {
						if (mapping[j] == i) {
							// If influence == its sign
							lp.setBit(i, (iaSSPath[i] > 0) == sign[j]);
							break;
						}
					}
				}
			}
			lp.setState((byte) v);
			sLPs.add(lp);
		}
		// Add potential missing LPs
		sLPs.addAll((new DependencyManager(mapping.length)).getAllClone());
		List<LogicalParameter> lLPs = new ArrayList<LogicalParameter>(sLPs);
		// Sort them before creating the formula
		Collections.sort(lLPs);
		return new Formula(lLPs);
	}
}
