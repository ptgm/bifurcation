package org.colomoto.lparam;

import java.io.File;
import java.io.IOException;

import org.colomoto.biolqm.LQMServiceManager;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.LogicalModelImpl;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.lparam.core.BifurcationHDPath;
import org.colomoto.lparam.core.DependencyManager;
import org.colomoto.lparam.core.Formula;
import org.colomoto.mddlib.MDDManager;

public class Play {

	private static void usage() {
		System.err.println("Expected arguments:");
		System.err.println(" - valid SBML model file");
		System.err.println(" - component name to study");
		System.exit(1);
	}

	public static void main(String[] argv) {

		// Verify arguments
		if (argv == null || argv.length != 2) {
			System.out.println("Two arguments expected");
			Play.usage();
		}
		File fModel = new File(argv[0]);
		if (!fModel.isFile()) {
			System.out.println("First argument is not a file");
			Play.usage();
		}

		// Load model
		LogicalModel m = null;
		try {
			m = LQMServiceManager.getFormat("sbml").importFile(fModel);
		} catch (IOException e) {
			System.out.println("Error loading model " + fModel.getName());
			Play.usage();
		}

		// Verify component
		NodeInfo node = m.getComponent(argv[1]);
		if (node == null) {
			System.out.println("Invalid component name");
			Play.usage();
		}
		int index = m.getComponents().indexOf(node);
		MDDManager ddmanager = m.getMDDManager();
		LP2MDD lp2mdd = new LP2MDD(m);
		int[] mapping = { 0, 1 };
		// check regulators
		BifurcationHDPath bifPath = Play.getPathUp((byte) 2, node.getMax());
		for (Formula f : bifPath.getPath()) {
			int[] kMDDs = m.getLogicalFunctions().clone();
			kMDDs[index] = lp2mdd.getBDD(f.getParams(), mapping);
			LogicalModel model = new LogicalModelImpl(m.getComponents(), ddmanager, kMDDs);

		}
	}

	// private static LogicalModel loadModel(File fName, NodeInfo node) throws
	// IOException {
	// RegulatoryGraph g = (RegulatoryGraph)
	// GSGraphManager.getInstance().open(fName);
	// List<RegulatoryNode> lNodes = g.getNodeOrder();
	// int i = 0;
	// for (; i < lNodes.size(); i++) {
	// if (lNodes.get(i).getId().equals(node.getNodeID()))
	// break;
	// }
	// Collection<RegulatoryMultiEdge> cEdges = g.getIncomingEdges(lNodes.get(i));
	// System.out.println(cEdges.);
	// RegulatoryGraph g = GSServiceManager.getService(name)
	// LogicalModelFormat format = LQMServiceManager.getFormat("sbml");
	// return format.importFile(fName);
	// }

	// private static Formula mdd2Formula(LogicalModel m, NodeInfo node) {
	// int index = m.getComponents().indexOf(node);
	// boolean core = index >= 0;
	// int[] kMDDs;
	//
	// if (core) {
	// kMDDs = m.getLogicalFunctions();
	// } else {
	// index = m.getExtraComponents().indexOf(node);
	// kMDDs = m.getExtraLogicalFunctions();
	// }
	// PathSearcher search = new PathSearcher(m.getMDDManager(), 1, node.getMax());
	// int[] path = search.getPath();
	// search.setNode(kMDDs[index]);
	//
	// }

	// TODO
	// 1. Load a model of the article
	// 2. Pick a target variable
	// 3. create a path with all possible functions
	// 4. for each function:
	// 4.1 translate the function to LogicalModel
	// 4.2 compute trap-sets of the model

	private static void goDown(byte n, byte max) {
		while (true) {
			DependencyManager pdg = new DependencyManager(n);
			Formula fBottom = pdg.createBottomFormula();
			Formula fTop = pdg.createTopFormula(max);

			System.out.println(" FIRST: " + fTop);
			BifurcationHDPath bifPath = new BifurcationHDPath(pdg, fTop, max);

			while (bifPath.decrease()) {
			}
			System.out.println("BOTTOM: " + fBottom);
			System.out.println("  LAST: " + bifPath.getFirst());
			if (!bifPath.getFirst().equals(fBottom)) {
				System.out.println(".... a sair!");
				System.out.println(pdg);
				break;
			}
		}
	}

	private static void goUp(byte n, byte max) {
		while (true) {
			DependencyManager pdg = new DependencyManager(n);
			Formula fBottom = pdg.createBottomFormula();
			Formula fTop = pdg.createTopFormula(max);

			System.out.println("FIRST: " + fBottom);
			BifurcationHDPath bifPath = new BifurcationHDPath(pdg, fBottom, max);

			while (bifPath.increase()) {
			}
			System.out.println("  TOP: " + fTop);
			System.out.println(" LAST: " + bifPath.getLast());
			if (!bifPath.getLast().equals(fTop)) {
				System.out.println(".... a sair!");
				System.out.println(pdg);
				break;
			}
		}
	}

	private static BifurcationHDPath getPathUp(byte n, byte max) {
		BifurcationHDPath bifPath = null;
		DependencyManager pdg = new DependencyManager(n);
		Formula fBottom = pdg.createBottomFormula();
		bifPath = new BifurcationHDPath(pdg, fBottom, max);

		while (bifPath.increase()) {
		}
		return bifPath;
	}
}
