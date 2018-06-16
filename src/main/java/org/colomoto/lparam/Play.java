package org.colomoto.lparam;

import java.io.File;

import org.colomoto.biolqm.LQMScriptLauncher;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.tool.trapspaces.TrapSpace;
import org.colomoto.biolqm.tool.trapspaces.TrapSpaceList;
import org.colomoto.biolqm.tool.trapspaces.TrapSpaceTool;
import org.colomoto.lparam.core.BifurcationHDPath;
import org.colomoto.lparam.core.DependencyManager;
import org.colomoto.lparam.core.Formula;

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
			System.out.println("First argument is not a file: " + argv[0]);
			Play.usage();
		}
		String nodeID = argv[1];

		GINML2Bifurcation gin2Bifurc = new GINML2Bifurcation(fModel);
		BifurcationHDPath bifPath = gin2Bifurc.getBifurcation(nodeID);
		bifPath.computePath();

		LQMScriptLauncher lqm = new LQMScriptLauncher(null);
		TrapSpaceTool trapTool = (TrapSpaceTool) lqm.getTool("trapspace");

		for (Formula f : bifPath.getPath()) {
			try {
				System.out.println("\n" + f);
				LogicalModel m = gin2Bifurc.getModel(nodeID, f);
				TrapSpaceList list = trapTool.getResult(m);
				for (NodeInfo n : list.nodes)
					System.out.print(n + " ");
				System.out.println();
				for (TrapSpace tspace : list) {
					System.out.println(" " + tspace);
				}
			} catch (Exception e) {
				System.out.println(f + " ERROR");
			}
		}
	}

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
