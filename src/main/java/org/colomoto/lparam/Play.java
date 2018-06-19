package org.colomoto.lparam;

import java.io.File;

import org.colomoto.biolqm.ConnectivityMatrix;
import org.colomoto.biolqm.LQMLauncher;
import org.colomoto.biolqm.LQMScriptLauncher;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.io.OutputStreamProvider;
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

        LogicalModelFormat outputFormat = LQMLauncher.getFormat("ginml");
		LQMScriptLauncher lqm = new LQMScriptLauncher(null);
		TrapSpaceTool trapTool = (TrapSpaceTool) lqm.getTool("trapspace");

		for (Formula f : bifPath.getPath()) {
			try {
				System.out.println("\n" + f);
				LogicalModel m = gin2Bifurc.getModel(nodeID, f);
//				System.out.println("\nout: " + m.getLogicalFunctions()[gin2Bifurc.getIndex(nodeID)]);
				System.out.println(m.getComponents());
				Play.printMDDs(true, m.getLogicalFunctions());
				Play.printMDDs(false, m.getExtraLogicalFunctions());
	            outputFormat.export(m, new OutputStreamProvider(
	            		f.toString().replaceAll(",", "_")
	            		+ ".ginml"));

				String[] sa = { "terminal", "all", "bdd" };
				trapTool.run(m, sa);
//				TrapSpaceList list = trapTool.getResult(m);
//				for (NodeInfo n : list.nodes)
//					System.out.print(n + " ");
//				System.out.println();
//				for (TrapSpace tspace : list) {
//					System.out.println(" " + tspace);
//				}
			} catch (Exception e) {
				System.out.println("  ERROR: " + e.getMessage());
			}
		}
	}

	private static void printMDDs(boolean core, int[] iaMDDs) {
		if (core) System.out.print("Core: ");
		else System.out.print("Extr: ");
		for (int i = 0; i < iaMDDs.length; i++) {
			System.out.print(iaMDDs[i] + " " );
		}
		System.out.println();
	}
	@Deprecated
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

	@Deprecated
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
}
