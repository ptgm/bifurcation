package org.colomoto.lparam;

import java.io.File;
import java.util.Collection;
import java.util.HashMap;

import org.colomoto.biolqm.LQMLauncher;
import org.colomoto.biolqm.LQMScriptLauncher;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.io.OutputStreamProvider;
import org.colomoto.biolqm.tool.fixpoints.FixpointList;
import org.colomoto.biolqm.tool.fixpoints.FixpointTool;
import org.colomoto.biolqm.tool.trapspaces.TrapSpaceTool;
import org.colomoto.lparam.core.BifurcationHDPath;
import org.colomoto.lparam.core.Formula;
import org.ginsim.common.application.GsException;
import org.ginsim.core.graph.GSGraphManager;
import org.ginsim.core.graph.regulatorygraph.RegulatoryGraph;
import org.ginsim.core.graph.regulatorygraph.RegulatoryMultiEdge;
import org.ginsim.core.graph.regulatorygraph.RegulatoryNode;

public class Launcher {

	private static void usage() {
		System.err.println("Expected arguments:");
		System.err.println(" - valid SBML model file");
		System.err.println(" - component name to study");
		System.exit(1);
	}

	public static void main(String[] argv) {
		// Argument validation -- begin
		if (argv == null || argv.length != 2) {
			System.out.println("Two arguments expected");
			Launcher.usage();
		}

		File fModel = new File(argv[0]);
		if (!fModel.isFile()) {
			System.out.println("First argument is not a file: " + argv[0]);
			Launcher.usage();
		}
		String nodeID = argv[1];
		
		RegulatoryGraph g = null;
		try {
			g = (RegulatoryGraph) GSGraphManager.getInstance().open(fModel);
		} catch (GsException e) {
			System.out.println("Could not load model: " + fModel);
			Launcher.usage();
		}

		GINML2Bifurcation gin2Bifurc = new GINML2Bifurcation(g);
		if (!gin2Bifurc.isFuncConsistent(nodeID)) {
			System.out.println("Function of " + nodeID + " does not respect the topology");
			Launcher.usage();
		}
		// Argument validation -- end


		// Compute BifurcationPath for node
		BifurcationHDPath bifPath = gin2Bifurc.getBifurcation(nodeID);
		bifPath.computePath();

//		LogicalModelFormat outputFormat = LQMLauncher.getFormat("ginml");
		LQMScriptLauncher lqm = new LQMScriptLauncher(null);
		TrapSpaceTool trapTool = (TrapSpaceTool) lqm.getTool("trapspace");
		FixpointTool fixpointTool = (FixpointTool) lqm.getTool("fixpoints");

		for (Formula f : bifPath.getPath()) {
			try {
				System.out.println("Formula: " + f);
				LogicalModel m = gin2Bifurc.getModel(nodeID, f);
				// System.out.println("\nout: " +
				// m.getLogicalFunctions()[gin2Bifurc.getIndex(nodeID)]);
//				System.out.println(m.getComponents());
//				Play.printMDDs(true, m.getLogicalFunctions());
//				Play.printMDDs(false, m.getExtraLogicalFunctions());
//				outputFormat.export(m, new OutputStreamProvider(f.toString().replaceAll(",", "_") + ".ginml"));

				// String[] sa = { "terminal", "all", "bdd" };
				// trapTool.run(m, sa);
				FixpointList list = fixpointTool.getResult(m, "");
				for (byte[] fp : list) {
					System.out.print(" SS: ");
					for (int i = 0; i < fp.length; i++) {
						System.out.print(fp[i]);
					}
					System.out.println();
				}
				
				// TrapSpaceList list = trapTool.getResult(m);
				// for (NodeInfo n : list.nodes)
				// System.out.print(n + " ");
				// System.out.println();
				// for (TrapSpace tspace : list) {
				// System.out.println(" " + tspace);
				// }
			} catch (Exception e) {
				System.out.println("  ERROR: " + e.getMessage());
			}
		}
	}
}
