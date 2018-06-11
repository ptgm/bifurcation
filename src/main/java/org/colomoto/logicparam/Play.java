package org.colomoto.logicparam;

import org.colomoto.logicparam.core.BifurcationHDPath;
import org.colomoto.logicparam.core.DependencyManager;
import org.colomoto.logicparam.core.Formula;

public class Play {

	public static void main(String[] argv) {
		byte n = Byte.parseByte(argv[0]);
		byte max = Byte.parseByte(argv[1]);
		Play.goUp(n, max);
	}
	
	// TODO
	// 1. Load a model of the article
	// 2. Pick a target variable
	// 3. create a path with all possible functions
	// 4. for each function:
	// 4.1  translate the function to LogicalModel
	// 4.2  compute trap-sets of the model

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
}
