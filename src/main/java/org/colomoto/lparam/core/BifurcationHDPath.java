package org.colomoto.lparam.core;

import java.util.ArrayList;
import java.util.Deque;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

/**
 * A bifurcation path is represented by a sequence of functions
 * {@see com.colomoto.lparam.core.Formula} satisfying the restrictions imposed
 * by the dependency graph {@see com.colomoto.lparam.core.DependencyManager}.
 * 
 * @author Pedro T. Monteiro
 * @author Wassim Abou-Jaoudé
 */
public class BifurcationHDPath {
	private byte maxTarget;
	private DependencyManager depGraph;
	private Deque<Formula> path;
	private boolean debug;

	public BifurcationHDPath(DependencyManager depGraph, Formula f, byte maxTarget) {
		this.depGraph = depGraph;
		this.path = new LinkedList<Formula>();
		this.path.add(f);
		this.maxTarget = maxTarget;
		this.debug = false;
	}

	/**
	 * Given a path (possibly only with a single function), it augments it by
	 * picking its most general function, computing its parents, (randomly) choosing
	 * one of them, and updating the current path restrictions. The relation between
	 * functions in the generated path must respect coherence with the Logical
	 * Parameter Dependency Graph.
	 */
	public boolean increase() {
		if (debug)
			System.out.println("\n------------------ Increasing another step -----------------------");
		// Pick the most general formula (last on the queue)
		Formula f = this.path.getLast();
		if (debug)
			System.out.println("Current Formula: " + f);
		// Get possible parent functions respecting DepGraph restrictions
		Tuple tNeighbour = HasseDiagram.getParent(this.depGraph, f, this.maxTarget);
		if (tNeighbour == null) {
			if (debug)
				System.out.println("------- END");
			// cannot increase anymore
			return false;
		}
		if (debug)
			System.out.println(" - Parent: " + tNeighbour.getFormula());
		this.path.addLast(tNeighbour.getFormula());

		this.depGraph.setDepEq(tNeighbour.getChangeLPs()); // 1st for LPs considered Equal
		// System.out.println(" . ChangeLPs: " + t.getChangeLPs());
		for (LogicalParameter lpChange : tNeighbour.getChangeLPs()) {
			for (LogicalParameter lp : tNeighbour.getFormula().getParams()) {
				// All that have the previous value of lpChange have the LT restriction
				if ((lp.getState() + 1) == lpChange.getState()) {
					// lpChange increased before lp -> restriction to keep in the future
					// NOTE: Independence is tested inside
					this.depGraph.setDepLT(lp, lpChange); // 2nd precedence between LPs
				}
			}
		}
		return true;
	}

	public boolean decrease() {
		if (debug)
			System.out.println("\n------------------ Decreasing another step -----------------------");
		// Pick the most specific formula (first on the queue)
		Formula f = this.path.getFirst();
		if (debug)
			System.out.println("Current Formula: " + f);
		// Get possible children functions respecting DepGraph restrictions
		Tuple tNeighbour = HasseDiagram.getChild(this.depGraph, f, this.maxTarget);
		if (tNeighbour == null) {
			if (debug)
				System.out.println("------- END");
			// cannot increase anymore
			return false;
		}
		if (debug)
			System.out.println(" - Child: " + tNeighbour.getFormula());
		this.path.addFirst(tNeighbour.getFormula());

		this.depGraph.setDepEq(tNeighbour.getChangeLPs()); // 1st for LPs considered Equal
		// System.out.println(" . ChangeLPs: " + t.getChangeLPs());
		for (LogicalParameter lpChange : tNeighbour.getChangeLPs()) {
			for (LogicalParameter lp : tNeighbour.getFormula().getParams()) {
				// All that have the previous value of lpChange have the LT restriction
				if ((lp.getState() - 1) == lpChange.getState()) {
					// lpChange decreased before lp -> restriction to keep in the future
					// NOTE: Independence is tested inside
					this.depGraph.setDepLT(lpChange, lp); // 2nd precedence between LPs
				}
			}
		}
		return true;
	}

	public void computePath() {
		// compute everything bellow
		while (this.decrease()) {
		}
		// compute everything above
		while (this.increase()) {
		}
	}

	public Formula getFirst() {
		return this.path.getFirst();
	}

	public Formula getLast() {
		return this.path.getLast();
	}

	public List<Formula> getPath() {
		return new ArrayList<Formula>(this.path);
	}

	public String toString() {
		String s = "";
		Iterator<Formula> it = this.path.descendingIterator();
		while (it.hasNext()) {
			s += "  " + it.next() + "\n";
		}
		return s + "Order: " + this.depGraph.getAllClone();
	}
}
