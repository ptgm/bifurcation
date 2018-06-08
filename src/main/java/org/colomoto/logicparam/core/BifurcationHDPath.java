package org.colomoto.logicparam.core;

import java.util.ArrayList;
import java.util.Deque;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Random;
import java.util.Set;

/**
 * @author Pedro T. Monteiro
 * @author Wassim Abou-Jaoudé
 */
public class BifurcationHDPath {
	private byte maxTarget;
	private DependencyManager depGraph;
	private Deque<Formula> path;

	public BifurcationHDPath(DependencyManager depGraph, Formula f, byte maxTarget) {
		this.depGraph = depGraph;
		this.path = new LinkedList<Formula>();
		this.path.add(f);
		this.maxTarget = maxTarget;
	}

	/**
	 * Given a path (possibly only with a single function), it augments it by
	 * picking its most general function, computing its parents, (randomly) choosing
	 * one of them, and updating the current path restrictions. The relation between
	 * functions in the generated path must respect coherence with the Logical
	 * Parameter Dependency Graph.
	 */
	public boolean increase() {
		// Pick the most general formula (last on the queue)
		Formula f = this.path.getLast();
		// Get possible parent functions respecting DepGraph restrictions
		Set<Tuple> sParents = HasseDiagram.getParents(this.depGraph, f, this.maxTarget);
		if (sParents.isEmpty()) {
			// System.out.println("------- END");
			// cannot increase anymore
			return false;
		} else if (sParents.size() == 1) {
			// If there's only one parent function, no choice (additional restriction) is
			// made
			Formula fnew = sParents.iterator().next().getFormula();
			// System.out.println("------ Single Successor");
			// System.out.println(" - " + fnew);
			this.path.addLast(fnew);
			return true;
		}
		// otherwise, need to update DepGraph with choice restrictions
		// choosing one at random
		int randPos = (new Random()).nextInt(sParents.size());
		Tuple t = (new ArrayList<Tuple>(sParents)).get(randPos);
		// Updating path with new formula
		this.path.addLast(t.getFormula());
		// System.out.println("------ Multip Successor");
		// System.out.println(" - " + t.getFormula());
		// Update Parameter Dependency Graph restrictions
		this.depGraph.setDepEq(t.getChangeLPs()); // 1st for LPs considered Equal
		// System.out.println(" . ChangeLPs: " + t.getChangeLPs());
		for (LogicalParameter lpChange : t.getChangeLPs()) {
			for (LogicalParameter lp : t.getFormula().getParams()) {
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
		// Pick the most specific formula (first on the queue)
		Formula f = this.path.getFirst();
		// Get possible children functions respecting DepGraph restrictions
		Set<Tuple> sChildren = HasseDiagram.getChildren(this.depGraph, f, this.maxTarget);
		if (sChildren.isEmpty()) {
//			 System.out.println("------- END");
			// cannot increase anymore
			return false;
		} else if (sChildren.size() == 1) {
			// If there's only one parent function, no choice (additional restriction) is
			// made
			Formula fnew = sChildren.iterator().next().getFormula();
//			 System.out.println("------ Single Successor");
//			 System.out.println(" - " + fnew);
			this.path.addFirst(fnew);
			return true;
		}
		// otherwise, need to update DepGraph with choice restrictions
		// choosing one at random
		int randPos = (new Random()).nextInt(sChildren.size());
		Tuple t = (new ArrayList<Tuple>(sChildren)).get(randPos);
		// Updating path with new formula
		this.path.addFirst(t.getFormula());
//		 System.out.println("------ Multip Successor");
//		 System.out.println(" - " + t.getFormula());
		// Update Parameter Dependency Graph restrictions
		this.depGraph.setDepEq(t.getChangeLPs()); // 1st for LPs considered Equal
//		 System.out.println(" . ChangeLPs: " + t.getChangeLPs());
		for (LogicalParameter lpChange : t.getChangeLPs()) {
			for (LogicalParameter lp : t.getFormula().getParams()) {
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
	
	public Formula getLast() {
		return this.path.getLast();
	}

	public Formula getFirst() {
		return this.path.getFirst();
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
