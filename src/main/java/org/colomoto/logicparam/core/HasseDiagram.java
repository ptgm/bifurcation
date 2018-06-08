package org.colomoto.logicparam.core;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.colomoto.logicparam.Utils;

/**
 * 
 * @author Pedro T. Monteiro
 * @author Wassim Abou-Jaoudé
 */
public class HasseDiagram {

	public static Set<Tuple> getParents(DependencyManager pdg, Formula f, byte maxTarget) {
		return HasseDiagram.getNeighbours(pdg, f, maxTarget, true, true);
	}

	public static Set<Tuple> getChildren(DependencyManager pdg, Formula f, byte maxTarget) {
		return HasseDiagram.getNeighbours(pdg, f, maxTarget, false, true);
	}

	public static Set<Tuple> getNeighbours(DependencyManager pdg, Formula f, byte maxTarget, boolean parent,
			boolean fullyAsync) {
		List<Integer> lPos = new ArrayList<Integer>();
		List<LogicalParameter> fLPs = f.getParams();
		param: for (int i = 0; i < fLPs.size(); i++) {
			LogicalParameter fLP = fLPs.get(i);
			if (parent && fLP.getState() >= maxTarget || !parent && fLP.getState() <= 0)
				continue; // cannot increase/decrease more
			Set<LogicalParameter> sNeighbours = parent?pdg.getParentDeps(fLP):
				pdg.getChildrenDeps(fLP);
			for (LogicalParameter lParent : sNeighbours) {
				// If at least one Parent/Child has same value -> cannot change
				if (f.getValueOf(lParent) == fLP.getState()) {
					continue param;
				}
			}
			lPos.add(i);
		}
		// System.out.println("===== Lpos: " + lPos); // <- LPs than can change
		Set<Tuple> sParentChild = new HashSet<Tuple>();
		if (fullyAsync) {
			// equal LogicalParameters are possible
			for (List<Integer> elems : Utils.getSubsets(lPos)) {
				// System.out.println("===== elems: " + elems);
				if (elems.isEmpty()) {
					// System.out.println(" empty");
					continue;
				}
				// Increase if equal LogicalParameters can all increase -- begin
				if (!pdg.isValidLPSet(elems)) {
					// System.out.println(" invalid");
					continue;
				}
				Formula fClone = f.clone();
				Set<LogicalParameter> sChangeParams = new HashSet<LogicalParameter>();
				for (Integer i : elems) {
					// if can increase() : v <= MaxTarget && v <= LPsDeps
					LogicalParameter lp = fClone.getParams().get(i);
					if (parent) {
						lp.increase();
					} else {
						lp.decrease();
					}
					sChangeParams.add(lp);
				}
				sParentChild.add(new Tuple(fClone, sChangeParams));
				// System.out.println("---------c " + fClone + " | " + sIncParams);
			}
		} else {
			// equal LogicalParameters are impossible
			for (Integer i : lPos) {
				Formula fClone = f.clone();
				LogicalParameter lp = fClone.getParams().get(i);
				if (parent) {
					lp.increase();
				} else {
					lp.decrease();
				}
				Set<LogicalParameter> sChangeParams = new HashSet<LogicalParameter>();
				sChangeParams.add(lp);
				sParentChild.add(new Tuple(fClone, sChangeParams));
			}
		}
		return sParentChild;
	}
}
