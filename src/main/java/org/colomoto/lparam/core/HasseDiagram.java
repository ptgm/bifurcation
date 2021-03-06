package org.colomoto.lparam.core;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.colomoto.lparam.Utils;

/**
 * Class to generate immediate neighboring functions, given a reference function
 * {@see com.colomoto.lparam.core.Formula} and a dependency graph on the logical
 * parameters {@see com.colomoto.lparam.core.DependencyManager}.
 * 
 * @author Pedro T. Monteiro
 * @author Wassim Abou-Jaoudé
 */
public class HasseDiagram {

	public static Tuple getParent(DependencyManager pdg, Formula f, byte maxTarget) {
		return HasseDiagram.getNeighbour(pdg, f, maxTarget, true, true);
	}

	public static Tuple getChild(DependencyManager pdg, Formula f, byte maxTarget) {
		return HasseDiagram.getNeighbour(pdg, f, maxTarget, false, true);
	}

	public static Tuple getNeighbour(DependencyManager pdg, Formula f, byte maxTarget, boolean parent,
			boolean fullyAsync) {
		List<Integer> lPos = new ArrayList<Integer>();
		List<LogicalParameter> fLPs = f.getParams();
		param: for (int i = 0; i < fLPs.size(); i++) {
			LogicalParameter fLP = fLPs.get(i);
			if (parent && fLP.getState() >= maxTarget || !parent && fLP.getState() <= 0)
				continue; // cannot increase/decrease more
			Set<LogicalParameter> sNeighbours = parent ? pdg.getParentDeps(fLP) : pdg.getChildrenDeps(fLP);
			for (LogicalParameter lpNeigh : sNeighbours) {
				// If at least one Parent/Child has same value -> cannot change
				if (f.getValueOf(lpNeigh) == fLP.getState()) {
					continue param;
				}
			}
			lPos.add(i); // <- LPs than can change
		}

		if (fullyAsync) {
			// If equal LogicalParameters are possible
			List<List<Integer>> lSubsets = Utils.getSubsets(lPos);
			Collections.shuffle(lSubsets);
			for (List<Integer> elems : lSubsets) {
				if (elems.isEmpty() || !pdg.isValidLPSet(elems)) {
					continue;
				}
				// Increase if equal LogicalParameters can all increase -- begin
				Formula fNeighbor = f.clone();
				Set<LogicalParameter> sChangeParams = new HashSet<LogicalParameter>();
				for (Integer i : elems) {
					// if can increase() : v <= MaxTarget && v <= LPsDeps
					LogicalParameter lp = fNeighbor.getParams().get(i);
					if (parent) {
						lp.increase();
					} else {
						lp.decrease();
					}
					sChangeParams.add(lp);
				}
				return new Tuple(fNeighbor, sChangeParams);
			}
		} else {
			// If equal LogicalParameters are not considered
			Collections.shuffle(lPos); // randomly choose a parameter
			Formula fClone = f.clone();
			LogicalParameter lp = fClone.getParams().get(lPos.get(0));
			if (parent) {
				lp.increase();
			} else {
				lp.decrease();
			}
			Set<LogicalParameter> sChangeParams = new HashSet<LogicalParameter>();
			sChangeParams.add(lp);
			return new Tuple(fClone, sChangeParams);
		}
		return null;
	}
}
