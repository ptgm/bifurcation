package org.colomoto.logicparam.core;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Powerset of all LogicalParameter as well as their corresponding relations. A
 * BifurcationPath is constructed satisfying these relations and the choice of a
 * path w.r.t. may add new relations (restrictions).
 * 
 * @author Pedro T. Monteiro
 * @author Wassim Abou-Jaoudé
 */
public class ParamDeps {
	private byte nvars;
	private List<LogicalParameter> sAll;
	// TODO change this to BitSets
	private Map<LogicalParameter, Set<LogicalParameter>> mEqual;
	private Map<LogicalParameter, Set<LogicalParameter>> mParents;
	private Map<LogicalParameter, Set<LogicalParameter>> mChildren;

	// Private constructor to be reused by Public Constructor & clone() method
	private ParamDeps() {
		this.sAll = new ArrayList<LogicalParameter>();
		this.mEqual = new HashMap<LogicalParameter, Set<LogicalParameter>>();
		this.mParents = new HashMap<LogicalParameter, Set<LogicalParameter>>();
		this.mChildren = new HashMap<LogicalParameter, Set<LogicalParameter>>();
	}

	public ParamDeps(byte nvars) {
		this();
		this.nvars = nvars;
		// Build dependency graph bottom-up
		LogicalParameter lpBottom = new LogicalParameter(nvars);
		this.sAll.add(lpBottom);
		this.mChildren.put(lpBottom, new HashSet<LogicalParameter>());
		this.buildGraphBottomUp(lpBottom);
		this.sAll.sort(new Comparator<LogicalParameter>() {
			@Override
			public int compare(LogicalParameter o1, LogicalParameter o2) {
				return o1.toString().compareTo(o2.toString());
			}
		});
		for (LogicalParameter lp : this.sAll) {
			this.mEqual.put(lp, new HashSet<LogicalParameter>());
		}
	}

	public String toString() {
		String s = "";
		for (LogicalParameter lp : this.sAll) {
			s += lp + " c:" + this.mChildren.get(lp) + " \tp: " + this.mParents.get(lp) + " \t=: " + this.mEqual.get(lp)
					+ "\n";
		}
		return s;
	}

	public ParamDeps clone() {// FIXME for mEqual Set
		// Create a new clean instance
		ParamDeps pdg = new ParamDeps();
		pdg.nvars = this.nvars;
		// clone the current LogicalParameters
		for (LogicalParameter lp : this.sAll) {
			LogicalParameter lpClone = lp.clone();
			pdg.sAll.add(lpClone);
			pdg.mChildren.put(lpClone, new HashSet<LogicalParameter>());
			pdg.mParents.put(lpClone, new HashSet<LogicalParameter>());
		}
		// Clone graph - Parents Set
		for (LogicalParameter lpKey : pdg.mParents.keySet()) {
			Set<LogicalParameter> mCloneParents = pdg.mParents.get(lpKey);
			for (LogicalParameter lpOrig : this.mParents.get(lpKey)) {
				// Get the proper reference to the cloned object
				LogicalParameter lpClone = pdg.sAll.get(pdg.sAll.indexOf(lpOrig));
				mCloneParents.add(lpClone);
			}
			pdg.mParents.put(lpKey, mCloneParents);
		}
		// Clone graph - Children Set
		for (LogicalParameter lpKey : pdg.mChildren.keySet()) {
			Set<LogicalParameter> mCloneChildren = pdg.mChildren.get(lpKey);
			for (LogicalParameter lpOrig : this.mChildren.get(lpKey)) {
				// Get the proper reference to the cloned object
				LogicalParameter lpClone = pdg.sAll.get(pdg.sAll.indexOf(lpOrig));
				mCloneChildren.add(lpClone);
			}
			pdg.mChildren.put(lpKey, mCloneChildren);
		}
		return pdg;
	}

	public List<LogicalParameter> getAllClone() {
		List<LogicalParameter> sLPs = new ArrayList<LogicalParameter>();
		for (LogicalParameter lp : this.sAll) {
			sLPs.add(lp.clone());
		}
		return sLPs;
	}

	public Formula createBottomFormula() {
		List<LogicalParameter> lLPs = this.getAllClone();
		for (LogicalParameter lp : lLPs) {
			byte curr = lp.getState();
			while (curr > 0) {
				curr = lp.decrease();
			}
		}
		return new Formula(lLPs);
	}

	public Formula createTopFormula(int maxTarget) {
		List<LogicalParameter> lLPs = this.getAllClone();
		for (LogicalParameter lp : lLPs) {
			byte curr = lp.getState();
			while (curr < maxTarget) {
				curr = lp.increase();
			}
		}
		return new Formula(lLPs);
	}

	public void addDepVars(int var1, int var2) { // FIXME for mEqual Set
		Set<LogicalParameter> sLPs = new HashSet<LogicalParameter>();
		for (LogicalParameter lp : this.sAll) {
			if (lp.isSetAt(var2) && !lp.isSetAt(var1)) {
				sLPs.add(lp);
			}
		}
		// Remove sLPs from sAll / mParents / mChildren
		this.sAll.removeAll(sLPs);
		for (LogicalParameter lp : sLPs) {
			this.mChildren.remove(lp);
			this.mParents.remove(lp);
		}
		// Remove sLPs as Parents/Children of valid parameters
		for (LogicalParameter lp : this.mChildren.keySet()) {
			this.mChildren.get(lp).removeAll(sLPs);
			this.mParents.get(lp).removeAll(sLPs);
		}
	}

	public Set<LogicalParameter> getParentDeps(LogicalParameter lpChild) {
		return Collections.unmodifiableSet(this.mParents.get(lpChild));
	}

	public Set<LogicalParameter> getChildrenDeps(LogicalParameter lpParent) {
		return Collections.unmodifiableSet(this.mChildren.get(lpParent));
	}

	public boolean isValidLPSet(List<Integer> elems) {
		for (int i : elems) {
			LogicalParameter lpi = this.sAll.get(i);
			for (LogicalParameter lpeq : this.mEqual.get(lpi)) {
				if (!elems.contains(this.sAll.indexOf(lpeq)))
					return false;
			}
		}
		return true;
	}

	public boolean getLPDependEq(LogicalParameter lp1, LogicalParameter lp2) {
		return this.mEqual.get(lp1).contains(lp2);
	}

	public void setLPDependLt(LogicalParameter lpSon, LogicalParameter lpParent) {
		// Parents (of lp) relation
		Set<LogicalParameter> parents = this.mParents.containsKey(lpSon) ? this.mParents.get(lpSon)
				: new HashSet<LogicalParameter>();
		parents.add(lpParent);
		this.mParents.put(lpSon, parents);
		// Children (of parent) relation
		Set<LogicalParameter> children = this.mChildren.containsKey(lpParent) ? this.mChildren.get(lpParent)
				: new HashSet<LogicalParameter>();
		children.add(lpSon);
		this.mChildren.put(lpParent, children);
		
		// Perform some simplifications
	}

	public void setLPDependEq(Set<LogicalParameter> sLPs) {
		for (LogicalParameter lp1 : sLPs) {
			for (LogicalParameter lp2 : sLPs) {
				if (lp1.equals(lp2)) continue;
				this.setLPDependEq(lp1, lp2);
				// Infer dependencies
				
			}
		}
	}

	public boolean setLPDependEq(LogicalParameter lp1, LogicalParameter lp2) {
		// If lp1 comparable with lp2 -> cannot be the same
		if (!lp1.isLessEqual(lp2) || !lp1.isGreaterEqual(lp2) || lp1.getState() != lp2.getState())
			return false;
		// Already equal
		if (this.mEqual.containsKey(lp1) && this.mEqual.get(lp1).contains(lp2))
			return true;
		// lp2 parents <-> lp1 parents
		for (LogicalParameter lp2Parent : this.mParents.get(lp2)) {
			this.setLPDependLt(lp1, lp2Parent);
		}
		for (LogicalParameter lp1Parent : this.mParents.get(lp1)) {
			this.setLPDependLt(lp2, lp1Parent);
		}
		// lp2 children <-> lp1 children
		for (LogicalParameter lp2Child : this.mChildren.get(lp2)) {
			this.setLPDependLt(lp2Child, lp1);
		}
		for (LogicalParameter lp1Child : this.mChildren.get(lp1)) {
			this.setLPDependLt(lp1Child, lp2);
		}
		this.mEqual.get(lp1).add(lp2);
		this.mEqual.get(lp2).add(lp1);
		return true;
	}

	private Set<LogicalParameter> computeParents(LogicalParameter lp) {
		Set<LogicalParameter> lpFathers = new HashSet<LogicalParameter>();
		for (int i = 0; i < this.nvars; i++) {
			if (!lp.isSetAt(i)) {
				LogicalParameter lpFather = lp.clone();
				lpFather.setBit(i, true);
				lpFathers.add(lpFather);
			}
		}
		return lpFathers;
	}

	private void buildGraphBottomUp(LogicalParameter lp) {
		if (lp.cardinality() == this.nvars) {
			// reached top element
			if (!this.mParents.containsKey(lp)) {
				this.mParents.put(lp, new HashSet<LogicalParameter>());
			}
			return;
		}
		for (LogicalParameter parent : this.computeParents(lp)) {
			// All
			if (!this.sAll.contains(parent))
				this.sAll.add(parent);
			this.setLPDependLt(lp, parent);
			if (!this.mParents.containsKey(parent)) {
				this.buildGraphBottomUp(parent);
			}
		}
	}
}
