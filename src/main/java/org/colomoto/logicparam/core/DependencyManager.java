package org.colomoto.logicparam.core;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

// FIXME This manager should have positions rather than LPs. Currently is a mess to get their value.
// Is the current value of an LP being taken from f or from the manager...?
public class DependencyManager {
	private byte nvars;
	private Map<LogicalParameter, Set<LogicalParameter>> mParents;
	private Map<LogicalParameter, Set<LogicalParameter>> mChildren;
	private Map<LogicalParameter, Set<LogicalParameter>> mEquals;
	private List<LogicalParameter> sAll;

	// Private constructor to be reused by Public Constructor & clone() method
	private DependencyManager() {
		this.nvars = 0;
		this.sAll = new ArrayList<LogicalParameter>();
		this.mEquals = new HashMap<LogicalParameter, Set<LogicalParameter>>();
		this.mParents = new HashMap<LogicalParameter, Set<LogicalParameter>>();
		this.mChildren = new HashMap<LogicalParameter, Set<LogicalParameter>>();
	}

	public DependencyManager(byte nvars) {
		this();
		this.nvars = nvars;
		// Start with Bottom LP
		LogicalParameter lpBottom = new LogicalParameter(nvars);
		this.sAll.add(lpBottom);
		this.mChildren.put(lpBottom, new HashSet<LogicalParameter>());
		// Build dependency graph Bottom-Up
		this.buildGraphBottomUp(lpBottom);
		// All LPs ordered
		this.sAll.sort(new Comparator<LogicalParameter>() {
			@Override
			public int compare(LogicalParameter o1, LogicalParameter o2) {
				return o1.toString().compareTo(o2.toString());
			}
		});
		// Initialize equals set
		for (LogicalParameter lp : this.sAll) {
			this.mEquals.put(lp, new HashSet<LogicalParameter>());
		}
	}

	public DependencyManager clone() {
		// Create a new clean instance
		DependencyManager dm = new DependencyManager();
		dm.nvars = this.nvars;
		// clone the current LogicalParameters
		for (LogicalParameter lp : this.sAll) {
			LogicalParameter lpClone = lp.clone();
			dm.sAll.add(lpClone);
			dm.mChildren.put(lpClone, new HashSet<LogicalParameter>());
			dm.mParents.put(lpClone, new HashSet<LogicalParameter>());
			dm.mEquals.put(lpClone, new HashSet<LogicalParameter>());
		}
		// Clone graph - Parents Set
		for (LogicalParameter lpKey : dm.mParents.keySet()) {
			Set<LogicalParameter> mCloneParents = dm.mParents.get(lpKey);
			for (LogicalParameter lpOrig : this.mParents.get(lpKey)) {
				// Get the proper reference to the cloned object
				LogicalParameter lpClone = dm.sAll.get(dm.sAll.indexOf(lpOrig));
				mCloneParents.add(lpClone);
			}
			dm.mParents.put(lpKey, mCloneParents);
		}
		// Clone graph - Children Set
		for (LogicalParameter lpKey : dm.mChildren.keySet()) {
			Set<LogicalParameter> mCloneChildren = dm.mChildren.get(lpKey);
			for (LogicalParameter lpOrig : this.mChildren.get(lpKey)) {
				// Get the proper reference to the cloned object
				LogicalParameter lpClone = dm.sAll.get(dm.sAll.indexOf(lpOrig));
				mCloneChildren.add(lpClone);
			}
			dm.mChildren.put(lpKey, mCloneChildren);
		}
		// Clone graph - Equal Set
		for (LogicalParameter lpKey : dm.mEquals.keySet()) {
			Set<LogicalParameter> mCloneEquals = dm.mEquals.get(lpKey);
			for (LogicalParameter lpOrig : this.mChildren.get(lpKey)) {
				// Get the proper reference to the cloned object
				LogicalParameter lpClone = dm.sAll.get(dm.sAll.indexOf(lpOrig));
				mCloneEquals.add(lpClone);
			}
			dm.mEquals.put(lpKey, mCloneEquals);
		}
		return dm;
	}

	public List<LogicalParameter> getAllClone() {
		List<LogicalParameter> sLPs = new ArrayList<LogicalParameter>();
		for (LogicalParameter lp : this.sAll) {
			sLPs.add(lp.clone());
		}
		return sLPs;
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
			if (!this.mEquals.containsKey(parent))
				this.mEquals.put(parent, new HashSet<LogicalParameter>());
			if (!this.mChildren.containsKey(parent))
				this.mChildren.put(parent, new HashSet<LogicalParameter>());
			if (!this.mParents.containsKey(lp))
				this.mParents.put(lp, new HashSet<LogicalParameter>());
			this.addDepLT(lp, parent);
			// If not explored yet
			if (!this.mParents.containsKey(parent)) {
				this.buildGraphBottomUp(parent);
			}
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
			for (LogicalParameter lpeq : this.mEquals.get(this.sAll.get(i))) {
				if (!elems.contains(this.sAll.indexOf(lpeq)))
					return false;
			}
		}
		return true;
	}

	private void addDepLT(LogicalParameter lpSon, LogicalParameter lpParent) {
		this.mChildren.get(lpParent).add(lpSon);
		this.mParents.get(lpSon).add(lpParent);
	}

	private void addDepEQ(LogicalParameter lpa, LogicalParameter lpb) {
		this.mEquals.get(lpa).add(lpb);
		this.mEquals.get(lpb).add(lpa);
	}

	public boolean isDepLT(LogicalParameter lp1, LogicalParameter lp2) {
		return this.mChildren.containsKey(lp1) && this.mChildren.get(lp1).contains(lp2);
	}

	// We assume that added dependencies do not violate the existing ones
	// i.e. no verification is performed, only inference of new dependencies
	public void setDepLT(LogicalParameter lp1, LogicalParameter lp2) {
		if (!lp1.isIndependent(lp2) || this.isDepLT(lp1, lp2))
			return; // already comparable || already LT
		// System.out.println(".setDepLT: " + lp1 + " < " + lp2);
		this.addDepLT(lp1, lp2);
		// Infer new dependencies Up & Down
		this.setDepLTIncr(lp1, lp2);
		this.setDepLTDecr(lp1, lp2);
		this.setDepLTIsolate(lp1, lp2);
	}

	private void setDepLTIsolate(LogicalParameter lp1, LogicalParameter lp2) {
		if (lp1.cardinality() <= 1)
			return;
		for (int i = 0; i < this.nvars; i++) {
			if (!lp1.isSetAt(i))
				continue;
			LogicalParameter lpa = lp1.clone();
			lpa.clear();
			lpa.setBit(i, true);
			this.setDepLT(lpa, lp2);
		}
	}

	// FIXME this sum case is missing
	// k1 + k3 < k2 + k4
	// k1 + k2 < k3 + k4
	// = 2*k1 + k/2 + k/3 < k/2 + k/3 + 2*k4
	private void setDepLTIncr(LogicalParameter lp1, LogicalParameter lp2) {
		for (int i = 0; i < this.nvars; i++) {
			if (!lp1.isSetAt(i) && !lp2.isSetAt(i)) {
				LogicalParameter lpa = lp1.clone();
				lpa.setBit(i, true);
				LogicalParameter lpb = lp2.clone();
				lpb.setBit(i, true);
				if (!lpa.isIndependent(lpb) || this.isDepLT(lpa, lpb))
					continue; // already comparable || already LT
				// System.out.println(" .setDepLT+: " + lpa + " < " + lpb);
				this.addDepLT(lpa, lpb);
				this.setDepLTIncr(lpa, lpb);
			}
		}
	}

	private void setDepLTDecr(LogicalParameter lp1, LogicalParameter lp2) {
		for (int i = 0; i < this.nvars; i++) {
			if (lp1.isSetAt(i) && lp2.isSetAt(i)) {
				LogicalParameter lpa = lp1.clone();
				lpa.setBit(i, false);
				LogicalParameter lpb = lp2.clone();
				lpb.setBit(i, false);
				if (!lpa.isIndependent(lpb) || this.isDepLT(lpa, lpb))
					continue; // already comparable || already LT
				// System.out.println(" .setDepLT-: " + lpa + " < " + lpb);
				this.addDepLT(lpa, lpb);
				this.setDepLTDecr(lpa, lpb);
			}
		}
	}

	public boolean isDepEQ(LogicalParameter lp1, LogicalParameter lp2) {
		return this.mEquals.get(lp1).contains(lp2);
	}

	// We assume that added dependencies do not violate the existing ones
	// i.e. no verification is performed, only inference of new dependencies
	public void setDepEq(LogicalParameter lp1, LogicalParameter lp2) {
		if (!lp1.isIndependent(lp2) || this.isDepEQ(lp1, lp2))
			return; // already comparable || already depEquals
		// System.out.println(".setDepEq: " + lp1 + " == " + lp2);
		this.addDepEQ(lp1, lp2);
		// Infer new dependencies Up & Down
		this.setDepEQIncr(lp1, lp2);
		this.setDepEQDecr(lp1, lp2);
	}

	// Don't have to propagate k1 < k2 when adding k2==k3
	private void setDepEQIncr(LogicalParameter lp1, LogicalParameter lp2) {
		for (int i = 0; i < this.nvars; i++) {
			if (!lp1.isSetAt(i) && !lp2.isSetAt(i)) {
				LogicalParameter lpa = lp1.clone();
				lpa.setBit(i, true);
				LogicalParameter lpb = lp2.clone();
				lpb.setBit(i, true);
				if (!lpa.isIndependent(lpb) || this.isDepEQ(lpa, lpb))
					continue; // already comparable || already EQ
				// System.out.println(" .setDepEQ+: " + lpa + " == " + lpb);
				this.addDepEQ(lpa, lpb);
				this.setDepEQIncr(lpa, lpb);
			}
		}
	}

	private void setDepEQDecr(LogicalParameter lp1, LogicalParameter lp2) {
		for (int i = 0; i < this.nvars; i++) {
			if (lp1.isSetAt(i) && lp2.isSetAt(i)) {
				LogicalParameter lpa = lp1.clone();
				lpa.setBit(i, false);
				LogicalParameter lpb = lp2.clone();
				lpb.setBit(i, false);
				if (!lpa.isIndependent(lpb) || this.isDepEQ(lpa, lpb))
					continue; // already comparable || already EQ
				// System.out.println(" .setDepEQ-: " + lpa + " == " + lpb);
				this.addDepEQ(lpa, lpb);
				this.setDepEQDecr(lpa, lpb);
			}
		}
	}

	public void setDepEq(Set<LogicalParameter> sLPs) { // for the same value
		for (LogicalParameter lp1 : sLPs) {
			for (LogicalParameter lp2 : sLPs) {
				if (lp1.getState() == lp2.getState()) { // independence is tested inside setDepEq()
					this.setDepEq(lp1, lp2);
				}
			}
		}
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

	public Formula createTopFormula(byte max) {
		List<LogicalParameter> lLPs = this.getAllClone();
		for (LogicalParameter lp : lLPs) {
			byte curr = lp.getState();
			while (curr < max) {
				curr = lp.increase();
			}
		}
		return new Formula(lLPs);
	}

	public String toString() {
		String s = "";
		for (LogicalParameter lp : this.sAll) {
			s += lp + " c:" + this.mChildren.get(lp) + " \tp: " + this.mParents.get(lp) + " \t=: "
					+ this.mEquals.get(lp) + "\n";
		}
		return s + this.sAll + "\n";
	}

}
