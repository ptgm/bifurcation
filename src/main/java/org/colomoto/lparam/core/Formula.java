package org.colomoto.lparam.core;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * 
 * @author Pedro T. Monteiro
 * @author Wassim Abou-Jaoudé
 */
public class Formula {
	private List<LogicalParameter> lParams;

	public Formula(List<LogicalParameter> lParams) {
		this.lParams = lParams;
	}

	public Formula clone() {
		List<LogicalParameter> newParams = new ArrayList<LogicalParameter>();
		for (LogicalParameter lp : lParams) {
			newParams.add(lp.clone());
		}
		return new Formula(newParams);
	}

	public boolean equals(Object o) {
		Formula f = (Formula)o;
		// Same number of parameters
		if (this.lParams.size() != f.lParams.size())
			return false;
		for (int i = 0; i < this.lParams.size(); i++) {
			// With the same value
			if (this.lParams.get(i).getState() != f.lParams.get(i).getState())
				return false;
			// With the same bit signature
			if (!this.lParams.get(i).equals(f.lParams.get(i)))
				return false;
		}
		return true;
	}
	public List<LogicalParameter> getParams() {
		return Collections.unmodifiableList(this.lParams);
	}

	public byte getValueOf(int i) {
		return this.lParams.get(i).getState();
	}
	
	public byte getValueOf(LogicalParameter lp) {
		int pos = this.lParams.indexOf(lp);
		if (pos < 0)
			return -1;
		return this.lParams.get(pos).getState();
	}

	public boolean isIndependent(Formula f) {
		return !this.isLessEqual(f) && !this.isGreaterEqual(f) && !this.equals(f);
	}

	public boolean isLessEqual(Formula f) {
		for (int i = 0; i < this.lParams.size(); i++) {
			if (!this.lParams.get(i).isLessEqual(f.lParams.get(i)))
				return false;
		}
		return true;
	}

	public boolean isGreaterEqual(Formula f) {
		for (int i = 0; i < this.lParams.size(); i++) {
			if (!this.lParams.get(i).isGreaterEqual(f.lParams.get(i)))
				return false;
		}
		return true;
	}

	public String toString() {
		String s = "";
		for (int i = 0; i < this.lParams.size(); i++) {
			if (!s.isEmpty())
				s += ",";
			s += this.lParams.get(i) + "@" + this.lParams.get(i).getState();
		}
		return "{" + s + "}";
	}
}
