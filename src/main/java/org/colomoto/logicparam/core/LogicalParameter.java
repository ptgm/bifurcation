package org.colomoto.logicparam.core;

import java.util.BitSet;

/**
 * 
 * @author Pedro T. Monteiro
 * @author Wassim Abou-Jaoudé
 */
public class LogicalParameter {
	private int nvars;
	private byte state; // value
	private BitSet signature;

	public LogicalParameter(int nvars) {
		this.nvars = nvars;
		this.state = 0;
		this.signature = new BitSet(this.nvars);
	}

	public LogicalParameter clone() {
		LogicalParameter lp = new LogicalParameter(this.nvars);
		lp.signature = (BitSet) this.signature.clone();
		lp.state = this.state;
		return lp;
	}

	public int hashCode() {
		return this.signature.hashCode();
	}

	public boolean equals(Object o) {
		// test if it is same entity (not if has same valuation)
		LogicalParameter lp = (LogicalParameter) o;
		if (this.nvars != lp.nvars)
			return false;

		for (int i = 0; i < this.nvars; i++) {
			if (this.signature.get(i) != lp.signature.get(i)) {
				return false;
			}
		}
		return true;
	}

	// BitSet operations -- begin
	public boolean intersects(LogicalParameter lp) {
		return this.signature.intersects(lp.signature);
	}

	public BitSet xor(LogicalParameter lp) {
		BitSet thisBS = (BitSet) this.signature.clone();
		thisBS.xor(lp.signature);
		return thisBS;
	}

	public LogicalParameter andNot(LogicalParameter lp) {
		LogicalParameter lpNew = lp.clone();
		lpNew.signature.andNot(this.signature);
		return lpNew;
	}
	public void clear() {
		this.signature.clear();
	}
	// BitSet operations -- end

	public byte getState() {
		return this.state;
	}

	public boolean isLessEqual(LogicalParameter lp) {
		for (int i = 0; i < this.nvars; i++) {
			if (lp.signature.get(i) && !this.signature.get(i)) {
				return false;
			}
		}
		return true;
	}

	public boolean isGreaterEqual(LogicalParameter lp) {
		for (int i = 0; i < this.nvars; i++) {
			if (this.signature.get(i) && !lp.signature.get(i)) {
				return false;
			}
		}
		return true;
	}

	public boolean isIndependent(LogicalParameter lp) {
		return !this.equals(lp) && !this.isGreaterEqual(lp)
				&& !this.isLessEqual(lp);
	}

	public int cardinality() {
		return this.signature.cardinality();
	}

	public boolean isSetAt(int bitIndex) {
		return this.signature.get(bitIndex);
	}

	public void setBit(int bitIndex, boolean v) {
		this.signature.set(bitIndex, v);
	}

	public byte decrease() {
		return --this.state;
	}

	public byte increase() {
		return ++this.state;
	}

	public String toString() {
		String s = "";
		for (int i = 0; i < this.nvars; i++) {
			s += this.signature.get(i) ? "1" : "0";
		}
		return s;
	}
}
