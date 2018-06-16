package org.colomoto.lparam.core;

import java.util.Set;

public class Tuple {
	// Aside from Formula, contains information about all LPs (e.g. non-changing ones)
	private Formula f;
	// Generated Formula by changing the value of sChangeLPs set of LPs
	private Set<LogicalParameter> sChangeLPs;

	public Tuple(Formula f, Set<LogicalParameter> sChangeLPs) {
		this.f = f;
		this.sChangeLPs = sChangeLPs;
	}

	public Formula getFormula() {
		return this.f;
	}

	public Set<LogicalParameter> getChangeLPs() {
		return this.sChangeLPs;
	}
}
