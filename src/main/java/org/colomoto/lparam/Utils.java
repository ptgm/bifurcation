package org.colomoto.lparam;

import java.util.ArrayList;
import java.util.List;

public class Utils {

	// Adapted from
	// https://www.geeksforgeeks.org/finding-all-subsets-of-a-given-set-in-java/
	public static List<List<Integer>> getSubsets(List<Integer> set) {
		List<List<Integer>> lRes = new ArrayList<List<Integer>>();
		int n = set.size();

		for (int i = 0; i < (1 << n); i++) {
			List<Integer> lTmp = new ArrayList<Integer>();
			// Print current subset
			for (int j = 0; j < n; j++) {
				// (1<<j) is a number with jth bit 1
				// so when we 'and' them with the
				// subset number we get which numbers
				// are present in the subset and which
				// are not
				if ((i & (1 << j)) > 0)
					lTmp.add(set.get(j));
			}
			lRes.add(lTmp);
		}
		return lRes;
	}
}
