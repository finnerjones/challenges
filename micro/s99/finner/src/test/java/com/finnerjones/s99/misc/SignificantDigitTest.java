package com.finnerjones.s99.misc;

import static org.junit.Assert.*;
import org.junit.Test;

public class SignificantDigitTest {

	@Test
	public void _0000003ShouldBe7() {
		assertEquals(7, findSigDig(0.0000003), 0.0d);
	}
	
	
	private int findSigDig(double n) {
		return 0;
	}
	
}
