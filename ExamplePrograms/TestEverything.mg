enum myEnum {
	one
	two
}

class test {
	S [
		S = {
			stop; end
			testswitchval; <one: S two: end>
		}
		
	]
	
	
	
	void stop(void x) {
		unit
	}
	
	void testswitchval(bool x) {
		if (true) {
			unit
		} else {
			unit
		}
	}
}


class test1 {
	U[
		U = end
	]
	
	test test
	myEnum e
	bool b

	void testunit(void x) {
		unit
	}
	
	void testEQ(void x) {
		e = one;
		e = two	
	}
	
	bool testlbl(bool x) {
		b = x;
		lbl: unit
	}
	
	void testnew(void x) {
		test = new test;
		test.stop(unit)
	}

	test[S] testParamRet(test[S] input) {
		input
	}
	
	void testcallParam(test[U] t) {
		t.stop(unit)
	}
	
	void testreadParam(test[S] t) {
		test = t;
		test.stop(unit)
	}
	
	void testcontinue(void x) {
		lbl: continue lbl
	}
	
	void testswitch(void x) {
		switch(test.testswitchval(unit)) {
			two: unit
			one: unit
		}
	}
	
	void testif(bool b) {
		if (b){one} else {two}
	}
	
}