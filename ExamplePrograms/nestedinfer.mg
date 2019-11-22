class Base {
	X[X = {foo;{bar;end}}]

	void foo(void x) { unit }
	void bar(void x) { unit }
}

class First {
	infer[]

	Base f1
	Base f2

	void use1Foo(void x) {
		f1.foo(unit)
	}	

	void use1Bar(void x) {
		f1.bar(unit)
	}

	void init1(void x) {
		f1 = new Base
	}

	void use2Foo(void x) {
		f2.foo(unit)
	}	

	void use2Bar(void x) {
		f2.bar(unit)
	}

	void init2(void x) {
		f2 = new Base
	}

	void completeRun(void x) {
		f1 = new Base;
		f1.foo(unit);
		f1.bar(unit)
	}
}

class Second {
	infer[]

	First f1
	First f2

	void useF1(void x) {
		f1.completeRun(unit)
	}

	void initBoth(void x) {
		f1 = new First1;
		f1.init1(unit);
		f1.init2(unit);
		f2 = new First2;
		f2.init1(unit);
		f2.init2(unit)
	}

	void useFirstFoo(void x) {
		f1.use1Foo(unit)
	}

	void useFirstBar(void x) {
		f1.use1Bar(unit)
	}

	void useFirst2(void x) {
		f1.use2Foo(unit);
		f1.use2Bar(unit)
	}

	void initSingle(void x) {
		f = new First1
	}

	void completeBoth(void x) {
		f2.use1Foo(unit);
		f2.use1Bar(unit);
		f2.use2Foo(unit);
		f2.use2Bar(unit)
	}
}

