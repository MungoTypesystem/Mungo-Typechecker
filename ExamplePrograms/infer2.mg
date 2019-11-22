class Base {
	X[X = {foo;{bar;end}}]

	void foo(void x) { unit }
	void bar(void x) { unit }
}

class First {
	infer[]

	Base f1

	void use1Foo(void x) {
		f1.foo(unit)
	}	

	void use1Bar(void x) {
		f1.bar(unit)
	}

	void init1(void x) {
		f1 = new Base
	}

	/*void completeRun(void x) {
		f1 = new Base;
		f1.foo(unit);
		f1.bar(unit)
	}*/
}