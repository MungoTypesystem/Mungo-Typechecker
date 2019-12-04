enum E {
    A
    B
}

class simple
{
	{f; <A: end B: end>}[]
	
	E f(void x) {
	    A	
	}
}

class InferMe
{
	infer []
	
	simple f1
	
	void init(void x) {
		f1 = new simple
	}
	
	void final(simple[{f;<A: end B: end>}] x) {
		switch(f1.f(unit)) {
            A: unit
            B: unit
        };
		switch(x.f(unit)) {
            A: unit
            B: unit
        }
	}
}
