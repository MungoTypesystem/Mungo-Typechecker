class Test {
	infer[]

	int x
	
    void f(void y) {
		x = 0;
		loop: 
			x = x + 1;
			if (x <= 100) {
				continue loop
			} else {
				unit
			}
    }

}
