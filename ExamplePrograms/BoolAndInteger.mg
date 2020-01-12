class Test<a[b]> {
	infer[]

	int x
	
    void x(a[b] y) {
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
