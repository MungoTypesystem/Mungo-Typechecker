class <C[U]> FileReader
{
	{open; ReadAble} [
		ReadAble = {notEOF; <True: {read; ReadAble} 
							 False: {close; End}>
	]
	
	void open(string file) {...}
	bool notEOF(void x) {...}
	C[U] read(void x)  {...}
	void close(void x) {...}
}

class Example {
	{main; end}[]
	
	FileReader<Bool[{getState; end}]> reader
	Bool readState;
	
	void main(void x) {
		reader = new FileReader;
		reader.open("myfile");
		while(reader.notEOF(unit)) {
			readState = read(unit);
			// use readState here
			
		};
		reader.close()
	}
}