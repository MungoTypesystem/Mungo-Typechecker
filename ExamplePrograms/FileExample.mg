class <C[U]> FileReader
{
	{open; readable} [
		readable = {notEOF; <True: {read; readable} 
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
		reader = new FileReader<Bool[{getState; end}]>;
		reader.open("myfile");
		while(reader.notEOF(unit)) {
			readState = read(unit);
			// use readState here
			
		};
		reader.close()
	}
}