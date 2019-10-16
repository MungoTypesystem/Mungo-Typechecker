enum FileStatus 
{
	NOTEOF
	EOF
}

class File {
	{open; readable} [
		readable = {
			isEOF; <NOTEOF: {read; readable}
					EOF: {close; end}>
		}
	]

	bool haveMoreData
	bool data
	
	void open(void x) {
		data = true;
		haveMoreData = true
	}
	
	FileStatus isEOF(void x) {
		if (haveMoreData) {
			NOTEOF
		} else {
			EOF
		}
	}
	
	bool read(void x) {
		haveMoreData = true;
		data
	}
	
	void close(void x) {
		unit
	}
}



class RecursiveRead
{
	{read; end
	 close; end}[]
	
	RecursiveRead reader
	
	void read(File[readable] f) {
		reader = new RecursiveRead;
		switch(f.isEOF(unit)) {
			NOTEOF: f.read(unit);
					reader.read(f)
			EOF:    unit; reader.close(f)
		}
	}
	
	void close(File[{close; end}] f) {
		f.close(unit)
	}
} 


class start {
	{main; end}[] 
	
	File file
	RecursiveRead recursiveRead
	
	void main(void x) {
		file = new File;
		file.open(unit);
		recursiveRead = new RecursiveRead;
		recursiveRead.read(file)
	}	
}
