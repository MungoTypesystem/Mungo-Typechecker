enum FileStatus 
{
	NOTEOF
	EOF
}


class RecursiveRead
{
	{readR; end
	 closeR; end}[]
	
	RecursiveRead reader
	
	void readR(File[readable] f) {
		reader = new RecursiveRead;
		switch(f.isEOF(unit)) {
			NOTEOF: f.read(unit);
					reader.readR(f)
			EOF:    reader.closeR(f)
		};
		reader = null
	}
	
	void closeR(File[{close; end}] f) {
		f.close(unit)
	}
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





class start {
	{main; end}[] 
	
	File file
	RecursiveRead recursiveRead
	
	void main(void x) {
		file = new File;
		file.open(unit);
		recursiveRead = new RecursiveRead;
		recursiveRead.readR(file)
	}	
}
