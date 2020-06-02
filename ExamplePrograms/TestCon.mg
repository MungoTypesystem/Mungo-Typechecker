enum Status {
    EOF
    NEOF
}


class ND {
    infer[]

    Status f(void x) { EOF }
    void g(void x) { x }
}


class newTest 
{
    {main; end}[]

    ND f

    void main(void x) {
        f = new ND;
        f.f(x);
        x
    }
} 

/*

class file 
{
    {open; X} [
        X = { isEOF; < EOF: {close; end} 
                       NEOF: {read; X}> }
    ]

    void open(void x) {
        x
    }

    Status isEOF(void x) {
        NEOF
    }   

    void read(void x) {
        x
    }

    void close(void x) {
        x
    }
}

class filereader 
{
    {main; end}[]

    file f
    
    void main(void x) {
        f = new file;
        f.open(x);
        k: switch(f.isEOF(x)) {
            EOF: f.close(x)
            NEOF: f.read(x);
                  continue k
        }
    }
} 

*/
