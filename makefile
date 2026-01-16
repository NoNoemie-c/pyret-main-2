build:
	dune build pyretc.exe

tt:
	dune build pyretc.exe
	./pyretc.exe t.arr
	gcc -g -no-pie t.s -o t
	./t

clean:
	dune clean
