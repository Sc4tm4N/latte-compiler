all: runtime.o

runtime.o: runtime.c
	gcc -m32 -D_GNU_SOURCE -c runtime.c -o runtime.o

own-pc:
	gcc -m32 -c runtime.c -o runtime.o

clean:
	rm -rf *.o
