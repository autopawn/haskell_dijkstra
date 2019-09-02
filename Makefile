run: compile
	./test example.txt
compile:
	ghc Dijkstra.hs Test.hs -o test
clean:
	rm *.hi || true
	rm *.o || true
	rm test || true
	rm solution || true
