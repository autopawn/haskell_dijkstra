run: compile
	./solution example.txt
compile:
	ghc Dijkstra.hs Solution.hs -o solution
clean:
	rm *.hi || true
	rm *.o || true
	rm test || true
	rm solution || true
