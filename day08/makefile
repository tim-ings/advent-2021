build: main.hs
	ghc main.hs -o main.out

run: build
	cat input.txt | ./main.out

run-sample: build
	cat sample.txt | ./main.out
