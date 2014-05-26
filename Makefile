all: compile  link

compile: 
	cabal install
link:
	ln -fs dist/build/Popkadurak/Popkadurak ./Popkadurak

test: compile run

run: 
	./Popkadurak sample

