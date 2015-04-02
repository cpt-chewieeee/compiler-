all: mini_l.lex mini_l.y
	bison -v -d --file-prefix=y mini_l.y
	flex mini_l.lex
	g++ -o parser y.tab.c lex.yy.c -lfl
	
clean:
	rm -f lex.yy.* *.out* *.o *.mil parser y.tab.* *.stat

