all:
	perl parse.pl
	-mkdir -p /Library/Scripts/_OmniGraffle
	-rm educards educards.o
	ghc --make educards.hs
	./educards > /Library/Scripts/_OmniGraffle/1.scpt

update:
	perl update.pl
