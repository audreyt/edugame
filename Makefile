all:
	perl parse.pl
	sudo mkdir -p /Library/Scripts/_OmniGraffle/1.scpt
	runghc educards.hs > /Library/Scripts/_OmniGraffle/1.scpt
