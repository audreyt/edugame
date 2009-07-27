all:
	perl parse.pl
	runghc educards.hs > /Library/Scripts/_OmniGraffle/1.scpt
