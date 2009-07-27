all:
	perl parse.pl
	-mkdir -p /Library/Scripts/_OmniGraffle
	runghc educards.hs > /Library/Scripts/_OmniGraffle/1.scpt

update:
	perl update.pl
