all:
	@-mkdir -p /Library/Scripts/_OmniGraffle
	runghc -O0 v2.hs > /Library/Scripts/_OmniGraffle/1.scpt
	open /Library/Scripts/_OmniGraffle/1.scpt 

update:
	perl update.pl
