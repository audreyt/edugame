all::
	@-mkdir -p /Library/Scripts/_OmniGraffle
	runghc -O0 v3.hs > /Library/Scripts/_OmniGraffle/1.scpt
	open /Library/Scripts/_OmniGraffle/1.scpt 

ghci::
	ghci v3.hs

tags::
	hasktags -c v3.hs EduGame/*hs

update::
	perl update.pl
