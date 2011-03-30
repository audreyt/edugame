all::
	@-mkdir -p /Library/Scripts/_OmniGraffle
	runghc -O0 v2.hs > /Library/Scripts/_OmniGraffle/1.scpt
	open /Library/Scripts/_OmniGraffle/1.scpt 

ghci::
	ghci v2.hs

tags::
	hasktags -c v2.hs EduCards/*hs

update::
	#perl update.pl ### broken
