# What: Run R scripts.
# GPL(C) moshahmed@gmail.com
# 2016-12-19

R:=c:/math/r323

V1=echo=TRUE,max.deparse.length=10000
V2=echo=TRUE,verbose=TRUE,max.deparse.length=10000

matmul::
	# perl quizr.pl 3 > quizr.R
	$(R)/bin/Rscript.exe -e 'for(i in 1:10){ '\
'cat(\"[[Q\",i,sep=\"\");source(\"$@.R\",$(V1));cat(\"]]Q\",i,\"\n\",sep=\"\") }' > run/$@.run
	tail -n3 $@.R run/$@.run

mcm-2::
	$(R)/bin/Rscript.exe -e 'source(\"$@.R\",$(V1))'
	$(R)/bin/Rscript.exe -e 'for(i in 1:10){ '\
'cat(\"[[Q\",i,sep=\"\");source(\"$@.R\",$(V1));cat(\"]]Q\",i,\"\n\",sep=\"\") }' > run/$@.run
	tail -n3 $@.R run/$@.run
