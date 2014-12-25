#######################################################################
# Title: Process MBA survey data.
#######################################################################
# 1. Setup
#######################################################################
## Do this once to download from the web.
# install.packages("psych")
# install.packages("GPArotation")
# install.packages("psy") 
# install.packages("nFactors")

library(psy)
library(psych)
library(GPArotation)
library(nFactors)
library(corrplot)
options(digits=3) # Easy to read precision. 

#######################################################################
# 2. Read the survey data into R.
#######################################################################
setwd(dirname(sys.frame(1)$ofile)) 
survey <- read.csv("data/n3.csv")
# names(survey)
titles <- read.csv("data/n3title.csv")
# titles$QNum <-NULL # Question number not needed.
# For printing output.
printcount <- 0
vimfold <- 1 
if (vimfold) {
  vimfolder <- "vim:fdm=marker:fmr={,}:\n" ; vimstart <- "{" ; vimend <- "}"
}else{
  vimfolder <- "" ; vistart <- "" ; viend <- ""
}

myfactors <- 16 # For factor analysis.

# Correlation matrix.
cormat<-cor(survey) # method=pearson,kendall,spearman
# cormat <- cor(survey,use="complete.obs",method="pearson")
 
# For a stack of pie charts (red=no,white=neutral,green=yes)
par(mfcol=c(7,8), oma=c(1,1,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(0,0,0))
colors <- c("red", "pink", "white", "cyan", "green")

#######################################################################
# 3. Correlation Matrix
#######################################################################

getPairs <- function(z) {
  z[lower.tri(z,diag=TRUE)] <- NA  # Drop diagonal and lower triangular matrix.
  z <- as.data.frame(as.table(z))  # Turn matrix into a 3-column table
  z <- na.omit(z)                  # Delete NA from above
  z <- z[order(-abs(z$Freq)),]     # Sort by correlation (+ve or -ve)
  return(z)
}

printTopCor <- function(titles,topic,survey,clist) {
  cmx <- cor(survey)
  cat (vimstart,"Survey:", topic, ", columns=c(", clist, ")\n")
  for (cn in clist){
    printcount <- printcount+1
    cnlong <- paste(titles$Long[match(cn, titles$Short)])
    cat(vimstart," C", printcount, ", Survey:", topic,
      ", Question: \"", cn, ": ", cnlong, "\"\n", sep="")
    # Print responses for this question.
    cat("\nResponse counts (1=Strong-no,2=No,3=Neutral,4=Yes,5=Strong-yes):", sep="")
    print(table(survey[,cn])) 
    # Get the subset of strongly correlated column pairs.
    sst <- subset(getPairs(cmx), abs(Freq) > .2 & Var1 == cn)
    # Convert factors into strings for printing.
    ssc <- rapply(sst, as.character, classes="factor", how="replace")
    if (nrow(sst) > 0 ) {
      #print(sst)
      cat("\nTop correlated columns to ",cn, " ~\n",sep="")
      for (kn in 1:nrow(sst)) {
        Freq<-sst[kn,"Freq"]
        dn<-ssc$Var2[kn]
        dlong <- paste(titles$Long[match(dn,titles$Short)])
        dlong <- substr(dlong, start=0, stop=50)
        cat( sprintf("  %02d %-14s: %+5.2f: %s..\n", kn,dn,Freq,dlong) )
      }
    }else{
      cat("\nNo strong correlations\n")
    }
    cat(vimend,"\n")
    flush.console()
  }
  cat(vimend,"\n")
}

sink("out/survey/correlations.txt") # Save output to a file.
# printTopCor(titles,"Full", survey, names(survey))
gql<- c("Gender", "Chivalry", "MaleDominated", "MaleChauvanist")
printTopCor(titles,"All", survey, gql)
printTopCor(titles,"Subset girls", survey = subset(survey, Gender==1), gql)
printTopCor(titles,"Subset boys", survey = subset(survey, Gender==0), gql)
cat(vimfolder)
sink() # stop saving output to a file.

# Study of outliers 
sink("out/survey/Outliers.txt") # Save output to a file.
cat(vimstart,"Outlier analysis\n")
# gql2 <- names(survey)
gql2 <- c("Chivalry", "MaleDominated", "MaleChauvanist")
for(cn in gql2) {
  clong <- paste(titles$Long[match(cn,titles$Short)])
  sst <- subset(survey, survey[,cn]<2 | survey[,cn]>4)
  sstlen <- length(sst[,'Gender'])
  cat(vimstart)
  cat(sprintf("Outlier (strong yes/no) %s (%s) count %d\n", cn, clong, sstlen))
  printTopCor(titles,paste("Subset outlier ",cn," (",clong,")\n",sep=""), sst, gql)
  cat(vimend,"\n")
}
cat(vimend,"Outlier analysis\n")
cat(vimfolder)
sink()

#######################################################################
# Plots
#######################################################################
# Plot the responses as pie charts.
par(mfcol=c(7,8), oma=c(1,1,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(0,0,0))
for (cn in names(survey) ) {
    pie(table(survey[,cn]), main=cn, col=colors)
}  

# Plot the matrix, clustering the related columns.
corrplot(cormat, method = "square", tl.cex=.7, tl.col='black',
  diag=F,type='lower',order="FPC")

corrplot(cormat, method = "square", tl.cex=.7, tl.col='black',
  diag=F,type='full',order="hclust", addrect=myfactors)

#######################################################################
# 4. PCA: Principal Component analysis
#######################################################################
pcomp <- principal(cormat, nfactors=10, n.obs=55, rotate="oblimin")

# Plot the correlation matrix (dense)
sortedmat <- mat.sort(cormat, pcomp)
cor.plot(sortedmat, cex.axis=0.5) 
 
#######################################################################
# 5. Paired t-test on Reading and Writing
#######################################################################
t.test( survey$Reading, survey$Writing, paired=T)
# t=1.4, df=76, p-value=0.1544

#######################################################################
## 6. Scree Plots
#######################################################################
# Scree Plot 1.
par(mfcol=c(1,1), oma=c(1,1,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(0,0,0))
fit <- factanal(survey, myfactors, scores=c("regression"), rotation="none")
scree.plot(fit$correlation)

# Scree Plot 2.
solution <- fa(r=cormat, nfactors=myfactors, rotate="oblimin", fm="pa")
plot(solution,labels=names(survey),cex=.7, ylim=c(-.1,1)) 

# Scree Plot 3.
eigenValues <- eigen(cormat)
ap <- parallel(subject=nrow(survey), var=ncol(survey), rep=100, cent=.05)
nonGraphicalScree <- nScree(x=eigenValues$values, aparallel=ap$eigen$qevpea)
plotnScree(nonGraphicalScree)

#######################################################################
# 7A. Factor analysis
#######################################################################
fa(r=cormat, nfactors=myfactors, rotate="varimax", SMC=FALSE, fm="minres")
fit <- factanal(survey, myfactors, scores=c("regression"), rotation="none")
print(fit, digits=2, cutoff=.3, sort=TRUE)
head(fit$scores)

# Test of the hypothesis that 16 factors are sufficient.
# The chi square statistic is 721 on 725 degrees of freedom.
# The p-value is 0.53 
 
# 7B. Plot Factor Map
load <- fit$loadings[,1:myfactors] 
plot(load,type="n")
text(load,labels=names(survey),cex=.6)
 
#######################################################################
