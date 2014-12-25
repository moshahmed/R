#######################################################################
# Title: Process survey data for Gender.
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
colors  <- c("red", "yellow", "green", "violet",
  "orange", "blue", "pink", "cyan")
 
# Read the survey data csv into R.
setwd(dirname(sys.frame(1)$ofile))
survey <- read.csv("data/n3.csv")
titles <- read.csv("data/n3title.csv")

# Output
Question<-'Gender-Cat'
outdir <- 'out/gender/'
outdir <- ''
if (outdir != '') {
  imgfile<-paste(outdir,Question,'.jpg',sep='')
  txtfile<-paste(outdir,Question,".txt",sep="")
  jpeg(imgfile, width=8, height=5, units = 'in', res = 300)
  sink(txtfile) 
}

#######################################################################
# Print the top correlated columns 
#######################################################################
cormat <- cor(survey) # Compute Correlation Matrix
for (i in 1:55 ) {
  cat(colnames(cormat)[i], ":\n");
  print("positive+\n");
  print(head(cormat[order(cormat[,i]),c(i)],5));
  print("negative-\n");
  print(head(cormat[order(-cormat[,i]),c(i)],5));
}  
 
# Plot the columns related to Gender
par(mfcol=c(4,4), oma=c(1,1,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(0,0,0))
for (cn in c("Gender","Chivalry","MaleDominated","MaleChauvanist")) {
  if (cn == "Gender") {
    xlab1 <- "Male / Female "
  }else{
    xlab1 <- "Strong no / No / Neutral / Yes / Strong yes"
  }
  clong <- paste(titles$Long[match(cn,titles$Short)])
  barplot(table(survey[,cn]),col=colors,xlab=xlab1,xaxt='n')
  xlab2 <- sprintf("%s: %s", cn, clong)
  legend("topleft", xlab2, bty="n")
}

#######################################################################
# Differences in opinion between Gender
#######################################################################
males <- subset(survey,Gender==0)
females <- subset(survey,Gender==1)
printcount <- 0

for (cn in names(survey)){
  if (cn=="Gender") next # t.test will give: error, constant data.
  mcn <- males[,cn]
  fcn <- females[,cn]
  # if (abs(mean(mcn) - mean(fcn)) < 0.01) next
  tv <- t.test(males[cn], females[cn], var.equal=T)
  if (tv$p.value >= .05) next
  printcount <- printcount+1
  likerts <- "Strong No / No / Neutral / Yes / Strong Yes"
  # Density and means
  meanm <- mean(mcn)
  meanf <- mean(fcn)
  densitym <- density(mcn)
  densityf <- density(fcn)
  # Plot both male/red and female/green answers.
  plot(densitym,col="red", main="",
    xlab=likerts, ylab="density",
    xlim=c(0,6), ylim=c(0,1), xaxt="n", yaxt="n",lwd=2)
  lines(densityf,col="green",lwd=5)
  # Shade the polygons also.
  # redTrans <- rgb(1,.1,.1,0.2)
  # greenTrans <- rgb(.1,1,.1,0.2)
  # polygon(density(na.omit(mcn)), density=-1, col=redTrans)
  # polygon(density(na.omit(fcn)), density=-1, col=greenTrans)
  clong <- paste(titles$Long[match(cn,titles$Short)])
  lines( x=c(meanm,meanm), y=c(0,.5), col="red")
  lines( x=c(meanf,meanf), y=c(0,.5), col="green")
  fmtLegend <- paste(
    "Gender ~ %s:\n%s\n",
    "Red/thin: Male(%d) m=%3.2f\n",
    "Green/thick: Female (%d) m=%3.2f",
    sep="") 
  fmtLegend2 <- sprintf(fmtLegend,
      cn,clong,
      nrow(males), meanm,
      nrow(females), meanf)
  legend("bottomleft", fmtLegend2, bty="n")
  # boxplot(mcn,fcn, names=c("Males","Females"), main=cn)
  cat(sprintf("%d Different %s pvalue=%f\n",
    printcount, cn, tv$p.value))
  print(tv$estimate)
  flush.console()
}

if (outdir != '') {
  dev.off()
  sink()
  print(sprintf("Wrote %s and %s", txtfile,imgfile))
}
 
