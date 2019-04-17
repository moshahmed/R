#######################################################################
# Title: Process survey data for Gender.
# source("c:/mosh/stats-r/survey/gender.R")
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
# setwd(dirname(sys.frame(1)$ofile))
# setwd("c:/mosh/stats-r/survey/")
survey <- read.csv("data/n3.csv")
titles <- read.csv("data/n3title.csv")

# Output
# outdir <- 'out/gender/'
outdir <- './'

Question<-'Gender-Cat'
printcount <- 0

males   <- subset(survey,Gender==0)
females <- subset(survey,Gender==1)

cormat <- cor(survey) # Compute Correlation Matrix

wrap_strings <- function(vector_of_strings,width){
  sapply(vector_of_strings,
    FUN=function(x){
      paste(strwrap(x,width=width), collapse="\n")
    })
}
#######################################################################
# Print the top correlated columns 
#######################################################################
if (outdir != '') {
  txtfile<-paste(outdir,Question,".txt",sep="")
  sink(txtfile) 
}
for (i in 1:55 ) {
  cat(sprintf("%d. %s\n", i, colnames(cormat)[i]))
  cat("  positive+\n");
  print(head(cormat[order(cormat[,i]),c(i)],5));
  cat("  negative-\n");
  print(head(cormat[order(-cormat[,i]),c(i)],5));
}  
if (outdir != '') {
  print(sprintf("Wrote %s", txtfile))
  sink() 
}

if (outdir != '') {
  imgfile<-paste(outdir,'survey','.jpg',sep='')
  jpeg(imgfile, width=8, height=5, units = 'in', res = 600)
} 
par(mfcol=c(4,4), oma=c(1,1,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(0,0,0))
# layout()
# Plot the columns related to Gender
for (cn in c("Gender","Chivalry","MaleDominated","MaleChauvanist")) {
  if (cn == "Gender") {
    xlab1 <- "Male / Female "
  }else{
    xlab1 <- "Strong no / No / Neutral / Yes / Strong yes"
  }
  clong <- paste('"',titles$Long[match(cn,titles$Short)],'"',sep='')
  clong <- wrap_strings(clong, 30)
  barplot(table(survey[,cn]),col=colors,xlab=xlab1,xaxt='n',cex.lab=.8)
  xlab2 <- sprintf("%s: %s", cn, clong)
  legend("topleft", xlab2, bty="n",cex=.7)

}
if (outdir != '') {
  dev.off()
  print(sprintf("Wrote %s", imgfile))
}

#######################################################################
# Differences in opinion between Gender
#######################################################################
for (cn in names(survey)){
  if (cn=="Gender") next # t.test will give: error, constant data.
  mcn <- males[,cn]
  fcn <- females[,cn]
  # if (abs(mean(mcn) - mean(fcn)) < 0.01) next
  tv <- t.test(males[cn], females[cn], var.equal=T)
  if (tv$p.value >= .05) next
  #
  if (outdir != '') {
    imgfile<-paste(outdir,'cmp-',cn,'.jpg',sep='')
    jpeg(imgfile, width=8, height=5, units = 'in', res = 600)
  }
  par(mfcol=c(3,3), oma=c(1,1,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(0,0,0))
  #
  printcount <- printcount+1
  likerts5 <- "Strong No / No / Neutral / Yes / Strong Yes"
  ylab2 <- "Gender Male vs Female density"
  # likerts3 <- "No/Neutral/Yes"
  # Density and means
  meanm <- mean(mcn)
  meanf <- mean(fcn)
  densitym <- density(mcn)
  densityf <- density(fcn)
  tablem <- table(factor(mcn,levels=1:5))
  tablef <- table(factor(fcn,levels=1:5))
  # Compute density/percentages.
  perm <- prop.table(tablem)
  perf <- prop.table(tablef)
  # Plot both male/red and female/green answers.
  plot(densitym,col="red", main="",
    panel.first = grid(),
    xlab=likerts5, ylab=ylab2,
    cex.lab=0.8,
    xlim=c(0,6), ylim=c(0,1), xaxt="n", yaxt="n",lwd=2)
  lines(densityf,col="green",lwd=5)
  # Shade the polygons also.
  redTrans   <- rgb(1 ,.1,.1,0.2)
  greenTrans <- rgb(.1,1,.1, 0.2)
  # polygon(density(na.omit(mcn)), density=-1, col=redTrans)
  # polygon(density(na.omit(fcn)), density=-1, col=greenTrans)
  clong <- paste(titles$Long[match(cn,titles$Short)])
  lines( x=c(meanm,meanm), y=c(0,.5), col="red")
  lines( x=c(meanf,meanf), y=c(0,.5), col="green")
  fmtLegend <- paste(
    "Gender ~ %s:\n%s\n",
    "Red/thin: Male(%d) %s=%3.2f\n",
    "Green/thick: Female (%d) %s=%3.2f",
    sep="") 
  unicode_mu <- "\u03BC"
  fmtLegend2 <- sprintf(fmtLegend,
      cn,clong,
      nrow(males),   unicode_mu, meanm,
      nrow(females), unicode_mu, meanf)
  legend("bottomleft", fmtLegend2, bty="n", cex=.6, inset=c(+0.05,.7))
  # OR: boxplot(survey$Reading ~ factor(survey$Gender), data=survey)
  mfnames <- c("Males","Females")
  boxplot(mcn,fcn, names=mfnames, 
    col=(c(redTrans,greenTrans)),
    horizontal=T, frame=T,
    xlim=c(0,5),
    # add=T,
    outline=T,
    axes=F,
    varwidth=T, # width=c(2,3),
    # boxwex=.6,
    main=cn)  
  axis(1)
  axis(2,las=2,cex.axis=.8,at=1:2, labels=mfnames, side=4, pos=4.2, lwd=0)
  barplot(rbind(perm,perf),beside=T,
    col=c("red","green"), # density=c(60,90),
    #panel.first = grid(),
    main="",
    xlab=likerts5, ylab=ylab2,
    cex.lab=0.7,
    #xlim=c(0,6),
    width=c(4,6),
    ylim=c(0,1), xaxt="n", yaxt="n",lwd=2
    )
  # Lines look like bars.
  # lines( x=c(meanm,meanm), y=c(0,.5), lwd=2, col="red")
  # lines( x=c(meanf,meanf), y=c(0,.5), lwd=5, col="green")
  legend("bottomleft", fmtLegend2, bty="n",cex=.6,inset=c(+.05,.7))
  box()

  #
  cat(sprintf("%d Different %s pvalue=%f\n",
    printcount, cn, tv$p.value))
  print(tv$estimate)
  if (outdir != '') {
    dev.off()
    print(sprintf("Wrote %s", imgfile))
  }  else {
    # readline(prompt="Press [enter] to continue")
  }
  flush.console()
}
