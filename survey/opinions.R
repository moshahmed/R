# Title: Process MBA survey data.
# 1. Setup

library(psy)
library(psych)
library(GPArotation)
library(nFactors)
library(corrplot)
options(digits=3) # Easy to read precision. 

# 2. Read the survey data into R.
setwd(dirname(sys.frame(1)$ofile)) 
survey <- read.csv("data/n3.csv")
titles <- read.csv("data/n3title.csv")

# outdir <- 'out/opinions/'
outdir <- ''
opcount <- 0

# Process opinion differences on Question
Opinions <- function(Question,setNo,setYes) {
  dlong <- paste(titles$Long[match(Question,titles$Short)])
  opcount <<- opcount + 1
  print(sprintf("%d. Processing Opinions: %s No=%d Yes=%d\n",
    opcount, cn2, nrow(setNo), nrow(setYes)))
  printcount <- 0
  imgfile<-paste(outdir,Question,'.jpg',sep='')
  txtfile<-paste(outdir,Question,".txt",sep="")
  jpeg(imgfile, width=8, height=5, units = 'in', res = 600)
  par(mfcol=c(4,4), oma=c(1,1,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(0,0,0))
  sink(txtfile,append=FALSE,split=TRUE)
  print(sprintf("Processing: %s\n",cn2))
  for (cn in names(survey)){
    if (cn==Question) next
    # Run the t-test
    tv <- t.test(setNo[cn], setYes[cn], var.equal=T)
    if (tv$p.value >= .05) next
    printcount <- printcount+1
    likerts5 <- "Strong No / No / Neutral / Yes / Strong Yes"
    likerts3 <- "No/Neutral/Yes"
    # Yes=green, No=red
    plot(density(setNo[,cn]),col="red", main="",
      xlab=likerts5,
      ylab="density",
      cex.lab=0.6,
      xlim=c(0,6), ylim=c(0,1), xaxt="n", yaxt="n",lwd=2)
    lines(density(setYes[,cn]),col="green",lwd=5)
    # Shade the polygons also.
    redTrans <- rgb(1,.1,.1,0.2)
    greenTrans <- rgb(.1,1,.1,0.2)
    polygon(density(na.omit(setNo[,cn])), density=-1, col=redTrans)
    polygon(density(na.omit(setYes[,cn])), density=-1, col=greenTrans)
    meanNo <- mean(setNo[,cn])
    meanYes <- mean(setYes[,cn])
    lines( x=c(meanNo,meanNo), y=c(0,.5), col="red")
    lines( x=c(meanYes,meanYes), y=c(0,.5), col="green")
    clong <- paste(titles$Long[match(cn,titles$Short)])
    clong2 <- substr(clong, start=0, stop=50)
    dlong2 <- substr(dlong, start=0, stop=50)
    fmtLegend <- paste(
      "%d.%d. %s (%s..)\n",
      "~ %s (%s..)\n",
      "Red/thin=No %s (%d) %s=%3.2f,\n",
      "Green/thick=%s (%d) %s=%3.2f",
      sep="")
    unicode_mu <- "\u03BC"
    fmtLegend2 <- sprintf(fmtLegend,
        opcount, printcount,
        cn,clong2,
        Question, dlong2,
        Question, nrow(setNo),  unicode_mu, meanNo,
        Question, nrow(setYes), unicode_mu, meanYes)
    legend("topleft", fmtLegend2, bty="n", cex=0.6) 
    #
    perNo <- prop.table(table(factor(setNo[,cn],levels=1:5)))
    perYes <- prop.table(table(factor(setYes[,cn],levels=1:5)))
    barplot(rbind(perNo,perYes),beside=T,
      col=c("red","green"), # density=c(60,90),
      panel.first = grid(),
      main="",
      xlab=likerts3,
      ylab="density",
      cex.lab=0.6,
      width=c(4,6),
      ylim=c(0,1), xaxt="n", yaxt="n",lwd=2
      ) 
    legend("bottomleft", fmtLegend2, bty="n",cex=.6,inset=c(-.01,.7))
    box()  
    cat(sprintf("%d.%d Different %s pvalue=%f\n",
      opcount, printcount, cn, tv$p.value))
    print(tv$estimate)
  }
  sink()
  #dev.copy(jpeg,imgfile)
  dev.off()
  print(sprintf("Wrote %s and %s", txtfile,imgfile))
  flush.console()
}

for (cn2 in names(survey)) {
  setNo <- subset(survey, survey[,cn2]<3)
  setYes <- subset(survey, survey[,cn2]>3)
  if (nrow(setNo)<1) next
  if (nrow(setYes)<1) next
  Opinions(cn2,setNo,setYes)
}

