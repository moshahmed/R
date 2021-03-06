#######################################################################
# Title: Process MBA survey data.
# GPL(C) moshahmed_at_gmail
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

# Read the survey data csv into R.
# For Survey  paper.
setwd(dirname(sys.frame(1)$ofile)) 
survey <- read.csv("data/n3.csv")
titles <- read.csv("data/n3title.csv")

# outdir <- 'out/vernacular/'
outdir <- './'

Opinions <- function(Question,setNo,setYes) {
  # Differences in opinion between Question
  printcount <- 0
  imgfile<-paste(outdir,Question,'.jpg',sep='')
  txtfile<-paste(outdir,Question,".txt",sep="")
  jpeg(imgfile, width=8, height=5, units = 'in', res = 600)
  sink(txtfile,append=FALSE,split=TRUE)
  par(mfcol=c(4,4), oma=c(1,1,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(0,0,0))
  for (cn in names(survey)){
    if (cn==Question) next
    dlong <- paste(titles$Long[match(Question,titles$Short)])
    if (nrow(setNo)<1) next
    if (nrow(setYes)<1) next
    # Run the t-test
    tv <- t.test(setNo[cn], setYes[cn], var.equal=T)
    if (tv$p.value >= .05) next
    print(sprintf("Processing Opinions: %s No=%d Yes=%d\n",
      cn, nrow(setNo), nrow(setYes)))
    printcount <- printcount+1
    meanNo <- mean(setNo[,cn])
    meanYes <- mean(setYes[,cn])
    likerts5 <- "Strong No / No / Neutral / Yes / Strong Yes"
    likerts3 <- "No/Neutral/Yes"
    # Plot the graph.
    xlab1 <- likerts5
    xlim1 <- c(0,6)
    if (cn == "Gender") {
      xlab1 <- "Male / Female "
      xlim1 <- c(-1,2)
    }
    ylab2 <- sprintf("%s, No vs Yes density", Question)
    # Plot No=red, Yes=green.
    plot(density(setNo[,cn]),col="red", main="",
      #panel.first = grid(),
      xlab=xlab1,
      ylab=ylab2,
      xlim=xlim1,
      cex.lab=0.6,
      ylim=c(0,1.1), xaxt="n", yaxt="n",lwd=2)
    lines(density(setYes[,cn]),col="green",lwd=5,lty=8)
    # Shade the polygons also.
    redTrans <- rgb(1,.1,.1,0.1)
    greenTrans <- rgb(.1,1,.1,0.1)
    polygon(density(na.omit(setNo[,cn])), density=-1, col=redTrans)
    polygon(density(na.omit(setYes[,cn])), density=-1, col=greenTrans)
    # Legend
    clong <- paste(titles$Long[match(cn,titles$Short)])
    clong2 <- substr(clong, start=0, stop=50)
    dlong2 <- substr(dlong, start=0, stop=50)
    unicode_mu <- "\u03BC"
    fmtLegend2 <- sprintf(paste("%d. %s (%s..)\n",
          "~ %s (%s..)\n",
          "Red/thin=No %s (%d) %s=%3.2f,\n",
          "Green/thick=%s (%d) %s=%3.2f",
          sep=""),
        printcount, cn,clong2,
        Question, dlong2,
        Question, nrow(setNo),  unicode_mu, meanNo,
        Question, nrow(setYes), unicode_mu, meanYes)
    legend("topleft", fmtLegend2, bty="n", cex=0.6) 
    # Draw the mean, No=red, Yes=green
    lines( x=c(meanNo,meanNo), y=c(0,.5), col="red")
    lines( x=c(meanYes,meanYes), y=c(0,.5), col="green")
    # 
    perNo <- prop.table(table(factor(setNo[,cn],levels=1:5)))
    perYes <- prop.table(table(factor(setYes[,cn],levels=1:5)))
    if (cn == "Gender") {
      perNo <- prop.table(table(factor(setNo[,cn],levels=0:1)))
      perYes <- prop.table(table(factor(setYes[,cn],levels=0:1)))
    }
    red2   <- rgb(1 ,.1,.1,0.7)
    green2 <- rgb(.1,1 ,.1,0.7)
    barplot(rbind(perNo,perYes),beside=T,
      col=c(red2,green2),
      #panel.first = grid(),
      main="",
      xlab=xlab1,
      ylab=ylab2,
      cex.lab=0.6,
      width=c(4,6),
      angle=c(10,20), density=c(80,80), # Slow rendering
      ylim=c(0,1), xaxt="n", yaxt="n",lwd=2
      ) 
    legend("bottomleft", fmtLegend2, bty="n",cex=.6,inset=c(-.01,.7))
    box()  
    # Print the p.value
    cat(sprintf("%d Different %s pvalue=%f\n",
      printcount, cn, tv$p.value))
    print(tv$estimate)
  }
  dev.off()
  sink()
  print(sprintf("Wrote %s and %s", txtfile,imgfile))
  flush.console()
}
Opinions("TownSmall",  subset(survey,TownSmall<3),  subset(survey,TownSmall>3))
Opinions("Vernacular", subset(survey,Vernacular<3), subset(survey,Vernacular>3)) 

