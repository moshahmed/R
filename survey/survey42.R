# Title: Process MBA survey data.
# 1. Setup

library(psy)
library(psych)
library(GPArotation)
library(nFactors)
library(corrplot)
options(digits=3) # Easy to read precision. 

# 2. Read the survey data into R.
# ignore xData columns
setwd(dirname(sys.frame(1)$ofile)) 
survey <- read.csv("data/m42.csv", comment.char='#') # stringsAsFactors=F
survey <- survey[,-grep("^[xX]", names(survey))]
survey[is.na(survey)]<-3
# survey$Gender <- as.numeric(survey$Gender)-1
# Reread the strings columns, and map it manually.
survey2 <- read.csv("data/m42.csv", comment.char='#', stringsAsFactors=F)
  #
  survey$Gender <- survey2$Gender
  survey$Gender[survey2$Gender == "Girl"] <-  5
  survey$Gender[survey2$Gender == "Boy"]  <-  1
  survey$Gender <- as.numeric(survey$Gender)
  #
  survey$Degree <- survey2$Degree
  survey$Degree[survey2$Degree == "BCom"] <- 2 
  survey$Degree[survey2$Degree == "BBM"] <- 3 
  survey$Degree[survey2$Degree == "BSc"] <- 4 
  survey$Degree[survey2$Degree == "Engg"] <- 5 
  survey$Degree <- as.numeric(survey$Degree)
  # 
  survey$PostMBA <- survey2$PostMBA
  survey$PostMBA[survey2$PostMBA == "Good Job"]  <- 3
  survey$PostMBA[survey2$PostMBA == "Start my business"]  <- 5
  survey$PostMBA[survey2$PostMBA == "Pass MBA"]  <- 0
  survey$PostMBA <- as.numeric(survey$PostMBA)
  # Computed columns
  survey$GenderConcious <- survey$Chivalry - survey$MaleDominated - survey$MaleChauvanist

titles <- read.csv("data/n42title.csv")

# ReScale likert columns [1,2,3,4,5] to [-2,-1,0,1,2]
likerts <- setdiff( names(survey),
  grep("^Gender|^Degree|^PostMBA", names(survey), value=TRUE))
for (cn2 in likerts) {
  survey[cn2] <- (survey[cn2] - 3)
}

outdir <- 'c:/tmp/'
outdir <- ""
outdir <- 'out/survey42/'

# Process opinion differences on Question
Opinions <- function(Question,setNo,setYes,outdir="") {
  dlong <- paste(titles$Long[match(Question,titles$Short)])
  print(sprintf("Processing Opinions: %s No=%d Yes=%d\n",
    cn2, nrow(setNo), nrow(setYes)))
  printcount <- 0
  if (outdir != "") {
    imgfile<-paste(outdir,Question,'.jpg',sep='')
    txtfile<-paste(outdir,Question,".txt",sep="")
    jpeg(imgfile, width=8, height=5, units = 'in', res = 300)
    sink(txtfile,append=FALSE,split=TRUE)
  }
  par(mfcol=c(3,3), oma=c(1,1,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(0,0,0))
  for (cn in names(survey)){
    # print(sprintf("subProcessing: %s with %s\n",cn2, cn))
    if (cn==Question) next
    # Run the t-test
    tv <- t.test(setNo[cn], setYes[cn], var.equal=T)
    if (tv$p.value >= .05) next
    printcount <- printcount+1
    # Plot setYes and setNo
    plot(density(na.omit(setNo[,cn])), col="red", main="", 
      panel.first = grid(),
      xlab="Strong No / No / Neutral / Yes / Strong Yes", ylab="density",
      xlim=c(-3,3), ylim=c(0,1), xaxt="n", yaxt="n",lwd=2)
    lines(density(na.omit(setYes[,cn])),col="green",lwd=5,lty=8)
    # Shade the polygons also.
    redTrans <- rgb(1,.1,.1,0.2)
    greenTrans <- rgb(.1,1,.1,0.2)
    polygon(density(na.omit(setNo[,cn])), density=-1, col=redTrans)
    polygon(density(na.omit(setYes[,cn])), density=-1, col=greenTrans)
    # More lines.
    meanNo <- mean(setNo[,cn])
    meanYes <- mean(setYes[,cn])
    lines( x=c(meanNo,meanNo), y=c(0,.5), col="red")
    lines( x=c(meanYes,meanYes), y=c(0,.5), col="green")
    clong <- paste(titles$Long[match(cn,titles$Short)])
    clong2 <- substr(clong, start=0, stop=50)
    dlong2 <- substr(dlong, start=0, stop=50)
    fmtLegend <- paste(
      "%d. %s (%s..)\n",
      "~ %s (%s..)\n",
      "Red/thin=No %s (%d) m=%3.2f,\n",
      "Green/thick=%s (%d) m=%3.2f",
      sep="")
    legend("topleft", sprintf(fmtLegend,
        printcount,
        cn,clong2,
        Question, dlong2,
        Question, nrow(setNo), meanNo,
        Question, nrow(setYes), meanYes),
      bty="n")
    cat(sprintf("%d Different %s pvalue=%f\n",
      printcount, cn, tv$p.value))
    print(tv$estimate)
  }
  if(outdir != "") {
    sink()
    #dev.copy(jpeg,imgfile)
    dev.off()
    print(sprintf("Wrote %s and %s", txtfile,imgfile))
  }
  flush.console()
}

for (cn2 in names(survey)) {
  setNo <- subset(survey, survey[cn2]<0)
  setYes <- subset(survey, survey[cn2]>0)
  if (nrow(setNo)<1) next
  if (nrow(setYes)<1) next
  Opinions(cn2,setNo,setYes,outdir)
  if(outdir==""){ break() }
}

