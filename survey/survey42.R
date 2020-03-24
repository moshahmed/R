# Title: Process MBA survey data.
# GPL(C) moshahmed_at_gmail
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

# ReScale likert columns [1,2,3,4,5] + shifted to [-2,-1,0,1,2]
shifted <- -3.
likertcolumns <- setdiff( names(survey),
  grep("^Gender|^Degree|^PostMBA", names(survey), value=TRUE))
for (cn2 in likertcolumns) {
  survey[cn2] <- (survey[cn2] +shifted)
}

# Output
outdir <- "./"
# outdir <- 'c:/tmp/'
# outdir <- 'out/survey42/'
opcount <- 0

# Process opinion differences on Question
Opinions <- function(Question,setNo,setYes,outdir="") {
  dlong <- paste(titles$Long[match(Question,titles$Short)])
  opcount <<- opcount + 1
  print(sprintf("%d. Processing Opinions: %s No=%d Yes=%d",
    opcount, cn2, nrow(setNo), nrow(setYes)))
  printcount <- 0
  if (outdir != "") {
    imgfile<-paste(outdir,Question,'.jpg',sep='')
    txtfile<-paste(outdir,Question,".txt",sep="")
    jpeg(imgfile, width=8, height=5, units='in', res=600)
    sink(txtfile,append=FALSE,split=FALSE)
  }
  par(mfcol=c(6,8), oma=c(1,1,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(0,0,0))
  for (cn in names(survey)){
    # print(sprintf("subProcessing: %s with %s\n",cn2, cn))
    if (cn==Question) next
    # Run the t-test
    tv <- t.test(setNo[cn], setYes[cn], var.equal=T)
    if (tv$p.value >= .05) next
    printcount <- printcount+1
    # Data
    meanNo <- mean(setNo[,cn])
    meanYes <- mean(setYes[,cn])
    perNo <- prop.table(table(factor(setNo[,cn],levels=-2:2)))
    perYes <- prop.table(table(factor(setYes[,cn],levels=-2:2)))
    # Labels
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
        opcount,printcount,
        cn,clong2,
        Question, dlong2,
        Question, nrow(setNo),  unicode_mu, meanNo-shifted,
        Question, nrow(setYes), unicode_mu, meanYes-shifted)
    # ==================================================
    # 1. Plot setYes and setNo
    likerts5 <- sprintf("Strong No / No / Neutral / Yes / Strong Yes\n%s", cn)
    ylab2 <- sprintf("%s, No vs Yes density", cn2)
    plot(density(na.omit(setNo[,cn])), col="red", main="", 
      # panel.first=grid(),
      xlab=likerts5,
      cex.lab=0.4,
      ylab=ylab2,
      xlim=c(-3,3), ylim=c(0,1), xaxt="n", yaxt="n",lwd=2)
    lines(density(na.omit(setYes[,cn])),col="green",lwd=5,lty=8)
    # Shade the polygons also.
    redTrans <- rgb(1,.1,.1,0.2)
    greenTrans <- rgb(.1,1,.1,0.2)
    polygon(density(na.omit(setNo[,cn])), density=-1, col=redTrans)
    polygon(density(na.omit(setYes[,cn])), density=-1, col=greenTrans)
    # More lines.
    lines( x=c(meanNo,meanNo), y=c(0,.5), col="red")
    lines( x=c(meanYes,meanYes), y=c(0,.5), col="green")
    legend("topleft",fmtLegend2, bty="n", cex=.4, inset=c(-0.05,-.1))
    # ==================================================
    # 2. Box plot
    boxplot(setNo[,cn],setYes[,cn], names=c("Males","Females"), 
      col=(c(redTrans,greenTrans)),
      xaxt="n", xlab=likerts5, cex.lab=0.4,
      yaxt="n", ylab=sprintf("%s (No vs Yes)", cn2, cn2),
      horizontal=T,
      cex.main=0.4,
      main=sprintf("%s ~ %s", cn, cn2)
      )
    # ==================================================
    # 3. Bar plot
    barplot(rbind(perNo,perYes),beside=T,
      col=c("red","green"), # density=c(60,90),
      # panel.first=grid(),
      main="",
      xaxt="n", xlab=likerts5, cex.lab=0.4,
      ylab=ylab2,
      width=c(4,6),
      ylim=c(0,1), yaxt="n",lwd=2
      ) 
    legend("bottomleft", fmtLegend2, bty="n",cex=.4,inset=c(-.06,.7))
    box()
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

