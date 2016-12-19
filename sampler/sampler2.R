# What: Randomly pick count rows from each section; to make groups.
count <-4
sections <- "ABC" # grep regexp on S.No.
file <- "sampler.csv"
 
# Process cmdline args
args <- commandArgs(trailingOnly = TRUE)
if (length(args)>0) {
  if("--help" %in% args) {
    cat("Usage: sampler2.cmd count/0 [sections/eg.ABC] [file.csv]")
    q(save="no")
  }
  count <- as.integer(args[1])
  if (length(args)>1) {
    sections <- args[2]
    if (length(args)>2) {
      file <- args[3]
    }
  }
}else{
  cat("Usage: sampler2.cmd count/0 [sections/eg.ABC] [file.csv]")
  q()
}

sections <- paste('[',sections,']')
students <- read.csv(file, stringsAsFactors=F)
selected <- students[ grep (sections, students$S.No, ignore.case=T), ]
df  <- data.frame(selected)
if (count==0 || count > nrow(df)) { # show all
  format(df[sample((1:nrow(df))), ])
}else{
  format(df[sample((1:nrow(df)), count), ])
}
