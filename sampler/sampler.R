# What: Randomly pick N students; use for making groups and questions.
# GPL(C) moshahmed_at_gmail
students <- read.csv("sampler.csv", stringsAsFactors=F)
# View(students)

sectiona <- students[grep ("^A", students$S.No), ]
sectionb <- students[grep ("^B", students$S.No), ]
sectionc <- students[grep ("^C", students$S.No), ]

df  <- data.frame(students)
dfa <- data.frame(sectiona)
dfb <- data.frame(sectionb)
dfc <- data.frame(sectionc)

pick<-4

print(sample(students$Name)) # To shuffle all the students
print(sample(students$Name,10))  # To pick 10 students

# This sorts the rows before printing, so can't shuffle.
print( students[students$S.No %in% sample(students$S.No, pick), ] )

print(  df[sample((1:nrow(df)),  pick), ] ) # students from all sections.
print( dfa[sample((1:nrow(dfa)), pick), ] ) # students from A section.
print( dfb[sample((1:nrow(dfb)), pick), ] ) # students from B section.
print( dfc[sample((1:nrow(dfc)), pick), ] ) # students from C section.

# Merge N students from each section and print the list.
format(
  merge(merge(
      dfa[sample((1:nrow(dfa)), pick), ], # students from A section.
      dfb[sample((1:nrow(dfb)), pick), ], # students from B section.
      all=T),
      dfc[sample((1:nrow(dfc)), pick), ], # students from C section.
      all=T ))
