setwd("G:/Work data/Analyses/2016/2016_04_22_AlfalfaRelativesHybridisation/R/data")

library(stringr)
library(dplyr)

#Read .loci file line by line
lociname <- "Msativa_relatives_clust75_min10.loci"
pisfilter <- 10

linn <- readLines(lociname)

dir.create(paste0(lociname,"_PIS",pisfilter))#create folder 
setwd(paste0(lociname,"_PIS",pisfilter))#get into directory

#Read it line by line
for (i in 1:length(linn)){
  print(linn[i])
  # if line starts by "//" then count the number of "*" (PIS) in line
  if (grepl("//", linn[i]) == TRUE){
    PIS_line <- i #saves line containing "//" 
    PIS<- str_count(linn[i], "\\*") # calculate PIS
    # PIS_PERCENT<- (PIS/(nchar(gsub('\\w+\\s+', '' ,linn[i-1]))))*100 # calculate the percentage of PIS in alignment
  }
  if (grepl("//", linn[i]) == FALSE){
    next
  }
  
  # If PIS is NOT > x
  if (grepl("//", linn[i]) == TRUE & (PIS < pisfilter)){ 
    PIS_line_saved <- PIS_line #retain the previous line number containing "//" 
  }
    else{}
  
    # PIS is >= x
  if (grepl("//", linn[i]) == TRUE & (PIS >= pisfilter)){ 
    # read alignment
    if (exists("PIS_line_saved") == FALSE)
    gene <- linn[c(1:(PIS_line-1))]
    
    if (exists("PIS_line_saved") == TRUE)
    gene <- linn[c((PIS_line_saved+1):(PIS_line-1))]
    PIS_line_saved <- PIS_line #retain the previous line number containing "//" (to be used in the next loop)
    
      if (any(grepl("W64996_Jx_16", gene)) == FALSE) { #is the outgroup taxa included? if FALSE then move on in the loop
      next
      }
    #insert first line of phylip format (ntaxa basepairs)
    first_line <- paste(length(gene), #no. taxa
                     nchar(gsub('\\w+\\s+', '' ,linn[i-1])), #no. basepairs
                     sep=" ")
    #assign gene name
    fname <- paste("gene", str_extract(linn[i],"[0-9]+"), sep="")
    assign(fname, c(first_line, gene))
    
    #save phylip file
    fileConn<-file(paste0(fname,".phy"))
    writeLines(assign(fname, c(first_line, gene)), fileConn)
    close(fileConn)
    }
  else{}
  }

  

