# This script will plot the individual observations of transcripts
# that were found differentially expressed between two groups

library (dplyr)
library (ggplot2)
library(wesanderson)

  setwd ("/Users/hsyed/Desktop/Score Boxplot//")
# Indicate the path of the file containing the tpm data
  
file <- file.path(getwd(), 'results.tsv') 
data <- read.table(file, header = T, row.names = 1, sep = "\t")

# introduce path of metadata file
metadata <- file.path(getwd(), 'metadata.tsv')
sampleInfo <- read.table(metadata, header = T, sep = "\t")


# Import a list of transcripts IDs that were found differentially expressed as above
transc_ids <- read.table ('gene list.txt', header = F, sep = "\t")

counter = 0

for (i in 1:nrow(transc_ids)) {
	  
	  counter = counter + 1
  trID <- toString(transc_ids [i,])

    match <- grep (trID, readLines(file), value = T)
    
    
    counts <- strsplit (match, "\t") [[1]]
      
      df = data.frame (
		           sampleNames <- sampleInfo$target_id,
			       condition <- sampleInfo$group,
			       labels <- sampleInfo$label,
			           counts <- as.numeric(counts[2:length(counts)])
			         )
      

      p <- ggplot(df, aes(factor(condition), counts))   
        fileName <- paste (trID, "pdf", sep = ".")  
        pdf (file = fileName)
	  print (p + geom_boxplot(notch=F,  aes(fill = factor(condition))) + 
		            theme_bw() +
			               # scale_fill_manual(values=wes_palette(n=6, name="IsleofDogs1")) +
			               geom_jitter(position = position_jitter(height = .2, width = .2), cex = 2, pch=21, col="black", bg="gray") +
				                  geom_text(aes(label=labels,hjust=0, vjust=1.5)) +
						             ggtitle (trID) +
							                xlab("Condition") +
									           ylab("Abundance (TPM)") +
										              # Control the text features
										              theme( axis.text.x = element_text(colour="black",size=15,angle=0,hjust=.5,vjust=.5,face="plain"),
												                      axis.text.y = element_text(colour="black",size=15,angle=0,hjust=1,vjust=0,face="plain"),  
														                        axis.title.x = element_text(colour="black",size=15,angle=0,hjust=.5,vjust=0,face="bold"),
														                        axis.title.y = element_text(colour="black",size=15,angle=90,hjust=.5,vjust=.5,face="bold"))
			             
			             
			             
			      )  
	  dev.off()
	    cat(counter, " figures created", "\n")
}






