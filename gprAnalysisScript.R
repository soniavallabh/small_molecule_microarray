# rbind is to vertical joining of data frames (new rows) as merge is to horizontal joining (new columns)
# mean(), sd(), colnames(), as.data.frame(), read.table(), write.table()

# paste() -- can't add strings together; have to use paste(x, y, sep = )
# nchar
# grep
# grepl
# gsub (find, replaceWith, lookHere)
# match()


options(stringsAsFactors = FALSE)

setwd("~/Desktop/gpr_data/Slide_Batch_3_2015.05.05")

# Read in txt files made from gpr files; join duplicates

rep1 = read.table("14403620_Set1.txt", skip = 34, sep = "\t", header = TRUE)
rep2 = read.table("14403621_Set1.txt", skip = 34, sep = "\t", header = TRUE)
##rep1 = read.table("14378376_Set2.txt", skip = 34, sep = "\t", header = TRUE)
##rep2 = read.table("14378377_Set2.txt", skip = 34, sep = "\t", header = TRUE)
#rep1 = read.table("14378324_Set3.txt", skip = 34, sep = "\t", header = TRUE)
#rep2 = read.table("14378325_Set3.txt", skip = 34, sep = "\t", header = TRUE)

rep1 = rep1[,c(1:5,9,14)]
rep2 = rep2[,c(1:5,9,14)]
totalData = rbind(rep1, rep2)
colnames(totalData) = tolower(colnames(totalData))

# Calculate and filter on zscore

totalData$fdivb = totalData$f635.median / totalData$b635.median

totalData$zscore = (totalData$fdivb - mean(totalData$fdivb)) / sd(totalData$fdivb)

# write.table(totalData, "batch3set1_no_filter.txt", sep="\t")



highZ = totalData[totalData$zscore >= 1.7,]
highZRedacted = highZ[,c(5,9)]

prelimHits = table(highZ$id)
Hits = as.data.frame(prelimHits)
colnames(Hits) = tolower(colnames(Hits))
colnames(Hits)[1] = "id"

# filter for only hits that appear 4x
HitsZ = merge(Hits, highZRedacted, by = "id")
HitsZ = (HitsZ[HitsZ$freq == 4, 1:3])
# filter out DMSO hits
# grepl means "grep logical", and returns TRUE or FALSE
#HitsZ$DMSOfilter = grepl(HitsZ$id, pattern = "AB00010722")
#HitsZ = HitsZ[HitsZ$DMSOfilter == FALSE,]

#aggregate zscores for hits
# MAKE THIS A MEAN OF Z SCORES RATHER THAN THE SUM
# HitsZAgg = aggregate(zscore ~ id, data = HitsZ, FUN = sum)
HitsZAgg = aggregate(zscore ~ id, data = HitsZ, FUN = mean)

# write out txt files of hits for each drug set
#write.table(HitsZAgg, "batch3set1.txt", sep="\t")
##write.table(HitsZAgg, "batch3set2.txt", sep="\t")
#write.table(HitsZAgg, "batch3set3.txt", sep="\t")



## PASTING TOGETHER ALL HITS FROM A SLIDE BATCH

HitsZAgg1 = read.table("batch3set1.txt", sep = "\t", header = TRUE)
HitsZAgg2 = read.table("batch3set2.txt", sep = "\t", header = TRUE)
HitsZAgg3 = read.table("batch3set3.txt", sep = "\t", header = TRUE)

fullHits = rbind(HitsZAgg1, HitsZAgg2, HitsZAgg3)


#write.table(fullHits, "batch3_fullHits.txt", sep="\t") 
# Double check whether fullHits contains the right number of rows:
#nrow(fullHits) == (nrow(HitsZAgg1) + nrow(HitsZAgg2) + nrow(HitsZAgg3))


### MATCHING WITH COMPOUND Broad IDs (BRD) USING "plate map for 15k"

plateMapDirectory = "../platemapsfor15k"

# initialize data frame "master"
master = data.frame(plate_map_name = character(), well_position = character(), broad_sample = character(), mg_per_ml = numeric(), nmoles_per_liter = numeric(), solvent = character(), plate_name = character())

# loop to read in all of the files in the directory, and populate columns of "master"
for (filename in list.files(plateMapDirectory)) {
  plate = read.table(paste (plateMapDirectory, filename, sep = "/"), sep = "\t", header = TRUE)
  plate_name = gsub("_.*", "", filename)
  plate$plate_name = plate_name
  master = rbind(master, plate)
}

# use gsub to create new columns with just plate and well identifiers 
fullHits$id = gsub("\n", "", fullHits$id)
fullHits$plate_name = gsub("\n", "", gsub(".* ", "", fullHits$id)) 
fullHits$well_number = gsub(" .*", "", gsub(".*-","", fullHits$id))

# Convert all well numbers to a 3-character format
fullHits$well_number_fixed = fullHits$well_number
fullHits$well_number_fixed[nchar(fullHits$well_number) == 2] = paste(substr(fullHits$well_number[nchar(fullHits$well_number) == 2], 1, 1), substr(fullHits$well_number[nchar(fullHits$well_number) == 2], 2, 2), sep = "0")

# paste plate and well identifiers together using "_" to create one identifier consistent across data frames
fullHits$plate_well_id = paste(fullHits$plate_name, fullHits$well_number_fixed, sep = "_")
master$plate_well_id = paste(master$plate_name, master$well_position, sep = "_")

# match on identifier to link old id with broad id
fullHits$broad_sample = master$broad_sample[match(fullHits$plate_well_id, master$plate_well_id)]

# write.table(fullHits, "batch3_fullHits.txt", sep="\t", quote = FALSE, row.names = FALSE) 


### CREATE A FILTER COLUMN FOR DUPS FROM JIYOUNG'S LIST

jiyoung_dups = read.table("jiyoung_dups.txt", sep = "\n", header = TRUE)
fullHits$jiyoung_dups = jiyoung_dups$jiyoung_dups[match(fullHits$broad_sample, jiyoung_dups$jiyoung_dups)]  

write.table(fullHits, "batch3_fullHits.txt", sep="\t", quote = FALSE, row.names = FALSE) 

