# rbind is to vertical joining of data frames (new rows) as merge is to horizontal joining (new columns)
# mean(), sd(), colnames(), as.data.frame(), read.table(), write.table()

# paste() -- can't add strings together; have to use paste(x, y, sep = )
# nchar
# grep
# grepl
# gsub (find, replaceWith, lookHere)
# match()


options(stringsAsFactors = FALSE)

# Set constants -- everything that will be hard coded

gpr_directory = "~/Desktop/gpr_data/Sanjay"
DMSO_pattern = "AB00010722"
out_directory = "~/Desktop/gpr_data"
out_file = "Sanjay_hits_automated"
zscore_cutoff = 1.7
minFreq = 4
maxFreq = 4

# Loop over input directory

gpr_master = data.frame(block = character(), column = integer(), row = integer(), 
                        id = character(), f = numeric(), b = numeric(), fdivb = numeric(), zscore = numeric() )

for( filename in list.files(gpr_directory)){
  gpr_temp = read.table(paste(gpr_directory, filename, sep = "/"), skip = 34, sep = "\t", header = TRUE)
  gpr_temp = gpr_temp[,c(1,2,3,5,9,14)]
  colnames(gpr_temp) = tolower(colnames(gpr_temp))
  gpr_temp$fdivb = gpr_temp$f635.median / gpr_temp$b635.median
  gpr_temp$zscore = (gpr_temp$fdivb - mean(gpr_temp$fdivb)) / sd(gpr_temp$fdivb)
  colnames(gpr_temp) = colnames(gpr_master)
  gpr_master = rbind(gpr_master, gpr_temp)
}

# Optional QC checks
#max(table(gpr_master$id))
#hist(table(gpr_master$id))




highZ = gpr_master[gpr_master$zscore >= zscore_cutoff,]
highZRedacted = highZ[,c("id","zscore")]

prelimHits = table(highZ$id)
Hits = as.data.frame(prelimHits)
colnames(Hits) = tolower(colnames(Hits))
colnames(Hits)[1] = "id"

# filter for only hits that appear 4x
HitsZ = merge(Hits, highZRedacted, by = "id")
HitsZ = (HitsZ[HitsZ$freq %in% range(minFreq:maxFreq), 1:3])

# filter out DMSO hits
# grepl means "grep logical", and returns TRUE or FALSE
HitsZ$DMSOfilter = grepl(HitsZ$id, pattern = DMSO_pattern)
HitsZ = HitsZ[HitsZ$DMSOfilter == FALSE,]

#aggregate zscores for hits
HitsZAgg = aggregate(zscore ~ id, data = HitsZ, FUN = "mean")
HitsZAgg$id = gsub("\n", "", HitsZAgg$id)

write.table(HitsZAgg, paste(out_directory, out_file, sep = "/"), quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)






