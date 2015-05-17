### MATCHING WITH COMPOUND Broad IDs (BRD) USING "plate map for 15k"

# TO DO: extract variable from hard-coding into a list of variables
plateMapDirectory = "../platemapsfor15k"
duplicates_file
out_directory
out_file


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



### CREATE A FILTER COLUMN FOR DUPS FROM JIYOUNG'S LIST

dups = read.table(duplicates_file, sep = "\n", header = TRUE)
colnames(dups) = c("broad_id")
fullHits$is_dup = fullHits$broad_id %in% dups$broad_id 

write.table(fullHits, paste(out_directory, out_file, sep = "/"), sep="\t", quote = FALSE, row.names = FALSE, col.names = TRUE) 

