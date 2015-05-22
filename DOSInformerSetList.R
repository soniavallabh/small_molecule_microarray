#LIST OF FULL DOS INFORMER SET WITH COMPOUND Broad IDs (BRD) USING "DOS_Informer_Set_Plate_maps"

setwd("~/Desktop/gpr_data")

plateMapDirectory = "DOS_Informer_Set_Plate_maps"

#initialize data frame "master"
master = data.frame(plate_map_name = character(), well_position = character(), broad_sample = character(), mg_per_ml = numeric(), nmoles_per_liter = numeric(), solvent = character(), plate_name = character())

# use grepl to select only txt files, not xlsx files

for (filename in list.files(plateMapDirectory)) {
  if(grepl(".*\\.txt", filename)) {
      plate = read.table(paste (plateMapDirectory, filename, sep = "/"), sep = "\t", header = TRUE)
      plate_name = gsub("_.*", "", filename)
      plate$plate_name = plate_name
      master = rbind(master, plate)
}}

write.table(master, "DOS_informer_set_full_list", sep="\t", quote = FALSE, row.names = FALSE, col.names = TRUE) 







