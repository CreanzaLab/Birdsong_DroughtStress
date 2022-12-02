# For processing Chipper output data from Reese's analysis code
# Kate Snyder
# 5/17/2021
# 7/23/2021 - changed file read into dfIn
# 6/10/2022 - changed files, added onsets/offsets, saved BoutNum as its own column
# 6/12/2022 - changed files, removed bouts.parsed from output, str_remove "Melospiza-melodia" where necessary
# 6/20/2022 - complex metadata file, now will look for those additional columns, so may need to edit that if going back to Simple matadata; added date conversion at end and Region assignment and new Era calc
# 7/14/2022 - got rid of some repetitive commented-out lines, added a couple comments, expanded date conversion correction
# 7/26/2022 - eachnote; 7/28 - note_sizes
# 7/31/2022 - added onsets_rescaled, offsets_rescaled to columns; ISSUE: Date Recorded coming out as NA - fixed?  
# 8/1/2022 - found that enddf$Longitude <  76.3 was missing the "-"; potentially included excess recordings to the east in Drought assignment - fixed; Added "Remarks" from metadata; Rearranged SyllNum syll_pattern_ID in first output df
# 8/2/2022 - realized that the note assignment still was still using SyllDF$onsets_rescaled even after I changed the column names to just "onsets"; corrected
# 8/23/22 - made drought area a little larger

# Reese's Chipper analysis spits out a .txt file
#   Open it in excel
#   Save it as .xls or .xlsx (e.g. AnalysisOutput_20210723_T160644_highfreqOutliersRedone.xls)
#   Find-Replace to get rid of start bracket and end bracket ("[" and "]")
#   Remove extra underscores in File Names (e.g. SegSyllsOutput_1707020_058S12_bout2.gzip) --> ...1707020058S12..
#   Save as excel (e.g. AnalysisOutput_20210723_T160644_highfreqOutliersRedone_nobrackets_removedExtraUnderscores.xls)


library(stringr)
library(readxl)
library(qdapRegex)
require(dplyr)

#setwd("~/Desktop/Ithaca Juncos")
setwd("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Gzip Files/Song Sparrow_KTS_Aggregated072022")
#setwd("/Users/kate/Documents/Creanza Lab/Ithaca Files/Individual Note Analyses")

## Song Sparrows
dfIn <- read_excel("AnalysisOutput_20220801_T232505.xlsx")
filelist <- list.files(pattern = ".gzip")
#dfIn <- dfIn[which(dfIn$FileName %in% filelist),]
#dfIn <- read_excel("AnalysisOutput_20220730_T152940.xlsx")
#metadata <- read.csv("Simple-SOSP-metadata_all-sources_20220612.csv")
metadata <- read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Reference Sheets/Song Sparrow/2022-07-30_SOSP_Complex_Metadata_addedJuvSong.csv")
species = "SOSP"


## Juncos
setwd("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Gzip Files/Dark Eyed Junco_KTS_Aggregated062022")
dfIn <- read_excel("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Gzip Files/Dark Eyed Junco_KTS_Aggregated062022/AnalysisOutput_20220827_T173633_edited.xlsx")
metadata <- read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Reference Sheets/Dark Eyed Junco/2022-08-12_DEJU_Complex_Metadata_SelfRecsCoordinates_Behavs_Added.csv")
colnames(metadata)[which(colnames(metadata)=="County.x")] <- "County"
species = "DEJU"
# metadata1 <- read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Reference Sheets/Dark Eyed Junco/Junco Reference Sheet.csv")
# MLmeta <- read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Reference Sheets/Dark Eyed Junco/ML__2022-06-14T20-57_daejun_audio_AddedCols_plusMLdeleted_sub90Wlon.csv")
# XCmeta <- read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Reference Sheets/Dark Eyed Junco/2022-06-14_XC_metadata_Junco-hyemalis_All_AddedCols.csv")
# cols <- c("FileName",	"Date.Collected",	"County",	"Era",	"Drought.Level")
# metadata <- rbind(metadata1[,cols],MLmeta[,cols],XCmeta[,cols])
# write.csv(metadata,"Simple-DEJU-metadata_all-sources_20220614.csv")
# NotDuped <- metadata[!duplicated(metadata$FileName),]
# write.csv(NotDuped,"Simple-DEJU-metadata_all-sources_noDups_20220614.csv")

splits <- str_split(dfIn$FileName, "_")

dfIn$FileName <- str_remove(dfIn$FileName,"Melospiza-melodia-")

#check that all processed files are present in the metadata - no output is good
for (i in 1:length(splits)) {
  if (str_remove(splits[[i]][2],"Melospiza-melodia-") %in% str_remove(metadata$FileName,"Melospiza-melodia-")) {
    dummy <- NA
  } else { 
    print(splits[[i]][2])
  }
}

for (i in 1:length(splits)) {
  if (str_remove(splits[[i]][2],"Junco-hyemalis-") %in% str_remove(metadata$FileName,"Junco-hyemalis-")) {
    dummy <- NA
  } else { 
    print(splits[[i]][2])
  }
}
dfIn$FileName <- str_replace(dfIn$FileName,"-600770-","-600770_")



#dfIn <- dfIn[1:100,]
columns <- c("All_Syllables_syllable_duration(ms)", "All_Syllables_sylls_Upper_Freq_(Hz)" , "All_Syllables_sylls_Lower_Freq_(Hz)" , "All_Syllables_sylls_freq_modulation(Hz)","onsets","offsets", "onsets_rescaled", "offsets_rescaled")
BoutLevelStatColumns <- c("bout_duration(ms)", "num_syllables", "num_syllable_per_bout_duration(1/ms)", "largest_syllable_duration(ms)", "smallest_syllable_duration(ms)", "avg_syllable_duration(ms)", "std_syllable_duration(ms)", "largest_silence_duration(ms)", "smallest_silence_duration(ms)", "avg_silence_duration(ms)", "std_silence_duration(ms)", "num_unique_syllables", "num_syllables_per_num_unique", "sequential_repetition", "syllable_stereotypy", "mean_syllable_stereotypy", "std_syllable_stereotypy", "avg_sylls_upper_freq(Hz)", "avg_sylls_lower_freq(Hz)", "max_sylls_freq(Hz)", "min_sylls_freq(Hz)", "overall_sylls_freq_range(Hz)", "largest_sylls_freq_modulation(Hz)", "smallest_sylls_freq_modulation(Hz)", "avg_sylls_freq_modulation(Hz)", "std_sylls_freq_modulation(Hz)", "num_notes", "num_notes_per_syll", "ms_per_pixel")
eachnote_columns <- c("All_Syllables_note_duration(ms)",	"All_Syllables_notes_Upper_Freq_(Hz)",	"All_Syllables_notes_Lower_Freq_(Hz)", "All_Syllables_notes_freq_modulation(Hz)", "note_lengths",	"note_shapes", "note_sizes",	"note_onsets",	"note_offsets", "note_onsets_unrescaled",	"note_offsets_unrescaled")


#### Start each-syllable pulling out ----
enddf <- set.seed(10)

for (i in 1:length(dfIn$FileName)) {    # want this to go through each line, pull out metadata, feed vector into below for loop
  testvec <- dfIn[i,]
  FileName1 <- testvec$FileName
  
  BoutLevelStats <- testvec[BoutLevelStatColumns]
  
  filenamesplit <- str_split(FileName1, "_")
  OGaudiofilenum <- filenamesplit[[1]][2]
  boutnum <- str_remove(filenamesplit[[1]][length(filenamesplit[[1]])], pattern = ".gzip")
  boutnum <- str_remove(boutnum, pattern = "bout")
  
  #metadatarow <- metadata[which(metadata$FileName == str_remove(OGaudiofilenum,"Melospiza-melodia-")),]
  metadatarow <- metadata[which(metadata$FileName == str_remove(OGaudiofilenum,"Junco-hyemalis-")),]
  DateRecorded <- metadatarow$Date.Collected
  County <- metadatarow$County
  Era <- metadatarow$Era
  DroughtLevel <- metadatarow$Drought.Level
  Longitude <- metadatarow$Longitude
  Latitude <- metadatarow$Latitude
  Time <- metadatarow$Time
  State <- metadatarow$State
  Source <- metadatarow$Source
  Remarks <- metadatarow$Remarks
  #Bouts.Parsed <- metadatarow$Bouts.Parsed
  
  Chipper_numsylls <- testvec$num_syllables
  
  syllable_pattern <- str_split(testvec$syllable_pattern, ", ")
  syllable_pattern_ID <- syllable_pattern[[1]]
  
  outdf <- set.seed(10)
  
  for (j in 1:length(columns)) { # want this to go through each column (cell) w/ multisyllable data, separating data
    column <- columns[j]
    tempcell <- testvec[column]
    testvec2 <- str_split(tempcell, "\r\n ")
    testvec22 <- testvec2[[1]]
    
    forout <- set.seed(10)
    
    for (k in 1:length(testvec22)) {  # goes through each object created by the prev str_split 
      tempout <- str_split(testvec22[k], " ")
      forout <- c(forout, tempout[[1]])  #vector of each syllable's measurement value
    }
    
    newvec <- as.numeric(forout)
    newvec2 <- na.omit(newvec)
    numsylls <- length(newvec2)
    print(paste(FileName1, column, numsylls, Chipper_numsylls, length(syllable_pattern_ID)))
    syllnum <- 1:numsylls
    
    if (j == 1) {
      FileName <- rep(FileName1, times = numsylls)
      OGfile <- rep(OGaudiofilenum, times = numsylls)
      outdf <- cbind(FileName, OGaudiofilenum, boutnum, syllnum, syllable_pattern_ID, DateRecorded, Latitude, Longitude, Time, State, County, Source, Era, DroughtLevel, Remarks, BoutLevelStats, newvec2)
     # print(outdf[1,])
    } else {
      outdf <- cbind(outdf, newvec2)
    }
    
  } # end for j
  colnames(outdf) <- c("File", "RecordingNum", "BoutNum", "SyllNum", "syllable_pattern_ID","DateRecorded", "Latitude", "Longitude", "Time", "State", "County", "Source", "Era", "DroughtLevel", "Remarks", BoutLevelStatColumns, columns)
  enddf <- rbind(enddf, outdf)

  colnames(enddf) <- c("File", "RecordingNum", "BoutNum", "SyllNum", "syllable_pattern_ID","DateRecorded", "Latitude", "Longitude", "Time", "State", "County", "Source", "Era", "DroughtLevel", "Remarks", BoutLevelStatColumns, columns)
  
} # end for i (pull out each syll)

enddfSyll <- as.data.frame(enddf)

# MUST ONLY RUN THESE TWO LINES ONCE
colnames(enddfSyll)[which(colnames(enddfSyll) %in% c("onsets","offsets"))] <- c("onsets_unrescaled", "offsets_unrescaled")
colnames(enddfSyll)[which(colnames(enddfSyll) %in% c("onsets_rescaled","offsets_rescaled"))] <- c("onsets", "offsets")

enddfSyll$DateRecorded <- as.Date(enddfSyll$DateRecorded, format = "%Y-%m-%d")   #as.Date(enddfSyll$DateRecorded, format = "%m/%d/%Y") # reformatted 7/31/2022, not sure why the metadata changed in new ref file
enddfSyll$DateRecorded <- str_replace(enddfSyll$DateRecorded,"^000","200")
enddfSyll$DateRecorded <- str_replace(enddfSyll$DateRecorded,"^001","201")
enddfSyll$DateRecorded <- str_replace(enddfSyll$DateRecorded,"^002","202")
enddfSyll$DateRecorded <- str_replace(enddfSyll$DateRecorded,"^009","199")
enddfSyll$DateRecorded <- str_replace(enddfSyll$DateRecorded,"^008","198")
enddfSyll$DateRecorded <- str_replace(enddfSyll$DateRecorded,"^007","197")
enddfSyll$DateRecorded <- str_replace(enddfSyll$DateRecorded,"^006","196")
enddfSyll$DateRecorded <- str_replace(enddfSyll$DateRecorded,"^005","195")
enddfSyll$DateRecorded <- str_replace(enddfSyll$DateRecorded,"^004","194")
enddfSyll$DateRecorded <- str_replace(enddfSyll$DateRecorded,"^003","193")
enddfSyll$DateRecorded <- as.Date(enddfSyll$DateRecorded, format = "%Y-%m-%d")
Datedf <- data.frame(year = as.numeric(format(enddfSyll$DateRecorded, format = "%Y")),
                           month = as.numeric(format(enddfSyll$DateRecorded, format = "%m")),
                           day = as.numeric(format(enddfSyll$DateRecorded, format = "%d")))
enddf <- cbind(enddfSyll, Datedf)


if (species == "SOSP") {
SyllDurOutlierAnalysis <- read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Reference Sheets/Song Sparrow/2022-06-19_SOSP_OutlierAnalysisLibrary_SyllDurationsOver600.csv") # "Omit" column seems inconsistent?
#HighFreqOutlierList <- read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Reference Sheets/Song Sparrow/2022-06-19_SOSP_OutlierAnalysisLibrary_HighFreq.csv")

enddf2 <- enddf

require(tidyr)
## do want this but right now it's making something weird
# SyllDurOutlierAnalysis$File.Name <- str_remove(SyllDurOutlierAnalysis$File.Name,"bout")
# SyllDurOutlierAnalysis <- separate(SyllDurOutlierAnalysis,File.Name,into=c("RecNum","Bout"), sep="_")
# SyllDurOutliersToss <- SyllDurOutlierAnalysis[which(SyllDurOutlierAnalysis$Omit=="1"),]
# SyllDurOutlierFile <- rep(0,length(enddf$File))
# SyllDurOutlierFile[which(enddf$RecordingNum %in% SyllDurOutliersToss$RecNum & enddf$BoutNum %in% SyllDurOutliersToss$Bout)] <- 1
# sum(SyllDurOutlierFile)
# enddf <- cbind(enddf, SyllDurOutlierFile)
} # end if species == SOSP

#Set Era
EraYear <- set.seed(10)
EraYear[enddf$year < 2006] <- "Pre2006"
EraYear[enddf$year >= 2006 & enddf$year < 2016] <- "Pre" 
EraYear[enddf$year == 2016 & enddf$month < 7] <- "Pre" # added 8/11/22, changed from just "2016"
EraYear[enddf$year == 2016 & enddf$month >= 7] <- "During"
EraYear[enddf$year >= 2017] <- "Post"
EraYear[enddf$year >= 2020] <- "2020-2021"
#enddf <- cbind(enddf, Era)
enddf$Era <- EraYear
enddf1 <- enddf
#enddfSub <- enddf %>% group_by(RecordingNum) %>% sample_n(1)
#enddf <- enddfSub

#Set Region
if (species == "SOSP") {
enddfRectControl <- enddf[which(enddf$Longitude < -72.5 & enddf$Longitude > -75 & enddf$Latitude < 42 & enddf$Latitude > 40),]
enddfRectControl %>% group_by(Era) %>% 
  summarise(nBirds = n())
ControlRecs <- unique(enddfRectControl$RecordingNum)
} else if (species == "DEJU") {
  enddfRectControl <- enddf[which(enddf$Latitude > 42 & enddf$Latitude < 44.6 & enddf$Longitude > -74.5 & enddf$Longitude < -72),]  # changed 8/24/22, LonMin -74.4 --> -74.5 (gain 1 more Control-Pre, 2012)
  enddfRectControl %>% group_by(Era) %>% 
    summarise(nBirds = n())
  ControlRecs <- unique(enddfRectControl$RecordingNum)
}

enddfRectDrought <- enddf[which(enddf$Longitude <  -75.45 & enddf$Longitude > -78.9 & enddf$Latitude < 43.2 & enddf$Latitude > 42.2),]   # Discovered 8/1/22 that enddf$Longitude <  76.3 was missing the "-"; potentially included excess recordings to the east  # 8/12/22 made -76.3 --> -76.2 to get another junco rec
# 8/23/22 - max longitude -76.2 --> -75.45, max latitude 43.1 --> 43.2
enddfRectDrought %>% group_by(Era) %>% 
  summarise(nBirds = n())
DroughtRecs <- unique(enddfRectDrought$RecordingNum)
# xmin = -78.80951, xmax = -76.30545, ymin = 42.27741, ymax = 43.07931)) 

Region <- rep(NA,length(enddf1$File))
Region[which(enddf1$RecordingNum %in% ControlRecs)] <- "Control"
Region[which(enddf1$RecordingNum %in% DroughtRecs)] <- "Drought"
sum(Region == "Control", na.rm=T)
enddfRegion <- cbind(enddf1,Region)


filelist <- list.files()
write.csv(enddfRegion, file = paste0(Sys.Date(), "_",species,"_syllables-and-bout-stats_plusMetadata_from8-27_Analysis.csv"))


#### NOTES - using RESCALED ("note_onsets") as of 7/31, using rescaled "onsets" as of 8/2 ----
problemfiles <- set.seed(10)
enddf = set.seed(10)
for (i in 1:length(dfIn$FileName)) {    # want this to go through each line, pull out metadata, feed vector into below for loop
  testvec <- dfIn[i,]
  FileName1 <- testvec$FileName
  
  SyllDF = enddfRegion[which(enddfRegion$File == FileName1),]  
  
  #BoutLevelStats <- testvec[BoutLevelStatColumns]
  
  filenamesplit <- str_split(FileName1, "_")
  OGaudiofilenum <- filenamesplit[[1]][2]
  boutnum <- str_remove(filenamesplit[[1]][length(filenamesplit[[1]])], pattern = ".gzip")
  boutnum <- str_remove(boutnum, pattern = "bout")
  
  metadatarow <- metadata[which(metadata$FileName == str_remove(OGaudiofilenum,"Melospiza-melodia-")),]
  metadatarow <- metadata[which(metadata$FileName == str_remove(OGaudiofilenum,"Junco-hyemalis-")),]
  DateRecorded <- metadatarow$Date.Collected
  County <- metadatarow$County
  Era <- metadatarow$Era
  DroughtLevel <- metadatarow$Drought.Level
  Longitude <- metadatarow$Longitude
  Latitude <- metadatarow$Latitude
  Time <- metadatarow$Time
  State <- metadatarow$State
  Source <- metadatarow$Source
  ms_per_pixel <- SyllDF$ms_per_pixel[1]
  #Bouts.Parsed <- metadatarow$Bouts.Parsed
  
  Chipper_numnotes <- testvec$num_notes
  
  #print("Checkpoint1")
  #print(FileName1)
  #print(Chipper_numnotes)
  
  outdf <- set.seed(10)
  
  for (j in 1:length(eachnote_columns)) { # want this to go through each column (cell) w/ multiNOTE data, separating data
    column <- eachnote_columns[j]
    tempcell <- testvec[column]
    testvec2 <- str_split(tempcell, "\r\n")
    testvec22 <- testvec2[[1]]
    testvec3 <- testvec22
    
    
    forout <- set.seed(10)
    
    for (k in 1:length(testvec3)) {  # goes through each object created by the prev str_split 
      tempout <- str_split(testvec3[k], " ")
      forout <- c(forout, tempout[[1]])  #vector of each syllable's measurement value
    }
    forout=forout[which(forout != "")]
    
    if (column != "note_shapes") {
    newvec <- as.numeric(forout)
    newvec2 <- na.omit(newvec)
    } else {
      newvec2 = str_remove_all(forout,"'")
    }
    numnotes <- length(newvec2)
    #print(paste(FileName1, column, numnotes, Chipper_numnotes))
    notenum <- 1:numnotes
    
    if (j == 1) {
      FileName <- rep(FileName1, times = numnotes)
      OGfile <- rep(OGaudiofilenum, times = numnotes)
      outdf <- cbind(FileName, OGaudiofilenum, ms_per_pixel, boutnum, DateRecorded, Latitude, Longitude, Time, State, County, Source, Era, DroughtLevel, notenum, newvec2)
      # print(outdf[1,])
    } else {
      outdf <- cbind(outdf, newvec2)
    }
    
  } # end for j
  colnames(outdf)[which(colnames(outdf) == "newvec2")] <- eachnote_columns
  outdf <- as.data.frame(outdf)
  
  #print("starting syll assignment")
  #print(length(outdf$note_onsets))
  
  # Use SyllDF to get syll number that each note belongs to
  SyllNum = rep(NA,times = length(outdf$note_onsets))
  SyllOnset = rep(NA,times = length(outdf$note_onsets))
  SyllOffset = rep(NA,times = length(outdf$note_onsets))
  #print(SyllDF$onsets)
  for (k in 1:length(outdf$note_onsets)) {
    whichsyllrow=which(as.numeric(outdf$note_onsets[k]) < SyllDF$offsets & as.numeric(outdf$note_offsets[k]) > SyllDF$onsets)
    if (length(whichsyllrow)==0) {
      whichsyll = NA
      whichsyllonset= NA
      whichsylloffset= NA
      #print(paste("Note", outdf$NoteNum[k],"not assigned"))
    } else if (length(whichsyllrow)==1) {
      whichsyll=SyllDF$SyllNum[whichsyllrow]
      whichsyllonset=SyllDF$onsets[whichsyllrow]
      whichsylloffset=SyllDF$offsets[whichsyllrow]
    } else {
      print("this note is in more than one syllable??")
      print(FileName1)
      print(outdf$note_onsets[k])
      print(outdf$note_offsets[k])
      print(SyllDF$onsets)
      print(SyllDF$offsets)
      print(SyllDF$onsets[whichsyllrow])
      print(SyllDF$offsets[whichsyllrow])
      problemfiles <- c(problemfiles, FileName1)
      whichsyll=paste(SyllDF$SyllNum[whichsyllrow], collapse = " ")
      whichsyllonset=paste(SyllDF$onsets[whichsyllrow], collapse = " ")
      whichsylloffset=paste(SyllDF$offsets[whichsyllrow], collapse = " ")
    }
    SyllNum[k]=whichsyll
    SyllOnset[k]=whichsyllonset
    SyllOffset[k]=whichsylloffset
  }
  outdf=cbind(outdf, SyllOnset, SyllOffset, SyllNum)
  
  #print(FileName1)
  #print(sort(unique(outdf$SyllNum)))
  #print(length(sort(unique(outdf$SyllNum))))
  #print(max(sort(unique(outdf$SyllNum))))
  
  colnames(outdf) <- c("File", "RecordingNum", "ms_per_pixel","BoutNum","DateRecorded", "Latitude", "Longitude", "Time", "State", "County", "Source", "Era", "DroughtLevel", "NoteNum", eachnote_columns, "SyllOnset","SyllOffset","SyllNum")
  enddf <- rbind(enddf, outdf)
  
  colnames(enddf) <- c("File", "RecordingNum", "ms_per_pixel","BoutNum","DateRecorded", "Latitude", "Longitude", "Time", "State", "County", "Source", "Era", "DroughtLevel", "NoteNum", eachnote_columns, "SyllOnset","SyllOffset","SyllNum")
  
} # end for i
enddfNotes <- as.data.frame(enddf)
write.csv(enddfNotes, paste0(Sys.Date(),"_",species,"_IndividualNoteAnalysis_useRescaledOnsOffs_from8-01_Analysis_ExpandedRegions.csv"))

enddfNotes[which(str_detect(enddfNotes$SyllNum," ")),]

# get rid of any notes that are not part of the syllable - either bc they are totally outside syllable or they are cut off by syll onset or offset - does this do anything anymore? - maybe I do want the NA notes removed after all? I can do that way faster though...
FileSyllNums=unique(enddfNotes[,c("File","SyllNum")])
enddfNotesUnite <- unite(data = enddfNotes, col = "FileSyllNum", c("File","SyllNum"), sep="_syll", remove=F)
anotherdf = set.seed(10)
for (i in 1:length(FileSyllNums$File)) {
  tempfile = FileSyllNums$File[i]
  tempsyll = FileSyllNums$SyllNum[i]
  tempdf = enddfNotes[which(enddfNotes$File == tempfile & enddfNotes$SyllNum == tempsyll),]
  print(tempfile)
  print(length(tempdf$File))
  if (length(tempdf$File) > 1) {
   # tempdf=tempdf[which(tempdf$note_onsets >= tempdf$SyllOnset | tempdf$note_offsets == tempdf$note_offsets),] # if any notes begin before the syllable onset but aren't clearly supposed to be there by having it define the offset, don't keep  - commented out 8/2
  #  tempdf=tempdf[which(tempdf$note_onsets == tempdf$SyllOnset | tempdf$note_offsets <= tempdf$note_offsets),] # if any notes end after the syllable offset but aren't clearly supposed to be there by having it define the onset, don't keep - commented out 8/2
    tempdf <- tempdf[!which(is.na(tempdf$SyllNum))] #just keep all notes that aren't NA 
  }
  anotherdf <- rbind(anotherdf,tempdf)
} # end note df refinement
anotherdf=enddfNotesUnite # Date unknown: seems the above for loop doesn't do what I want... cuts down from # notes length to # sylls (approx); 
note_thickness=as.numeric(anotherdf$note_sizes)/as.numeric(anotherdf$note_lengths)
anotherdf=cbind(anotherdf,note_thickness)
write.csv(anotherdf, paste0(Sys.Date(),"_IndividualNoteAnalysis_onlyInclNotes2.csv"))

#### Note summaries by syll ----
 enddfNotes <- anotherdf #; maybe not - 8/2... except maybe? bc note_thickness
# want to get amount of silence within syllable - getting closer to working
newdfout = set.seed(10)
for (i in 1:length(unique(enddfNotes$File))) { # go through each bout
  tempfile = unique(enddfNotes$File)[i]
  tempdf = enddfNotes[which(enddfNotes$File==tempfile),]
  percentSound = set.seed(10)
  oneboutout = set.seed(10)
  for (j in 1:length(unique(tempdf$SyllNum))) {  # go through each syllable
    #tempsyll = unique(tempdf$SyllNum)[j]
    tempsyll = j
    if (!is.na(tempsyll) & !str_detect(tempsyll," ")) {  # when tempsyll = j, this is irrelevant
      tempsylldf = tempdf[which(tempdf$SyllNum == tempsyll | str_detect(tempdf$SyllNum, paste0(" ",j,"$")) | str_detect(tempdf$SyllNum, paste0("^",j," ")) | str_detect(tempdf$SyllNum, paste0(" ",j," "))),]
      if (length(tempsylldf$File) > 0) {  # if non-empty syllable
        noteSound = set.seed(10)
        for (k in 1: length(tempsylldf$note_onsets)) {  # collect time pixels from each note
          onenoteSoundPixels=tempsylldf$note_onsets[k]:tempsylldf$note_offsets[k]
          noteSound <- c(noteSound,onenoteSoundPixels)
        }
        
        #syllPixels = tempsylldf$SyllOnset[1]:tempsylldf$SyllOffset[1]  # replaced the below when I wasn't including notes across multiple sylls
        
        # if (str_detect(tempsylldf$SyllNum," ")) {   # included this when I was getting notes across multiple syllables, pulls in enddfRegion froom a while ago
           tempsyllrow <- enddfRegion[which(enddfRegion$File == tempfile & enddfRegion$SyllNum == tempsyll),]
           syllPixels <- tempsyllrow$onsets[1]:tempsyllrow$offsets[1]
        # } else {
        #   syllPixels = tempsylldf$SyllOnset[1]:tempsylldf$SyllOffset[1]
        # }
        percentSound=sum(syllPixels %in% noteSound)/length(syllPixels)
        SilencePositions = which(!syllPixels %in% noteSound)
        if (length(SilencePositions) != 0) {
          FirstSilencePosition=which(!syllPixels %in% noteSound)[1]/length(syllPixels)
          LastSilencePosition=which(!syllPixels %in% noteSound)[length(which(!syllPixels %in% noteSound))]/length(syllPixels)
        } else {
          FirstSilencePosition=1  # changed from NA 8/2/22
          LastSilencePosition=1
        }
      } else { # if NOT (length(tempsylldf$File) > 0)  (aka if an empty syllable)
        tempsylldf = tempdf[1,c("File","RecordingNum","BoutNum","SyllNum")]
        percentSound=NA
        FirstSilencePosition=NA
        LastSilencePosition=NA
      }
    } else {  # if not (!is.na(tempsyll) & !str_detect(tempsyll," "))
      tempsylldf = tempdf[1,c("File","RecordingNum","BoutNum","SyllNum")]
      tempsylldf[1,4] = NA 
      percentSound=NA
      FirstSilencePosition=NA
      LastSilencePosition=NA
    } # end if (!is.na(tempsyll) & !str_detect(tempsyll," "))
    tempdfout=cbind(tempsylldf[1,c("File","RecordingNum","BoutNum","SyllNum")], percentSound, FirstSilencePosition,LastSilencePosition)
    oneboutout = rbind(oneboutout,tempdfout)
  }  # end go through each syllable
  newdfout = rbind(newdfout,oneboutout)
}  # end go through each bout
newdfoutUnite <- unite(data = newdfout, col = RecBoutSyll_ID, c(RecordingNum,BoutNum,SyllNum), sep="_", remove = FALSE)

## Back to syllable-level stats incorporating notes
testunite <- unite(data = enddfNotes,col = RecBoutSyll_ID, c(RecordingNum,BoutNum,SyllNum), sep="_", remove = FALSE)
testunite$note_lengths <- as.numeric(testunite$note_lengths)
testunite$note_sizes <- as.numeric(testunite$note_sizes)
rm(note_thickness)
SyllNoteInfo = testunite %>% group_by(RecBoutSyll_ID,RecordingNum, BoutNum, SyllNum) %>% summarize(nNotes=n(), MaxNoteThickness = max(note_thickness), MinNoteThickness = min(note_thickness), MaxNoteSize = max(note_sizes), MinNoteSize = min(note_sizes), MaxNoteLength = max(note_lengths), MinNoteLength = min(note_lengths), MedNoteThickness = median(note_thickness), MedNoteSize = median(note_sizes), MedNoteLength = median(note_lengths))
#SyllNoteInfo = enddfNotes %>% group_by(RecordingNum, BoutNum, SyllNum) %>% summarize(nNotes=n(), MaxNoteThickness = max(note_thickness))
SyllNoteInfo2 = testunite %>% group_by(RecBoutSyll_ID,RecordingNum, BoutNum, SyllNum) %>% count(note_shapes)
#SyllNoteInfo2 = enddfNotes %>% group_by(RecordingNum, BoutNum, SyllNum) %>% count(note_shapes)
SyllNoteInfo3 = SyllNoteInfo2 %>% spread(note_shapes,n)
SyllNoteInfoMerge1=merge(SyllNoteInfo,SyllNoteInfo3, by=c("RecBoutSyll_ID","RecordingNum","BoutNum","SyllNum"))
SyllNoteInfoMerge1[,"RecordingNum"] = as.character(SyllNoteInfoMerge1[,"RecordingNum"])
SyllNoteInfoMerge1[,"BoutNum"] = as.character(SyllNoteInfoMerge1[,"BoutNum"])
enddfRegionUnite <- unite(data = enddfRegion, col = RecBoutSyll_ID, c(RecordingNum,BoutNum,SyllNum), sep="_", remove = FALSE)
SyllNoteInfoMerge2=merge(SyllNoteInfoMerge1,enddfRegionUnite[,c("File","RecordingNum","BoutNum","SyllNum","RecBoutSyll_ID", "onsets", "offsets","Latitude","Longitude","Era","Region","syllable_pattern_ID","year","month","day")], by=c("RecBoutSyll_ID","RecordingNum","BoutNum","SyllNum"), all = T)  
SyllNoteInfoMerge3=merge(SyllNoteInfoMerge2,newdfoutUnite, by=c("File","RecordingNum","BoutNum","SyllNum","RecBoutSyll_ID"), all = T)

# write syllable-level note stats
write.csv(SyllNoteInfoMerge3, paste0(Sys.Date(),species,"_NoteAnalysisBySyll_wMedians.csv"))

percSoundNAs <- SyllNoteInfoMerge3$RecBoutSyll_ID[which(is.na(SyllNoteInfoMerge3$percentSound))]
dups <- SyllNoteInfoMerge3$RecBoutSyll_ID[which(duplicated(SyllNoteInfoMerge3$RecBoutSyll_ID))]

droprows = SyllNoteInfoMerge3$RecBoutSyll_ID %in% dups & is.na(SyllNoteInfoMerge3$percentSound)
lessdupsdf <- SyllNoteInfoMerge3[!droprows,]
write.csv(lessdupsdf, paste0(Sys.Date(),species,"_NoteAnalysisBySyll2.1.csv"))

removedTooLongSyllsDF<- lessdupsdf[which(!str_detect(lessdupsdf$RecBoutSyll_ID," ")),]
removedNAsdf <- removedTooLongSyllsDF[!is.na(removedTooLongSyllsDF$SyllNum),]
write.csv(removedNAsdf, paste0(Sys.Date(),species,"_NoteAnalysisBySyll3.csv"))  # this is the way

PrevClusts <- read.csv("/Users/kate/Library/CloudStorage/Box-Box/Kate_Nicole/SyllableSimilaritySonos/2022-06-24_ClustsOverall-Post20220619-batch.csv")
PrevClustCols <- c("SyllRef", "Clust20220619.Overall" )
df  <- merge(removedNAsdf,PrevClusts[,PrevClustCols], by.x = "RecBoutSyll_ID", by.y = "SyllRef", all.x = T)
write.csv(df,paste0(Sys.Date(),"_NoteAnalysisBySyll_withPrevClusts.csv"))

sylldf <- df
#sylldf <- read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Gzip Files/Song Sparrow_KTS_Aggregated072022/2022-08-03_NoteAnalysisBySyll_withPrevClusts.csv")
for (i in c("downsweep","downsweep2","downsweep3", "upsweep","upsweep2","upsweep3", "flat","parabola")) {
  sylldf[,i][is.na(sylldf[,i])] <- 0
}
sylldf$downsweeps <- sylldf$downsweep+sylldf$downsweep2+sylldf$downsweep3
sylldf$upsweeps <- sylldf$upsweep+sylldf$upsweep2+sylldf$upsweep3

#sylldf=merge(sylldf, enddfRegionUnite[,c("RecBoutSyll_ID","All_Syllables_syllable_duration.ms.","All_Syllables_sylls_freq_modulation.Hz.", "All_Syllables_sylls_Upper_Freq_.Hz.","All_Syllables_sylls_Lower_Freq_.Hz.")], by="RecBoutSyll_ID")
sylldf=merge(sylldf, enddfRegionUnite[,c("RecBoutSyll_ID","All_Syllables_syllable_duration(ms)","All_Syllables_sylls_freq_modulation(Hz)", "All_Syllables_sylls_Upper_Freq_(Hz)","All_Syllables_sylls_Lower_Freq_(Hz)")], by="RecBoutSyll_ID")
#sylldf$MaybeShortWhistle <- sylldf$All_Syllables_sylls_freq_modulation.Hz./42.98245614 < 21 & sylldf$All_Syllables_syllable_duration.ms. <= 100 & sylldf$nNotes < 3
sylldf$MaybeShortWhistle <- sylldf[,"All_Syllables_sylls_freq_modulation(Hz)"]/42.98245614 < 21 & sylldf[,"All_Syllables_syllable_duration(ms)"] <= 100 & sylldf$nNotes < 3
write.csv(sylldf, paste0("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Gzip Files/Song Sparrow_KTS_Aggregated072022/",Sys.Date(),"_NoteAnalysisBySyll_withPrevClusts.csv"))


#### Average bout data over recordings ----
# 8/26/22 - need to add ability to remove files based on Problem File Notes
# Make bout-level-stats-only df
unique(enddfRegion$File)
dfBoutLevelStats <- enddfRegion %>% group_by(File) %>% sample_n(1)
dfBoutLevelStats<- dfBoutLevelStats[,which(!colnames(dfBoutLevelStats) %in% columns)]
dfBoutLevelStats<- dfBoutLevelStats[,which(!colnames(dfBoutLevelStats) %in% c("SyllNum","syllable_pattern_ID","onsets_unrescaled", "offsets_unrescaled"))]
write.csv(dfBoutLevelStats,file = paste0(Sys.Date(), "_",species,"_bout-stats_plusMetadata.csv"))

# load problem file notes
if (species == "DEJU") {
dfBoutLevelStats=read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Gzip Files/Dark Eyed Junco_KTS_Aggregated062022/2022-08-27_DEJU_bout-stats_plusMetadata.csv") 
dfBoutLevelStats = dfBoutLevelStats %>% unite(col="Rec_Bout",c("RecordingNum","BoutNum"), sep="_",remove=FALSE)
ProblemFiles=read_excel("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Reference Sheets/Dark Eyed Junco/Problem Files Notes.xlsx")
ProblemFiles= ProblemFiles %>% unite(col="Rec_Bout",c("File Name","Bout Number"), sep="_",remove=FALSE)
colnames(ProblemFiles)[which(colnames(ProblemFiles)=="Chippered?")] = "Chippered"

df = merge(dfBoutLevelStats,ProblemFiles, all.x=T)
df$Chippered[which(is.na(df$Chippered))] = "Yes"
df$Syllables[which(is.na(df$Syllables))] = "Yes-NonProblemFile"
df$Subset[which(is.na(df$Subset))] = "No-NonProblemFile"
#write.csv(df,file = paste0(Sys.Date(), "_",species,"_bout-stats_plusMetadata_ProblemFileNotes.csv"))

df <- df[which(df$Chippered != "No"),]

# add columns and/or remove values for subsetted bouts
df$num_syllables_some_manual = df$num_syllables
df$num_syllables_some_manual[which(!is.na(df$`Manual Syllable Count`))] = df$`Manual Syllable Count`[which(!is.na(df$`Manual Syllable Count`))]
df$num_syllables_some_manual = as.numeric(df$num_syllables_some_manual)
df$num_syllables[which(df$Subset == "Yes")] = NA 

df$num_unique_syllables_some_manual = df$num_unique_syllables
df$num_unique_syllables[which(df$Subset == "Yes")] = NA 
df$num_unique_syllables_some_manual[which(!is.na(df$`Types of Unique Syllables`))] = df$`Types of Unique Syllables`[which(!is.na(df$`Types of Unique Syllables`))]
df$num_unique_syllables_some_manual = as.numeric(df$num_unique_syllables_some_manual)

df$num_syllables_per_num_unique_some_manual = df$num_syllables_per_num_unique
df$num_syllables_per_num_unique[which(df$Subset == "Yes")] = NA 
df$num_syllables_per_num_unique_some_manual = as.numeric(df$num_syllables_some_manual)/as.numeric(df$num_unique_syllables_some_manual)
df$num_syllables_per_num_unique_some_manual = as.numeric(df$num_syllables_per_num_unique_some_manual)

df$bout_duration.ms.[which(df$Subset == "Yes")] = NA 
#df$num_syllable_per_bout_duration.1.ms.[which(df$Subset == "Yes")] = NA # don't need to remove
df$num_notes[which(df$Subset == "Yes")] = NA 

#write.csv(df,file = paste0(Sys.Date(), "_",species,"_bout-stats_plusMetadata_ProblemFile-Adjusted.csv"))
BoutLevelStatColumns = c(BoutLevelStatColumns, "num_syllables_some_manual", "num_unique_syllables_some_manual", "num_syllables_per_num_unique_some_manual")
dfBoutLevelStats = df
}

# Subset and average
if (species == "DEJU") {
BoutLevelStatColumnsNoSterotypy <- BoutLevelStatColumns[which(BoutLevelStatColumns != "std_syllable_stereotypy")]
BoutLevelStatColumnsNoSterotypy <- BoutLevelStatColumns[which(!BoutLevelStatColumns %in% c("std_syllable_stereotypy","syllable_stereotypy"))]
BoutLevelStatColumnsNoSterotypy <- str_replace(BoutLevelStatColumnsNoSterotypy, "\\(", "\\.")
BoutLevelStatColumnsNoSterotypy <- str_replace(BoutLevelStatColumnsNoSterotypy, "\\)", "\\.")
BoutLevelStatColumnsNoSterotypy <- str_replace(BoutLevelStatColumnsNoSterotypy, "\\/", "\\.")
dfBoutLevelStatsOnlyCols <- dfBoutLevelStats[,c("RecordingNum",BoutLevelStatColumnsNoSterotypy)]
colsToSubset = names(colSums(is.na(dfBoutLevelStatsOnlyCols))[which(colSums(is.na(dfBoutLevelStatsOnlyCols)) > 0)])

NoNACols = BoutLevelStatColumnsNoSterotypy[which(!BoutLevelStatColumnsNoSterotypy %in% colsToSubset)]
dfBoutLevelStatsOnlyNoNACols = dfBoutLevelStats[,c("RecordingNum",NoNACols)]
NoNAcolumnMeans <- dfBoutLevelStatsOnlyNoNACols %>% group_by(RecordingNum) %>% summarise(across(NoNACols[1]:NoNACols[length(NoNACols)], mean))  

dfToSubset=dfBoutLevelStats[,c("RecordingNum",colsToSubset)]
dfSubset = dfToSubset[which(!is.na(dfToSubset$bout_duration.ms.)),]
dfSubsetMeans <- dfSubset %>% group_by(RecordingNum) %>% summarise(across(colsToSubset[1]:colsToSubset[length(colsToSubset)], mean)) 
columnMeans = merge(dfSubsetMeans,NoNAcolumnMeans, all=T)
DEJUpfLab = "_wProblemFileAdjust_DupRecordistsRemoved"
}

if (species == "SOSP") {
columnMeans <- dfBoutLevelStatsOnlyCols %>% group_by(RecordingNum) %>% summarise(across(BoutLevelStatColumnsNoSterotypy[1]:BoutLevelStatColumnsNoSterotypy[length(BoutLevelStatColumnsNoSterotypy)], mean))  # -1 because don't want double ms_per_pixel - actually decided to keep it for a slight sanity check that the mean isn't any different between them in next part
DEJUpfLab = ""
}

# Drop duplicate recordist files
DEJUdupRecordists=read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Reference Sheets/Dark Eyed Junco/2022-08-12_DEJU-duplicate-recordists_withnotes_boutCounts_CompleteDropFiles_VisualCompareNotes.csv")
DEJUmeans=merge(columnMeans,DEJUdupRecordists[,c("FileName","DropFiles")], by.x = "RecordingNum", by.y = "FileName", all.x = TRUE)
DEJUmeansDupsRemoved = DEJUmeans[which(is.na(DEJUmeans$DropFiles)),]
columnMeans=DEJUmeansDupsRemoved

# subset metadata & merge, output
RecLevelMetadata <- dfBoutLevelStats[,c("File","RecordingNum","ms_per_pixel","DateRecorded","Latitude","Longitude","Time","State","County","Source","Era","Region","DroughtLevel","Remarks","year","month","day")] %>% group_by(RecordingNum) %>% sample_n(1)
DataAveragedByRecording <- merge(RecLevelMetadata, columnMeans, by="RecordingNum")
write.csv(DataAveragedByRecording, file = paste0(Sys.Date(), "_",species,"_AverageFeatures_ByRecording",DEJUpfLab,".csv"))

DataAveragedByRecording %>% group_by(Era, Region) %>% summarize(N=n())

