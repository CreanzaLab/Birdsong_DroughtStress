# Adapted from All Sparrows Complete 6.20.22kts.R
# Analyze/plot averaged features by recording
# 11/15/22 - added adjusted pval calculation to stats function & bonferroni-adjusted pval threshold
# 11/21/22 - changed browning-forsythe test! 

#Set Working Directory/
species= "SOSP-smallerRange"
setwd("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/All The Sparrows/Gzips and R Analysis")
dataSmallerRange=read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Gzip Files/Song Sparrow_KTS_Aggregated072022/2022-08-11_AverageFeatures_ByRecording.csv")
data=dataSmallerRange
data=read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Gzip Files/Song Sparrow_KTS_Aggregated072022/2022-08-23_SOSP_AverageFeatures_ByRecording.csv")

# 11/15/22
species = "SOSP"
data = read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Gzip Files/Song Sparrow_KTS_Aggregated072022/2022-11-16_SOSP_AverageFeatures_ByRecording.csv")
otherlabel = ""

species = "DEJU"
setwd("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/All The Juncos/")
dataSmallerRange=read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Gzip Files/Dark Eyed Junco_KTS_Aggregated062022/2022-08-12_DEJU_AverageFeatures_ByRecording.csv") 
#data=read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Gzip Files/Dark Eyed Junco_KTS_Aggregated062022/2022-08-24_DEJU_AverageFeatures_ByRecording.csv")
data = read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/All The Juncos/2022-08-28_DEJU_AverageFeatures_ByRecording_wProblemFileAdjust_DupRecordistsRemoved.csv")
otherlabel = "_ProblemFilesAdjusted_DupRecordistsRemoved_8-28-22Averages"
data = read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Gzip Files/Dark Eyed Junco_KTS_Aggregated062022/2022-08-28_DEJU_AverageFeatures_ByRecording_wProblemFileAdjust.csv")
otherlabel = "_ProblemFilesAdjusted"

# 11/19/22
species = "SOSP"
pathout = "/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/All The Sparrows/"
data=read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Gzip Files/Song Sparrow_KTS_Aggregated072022/2022-11-19_SOSP_AverageFeatures_ByRecording_WithMedNotes.csv")

species = "DEJU"
pathout = "/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/All The Juncos/"
data=read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/All The Juncos/2022-11-19_DEJU_AverageFeatures_ByRecording_WithMedNotes.csv")
data=read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/All The Juncos/2022-08-28_DEJU_AverageFeatures_ByRecording_wProblemFileAdjust_DupRecordistsRemoved.csv")
otherlabel = ""


imp_col_old <- c("bout_duration.ms.", "num_syllables", "num_syllable_per_bout_duration.1.ms.", "largest_syllable_duration.ms.", "smallest_syllable_duration.ms.", "avg_syllable_duration.ms.", "std_syllable_duration.ms.", "largest_silence_duration.ms.", "smallest_silence_duration.ms.", "avg_silence_duration.ms.", "std_silence_duration.ms.", "num_unique_syllables", "num_syllables_per_num_unique", "sequential_repetition", "mean_syllable_stereotypy", "std_syllable_stereotypy", "avg_sylls_upper_freq.Hz.", "avg_sylls_lower_freq.Hz.", "max_sylls_freq.Hz.", "min_sylls_freq.Hz.", "overall_sylls_freq_range.Hz.", "largest_sylls_freq_modulation.Hz.", "smallest_sylls_freq_modulation.Hz.", "avg_sylls_freq_modulation.Hz.", "std_sylls_freq_modulation.Hz.", "num_notes", "num_notes_per_syll")
imp_col_new <- c("bout_duration.ms.", "num_syllables", "num_syllable_per_bout_duration.1.ms.", "avg_syllable_duration.ms.", "std_syllable_duration.ms.", "num_unique_syllables", "num_syllables_per_num_unique", "avg_sylls_upper_freq.Hz.", "avg_sylls_lower_freq.Hz.", "overall_sylls_freq_range.Hz.", "num_notes", "num_notes_per_syll")   #, "meanMedianNotesPerSyll"

if (species == "DEJU") {
imp_col <- c(imp_col_new, "num_syllables_some_manual", "num_unique_syllables_some_manual", "num_syllables_per_num_unique_some_manual", "mean_syllable_stereotypy")
} else if (species == "SOSP") {
  imp_col <- c(imp_col_new, "largest_syllable_duration.ms.", "smallest_syllable_duration.ms.")
}


require(tidyr)
library(ggplot2)
library(gridExtra)
data <- data[which(data$Era %in% c("Post","Pre")),]
data <- data[which(data$Region %in% c("Control","Drought")),]
data %>% group_by(Source) %>% summarize(N=n())

data$Era <- factor(data$Era, levels = c("Pre","Post"), ordered = TRUE)
data$Region <- factor(data$Region, levels = c("Drought","Control"), ordered = TRUE)
colnames(data) <- str_replace(colnames(data), "sylls_freq_modulation.Hz.","sylls_freq_range.Hz.")

colnums <- which(colnames(data) %in% imp_col)
print(colnums)

west <- data

log=FALSE

ggplots <- list()
labels <- read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/All The Sparrows/Gzips and R Analysis/Graph Label List.csv")
newlabels = cbind(c("num_syllables_some_manual", "num_unique_syllables_some_manual", "num_syllables_per_num_unique_some_manual", "meanMedianNotesPerSyll"), c("Total number of syllables", "Number of unique syllables", "Number of syllables per unique syllables", "Mean of median number of notes per syllable per bout"))
colnames(newlabels) = colnames(labels)
labels = rbind(labels, newlabels)

featurelist = c()
index=0
for (i in colnums) {
  index = index+1
  print(index)
  feature <- colnames(west)[i]
  print(feature)
  featurelist = c(featurelist,feature)
  label_name <- labels$Human.Language [which(as.character(labels$File.Title) == as.character(feature))]
  print(label_name)
  #label_name = as.character(feature)
  
  west$feature <- as.numeric(as.character(west[,feature]))
  if (log) {
    west$feature <- log(west$feature)
    loglabel="log"
  } else {
    loglabel=""
  }
  
  #feature_a <- colnames(west_corrected)[i]
  #label_name <- labels$Human.Language [which(as.character(labels$File.Title) == as.character(feature_a))]
  #print(feature_a)
  #print(label_name)
  #west_corrected$feature <- as.numeric(as.character(west_corrected[,feature_a]))
  

    g <- ggplot(data = west, aes(x=interaction(Era,Region), y=feature))
    ggplots[[index]] <- g + geom_boxplot(outlier.shape = NA)+ xlab("")+ ylab(label_name)+ geom_dotplot(binaxis='y',stackdir='center',dotsize = .4, binpositions = "all", aes(fill=Region, color = Region),show.legend = F) + scale_color_manual(values=c("red","blue"),aesthetics = c("fill", "color")) + ggtitle(paste(loglabel, label_name)) + theme(plot.title = element_text(hjust = 0.5, size=8)) + theme_classic() 

   # g <- ggplot(data = west, aes(x=interaction(Era,Region), y=feature))
  #  ggplots[[index]] <- g + geom_boxplot(outlier.shape = NA)+ xlab("Time Relative to Drought")+ ylab(label_name)+ geom_dotplot(binaxis='y',stackdir='center',dotsize = .4, binpositions = "all", aes(fill=Region, color = Region),show.legend = F) + scale_color_manual(values=c("red","blue"),aesthetics = c("fill", "color")) + theme(plot.title = element_text(hjust = 0.5, size=8)) + theme_classic() + ggtitle(paste(loglabel, label_name, "in",species,"Song"))

}
featurelist

file_name <- paste0(pathout, Sys.Date(),"All_",species,loglabel,"_boxdotplots",otherlabel,".pdf")

pdf(file = as.character(file_name), height = 15, width = 11)
grid.arrange(ggplots[[1]],ggplots[[2]],ggplots[[3]],ggplots[[4]],ggplots[[5]],ggplots[[6]],ncol=2,nrow=3)
grid.arrange(ggplots[[7]],ggplots[[8]],ggplots[[9]],ggplots[[10]],ggplots[[11]],ggplots[[12]],ncol=2,nrow=3)
#grid.arrange(ggplots[[13]],ggplots[[14]],ncol=2,nrow=3)   # SOSP
grid.arrange(ggplots[[13]],ggplots[[14]],ggplots[[15]],ggplots[[16]],ggplots[[17]],ncol=2,nrow=3) # DEJU
#grid.arrange(ggplots[[19]],ggplots[[20]],ggplots[[21]],ggplots[[22]],ggplots[[23]],ncol=2,nrow=3)
dev.off()


# for in-text figure
file_name <- paste0(pathout, Sys.Date(),"All_",species,loglabel,"_boxdotplots",otherlabel,".pdf")

#song sparrow
pdf(file = as.character(file_name), height = 15, width = 10)
grid.arrange(ggplots[[2]],ggplots[[8]],ggplots[[14]],ggplots[[6]],ggplots[[13]],ggplots[[1]],ncol=2,nrow=3)
dev.off()

#junco 
pdf(file = as.character(file_name), height = 15, width = 10)
grid.arrange(ggplots[[2]],ggplots[[3]],ggplots[[13]],ggplots[[7]],ggplots[[5]],ggplots[[1]],ncol=2,nrow=3)
dev.off()

#### Stats ----
# from sparrowfunc_MLS_control.R


library(onewaytests)

#Example: sparrowfunc(file = "2020July_Junco_Syll_Averaging Additional Columns.csv", log = TRUE)
#Example: juncofunc(file = "2020July_Junco_Syll_Averaging Additional Columns.csv", log = FALSE)
#         statsfunc(file = "/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Gzip Files/Dark Eyed Junco_KTS_Aggregated062022/2022-08-12_DEJU_AverageFeatures_ByRecording.csv", log = FALSE, species = "DEJU")
#         statsfunc(file = "/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Gzip Files/Song Sparrow_KTS_Aggregated072022/2022-08-23_SOSP_AverageFeatures_ByRecording.csv", log = TRUE, species = "SOSP")
         statsfunc(file = dataSOSP, log = T, species = "SOSP", otherlabel = "_WithBonferroni-and-RealBF")
         statsfunc(file = datajunco, log = T, species = "DEJU", otherlabel = "_WithBonferroni-and-RealBF_onlyCompleteBoutsIncluded")
#         statsfunc(file = "/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/All The Juncos/2022-08-28_DEJU_AverageFeatures_ByRecording_wProblemFileAdjust_DupRecordistsRemoved.csv", log = FALSE, species = "DEJU", otherlabel = "_ProblemFilesAdjusted_DupRecordistsRemoved")
#         statsfunc(file = "/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Gzip Files/Dark Eyed Junco_KTS_Aggregated062022/2022-08-28_DEJU_AverageFeatures_ByRecording_wProblemFileAdjust.csv", log = FALSE, species = "DEJU", otherlabel = "_ProblemFilesAdjusted")

###############SYLLABLE ANALYSIS####################
##
statsfunc <- function(file = "2021-08-10Sparrow_Syll_Averaging.csv", log = FALSE, species, otherlabel = "") {
  if (is.character(file)) {
  sylldf <- read.csv(file)
  sylldf$Era <- as.factor(sylldf$Era)
  } else {
    sylldf = file
  }
 
  bothdf <- set.seed(10)
   
  for (region in c("Drought","Control")) {
  postdf <- sylldf[which(sylldf$Region == region & sylldf$Era == "Post"),]
  predf <- sylldf[which(sylldf$Region == region & sylldf$Era == "Pre"),]
  #westdurdf <- sylldf[which(sylldf$Region == "Drought" & sylldf$Era == "During"),]
  df <- sylldf[which(sylldf$Region == region & sylldf$Era %in% c("Post","Pre")),]
  
  # controlpostdf <- sylldf[which(sylldf$Region == "Control" & sylldf$Era == "Post"),]
  # controlpredf <- sylldf[which(sylldf$Region == "Control" & sylldf$Era == "Pre"),]
  # #controldurdf <- sylldf[which(sylldf$Region == "Control" & sylldf$Era == "During"),]
  # controldf <- sylldf[which(sylldf$Region == "Control" & sylldf$Era != "During"),]

  
  outdf<- set.seed(10)
  for (feature in colnames(sylldf)[colnums]) {
    print(paste("*********************",feature,"**********************"))
    tempfeat <- df[,feature]
    tempfeatpre <- predf[,feature]
    #tempfeatdur <- westdurdf[,feature]
    tempfeatpost <- postdf[,feature]
    nPre = sum(!is.na(tempfeatpre))
    nPost = sum(!is.na(tempfeatpost))
    print("linear")
    print(tempfeatpost[1:10])
    if (log == TRUE) {
      if (feature != "sequential_repetition") {
      tempfeat <- log(tempfeat)
      df[,feature] <- tempfeat
      tempfeatpre <- log(tempfeatpre)
      #tempfeatdur <- log(tempfeatdur)
      tempfeatpost <- log(tempfeatpost)
      loglabel = "log"
      print(loglabel)
      print(tempfeatpost[1:10])
      } else {
        loglabel=""
      }
    } else {
      loglabel = NULL
    }
    
    if (is.numeric(tempfeat) & dim(table(tempfeat)) > 1) {
      outshapiro <- shapiro.test(tempfeat)
      outshapiropre <- shapiro.test(tempfeatpre)
      #outshapiroduring <- shapiro.test(tempfeatdur)
      outshapiropost <- shapiro.test(tempfeatpost)
      outkruskal <- kruskal.test(tempfeat ~ df$Era) #nonparametric alternative to one-way ANOVA, which extends the two-samples Wilcoxon test in the situation where there are more than two groups (omnibus)
      lmout <- lm(tempfeat ~ df$Era)
      anovaout <- anova(lmout)  #parametric, omnibus
      
      
     # outwilcox <- wilcox.test(df[,feature] ~ df$Era, exact = TRUE)
      outwilcox <- wilcox.test(tempfeat ~ df$Era, exact = TRUE)
      
      #T-test
      #t_results <- t.test(df[,feature] ~ df$Era)
      t_results <- t.test(tempfeat ~ df$Era)
      
      #Brown-Forsythe
      #loopform <- as.formula(paste(feature,"~ Era"))
      #outbf <- bf.test(loopform, data = df, na.rm = TRUE)
      #outbf <- set.seed(10)
      #outbf$p.value <- NA
      outbf <- brown.forsythe.test(df[,feature],df$Era)
      
      preSumm <- summary(tempfeatpre)
      postSumm <- summary(tempfeatpost)
      
      if (length(unique(df$Era))==2) {
        if (outshapiropost$p.value < 0.05 | outshapiropre$p.value < 0.05) {
          pvalToUse = outwilcox$p.value
          print(pvalToUse)
        } else {
          pvalToUse = t_results$p.value
          print(pvalToUse)
        }
      }
      
      newrow <- c(feature, outshapiro$p.value, outshapiropre$p.value, outshapiropost$p.value, outkruskal$p.value, anovaout$`Pr(>F)`[1], outwilcox$p.value, t_results$p.value, pvalToUse, outbf$p.value, nPre, preSumm[1], preSumm[4], preSumm[6], nPost, postSumm[1], postSumm[4], postSumm[6])
      outdf <- rbind(outdf, newrow)
      
    }
  }
  colnames(outdf) <- paste0(c("Feature","ShapiroAllP","ShapiroPreP","ShapiroPostP","KruskalP","anovaP","WilcoxP","TtestP", "PvalToUse","BrownForsytheP", "PreN", "PreMin", "PreMean", "PreMax", "PostN", "PostMin", "PostMean", "PostMax"),"_", region)
  bothdf=cbind(bothdf,outdf)
  }  # end for region
  bothdf <- as.data.frame(bothdf)
  bothdf$PvalToUse_Control = as.numeric(bothdf$PvalToUse_Control)
  bothdf$PvalToUse_Drought = as.numeric(bothdf$PvalToUse_Drought)
  adjPvalToUse_Drought=p.adjust(bothdf$PvalToUse_Drought, "fdr")
  adjPvalToUse_Control=p.adjust(bothdf$PvalToUse_Control, "fdr")
  bothdf$BonferroniRank_Drought = rep(0, length(bothdf$Feature_Drought))
  bothdf$BonferroniRank_Drought[order(bothdf$PvalToUse_Drought)] <- length(bothdf$PvalToUse_Drought):1
  bothdf$BonferroniPThreshold_Drought = 0.05/bothdf$BonferroniRank_Drought
  bothdf$BonferroniSignificant_Drought = bothdf$BonferroniPThreshold_Drought > bothdf$PvalToUse_Drought
  bothdf$BonferroniRank_Control = rep(0, length(bothdf$Feature_Control))
  bothdf$BonferroniRank_Control[order(bothdf$PvalToUse_Control)] <- length(bothdf$PvalToUse_Control):1
  bothdf$BonferroniPThreshold_Control = 0.05/bothdf$BonferroniRank_Control
  bothdf$BonferroniSignificant_Control = bothdf$BonferroniPThreshold_Control > bothdf$PvalToUse_Control
  bothdf <- cbind(bothdf, adjPvalToUse_Drought, adjPvalToUse_Control)
  rownames(bothdf) <- NULL
  write.csv(bothdf, file = paste0(Sys.Date(), "All_",species, " ", loglabel, "Syll Analyses_With_Control",otherlabel,".csv"))
}
logDEJU=read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/All The Juncos/2022-11-16All_DEJU logSyll Analyses_With_Control_WithBonferroni.csv")
linDEJU=read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/All The Juncos/2022-11-16All_DEJU Syll Analyses_With_Control_WithBonferroni.csv")
logSOSP=read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/All The Juncos/2022-11-16All_SOSP logSyll Analyses_With_Control_WithBonferroni.csv")
linSOSP=read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/All The Juncos/2022-11-16All_SOSP Syll Analyses_With_Control_WithBonferroni.csv")


logDEJU$Feature_Drought[logDEJU$BonferroniSignificant_Drought]
logDEJU$Feature_Drought[logDEJU$BonferroniSignificant_Control]
linDEJU$Feature_Drought[linDEJU$BonferroniSignificant_Drought]
linDEJU$Feature_Drought[linDEJU$BonferroniSignificant_Control]
logSOSP$Feature_Drought[logSOSP$BonferroniSignificant_Drought]
logSOSP$Feature_Drought[logSOSP$BonferroniSignificant_Control]
linSOSP$Feature_Drought[linSOSP$BonferroniSignificant_Drought]
linSOSP$Feature_Drought[linSOSP$BonferroniSignificant_Control]


bothdf$Feature_Drought[bothdf$adjPvalToUse_Drought]

         
         
statdf=read_excel("2022-08-28All_DEJU Syll Analyses_With_Control_ProblemFilesAdjusted_DupRecordistsRemoved_PvalToUse.xlsx")
statdf=statdf[which(statdf$Feature_Drought %in% imp_col),]
p.adjust(statdf$WilcoxP_Drought, "fdr")
p.adjust(statdf$TtestP_Drought, "fdr")
adjP=p.adjust(statdf$PvalToUse, "fdr")
cbind(statdf$Feature_Drought,adjP)


 #### plot recordings over time ----
data=read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Gzip Files/Song Sparrow_KTS_Aggregated072022/2022-08-23_SOSP_AverageFeatures_ByRecording.csv")
data=data[which(data$year < 2020),]
dupdf = read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Reference Sheets/Song Sparrow/2022-07-30_all-InRegion-duplicate-recordists_addedCols_boutCounts_DropRecsNotes.csv")
droprecs=dupdf$FileName[which(dupdf$DropFiles==1)]
data=data[which(!data$RecordingNum %in% droprecs),]

filelist=list.files("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Gzip Files/Song Sparrow_KTS_Aggregated072022/", pattern=".gzip")
for (i in 1:length(data$RecordingNum)) {
  temprec = data$RecordingNum[i]
  if (sum(str_detect(filelist,temprec)) == 0) {
    print(temprec)
  }
  #tempfile=filelist[which(str_detect(filelist, temprec))]
  #print(tempfile)
}

data = dataSOSP
species = "SOSP"
dataSOSP %>% group_by(Era,Region) %>% summarise(n=n())

data = datajunco
data$Source[which(is.na(data$Source))] = "XC"
species = "DEJU"

yearcounts = data %>% group_by(year, Region) %>% summarize(N=n())
as.data.frame(yearcounts)
yearcounts = rbind(as.data.frame(yearcounts),c(2009,"Control",0))
yearcounts = rbind(as.data.frame(yearcounts),c(2008,"Control",0))
if (species == "DEJU") {
yearcounts = rbind(as.data.frame(yearcounts),c(2013,"Control",0))
yearcounts = rbind(as.data.frame(yearcounts),c(2014,"Control",0))
yearcounts = rbind(as.data.frame(yearcounts),c(2015,"Control",0))
}
yearcounts$N = as.numeric(yearcounts$N)
pdf(file = paste0("/Users/kate/Library/CloudStorage/Box-Box/Kate_Nicole/Dissertation Materials/Barplot_IncludedRecordings-per-year-by-region_", species, ".pdf"), width = 7, height =3.5)
ggplot(data = yearcounts, aes(x=year, y=N, fill=Region)) + geom_bar(stat="identity", position = "dodge") + scale_fill_manual(values=c("red","blue")) + ylim(0,60) + ggtitle(label=paste("Recordings included in",species,"analysis")) + theme_minimal()
dev.off()
data %>% group_by(Era, Region, Source) %>% summarize(N=n())

## kinda scratch: get number of recordings originally found in databases
enddf = read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Reference Sheets/Song Sparrow/2022-07-30_SOSP_Complex_Metadata_addedJuvSong.csv")
enddf = enddf[which(enddf$year < 2020),]
enddf = enddf[which(is.na(enddf$JuvenileSongToDrop)),]
enddf = enddf[which(str_detect(enddf$Behaviors, regex("Song", ignore_case = T))),]
enddf = enddf[which(!enddf$FileName %in% droprecs),]
enddfsub=enddf[which(enddf$FileName %in% data$RecordingNum),]
enddfsub <- enddfsub %>% unite("Fields",c(Recordist,Latitude,Longitude,Date.Collected), na.rm=TRUE, remove=FALSE)

enddfRectControl <- enddf[which(enddf$Longitude < -72.5 & enddf$Longitude > -75 & enddf$Latitude < 42 & enddf$Latitude > 40),]
enddfRectControl %>% group_by(Era) %>% 
  summarise(nBirds = n())
ControlRecs <- unique(enddfRectControl$FileName)

enddfRectDrought <- enddf[which(enddf$Longitude <  -75.4 & enddf$Longitude > -78.9 & enddf$Latitude < 43.2 & enddf$Latitude > 42.2),]   # Discovered 8/1/22 that enddf$Longitude <  76.3 was missing the "-"; potentially included excess recordings to the east
# 8/22/22 - want Latitude to be < 43.2 (from 43.1)? Get one more recording (2018); want Longitude to be < -75.4 (from -76.3)? would get 2 more points- 2008+2014 - changed this in post chipper processing
enddfRectDrought %>% group_by(Era) %>% 
  summarise(nBirds = n())
DroughtRecs <- unique(enddfRectDrought$FileName)
# xmin = -78.80951, xmax = -76.30545, ymin = 42.27741, ymax = 43.07931)) 

Region <- rep(NA,length(enddf$FileName))
Region[which(enddf$FileName %in% ControlRecs)] <- "Control"
Region[which(enddf$FileName%in% DroughtRecs)] <- "Drought"
sum(Region == "Control", na.rm=T)
sum(Region == "Drought", na.rm=T)
enddf$Region <- Region
enddf$Region <- factor(enddf$Region, levels = c("Drought","Control"), ordered = TRUE)
enddf=enddf[which(enddf$Region %in% c("Control","Drought") & enddf$year >= 2006 & enddf$year < 2020),]
enddf %>% group_by(Era,Region) %>% summarize(N=n())

yearcounts = enddf %>% group_by(year, Region) %>% summarize(N=n())
as.data.frame(yearcounts)
yearcounts = rbind(as.data.frame(yearcounts),c(2008,"Control",0))
yearcounts = rbind(as.data.frame(yearcounts),c(2009,"Control",0))
yearcounts$N = as.numeric(yearcounts$N)
ggplot(data = yearcounts, aes(x=year, y=N, fill=Region)) + geom_bar(stat="identity", position = "dodge") + scale_fill_manual(values=c("red","blue")) + ylim(0,max(yearcounts$N + 5)) + ggtitle(label="Song recordings available") + theme_minimal()

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
# Continental US: xlim = c(-127.15, -66), ylim = c(24.65, 50.97)
# NY State: xlim = c(-80.15, -72), ylim = c(38.65, 45.97)
# Northeast US: xlim = c(-81.15, -71), ylim = c(38, 46.57)
# Whole US: xlim = c(-170.15, -50), ylim = c(24.65, 77.97)
xlim= c(-81.15, -71)
ylim=c(38, 46.57)
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  geom_point(data = data, mapping = aes(x = Longitude, y=Latitude, col = Region), size = 1.5) + scale_color_manual(values=c("blue","red", "gray"))




# check against Maria data
mariadata=read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Individual Syll Analysis/2021-08-10Sparrow_Syll_Averaging.csv")
mariadata$V1 <- str_remove(mariadata$V1, "_")
mariadata$V1 <- str_remove(mariadata$V1, "S12")
mariadata %>% group_by(Era,Location) %>% summarize(N=n())
mariasubset=enddf[which(enddf$FileName %in% mariadata$V1),]
mariasubset %>% group_by(Era) %>% summarize(N=n())

mariadata$V1[!mariadata$V1 %in% enddf$FileName] 
# c("169105391",  "149583171",  "149095261",  "148666651",  "168301511",  "171253791")  # in mariadata but not in metadata WHEN metadata subsetted by year & to just within regions

data=read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Gzip Files/Song Sparrow_KTS_Aggregated072022/2022-08-11_AverageFeatures_ByRecording.csv")
#data$RecordingNum <- as.numeric(data$RecordingNum)
data$RecordingNum[!data$RecordingNum %in% mariadata$V1]
missingfiles=mariadata$V1[!mariadata$V1 %in% data$RecordingNum]
missingfilesdf=enddf[which(enddf$FileName %in% missingfiles),]
write.csv(missingfilesdf, "2022-08-14_SOSP files removed from dataset since 2021.csv")

dupRecordists=read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Reference Sheets/Song Sparrow/2022-07-30_all-InRegion-duplicate-recordists_AddedCols.csv")
missingfiles[!missingfiles %in% dupRecordists$FileName]



oldermeta <- read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Reference Sheets/Song Sparrow/2022-06-24_SOSP_Complex_Metadata_SelfRecsCoordinatesAdded_AllMLXC_addedMissingNYCoords_IncludeOGEras_UsedRegions_Behaviors.csv")
mariadata$V1[!mariadata$V1 %in% oldermeta$FileName]
enddf$FileName[!enddf$FileName %in% oldermeta$FileName]

oldermeta <- read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Reference Sheets/Song Sparrow/All Sparrow Reference Sheet.csv")
oldermeta %>% group_by(Era) %>% summarize(N=n())


olderdata <- read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Individual Syll Analysis/2021-08-19_songsparrow_syllables.csv")
olderdata1perrec=olderdata %>% group_by(RecordingNum) %>% sample_n(1)
olderdata1perrec %>% group_by(Era) %>% summarize(n=n())
olderdata1perrec$RecordingNum[!olderdata1perrec$RecordingNum %in% mariadata$V1]
filesmissingfromMy2021Data <- olderdata1perrec$RecordingNum[!olderdata1perrec$RecordingNum %in% mariadata$V1] # which weren't used in maria's data
MariaDidntUseFiles=enddf[which(enddf$FileName %in% filesmissingfromMy2021Data),]


mariadata2 <- read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/All The Sparrows/Gzips and R Analysis/2021-09-03Sparrow_Syll_Averaging.csv")
olderdata1perrec$RecordingNum[!olderdata1perrec$RecordingNum %in% mariadata2$V1] 
outliers=read_excel("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Reference Sheets/Song Sparrow/Outlier Analysis Library.xlsx")

mariadata3=read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/All The Sparrows/Gzips and R Analysis/All_Sparrow_Syll_Averaging_with_Control.csv")
olderdata1perrec$RecordingNum[!olderdata1perrec$RecordingNum %in% mariadata3$V1] 



#### Find example recordings ----
juncobouts = read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Gzip Files/Dark Eyed Junco_KTS_Aggregated062022/2022-08-28_DEJU_bout-stats_plusMetadata_ProblemFile-Adjusted.csv")
juncorecs = read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Gzip Files/Dark Eyed Junco_KTS_Aggregated062022/2022-08-28_DEJU_AverageFeatures_ByRecording_wProblemFileAdjust_DupRecordistsRemoved.csv")
juncobouts = juncobouts[which(juncobouts$RecordingNum %in% juncorecs$RecordingNum),]
juncobouts= juncobouts %>% filter(Era %in% c("Pre","Post") & Region %in% c("Drought","Control"))
boutdata = juncobouts
species = "DEJU"
data = juncorecs
featuresToUse = c("num_notes","bout_duration.ms.", "num_syllables_per_num_unique_some_manual")

species = "SOSP"
data = read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Gzip Files/Song Sparrow_KTS_Aggregated072022/2022-11-16_SOSP_AverageFeatures_ByRecording.csv")
boutdata = read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Gzip Files/Song Sparrow_KTS_Aggregated072022/2022-11-16_SOSP_bout-stats_plusMetadata_RemovedUnusedBouts.csv")
featuresToUse = c("num_notes_per_syll","num_syllables","num_syllables_per_num_unique")
# Drought-Pre = 133113-12 Drought Pre  
# Drought-Post = 160492581-3 Drought Post
# 149903-6 Control Pre
# 106627891-1 Control Post
# boutdata[which(boutdata$RecordingNum==133113 & boutdata$BoutNum == 12),c("Era","Region","RecordingNum","BoutNum",featuresToUse)]

data= data %>% filter(Era %in% c("Pre","Post") & Region %in% c("Drought","Control"))
whichRegion = "Control"

subdata = data[,featuresToUse]
colnames(subdata) = c("feature1", "feature2", "feature3")
subdatascaled = scale(subdata)
scaleddata = cbind(data[,c("Region","Era")], subdatascaled)

#median
medianSignificantVals = scaleddata %>% group_by(Era,Region) %>% summarize(feature1 = median(feature1, na.rm = T), feature2 = median(feature2, na.rm = T), feature3 = median(feature3, na.rm = T))
refdf = medianSignificantVals[which(medianSignificantVals$Region == whichRegion),]

#mean 
#meanSignificantVals = scaleddata %>% group_by(Era,Region) %>% summarize(meanNotesPerSyll = mean(num_notes_per_syll), meanNumSylls = mean(num_syllables), meanNumSyllsPerUnique = mean(num_syllables_per_num_unique))
#refdf = meanSignificantVals[which(meanSignificantVals$Region == "Drought"),]

boutdata= boutdata[which(boutdata$Era %in% c("Pre","Post") & boutdata$Region %in% c("Drought","Control")),]
colnames(boutdata)
boutdatascaled = scale(boutdata[,featuresToUse])
colnames(boutdatascaled) = c("feature1", "feature2", "feature3")
boutdatascaled = cbind(boutdata[,c("RecordingNum","BoutNum","Region","Era")], boutdatascaled)

preRef = refdf[which(refdf$Era=="Pre"),3:5] %>% slice(rep(1, length(boutdatascaled$RecordingNum)))
postRef = refdf[which(refdf$Era=="Post"),3:5] %>% slice(rep(1, length(boutdatascaled$RecordingNum)))
distancePre = sqrt((boutdatascaled[,"feature1"]-preRef[,"feature1"])^2 + (boutdatascaled[,"feature2"]-preRef[,"feature2"])^2 + (boutdatascaled[,"feature3"]-preRef[,"feature3"])^2)
distancePre <- distancePre$feature1
boutdatascaled = cbind(boutdatascaled,distancePre)
distancePost = sqrt((boutdatascaled[,"feature1"]-postRef[,"feature1"])^2 + (boutdatascaled[,"feature2"]-postRef[,"feature2"])^2 + (boutdatascaled[,"feature3"]-postRef[,"feature3"])^2)
distancePost <- distancePost$feature1
boutdatascaled$distancePost = distancePost

boutdatascaled[which(boutdatascaled$distancePre < 0.3 | boutdatascaled$distancePost < 0.3),]

boutdatascaledDrought = boutdatascaled[which(boutdatascaled$Region == whichRegion),]
colnames(boutdatascaledDrought)[5:7] = featuresToUse
minPreDist = boutdatascaledDrought %>% filter(Era == "Pre" & Region == whichRegion) %>% summarize(minPreDist = min(distancePre, na.rm = T))
minPostDist = boutdatascaledDrought %>% filter(Era == "Post" & Region == whichRegion) %>% summarize(minPostDist = min(distancePost, na.rm = T))

boutdatascaledDrought[which(boutdatascaledDrought$distancePre == minPreDist$minPreDist),]
boutdatascaledDrought[which(boutdatascaledDrought$distancePost == minPostDist$minPostDist),]

colnames(refdf)[3:5] = featuresToUse
pdf(file = paste0("scatterplot_scaled_features_","SOSP","_PrePost",whichRegion,".pdf"))
ggplot(data = boutdatascaledDrought, aes(x=num_notes_per_syll, y = num_syllables_per_num_unique, col = Era)) +
  geom_point() +
  scale_color_manual(values=c("#a6611a","#018571"))  +
  geom_point(data = refdf, aes(x=num_notes_per_syll,y=num_syllables_per_num_unique, shape = Era), size = 3, col = "black") +
  ggtitle(label = paste(species, whichRegion, "Region, median center; pre = green, post = brown"))  #+
  #theme_classic()
dev.off()

colnames(refdf)[3:5] = featuresToUse
pdf(file = paste0("scatterplot_scaled_features_","DEJU","_PrePost",whichRegion,".pdf"))
ggplot(data = boutdatascaledDrought, aes(y=num_notes, x = bout_duration.ms., col = Era)) +
  geom_point() +
  scale_color_manual(values=c("#a6611a","#018571"))  +
  geom_point(data = refdf, aes(y=num_notes,x=bout_duration.ms., shape = Era), size = 3, col = "black") +
  ggtitle(label = paste(species, whichRegion, "Region, median center; pre = green, post = brown"))  #+
#theme_classic()
dev.off()


#### Prior (less flexible) version of finding example recordings
juncobouts = read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Gzip Files/Dark Eyed Junco_KTS_Aggregated062022/2022-08-28_DEJU_bout-stats_plusMetadata_ProblemFile-Adjusted.csv")
juncorecs = read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Gzip Files/Dark Eyed Junco_KTS_Aggregated062022/2022-08-28_DEJU_AverageFeatures_ByRecording_wProblemFileAdjust_DupRecordistsRemoved.csv")
juncobouts = juncobouts[which(juncobouts$RecordingNum %in% juncorecs$RecordingNum),]
juncobouts= juncobouts %>% filter(Era %in% c("Pre","Post") & Region %in% c("Drought","Control"))

species = "SOSP"
data = read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Gzip Files/Song Sparrow_KTS_Aggregated072022/2022-11-16_SOSP_AverageFeatures_ByRecording.csv")

 data = data %>% filter(Era %in% c("Pre","Post") & Region %in% c("Drought","Control"))

subdata = data[,c("num_notes_per_syll","num_syllables","num_syllables_per_num_unique")]
subdatascaled = scale(subdata)
scaleddata = cbind(data[,c("Region","Era")], subdatascaled)

whichRegion = "Control"

#median
medianSignificantVals = scaleddata %>% group_by(Era,Region) %>% summarize(meanNotesPerSyll = median(num_notes_per_syll), meanNumSylls = median(num_syllables), meanNumSyllsPerUnique = median(num_syllables_per_num_unique))
refdf = medianSignificantVals[which(medianSignificantVals$Region == whichRegion),]

#mean 
#meanSignificantVals = scaleddata %>% group_by(Era,Region) %>% summarize(meanNotesPerSyll = mean(num_notes_per_syll), meanNumSylls = mean(num_syllables), meanNumSyllsPerUnique = mean(num_syllables_per_num_unique))
#refdf = meanSignificantVals[which(meanSignificantVals$Region == "Drought"),]
boutdata = read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Gzip Files/Song Sparrow_KTS_Aggregated072022/2022-11-16_SOSP_bout-stats_plusMetadata_RemovedUnusedBouts.csv")
boutdata= boutdata[which(boutdata$Era %in% c("Pre","Post") & boutdata$Region %in% c("Drought","Control")),]
colnames(boutdata)
boutdatascaled = scale(boutdata[,c("num_notes_per_syll","num_syllables","num_syllables_per_num_unique")])
boutdatascaled = cbind(boutdata[,c("RecordingNum","BoutNum","Region","Era")], boutdatascaled)

plot(boutdatascaled$num_notes_per_syll, boutdatascaled$num_syllables_per_num_unique) 

#preRef = refdf[1,3:5] %>% slice(rep(1, length(boutdatascaled$RecordingNum)))
#postRef = refdf[2,3:5] %>% slice(rep(1, length(boutdatascaled$RecordingNum)))
distancePre = sqrt((boutdatascaled$num_syllables_per_num_unique-preRef$meanNumSyllsPerUnique)^2+(boutdatascaled$num_syllables-preRef$meanNumSylls)^2+(boutdatascaled$num_notes_per_syll-preRef$meanNotesPerSyll)^2)
boutdatascaled$distancePre = distancePre
distancePost = sqrt((boutdatascaled$num_syllables_per_num_unique-postRef$meanNumSyllsPerUnique)^2+(boutdatascaled$num_syllables-postRef$meanNumSylls)^2+(boutdatascaled$num_notes_per_syll-postRef$meanNotesPerSyll)^2)
boutdatascaled$distancePost = distancePost
boutdatascaled %>% filter(Era == "Pre") %>% summarize(minPreDist = min(distancePre))
boutdatascaled %>% filter(Era == "Post") %>% summarize(minPostDist = min(distancePost))

boutdatascaled[which(boutdatascaled$distancePre < 0.1 | boutdatascaled$distancePost < 0.1),]

boutdatascaledDrought = boutdatascaled[which(boutdatascaled$Region == whichRegion),]
boutdatascaledDrought %>% filter(Era == "Pre") %>% summarize(minPreDist = min(distancePre))
boutdatascaledDrought %>% filter(Era == "Post") %>% summarize(minPostDist = min(distancePost))
boutdatascaledDrought[which(boutdatascaledDrought$distancePre < 0.23 | boutdatascaledDrought$distancePost < 0.12),]

species = "SOSP"
whichRegion = "Control"
refdfSub = refdf %>% filter(Region == whichRegion, Era %in% c("Pre","Post"))
pdf(file = paste0("scatterplot_scaled_features_",species,"_PrePost",whichRegion,"-LinearScale.pdf"))
ggplot(data = boutdata %>% filter(Region == whichRegion, Era != "During"), aes(x=num_notes_per_syll, y = num_syllables_per_num_unique, col = Era)) +
  geom_point() +
  scale_color_manual(values=c("#a6611a","#018571"))  +
  geom_point(data = refdfSub, aes(x=num_notes_per_syll,y=num_syllables_per_num_unique, shape = Era), size = 4, col = "gold") +
  ggtitle(label = paste(species, whichRegion, "Region, median center; pre = green, post = brown"))  #+
dev.off()

species = "DEJU"
whichRegion = "Drought"
refdfSub = refdf %>% filter(Region == whichRegion, Era %in% c("Pre","Post"))
pdf(file = paste0("scatterplot_scaled_features_",species,"_PrePost",whichRegion,"-LinearScale.pdf"))
ggplot(data = boutdata %>% filter(Region == whichRegion, Era != "During"), aes(x=bout_duration.ms., y = num_notes, col = Era)) +
  geom_point() +
  scale_color_manual(values=c("#a6611a","#018571"))  +
  geom_point(data = refdfSub, aes(y=num_notes,x=bout_duration.ms., shape = Era), size = 4, col = "gold") +
  ggtitle(label = paste(species, whichRegion, "Region, median center; pre = green, post = brown"))  #+
dev.off()

#### End find example recs --- 
 
refdf = data %>% group_by(Era,Region) %>% summarize(num_notes_per_syll = median(num_notes_per_syll, na.rm = T), num_syllables_per_num_unique = median(num_syllables_per_num_unique, na.rm = T))

refdf = data %>% group_by(Era,Region) %>% summarize(num_notes = median(num_notes, na.rm = T), bout_duration.ms. = median(bout_duration.ms., na.rm = T))
