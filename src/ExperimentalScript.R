# This script will create .csv regressor files that Matlab can use to make regressors.
# Just hit source at the top right corner and that should be it!


# -------------------------------------- set-up variables --------------------------------

# Load libraries.
library(tidyverse)
library(zoo)
library(plyr)
library(kableExtra)
library(magick)

# Remove global environment.
rm(list = ls())

# Create directory path.
parent.folder <- "D:/ObjectCategory_fMRI/data/BIDS_input"


# ------------------------------------- accuracy dataframe -------------------------------

# Empty dataframe for accuracy data.
Accuracy <- data.frame(Participant = integer(),
                 Accuracy = integer(), 
                 stringsAsFactors = FALSE)

#Loop through participants.
for (i in 1:length(list.files(parent.folder, recursive = FALSE))) {
  
  # Empty lists to store participant c's blocks.
  filelist <- list()
  
  # Iteration placeholder for each block.
  c <- 1
  
  # Chain of directory locations.
  Dir1 <- paste0("D:/ObjectCategory_fMRI/data/RawData/", list.files(parent.folder, recursive = FALSE)[i])
  Dir2 <- paste0(Dir1, "/", list.files(Dir1, recursive = FALSE)[1])
  Dir3 <- paste0(Dir2, "/", list.files(Dir2, "Behavioural"))
  Dir4 <- paste0(Dir3, "/")
  Dir5 <- paste0(Dir4, list.files(Dir4, pattern = ".txt$"))
  
  # Set working directory to product of directory chain.
  setwd(Dir3) 
  
  #Loop through each block for current participant.
  for (j in 1:length(Dir5)) {
    
    # Read each file and place in list.
    filelist[[c]] <- assign(paste0("Participant_", i, "Block_", j, "Modality.Congruency"), read.table(Dir5[j], header = TRUE, sep = "", dec = "."))
    
    # Add participant to accuracy dataframe.
    Accuracy <- rbind(Accuracy, data.frame("Participant" = filelist[[c]]$Subject,
                                           "Block" = filelist[[c]]$Block,
                                           "Modality" = filelist[[c]]$Modality,
                                           "Accuracy" = filelist[[c]]$Correct))
  }
}

# Change accuracy to integer.
Accuracy$Accuracy <- case_when(Accuracy$Accuracy == "True" ~ 1,
                               Accuracy$Accuracy == "False" ~ 0,
                               Accuracy$Accuracy == 0 ~ 0)

# Get mean accuracy per participant, per block, per modality.
Participant_ACC <- ddply(Accuracy, c("Participant", "Block", "Modality"), summarise, 
                         "Percent Correct" = mean(Accuracy) * 100)

# Nice output.
kbl(Participant_ACC, digits = 0) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  row_spec(which(Participant_ACC$`Percent Correct` < 80), bold = T, color = "white", background = "red") %>%
  save_kable("D:/ObjectCategory_fMRI/results/accuracy.scores.png")


# ----------------------------------- create regressors ---------------------------------

#Loop through participants.
for (i in 1:length(list.files(parent.folder, recursive = FALSE))) {
  
  # Empty lists to store participant c's blocks.
  filelist <- list()
  
  # Iteration placeholder for each block.
  c <- 1
  
  # Chain of directory locations.
  Dir1 <- paste0("D:/ObjectCategory_fMRI/data/RawData/", list.files(parent.folder, recursive = FALSE)[i])
  Dir2 <- paste0(Dir1, "/", list.files(Dir1, recursive = FALSE)[1])
  Dir3 <- paste0(Dir2, "/", list.files(Dir2, "Behavioural"))
  Dir4 <- paste0(Dir3, "/")
  Dir5 <- paste0(Dir4, list.files(Dir4, pattern = ".txt$"))
  
  # Set working directory to product of directory chain.
  setwd(Dir3)
  
  #Loop through each block for current participant.
  for (j in 1:length(Dir5)) {
    
    # Read each file and place in list.
    filelist[[c]] <- assign(paste0("Participant_", i, "Block_", j, "Modality.Congruency"), read.table(Dir5[j], header = TRUE, sep = "", dec = "."))
    
    # New column representing length of post duration.
    filelist[[c]]$diff <-  lead(filelist[[c]]$TrialOnset) - filelist[[c]]$ResponseOnset
    
    # Last column is na, so, change to column mean.
    filelist[[c]]$diff <- na.aggregate(filelist[[c]]$diff)
    
    # Placeholder onsets dataframe for current participant at current block.
    Onsets <- assign(paste0("Participant_", i, "Block_", j, "file"),
                      list("Olfaction.Congruent_CueOnset" = filelist[[j]]$cueOnset[filelist[[j]]$Modality == "Olfaction" & filelist[[j]]$Congruency == "1"] - filelist[[j]]$TrialOnset[1],
                           "Olfaction.Congruent_TargetOnset" = filelist[[j]]$targetOnset[filelist[[j]]$Modality == "Olfaction" & filelist[[j]]$Congruency == "1"] - filelist[[j]]$TrialOnset[1],
                           "Olfaction.Incongruent_CueOnset" = filelist[[j]]$cueOnset[filelist[[j]]$Modality == "Olfaction" & filelist[[j]]$Congruency == "0"] - filelist[[j]]$TrialOnset[1],
                           "Olfaction.Incongruent_TargetOnset" = filelist[[j]]$targetOnset[filelist[[j]]$Modality == "Olfaction" & filelist[[j]]$Congruency == "0"] - filelist[[j]]$TrialOnset[1],
                           "Visual.Congruent_CueOnset" = filelist[[j]]$cueOnset[filelist[[j]]$Modality == "Vision" & filelist[[j]]$Congruency == "1"] - filelist[[j]]$TrialOnset[1],
                           "Visual.Congruent_TargetOnset" = filelist[[j]]$targetOnset[filelist[[j]]$Modality == "Vision" & filelist[[j]]$Congruency == "1"] - filelist[[j]]$TrialOnset[1],
                           "Visual.Incongruent_CueOnset" = filelist[[j]]$cueOnset[filelist[[j]]$Modality == "Vision" & filelist[[j]]$Congruency == "0"] - filelist[[j]]$TrialOnset[1],
                           "Visual.Incongruent_TargetOnset" = filelist[[j]]$targetOnset[filelist[[j]]$Modality == "Vision" & filelist[[j]]$Congruency == "0"] - filelist[[j]]$TrialOnset[1],
                           "Olfaction.Congruent_CountdownOnset" = filelist[[j]]$CountdownOnset[filelist[[j]]$Modality == "Olfaction" & filelist[[j]]$Congruency == "1"] - filelist[[j]]$TrialOnset[1],
                           "Olfaction.Congruent_Post_Start" = filelist[[j]]$ResponseOnset[filelist[[j]]$Modality == "Olfaction" & filelist[[j]]$Congruency == "1"] - filelist[[j]]$TrialOnset[1],
                           "Olfaction.Incongruent_CountdownOnset" = filelist[[j]]$CountdownOnset[filelist[[j]]$Modality == "Olfaction" & filelist[[j]]$Congruency == "0"] - filelist[[j]]$TrialOnset[1],
                           "Olfaction.Incongruent_Post_Start" = filelist[[j]]$ResponseOnset[filelist[[j]]$Modality == "Olfaction" & filelist[[j]]$Congruency == "0"] - filelist[[j]]$TrialOnset[1],
                           "Visual.Congruent_CountdownOnset" = filelist[[j]]$CountdownOnset[filelist[[j]]$Modality == "Vision" & filelist[[j]]$Congruency == "1"] - filelist[[j]]$TrialOnset[1],
                           "Visual.Congruent_Post_Start" = filelist[[j]]$ResponseOnset[filelist[[j]]$Modality == "Vision" & filelist[[j]]$Congruency == "1"] - filelist[[j]]$TrialOnset[1],
                           "Visual.Incongruent_CountdownOnset" = filelist[[j]]$CountdownOnset[filelist[[j]]$Modality == "Vision" & filelist[[j]]$Congruency == "0"] - filelist[[j]]$TrialOnset[1],
                           "Visual.Incongruent_Post_Start" = filelist[[j]]$ResponseOnset[filelist[[j]]$Modality == "Vision" & filelist[[j]]$Congruency == "0"] - filelist[[j]]$TrialOnset[1]))
    
    # Where file is going.
    Dir1_a <- paste0("D:/ObjectCategory_fMRI/data/Regressors/", list.files(parent.folder, recursive = FALSE)[i])
    Dir2_a <- paste0(Dir1_a, "/")
    
     # Either: 1.write .csv file, or: 2. skip if exists in current directory:
    if (!file.exists(paste0(Dir2_a,"Modality.Congruency_Block_", j, "_Onsets.csv"))) {
      
      # 1. Write and save .csv file at current participant folder;
      write.csv(Onsets, paste0(Dir2_a,"Modality.Congruency_Block_", j, "_Onsets.csv"), na = "", row.names = FALSE)
      
    } else {
      
      # 2. or, skip and print message.
      print(paste0("Files: Modality.Congruency_Block_", j, "_Onsets.csv already Exist"))
      }
    
    # Placeholder durations dataframe for current participant at current block.
    Durations <- assign(paste0("Participant_", i, "Block_", j, "file"),
                     list("Olfaction.Congruent_CueOnset.Duration" = filelist[[j]]$CountdownOnset[filelist[[j]]$Modality == "Olfaction" & filelist[[j]]$Congruency == "1"] - filelist[[j]]$cueOnset [filelist[[j]]$Modality == "Olfaction" & filelist[[j]]$Congruency == "1"],
                          "Olfaction.Congruent_TargetOnset.Duration" = filelist[[j]]$ResponseOnset[filelist[[j]]$Modality == "Olfaction" & filelist[[j]]$Congruency == "1"] - filelist[[j]]$targetOnset[filelist[[j]]$Modality == "Olfaction" & filelist[[j]]$Congruency == "1"],
                          "Olfaction.Incongruent_CueOnset.Duration" = filelist[[j]]$CountdownOnset[filelist[[j]]$Modality == "Olfaction" & filelist[[j]]$Congruency == "0"] - filelist[[j]]$cueOnset[filelist[[j]]$Modality == "Olfaction" & filelist[[j]]$Congruency == "0"],
                          "Olfaction.Incongruent_TargetOnset.Duration" = filelist[[j]]$ResponseOnset[filelist[[j]]$Modality == "Olfaction" & filelist[[j]]$Congruency == "0"] - filelist[[j]]$targetOnset[filelist[[j]]$Modality == "Olfaction" & filelist[[j]]$Congruency == "0"],
                          "Visual.Congruent_CueOnset.Duration" = filelist[[j]]$CountdownOnset[filelist[[j]]$Modality == "Vision" & filelist[[j]]$Congruency == "1"] - filelist[[j]]$cueOnset[filelist[[j]]$Modality == "Vision" & filelist[[j]]$Congruency == "1"],
                          "Visual.Congruent_TargetOnset.Duration" = filelist[[j]]$ResponseOnset[filelist[[j]]$Modality == "Vision" & filelist[[j]]$Congruency == "1"] - filelist[[j]]$targetOnset[filelist[[j]]$Modality == "Vision" & filelist[[j]]$Congruency == "1"],
                          "Visual.Incongruent_CueOnset.Duration" = filelist[[j]]$CountdownOnset[filelist[[j]]$Modality == "Vision" & filelist[[j]]$Congruency == "0"] - filelist[[j]]$cueOnset[filelist[[j]]$Modality == "Vision" & filelist[[j]]$Congruency == "0"],
                          "Visual.Incongruent_TargetOnset.Duration" = filelist[[j]]$ResponseOnset[filelist[[j]]$Modality == "Vision" & filelist[[j]]$Congruency == "0"] - filelist[[j]]$targetOnset[filelist[[j]]$Modality == "Vision" & filelist[[j]]$Congruency == "0"],
                          "Olfaction.Congruent_Fixation.Duration" = filelist[[j]]$cueOnset[filelist[[j]]$Modality == "Olfaction" & filelist[[j]]$Congruency == "1"] - filelist[[j]]$TrialOnset[filelist[[j]]$Modality == "Olfaction" & filelist[[j]]$Congruency == "1"], 
                          "Olfaction.Congruent_Countdown.Duration" = filelist[[j]]$targetOnset[filelist[[j]]$Modality == "Olfaction" & filelist[[j]]$Congruency == "1"] - filelist[[j]]$CountdownOnset[filelist[[j]]$Modality == "Olfaction" & filelist[[j]]$Congruency == "1"],
                          "Olfaction.Congruent_Post.Duration" = filelist[[j]]$diff[filelist[[j]]$Modality == "Olfaction" & filelist[[j]]$Congruency == "1"],
                          "Olfaction.Incongruent_Fixation.Duration" = filelist[[j]]$cueOnset[filelist[[j]]$Modality == "Olfaction" & filelist[[j]]$Congruency == "0"] - filelist[[j]]$TrialOnset[filelist[[j]]$Modality == "Olfaction" & filelist[[j]]$Congruency == "0"],
                          "Olfaction.Incongruent_Countdown.Duration" = filelist[[j]]$targetOnset[filelist[[j]]$Modality == "Olfaction" & filelist[[j]]$Congruency == "0"] - filelist[[j]]$CountdownOnset[filelist[[j]]$Modality == "Olfaction" & filelist[[j]]$Congruency == "0"],
                          "Olfaction.Incongruent_Post.Duration" = filelist[[j]]$diff[filelist[[j]]$Modality == "Olfaction" & filelist[[j]]$Congruency == "0"],
                          "Visual.Congruent_Fixation.Duration" = filelist[[j]]$cueOnset[filelist[[j]]$Modality == "Vision" & filelist[[j]]$Congruency == "1"] - filelist[[j]]$TrialOnset[filelist[[j]]$Modality == "Vision" & filelist[[j]]$Congruency == "1"],
                          "Visual.Congruent_Countdown.Duration" = filelist[[j]]$targetOnset[filelist[[j]]$Modality == "Vision" & filelist[[j]]$Congruency == "1"] - filelist[[j]]$CountdownOnset[filelist[[j]]$Modality == "Vision" & filelist[[j]]$Congruency == "1"],
                          "Visual.Congruent_Post.Duration" = filelist[[j]]$diff[filelist[[j]]$Modality == "Vision" & filelist[[j]]$Congruency == "1"],
                          "Visual.Incongruent_Fixation.Duration" = filelist[[j]]$cueOnset[filelist[[j]]$Modality == "Vision" & filelist[[j]]$Congruency == "0"] - filelist[[j]]$TrialOnset[filelist[[j]]$Modality == "Vision" & filelist[[j]]$Congruency == "0"],
                          "Visual.Incongruent_Countdown.Duration" = filelist[[j]]$targetOnset[filelist[[j]]$Modality == "Vision" & filelist[[j]]$Congruency == "0"] - filelist[[j]]$CountdownOnset[filelist[[j]]$Modality == "Vision" & filelist[[j]]$Congruency == "0"],
                          "Visual.Incongruent_Post.Duration" = filelist[[j]]$diff[filelist[[j]]$Modality == "Vision" & filelist[[j]]$Congruency == "0"]))
    
    # Where file is going.
    Dir1_b <- paste0("D:/ObjectCategory_fMRI/data/Regressors/", list.files(parent.folder, recursive = FALSE)[i])
    Dir2_b <- Dir2_a <- paste0(Dir1_a, "/")
    
    # Either: 1.write .csv file, or: 2. skip if exists in current directory:
    if (!file.exists(paste0(Dir2_b,"Modality.Congruency_Block_", j, "_Durations.csv"))) {
      
      # 1. Write and save .csv file at current participant folder;
      write.csv(Durations, paste0(Dir2_b,"Modality.Congruency_Block_", j, "_Durations.csv"), na = "", row.names = FALSE)
      
    } else {
      
      # 2. or, skip and print message.
      print(paste0("Files: Modality.Congruency_Block_", j, "_Durations.csv already Exist"))
    }
    
    # Next participant.
    c <- c + 1
  }
}


# ----------------------------------- make folders  ---------------------------------

# Participant List
Participant_List <- list("sub-01", "sub-02", "sub-03", "sub-04")

# Loop makes directories if they don't exist.
for (i in Participant_List) {
  
  # Set working dierctory.
  parent.folder <- "D:/ObjectCategory_fMRI/data/BIDS_input"
  
  # Make these directories if they don't exist.
  if (!file.exists(i)) {
    
    # Directory creation.
    dir.create(file.path(parent.folder, i), showWarnings = FALSE) # Participant directory.
    dir.create(paste0(file.path(parent.folder, i), "/anat"), showWarnings = FALSE) # anat sub-directory.
    dir.create(paste0(file.path(parent.folder, i), "/func"), showWarnings = FALSE) # func sub-directory.
  }
}

# Remove excess variables.
rm(filelist, c, Dir1, Dir2, Dir3, Dir4, i, j, parent.folder, Dir1_a, Dir1_b, Dir2_a, Dir2_b, Dir5)