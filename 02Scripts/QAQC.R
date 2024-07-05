##QAQC

#Get QAQC results for all the years in both cities

D22 <- read.csv("../01Data/QAQC/Durham2022.csv")
D22$Year <- 2022

##this is going to take a bit more work
D23_dup <- read.csv("../01Data/QAQC/Durham2023.csv")
D23_full <- read.csv("../01Data/QAQC/Durham2023_full.csv")
qaqc_trees <- unique(D23_dup$Tree.Name)
rows_toAdd <- D23_full[D23_full$Tree.Name %in% qaqc_trees, c("Tree.Name","Discoloration",
                                                             "Defoliation" ,"Dieback","CrownStress","Crown.Light.Exposure")]
D23 <- rbind(D23_dup[,c("Tree.Name","Discoloration", "Defoliation" ,"Dieback","CrownStress","Crown.Light.Exposure")], rows_toAdd)

D23$Year <- 2023


##chicago data
C21 <- read.csv("../01Data/QAQC/Chicago2021.csv")
C21$Year <- 2021
C22 <- read.csv("../01Data/QAQC/Chicago2022.csv")
C22$Year <- 2022
C23 <- read.csv("../01Data/QAQC/Chicago2023.csv")
C23$Year <- 2023

##get num trees

options <- c("Durham2022", "Durham2023", "AllDurham", "Chicago2021", 
             "Chicago2022", "Chicago2023", "AllChicago", "All")
nOpt <- length(options)

##do some renaming for ease
names(D22)[names(D22) == "NAME"] <- "TreeName"
names(D23)[names(D23) == "Tree.Name"] <- "TreeName"
names(C21)[names(C21) == "Tree.ID."] <- "TreeName"
names(C22)[names(C22) == "Tree.ID."] <- "TreeName"
names(C23)[names(C23) == "Tree.Name"] <- "TreeName"

names(D23)[names(D23) == "Crown.Light.Exposure"] <- "CrownLight"
names(C21)[names(C21) == "Crown.Light.Exposure"] <- "CrownLight"
names(C22)[names(C22) == "Crown.Light.Exposure"] <- "CrownLight"

names(C21)[names(C21) == "Leaf.Discoloration"] <- "Discoloration"
names(C22)[names(C22) == "Leaf.Discoloration"] <- "Discoloration"

names(C21)[names(C21) == "Leaf.Defoliation"] <- "Defoliation"
names(C22)[names(C22) == "Leaf.Defoliation"] <- "Defoliation"

names(C21)[names(C21) == "Fine.Twig.Dieback"] <- "Dieback"
names(C22)[names(C22) == "Fine.Twig.Dieback"] <- "Dieback"

names(C21)[names(C21) == "Crown.Vigor"] <- "CrownStress"
names(C22)[names(C22) == "Crown.Vigor"] <- "CrownStress"

#####Create overall storage matrix
cNms <- c("Option", "nTrees", "AvgDiscDiff", "AvgDefDiff", "AvgDbkDiff", "AvgCSDiff", "AvgCLEDiff")
QAQCMat <- as.data.frame(matrix(NA, ncol=length(cNms), nrow=nOpt))
names(QAQCMat) <- cNms

#get estimates for each dataset 
for (d in 1:nOpt){
  
  #pick the right df
  if (d == 1){ #Durham 2022
    df <- D22
    
  } else if (d == 2){ #Durham 2023
    df <- D23
    
  } else if (d == 3){ # All Durham
    aDur <- rbind(D22[,c("TreeName", "Discoloration", "Defoliation", "Dieback", "CrownStress", "CrownLight", "Year")], 
                  D23[,c("TreeName", "Discoloration", "Defoliation", "Dieback", "CrownStress", "CrownLight","Year")])
    
    df <- aDur
    
  } else if (d == 4){ #Chicago 2021
    df <- C21
    
  } else if (d == 5){ #Chicago 2022
    df <- C22
    
  } else if (d == 6){ #Chicago 2023
    df <- C23
    
  } else if (d == 7){ #All Chicago
    
    aChi <- rbind(C21[,c("TreeName", "Discoloration", "Defoliation", "Dieback", "CrownStress", "CrownLight", "Year")], 
                C22[,c("TreeName", "Discoloration", "Defoliation", "Dieback", "CrownStress", "CrownLight", "Year")], 
                C23[,c("TreeName", "Discoloration", "Defoliation", "Dieback", "CrownStress", "CrownLight", "Year")])
    
    df <- aChi
    
  } else if (d == 8){ #All
    
    a <- rbind(aDur, aChi)
    df <- a
    
  }
  
  #number of QAQC trees to look at
  trees <- unique(df$TreeName)
  ntrees <- length(trees)
  
  #create storage matrix
  stCols <- c("DiscDiff", "DefDiff", "DbkDiff", "CSDiff", "CLEDiff")
  StoMat <- as.data.frame(matrix(NA, ncol=length(stCols), nrow=ntrees*2)) #extra rows
  names(StoMat) <- stCols
  
  t_iter <- 1
  
  for (t in 1:ntrees){
    tr_nm <- trees[t]
    
    tdat <- df[c(df$TreeName == tr_nm),]
    
    #duplicate years
    if(length(tdat$TreeName) > 2){
      yrs <- unique(tdat$Year)
      
      for (y in 1:length(yrs)){
        
        td <- tdat[c(tdat$Year == yrs[y]),]
        
        dDisc <- abs(td$Discoloration[1] - td$Discoloration[2])
        dDef <- abs(td$Defoliation[1] - td$Defoliation[2])
        dDbk <- abs(td$Dieback[1] - td$Dieback[2])
        dCS <- abs(td$CrownStress[1] - td$CrownStress[2])
        dCLE <- abs(td$CrownLight[1] - td$CrownLight[2])
        
        StoMat$DiscDiff[t_iter] <- dDisc
        StoMat$DefDiff[t_iter] <- dDef
        StoMat$DbkDiff[t_iter] <- dDbk
        StoMat$CSDiff[t_iter] <- dCS
        StoMat$CLEDiff[t_iter] <- dCLE
        
        t_iter <- t_iter + 1
      }
      
    } else {
      
      dDisc <- abs(tdat$Discoloration[1] - tdat$Discoloration[2])
      dDef <- abs(tdat$Defoliation[1] - tdat$Defoliation[2])
      dDbk <- abs(tdat$Dieback[1] - tdat$Dieback[2])
      dCS <- abs(tdat$CrownStress[1] - tdat$CrownStress[2])
      dCLE <- abs(tdat$CrownLight[1] - tdat$CrownLight[2])
      
      StoMat$DiscDiff[t_iter] <- dDisc
      StoMat$DefDiff[t_iter] <- dDef
      StoMat$DbkDiff[t_iter] <- dDbk
      StoMat$CSDiff[t_iter] <- dCS
      StoMat$CLEDiff[t_iter] <- dCLE
      
      t_iter <- t_iter + 1
    } #depending on number of years
    
    
  } # for each tree
  
  QAQCMat$Option[d] <- options[d]
  QAQCMat$nTrees[d] <- length(na.omit(StoMat$DiscDiff))
  QAQCMat$AvgDiscDiff[d] <- mean(na.omit(StoMat$DiscDiff))
  QAQCMat$AvgDefDiff[d] <- mean(na.omit(StoMat$DefDiff))
  QAQCMat$AvgDbkDiff[d] <- mean(na.omit(StoMat$DbkDiff))
  QAQCMat$AvgCSDiff[d] <- mean(na.omit(StoMat$CSDiff))
  QAQCMat$AvgCLEDiff[d] <- mean(na.omit(StoMat$CLEDiff))
  
  
} ##df loop

write.csv(QAQCMat, "QAQCResults_raw.csv")


#####################################################now come in and change the categories to match what you are using for analyses

##Chicago
#collapsing the categories here for ease
C21$Dieback[((C21$Dieback > 3) & (C21$Dieback <= 6))] <- 4 #11-25%
C21$Dieback[(C21$Dieback > 6)] <- 5 #25%+
C21$Dieback[C21$CrownStress >= 5] <- 5 #trees are dead

C22$Dieback[((C22$Dieback > 3) & (C22$Dieback <= 6))] <- 4 #11-25%
C22$Dieback[(C22$Dieback > 6)] <- 5 #25%+
C22$Dieback[C22$CrownStress >= 5] <- 5 #trees are dead

C23$Dieback[((C23$Dieback > 3) & (C23$Dieback <= 6))] <- 4 #11-25%
C23$Dieback[(C23$Dieback > 6)] <- 5 #25%+
C23$Dieback[C23$CrownStress >= 5] <- 5 #trees are dead

##because there are not enough categories
C21$Defoliation[C21$Defoliation > 3] <- 3
C21$Discoloration[C21$Discoloration > 4] <- 4

C22$Defoliation[C22$Defoliation > 3] <- 3
C22$Discoloration[C22$Discoloration > 4] <- 4

C23$Defoliation[C23$Defoliation > 3] <- 3
C23$Discoloration[C23$Discoloration > 4] <- 4


#Durham
D22$Dieback[((D22$Dieback > 3) & (D22$Dieback <= 6))] <- 4 #11-25%
D22$Dieback[(D22$Dieback > 6)] <- 5 #25%+
D22$Dieback[D22$CrownStress >= 5] <- 5 #trees are dead

D23$Dieback[((D23$Dieback > 3) & (D23$Dieback <= 6))] <- 4 #11-25%
D23$Dieback[(D23$Dieback > 6)] <- 5 #25%+
D23$Dieback[D23$CrownStress >= 5] <- 5 #trees are dead

##because there are not enough categories
D22$Defoliation[D22$Defoliation > 3] <- 3
D22$Discoloration[D22$Discoloration > 4] <- 4

D23$Defoliation[D23$Defoliation > 3] <- 3
D23$Discoloration[D23$Discoloration > 4] <- 4


#####Create overall storage matrix
cNms <- c("Option", "nTrees", "AvgDiscDiff", "AvgDefDiff", "AvgDbkDiff", "AvgCSDiff", "AvgCLEDiff")
QAQCMat <- as.data.frame(matrix(NA, ncol=length(cNms), nrow=nOpt))
names(QAQCMat) <- cNms

#get estimates for each dataset 
for (d in 1:nOpt){
  
  #pick the right df
  if (d == 1){ #Durham 2022
    df <- D22
    
  } else if (d == 2){ #Durham 2023
    df <- D23
    
  } else if (d == 3){ # All Durham
    aDur <- rbind(D22[,c("TreeName", "Discoloration", "Defoliation", "Dieback", "CrownStress", "CrownLight", "Year")], 
                  D23[,c("TreeName", "Discoloration", "Defoliation", "Dieback", "CrownStress", "CrownLight","Year")])
    
    df <- aDur
    
  } else if (d == 4){ #Chicago 2021
    df <- C21
    
  } else if (d == 5){ #Chicago 2022
    df <- C22
    
  } else if (d == 6){ #Chicago 2023
    df <- C23
    
  } else if (d == 7){ #All Chicago
    
    aChi <- rbind(C21[,c("TreeName", "Discoloration", "Defoliation", "Dieback", "CrownStress", "CrownLight", "Year")], 
                  C22[,c("TreeName", "Discoloration", "Defoliation", "Dieback", "CrownStress", "CrownLight", "Year")], 
                  C23[,c("TreeName", "Discoloration", "Defoliation", "Dieback", "CrownStress", "CrownLight", "Year")])
    
    df <- aChi
    
  } else if (d == 8){ #All
    
    a <- rbind(aDur, aChi)
    df <- a
    
  }
  
  #number of QAQC trees to look at
  trees <- unique(df$TreeName)
  ntrees <- length(trees)
  
  #create storage matrix
  stCols <- c("DiscDiff", "DefDiff", "DbkDiff", "CSDiff", "CLEDiff")
  StoMat <- as.data.frame(matrix(NA, ncol=length(stCols), nrow=ntrees*2)) #extra rows
  names(StoMat) <- stCols
  
  t_iter <- 1
  
  for (t in 1:ntrees){
    tr_nm <- trees[t]
    
    tdat <- df[c(df$TreeName == tr_nm),]
    
    #duplicate years
    if(length(tdat$TreeName) > 2){
      yrs <- unique(tdat$Year)
      
      for (y in 1:length(yrs)){
        
        td <- tdat[c(tdat$Year == yrs[y]),]
        
        dDisc <- abs(td$Discoloration[1] - td$Discoloration[2])
        dDef <- abs(td$Defoliation[1] - td$Defoliation[2])
        dDbk <- abs(td$Dieback[1] - td$Dieback[2])
        dCS <- abs(td$CrownStress[1] - td$CrownStress[2])
        dCLE <- abs(td$CrownLight[1] - td$CrownLight[2])
        
        StoMat$DiscDiff[t_iter] <- dDisc
        StoMat$DefDiff[t_iter] <- dDef
        StoMat$DbkDiff[t_iter] <- dDbk
        StoMat$CSDiff[t_iter] <- dCS
        StoMat$CLEDiff[t_iter] <- dCLE
        
        t_iter <- t_iter + 1
      }
      
    } else {
      
      dDisc <- abs(tdat$Discoloration[1] - tdat$Discoloration[2])
      dDef <- abs(tdat$Defoliation[1] - tdat$Defoliation[2])
      dDbk <- abs(tdat$Dieback[1] - tdat$Dieback[2])
      dCS <- abs(tdat$CrownStress[1] - tdat$CrownStress[2])
      dCLE <- abs(tdat$CrownLight[1] - tdat$CrownLight[2])
      
      StoMat$DiscDiff[t_iter] <- dDisc
      StoMat$DefDiff[t_iter] <- dDef
      StoMat$DbkDiff[t_iter] <- dDbk
      StoMat$CSDiff[t_iter] <- dCS
      StoMat$CLEDiff[t_iter] <- dCLE
      
      t_iter <- t_iter + 1
    } #depending on number of years
    
    
  } # for each tree
  
  QAQCMat$Option[d] <- options[d]
  QAQCMat$nTrees[d] <- length(na.omit(StoMat$DiscDiff))
  QAQCMat$AvgDiscDiff[d] <- mean(na.omit(StoMat$DiscDiff))
  QAQCMat$AvgDefDiff[d] <- mean(na.omit(StoMat$DefDiff))
  QAQCMat$AvgDbkDiff[d] <- mean(na.omit(StoMat$DbkDiff))
  QAQCMat$AvgCSDiff[d] <- mean(na.omit(StoMat$CSDiff))
  QAQCMat$AvgCLEDiff[d] <- mean(na.omit(StoMat$CLEDiff))
  
  
} ##df loop

write.csv(QAQCMat, "QAQCResults_mod.csv")
