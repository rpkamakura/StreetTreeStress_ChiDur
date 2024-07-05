## work on reformatting health check data

columnNames <- c("tree_name", "user_email", "project_name", "date_added", 
                 "dbh", "fine_twig_dieback", "leaf_discoloration", 
                 "leaf_defoliation", "crown_light_exposure", "crown_vigor", 
                 "crown_density_trans", "treeheight", "notes")

#get the raw data
ChicagoTreeLvlCombined <- read.csv("../01Data/02ChicagoSampling/StreetTreeData_v2023/091223AllYearsComb.csv")
DurhamTreeLvlCombined <- read.csv("../01Data/03DurhamSampling/SamplingS23/091523AllYearsCombDurham.csv")

#get 2023 paper
Chicago23 <- ChicagoTreeLvlCombined[ChicagoTreeLvlCombined$Year == 2023 & !is.na(ChicagoTreeLvlCombined$Discoloration),]
Durham23 <- DurhamTreeLvlCombined[DurhamTreeLvlCombined$Year == 2023 & !is.na(DurhamTreeLvlCombined$Discoloration),]

#round the data
Chicago23$CrownLight <- round(Chicago23$CrownLight)
Chicago23$Discoloration <- round(Chicago23$Discoloration)
Chicago23$Defoliation <- round(Chicago23$Defoliation)
Chicago23$Dieback <- round(Chicago23$Dieback)
Chicago23$CrownVigor <- round(Chicago23$CrownVigor)

TreesDur <- Durham23$TreeName
TreesChi <- Chicago23$TreeName

numTrees <- length(TreesChi) + length(TreesDur)

#create new dataframe
HTHCStorageMat <- as.data.frame(matrix(NA, nrow=numTrees, ncol=length(columnNames)))
names(HTHCStorageMat) <- columnNames

HTHCStorageMat$user_email <- rep("rep31@duke.edu", numTrees)

#start adding data
HTHCStorageMat$tree_name[1:length(TreesDur)] <- Durham23$TreeName
HTHCStorageMat$tree_name[(length(TreesDur)+1):(length(TreesDur)+length(TreesChi))] <- Chicago23$TreeName

HTHCStorageMat$date_added[1:length(TreesDur)] <- Durham23$Date
HTHCStorageMat$date_added[(length(TreesDur)+1):(length(TreesDur)+length(TreesChi))] <- Chicago23$Date

HTHCStorageMat$dbh[1:length(TreesDur)] <- Durham23$DBHTot
HTHCStorageMat$dbh[(length(TreesDur)+1):(length(TreesDur)+length(TreesChi))] <- Chicago23$TotalDBH

HTHCStorageMat$fine_twig_dieback[1:length(TreesDur)] <- Durham23$Dieback
HTHCStorageMat$fine_twig_dieback[(length(TreesDur)+1):(length(TreesDur)+length(TreesChi))] <- Chicago23$Dieback

HTHCStorageMat$leaf_discoloration[1:length(TreesDur)] <- Durham23$Discoloration
HTHCStorageMat$leaf_discoloration[(length(TreesDur)+1):(length(TreesDur)+length(TreesChi))] <- Chicago23$Discoloration

HTHCStorageMat$leaf_defoliation[1:length(TreesDur)] <- Durham23$Defoliation
HTHCStorageMat$leaf_defoliation[(length(TreesDur)+1):(length(TreesDur)+length(TreesChi))] <- Chicago23$Defoliation

HTHCStorageMat$crown_light_exposure[1:length(TreesDur)] <- Durham23$CrownLightExposure
HTHCStorageMat$crown_light_exposure[(length(TreesDur)+1):(length(TreesDur)+length(TreesChi))] <- Chicago23$CrownLight

HTHCStorageMat$crown_vigor[1:length(TreesDur)] <- Durham23$CrownVigor
HTHCStorageMat$crown_vigor[(length(TreesDur)+1):(length(TreesDur)+length(TreesChi))] <- Chicago23$CrownVigor

##Dictionary to modify Dieback
DBKcur <- 1:21
DBKorig <- c(1, seq(from=5, to=100, by=5))
DBKdf <- as.data.frame(matrix(0, nrow=21, ncol=2))
names(DBKdf) <- c("Current", "HTHC")

DBKdf$Current <- DBKcur
DBKdf$HTHC <- DBKorig

HTHCStorageMat$fine_twig_dieback <- as.numeric(HTHCStorageMat$fine_twig_dieback)
HTHCStorageMat$fine_twig_dieback <- DBKdf$HTHC[HTHCStorageMat$fine_twig_dieback]

##Pull tree project information and check that the trees are in HTHC
