############   2AggrAvg.R aggregates TSDat into AggrDat, uses averages.
###                     takes TSDat123.csv from 1ProcDat.R as input
#
setwd("/")

{############################## Get data ###############################################
  load("PolsVars.Rdata")
  TSDat <- read.table('TSDat123.csv', sep=",", header=TRUE, stringsAsFactors = FALSE)
  TSDat$Llama <- 0;   TSDat$Llama[TSDat$NGA == "Cuzco"] <- 1   ### "Other" animal = Llama
  rm(variables,ArchaeoVars,DistMatrix, CanonVars) #keep polities_all in memory so PolName and World.Region can be added 
}

{#################################### Aggregate variables into CCs ###########################
  AggrSCDat <- data.frame(NGA=TSDat$NGA, PolID=TSDat$PolID, Time=TSDat$Time, 
                          Pop=NA, Cap=NA, Terr=NA, Agri=NA, SL=NA, PolName=NA, uniq=TSDat$uniq, Dupl=TSDat$Dupl)                   
  for(i in 1:nrow(TSDat)){
    if(TSDat$PolPop[i] != 0 & TSDat$PolPop[i] != -999){AggrSCDat$Pop[i] <- log10(TSDat$PolPop[i])}       ### Pop
    if(TSDat$PolTerr[i] != 0 & TSDat$PolTerr[i] != -999){AggrSCDat$Terr[i] <- log10(TSDat$PolTerr[i])}    ### Terr 
    if(TSDat$CapPop[i] != 0 & TSDat$CapPop[i] != -999){AggrSCDat$Cap[i] <- log10(TSDat$CapPop[i])}       ### Cap
    
    # Settlement levels
    dt <- subset(TSDat, select = c(SettlHier))[i,]                                  ### SL = SettlHier
    dt <- dt[dt != -999]
    if(length(dt) > 0){AggrSCDat$SL[i] <- dt} 
                              
    ##### Add PolName and World.Region to AggrSCDat
    
    AggrSCDat$PolName[i] <- polities_all$PolName[which(polities_all$PolID == TSDat[i,]$PolID)][1]
    }
}

AggrDat <- AggrSCDat


{#########################################################################
  ### Agri 
  HistYield <- read.table('HistYield+.csv', sep=",", header=TRUE, stringsAsFactors = 	FALSE)
  AggrDat$Agri <- NA
  #AggrDat$Agri_t1 <- NA
  for(i in 1:nrow(AggrDat)){
    NGA <- AggrDat$NGA[i]
    Time <- AggrDat$Time[i]
    if(Time >= -10000){
      HY <- HistYield[, HistYield[1,] == Time]
      HY <- HY[HistYield[,1] == NGA]
      if(length(HY) == 1) {AggrDat$Agri[i] <- HY}
    }
  }  
}
 

  
AggrDat <- AggrDat[AggrDat$Dupl == "n",]
AggrDat <- AggrDat[AggrDat$uniq == "y",]
  
# Remove erroneous territory data
AggrDat[which(AggrDat$PolID=='MlJeJe1'),]$Terr = NA # Cap==Pop; Village territory can't be calculated
AggrDat[which(AggrDat$PolID=='EcJivaE'),]$Terr = NA # Cap==Pop; Village territory can't be calculated
AggrDat[which(AggrDat$PolID=='USMisSp'),]$Terr = NA # Cap==Pop; Village territory can't be calculated
AggrDat[which(AggrDat$PolID=='IrSusa1'),]$Terr = NA # Terr given is area size of Cap, not Terr
  
  
save(AggrDat, file = "/home/david/work/Supplementary Information (Urbanization)/TableData.Rdata")


