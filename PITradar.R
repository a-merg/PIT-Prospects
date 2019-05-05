PITradar <- function(guy, archetype, floor){
  
  ## Read in data (source: Draft Model Starter Kit - Will Schreefer)
  NCAAmain <- read_excel("NCAARealGM.xlsx", sheet = 1, col_names = TRUE)
  NCAAbpm <- read_excel("FinalBPM.xlsx", sheet = 1, col_names = TRUE)
  PastPIT <- read_excel("Past PIT Prospects.xlsx", sheet = 1, col_names = TRUE)
  CurrentPIT <- read_excel("Past PIT Prospects.xlsx", sheet = 2, col_names = TRUE)
  
  ## Set up columns
  colnames(NCAAmain) <- NCAAmain[1,]
  NCAAmain1 <- NCAAmain[2:30217,]
  NCAAmain1 <- NCAAmain1[,!duplicated(colnames(NCAAmain1))]
  colnames(NCAAbpm)[6:7] <- c("Est-OBPM", "Est-DBPM")
  
  ## Filter for senior year data, removing players w/out any games played
  SrNCAA <- subset(NCAAmain1, `Year at School` == "Sr")
  SrNCAA <- subset(SrNCAA, `GP` != "-")
  max_year <- NCAAbpm %>% group_by(PlayerName) %>% summarise(TermYear = max(TermYear))
  SrBPM <- left_join(max_year, NCAAbpm)
  
  ## Modify main NCAA name column to leave only player names
  colnames(SrNCAA)[1] <- "Player"
  SrNCAA$Player <- gsub("/player/", "", SrNCAA$Player)
  SrNCAA$Player <- gsub("/Summary", "", SrNCAA$Player)
  SrNCAA <- separate(SrNCAA, Player, "PlayerName", sep = "/")
  SrNCAA <- SrNCAA %>% mutate_all(funs(gsub("-", " ", .)))
  
  ## Remove duplicate player names from main NCAA data frame
  SrNCAA <- SrNCAA[!duplicated(SrNCAA$PlayerName),]
  
  ## Merge NCAA main and Past PIT data frames
  SrNCAAxBPM <- merge(SrNCAA, SrBPM)
  PITmerge <- merge(SrNCAAxBPM, PastPIT, by.x = "PlayerName", by.y = "Player")
  
  ## Condense Position variables to "Bigs" "Wings" "Guards"
  PITmerge$Position <- gsub("C", "Bigs", PITmerge$Position)
  PITmerge$Position <- gsub("PF", "Bigs", PITmerge$Position)
  PITmerge$Position <- gsub("FBigs", "Bigs", PITmerge$Position)
  PITmerge$Position <- gsub("F-Bigs", "Bigs", PITmerge$Position)
  PITmerge$Position <- gsub("SF", "Wings", PITmerge$Position)
  PITmerge$Position <- gsub("G-F", "Wings", PITmerge$Position)
  PITmerge$Position <- gsub("GF", "Wings", PITmerge$Position)
  PITmerge$Position <- gsub("F", "Wings", PITmerge$Position)
  PITmerge$Position <- gsub("PG", "Guards", PITmerge$Position)
  PITmerge$Position <- gsub("SG", "Guards", PITmerge$Position)
  PITmerge$Position <- gsub("G", "Guards", PITmerge$Position)
  PITmerge$Position <- gsub("Guardsuards", "Guards", PITmerge$Position)
  
  PITmerge2 <- PITmerge[!PITmerge$PlayerName == "Justin Robinson",]
  
  ## Merge current PIT data frame
  PITfull <- merge(PITmerge2, CurrentPIT, all.x = TRUE, all.y = TRUE)
  
  if (!floor %in% c("offense", "defense")){
    stop('invalid outcome')
  } else if (floor == "offense") {
    PIToff <- PITfull[c("PlayerName", "Position", "Highest Level Reached", "Year", 
                        "School", "GP", "MIN", "3PA","3P%", "FTA","FT%", "FT/FGA", 
                        "PTS", "OWS","USG%","TS%","AST%", "Ast/TO", "TOV%")]
    PIToff[,6:19] <- as.data.frame(sapply(PIToff[,6:19], as.numeric))
    PIToff <- PIToff[rowSums(is.na(PIToff)) == 0,]
    PIToff <- PIToff %>% mutate(`PTS/36` = (((GP * PTS)/(GP * MIN)*36)))
    PIToff <- PIToff %>% mutate(`3PA/36` = (((GP * `3PA`)/(GP * MIN)*36)))
    PIToffPosition <- subset(PIToff, Position == archetype)
    OffPosPercentile <- cbind(PIToffPosition[,1:6], 
                                 as.data.frame(sapply(PIToffPosition[, 7:21], percent_rank)))
    OffPosNBA <- subset(OffPosPercentile, 
                           OffPosPercentile$`Highest Level Reached` == "NBA")
    OffPosMedian <- sapply(OffPosNBA, median)
    OffPosMedian <- as.data.frame(t(as.data.frame(OffPosMedian)))
    OffPosMedian[1] <- "NBA-level Median"
    MedOffPos <- rbind(OffPosPercentile, OffPosMedian)
    MedOffPos[,6:21] <- as.data.frame(sapply(MedOffPos[,6:21], as.numeric))
    OffPos_radar <- MedOffPos %>% 
      filter(PlayerName == guy | PlayerName == "NBA-level Median") %>% 
      select(1,15:16,20:21,12,17,19)
    ggradar(OffPos_radar, group.line.width = .75, 
            group.point.size = 1.75, 
            legend.text.size = 10, 
            grid.label.size = 4, 
            gridline.mid.colour = "grey", 
            background.circle.transparency = .1, 
            font.radar = "Verdana", 
            legend.position = "bottom", 
            axis.label.offset = 1.12)
  } else {
    PITdef <- PITfull[c("PlayerName", "Position", "Highest Level Reached", "Year", 
                        "School", "GP", "MIN", "PF", "TRB%","STL%", "BLK%", "DWS")]
    PITdef[,6:12] <- as.data.frame(sapply(PITdef[,6:12], as.numeric))
    PITdef <- PITdef[rowSums(is.na(PITdef)) == 0,]
    PITdef <- PITdef %>% mutate(`FL/36` = (((GP * PF)/(GP * MIN)*36)))
    PITdefPosition <- subset(PITdef, Position == archetype)
    DefPosPercentile <- cbind(PITdefPosition[,1:6], 
                                 as.data.frame(sapply(PITdefPosition[, 7:13], percent_rank)))
    DefPosNBA <- subset(DefPosPercentile, 
                           DefPosPercentile$`Highest Level Reached` == "NBA")
    DefPosMedian <- sapply(DefPosNBA, median)
    DefPosMedian <- as.data.frame(t(as.data.frame(DefPosMedian)))
    DefPosMedian[1] <- "NBA-level Median"
    MedDefPos <- rbind(DefPosPercentile, DefPosMedian)
    MedDefPos[,6:13] <- as.data.frame(sapply(MedDefPos[,6:13], as.numeric))
    DefPos_radar <- MedDefPos %>% 
      filter(PlayerName == guy | PlayerName == "NBA-level Median") %>% 
      select(1,7,9:13)
    ggradar(DefPos_radar, group.line.width = .75, 
            group.point.size = 1.75, 
            legend.text.size = 10, 
            grid.label.size = 4, 
            gridline.mid.colour = "grey", 
            background.circle.transparency = .1, 
            font.radar = "Verdana", 
            legend.position = "bottom", 
            axis.label.offset = 1.12)
  }}
    