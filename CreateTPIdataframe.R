###################################################################################
# Author: Daniel Krueger and Jacob Miller

# TPI Network Analyses
###################################################################################


###################################################################################
########################### install fc.rstats #####################################
devtools::install_github("FCrSTATS/fc.rstats")

###################################################################################


###################################################################################
############################ load libraries #######################################
require(FC.rSTATS)
require(rvest)
require(dplyr)
require(tnet)
#setwd("~/Desktop/Thesis/opta_new copy")

###################################################################################


###################################################################################
############################ readfc24 function ####################################
filename='f24-98-2012-429860-eventdetails.xml'
readf24 <- function(filename){
  
  ############################################################################# 
  ####################### get all game events ################################# 
  #####data frame
  events <- parse_f24(filename)
  #####xml
  html <- xmlParse(filename)
  xmlData <- xmlToList(html)
  
  ###get game attributes
  gameAttrs <- xmlData$Game$.attrs
  gameDate <- strsplit(gameAttrs['game_date'], "T")[[1]][1]
  seasonName <- gameAttrs['season_name']
  seasonID <- gameAttrs['season_id']
  gameTime <- strsplit(gameAttrs['game_date'], "T")[[1]][2]
  
  ###remove deleted events
  events <- events[-which(events$type_id==43), ]
  head(events)
  
  ###get team IDs
  homeID <- gameAttrs['home_team_id']
  homeTeam <- gameAttrs['home_team_name']
  awayID <- gameAttrs['away_team_id']
  awayTeam <- gameAttrs['away_team_name']
  print(paste('Home team ID and name:',homeID, homeTeam))
  print(paste('Away team ID and name:', awayID, awayTeam))
  ############################################################################# 
  
  
  
  #get home/away team data
  ############################################################################# 
  ########################### home data ######################################
  homeData <- events[which(events$team_id==homeID), ]
  
  ###first two rows are qualifiers
  homeQuals <- homeData[1:2, ]
  homeData <- homeData[3:nrow(homeData), ]
  
  ###order by period_id, min, sec
  homeData <- homeData[order(homeData$period_id, homeData$min, homeData$sec), ]
  
  ###make a marker for home data frame
  homeData$marker <- 1:nrow(homeData)
  
  ###get total number of goals
  homeGoals <- nrow(homeData[which(homeData$type_id==16),])
  
  ###subset to successful passes only
  homePassData <- homeData[which(homeData$type_id==1 & homeData$outcome==1), ]
  #####order by marker for consistency
  homePassData <- homePassData[order(homePassData$marker), ]
  

  #build edge data frame
  edgeDict <- c()
  edgeDict['Edge'] <- NA
  
  ###iterate through rows of passing data to build edgeData
  for(row in 1:nrow(homePassData)){
    
    #get sender name, next event's row ID, and receiver's name
    sender <- homePassData[row,]$player_id
    rowID <- homePassData[row, ]$marker + 1
    receiver <- homeData[rowID, ]$player_id
    
    ###define edge as "senderName-receiverName"
    edge <- paste0(sender, "-",receiver)
    #####if receiver is NA, then skip this pass
    if(is.na(receiver)){
      next
    }
    #####define a new edge if edge is not in the dictionary
    else if(is.na(edgeDict[edge])){
      edgeDict[edge] <- 1
    }
    #####otherwise, add one to the current edge's value
    else{
      edgeDict[edge] <- edgeDict[edge] + 1
    }
  }
  
  ###convert dictionary to data frame
  edgeData <- as.data.frame(edgeDict[2:length(edgeDict)])

  ###convert to 3 column data frame with the following columns:
  ### senderID, receiverID, numPasses
  idList <- rownames(edgeData)
  numPassesList <- as.list(edgeData)[[1]]
  senderList <- c()
  receiverList <- c()
  #####iterate through idList to build tnet compatible data frame
  for(row in 1:length(idList)){
    
    #parse edge into senderID and receiverID
    stringlist <- strsplit(idList[row],'-')[[1]]
    senderID <- stringlist[1]
    receiverID <- stringlist[2]
    
    senderList[row] <- senderID
    receiverList[row] <- receiverID
  }
  #####bind columns together in final tnet compatible data frame
  tnetframe <- cbind(as.numeric(senderList), 
                     as.numeric(receiverList),
                     as.numeric(numPassesList))
  
  
  #run tnet on tnetframe
  netDataH <- degree_w(tnetframe)
  
  ###get values for edges in our data
  netDataH <- as.data.frame(netDataH[which(netDataH[,2]!=0), ])
  #####calculate TPI score
  avgPosH <- homePassData %>%
    group_by(player_id) %>%
    summarise(x=mean(x), y=mean(y)) %>% 
    arrange(as.numeric(player_id))
  #add to netdata for home
  netDataH$x <- avgPosH$x
  netDataH$y <- avgPosH$y
  netDataH <- netDataH %>%
    mutate(alpha=ifelse(x<=33.3, .5, ifelse(x>=33.3&x<66.6, 1, ifelse(x>=66.6, 1.5,NA))))
  tpiListH <- c()
  for(i in 1:nrow(netDataH)){
    tpiH_i <- (netDataH$degree[i])*(netDataH$output[i]/netDataH$degree[i])^netDataH$alpha[i]
    tpiListH[i] <- tpiH_i
  }
  
  hometpiScore <- sum(tpiListH)
  #####build final data frame
  dataFhome <- data.frame(team=homeTeam, tpi=hometpiScore, 
                          goals=homeGoals,
                          gameDate=gameDate, gameTime=gameTime,
                          seasonID=seasonID, seasonName=seasonName)
  ############################################################################# 
  
  
  ############################################################################# 
  ########################### away data #######################################
  awayData <- events[which(events$team_id==awayID), ]
  
  ###first two rows are qualifiers
  awayQuals <- awayData[1:2, ]
  awayData <- awayData[3:nrow(awayData), ]
  
  ###order by period_id, min, sec
  awayData <- awayData[order(awayData$period_id, awayData$min, awayData$sec), ]
  
  ###make a marker for home data frame
  awayData$marker <- 1:nrow(awayData)
  
  ###get total number of goals
  awayGoals <- nrow(awayData[which(awayData$type_id==16),])
  
  ###subset to successful passes only
  awayPassData <- awayData[which(awayData$type_id==1 & awayData$outcome==1), ]
  #####order by marker for consistency
  awayPassData <- awayPassData[order(awayPassData$marker), ]
  
  
  #build edge data frame
  edgeDict <- c()
  edgeDict['Edge'] <- NA
  
  ###iterate through rows of passing data to build edgeData
  for(row in 1:nrow(awayPassData)){
    
    #get sender name, next event's row ID, and receiver's name
    sender <- awayPassData[row,]$player_id
    rowID <- awayPassData[row, ]$marker + 1
    receiver <- awayData[rowID, ]$player_id
    
    ###define edge as "senderName-receiverName"
    edge <- paste0(sender, "-",receiver)
    #####if receiver is NA, then skip this pass
    if(is.na(receiver)){
      next
    }
    #####define a new edge if edge is not in the dictionary
    else if(is.na(edgeDict[edge])){
      edgeDict[edge] <- 1
    }
    #####otherwise, add one to the current edge's value
    else{
      edgeDict[edge] <- edgeDict[edge] + 1
    }
  }
  
  ###convert dictionary to data frame
  edgeData <- as.data.frame(edgeDict[2:length(edgeDict)])
  
  ###convert to 3 column data frame with the following columns:
  ### senderID, receiverID, numPasses
  idList <- rownames(edgeData)
  numPassesList <- as.list(edgeData)[[1]]
  senderList <- c()
  receiverList <- c()
  #####iterate through idList to build tnet compatible data frame
  for(row in 1:length(idList)){
    
    #parse edge into senderID and receiverID
    stringlist <- strsplit(idList[row],'-')[[1]]
    senderID <- stringlist[1]
    receiverID <- stringlist[2]
    
    senderList[row] <- senderID
    receiverList[row] <- receiverID
  }
  #####bind columns together in final tnet compatible data frame
  tnetframe <- cbind(as.numeric(senderList), 
                     as.numeric(receiverList),
                     as.numeric(numPassesList))
  
  #run tnet on tnetframe
  netDataA <- degree_w(tnetframe)
  
  ###get values for edges in our data
  netDataA <- as.data.frame(netDataA[which(netDataA[,2]!=0), ])
  #####calculate TPI score
  avgPosA <- awayPassData %>%
    group_by(player_id) %>%
    summarise(x=mean(x), y=mean(y)) %>% 
    arrange(as.numeric(player_id))
  netDataA$x <- avgPosA$x
  netDataA$y <- avgPosA$y
  netDataA <- netDataA %>%
    mutate(alpha=ifelse(x<=33.3, .5, ifelse(x>=33.3&x<66.6, 1, ifelse(x>=66.6, 1.5,NA))))
  tpiListA <- c()
  for(i in 1:nrow(netDataA)){
    tpiA_i <- (netDataA$degree[i])*(netDataA$output[i]/netDataA$degree[i])^netDataA$alpha[i]
    tpiListA[i] <- tpiA_i
  }
  awaytpiScore <- sum(tpiListA)
  #####build final data frame
  dataFaway <- data.frame(team=awayTeam, tpi=awaytpiScore, 
                          goals=awayGoals,
                          gameDate=gameDate, gameTime=gameTime,
                          seasonID=seasonID, seasonName=seasonName)
  #############################################################################
  
  
  #############################################################################
  ######################### final data frame ##################################
  ###determine winner by comparing number of goals scores by each team
  if(homeGoals > awayGoals){
    homeWin = 1
    awayWin = 0
    tie = 0
  }
  else if(homeGoals < awayGoals){
    homeWin = 0
    awayWin = 1
    tie = 0
  }
  else{
    homeWin = 0
    awayWin = 0
    tie = 1
  }
  ###home and away data frames
  #####home
  dataFhome <- data.frame(team=homeTeam, tpi=hometpiScore, 
                          goals=homeGoals, win=homeWin, tie=tie,
                          gameDate=gameDate, gameTime=gameTime,
                          seasonID=seasonID, seasonName=seasonName)
  #####away
  dataFaway <- data.frame(team=awayTeam, tpi=awaytpiScore, 
                          goals=awayGoals, win=awayWin, tie=tie,
                          gameDate=gameDate, gameTime=gameTime,
                          seasonID=seasonID, seasonName=seasonName)
  ####final data frame
  dataF <- rbind(dataFhome,dataFaway)
  return(dataF)
  #############################################################################
  
  
}
###################################################################################
files <- list.files(pattern='.xml')
dataF <- data.frame(team=NA, tpi=NA, 
                    goals=NA, win=NA, tie=NA,
                    gameDate=NA, gameTime=NA,
                    seasonID=NA, seasonName=NA)
for(file in files){
  fileData <- readf24(file)
  dataF <- rbind(dataF, fileData)
  cat('\n\n')
}
dataF <- dataF[-1,]