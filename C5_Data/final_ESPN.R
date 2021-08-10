#' Ted Kwartler
#' Purpose: Obtain women's basketball statistics for Sport Analytics book
#' 

# Libs
library(rvest)
library(missForest)

# Inputs
yrs <- 2010:2020

# Construct the annual stats 
allYrs <- list()
for(i in 1:length(yrs)){
  espn <- paste0('http://www.espn.com/womens-college-basketball/statistics/team/_/year/',yrs[i],'/cat/')
  allYrs[[i]] <- espn
}
allYrs <- unlist(allYrs)

# For each yr, append the url for stat groups
allYrStats <- list()
for(i in 1:length(allYrs)){
  x <- paste0(allYrs[i], 1:13)
  allYrStats[[i]]<- x
}
allYrStats <- unlist(allYrStats)

# Obtain the tables from ESPN
allYrs <- list()
for(i in 1:length(yrs)){
  idx <- grep(yrs[i], allYrStats)
  
  oneYr <- list()
  for(j in 1:length(idx)){
    x <- read_html(allYrStats[j])
    x <- x %>% html_nodes(xpath = '//table') %>% html_table(fill = T)
    x <- x[[1]]
    
    # Clean up the page with the table error
    if(i == 4 & j ==7){ #aug 2021, webpage table error:http://www.espn.com/womens-college-basketball/statistics/team/_/year/2019/cat/7
      x$X9 <- NULL
      x$X8 <- NULL
      x <- x[-2,]
    }
    
    names(x) <- x[1,]
    x <- x[-1,]
    x$RK <- NULL
    oneYr[[j]] <- x
    #Sys.sleep(1)
    print(j)
  }
  y <- plyr::join_all(oneYr, by = c('NAME'= 'NAME',
                                    'GM' = 'GM',
                                    'W-L' = 'W-L'))
  y$yr <- yrs[i]
  names(y) <- make.names(names(y))
  y$win <- unlist(lapply(strsplit(y$W.L, '-'), head, 1))
  y$loss <- unlist(lapply(strsplit(y$W.L, '-'), tail, 1))
  

  allYrs[[i]] <- y
}

# Save a copy of data object
saveRDS(allYrs,'allData.rds')

# Simple CSV
allBB <- do.call(rbind, allYrs )
write.csv(allBB, 'raw_allBB.csv', row.names = F)

# Chk for missing PCT
round(colSums(apply(allBB, 2, is.na)) / nrow(allBB),2)

# Select the offense stats with least amount of missing
offIDX <- c(1,40,41,42,5, 15, 23, 26, 28, 33)
offensiveStats <- allBB[,offIDX]
names(offensiveStats) <- c('NAME', 'yr','win','loss','PPG', 
                           'FG_PCT','Three_Pt_FG_PCT','FT_PCT',
                           'Rebounds_PG','Assists_PG')
write.csv(offensiveStats, 'raw_offensive_stats.csv', row.names  = F)

# Select the defensive with the least amount of missing
defIDX <- c(1, 40,41,42, 11, 18, 30, 35, 37, 39)
defensiveStats <- allBB[,defIDX]
names(defensiveStats) <- c('NAME', 'yr', 'win', 'loss', 
                           'Opponent_Points_PG', 'Opponent_FG_PCT','OPP_Rebounds_PG', 
                           'Blocks_PG','Steals_PG', 'Turnovers_PG')
write.csv(defensiveStats, 'raw_defensive_stats.csv', row.names  = F)

# Imputation of missing w/RF
tmp <- apply(offensiveStats[,5:10], 2, as.numeric)
offensiveStatsClean <- missForest(tmp)
tmpRound <- round(offensiveStatsClean$ximp,1)
cleanOffensive <- data.frame(offensiveStats[,1:4], tmpRound)
summary(cleanOffensive)
write.csv(cleanOffensive, 'imputed_OffensiveStats.csv', row.names = F)

tmp <- apply(defensiveStats[,5:10], 2, as.numeric)
defensiveStatsClean <- missForest(tmp)
tmpRound <- round(defensiveStatsClean$ximp,1)
cleanDefensive <- data.frame(defensiveStats[,1:4], tmpRound)
write.csv(cleanDefensive, 'imputed_DefensiveStats.csv', row.names = F)

# End