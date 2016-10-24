library(dplyr)
library(tidyr)
library(googlesheets)

processWeeks <- function(i) {
    # Auth into drive
    gs_auth()
    
    # Read teams current information
    ss <<- gs_url("https://docs.google.com/spreadsheets/d/1_Vu1pKEjHWTlYiHPUW5GC5t4XTsHxs0Ni4Sjr44oAqk")
    teams <<- gs_read_csv(ss, "teams")
    ranking <<- gs_read_csv(ss, "ranking")
    details <<- gs_read_csv(ss, "details")
    
    # Process weeks data
    i <- as.matrix(i)
    apply(i, 1, processWeek)
    
    # Update classification
    computeRanking(teams, details)
    
    # Upload ranking and details
    gs_edit_cells(ss, "ranking", ranking, "A1")
    gs_edit_cells(ss, "details", details, "A1")
}


processWeek <- function(i) {
    # Sys sleep
    Sys.sleep(4)
    
    # Read week games
    games <- gs_read_csv(ss, paste0("week", i))
    
    # Craete details of games
    invisible(apply(games, 1, extractGamesData, i=i))
}

extractGamesData <- function(match, i) {
    draw = (match[5] == match[6])
    localWin = (match[5] > match[6])
    
    # Transform to int
    match <- strtoi(match)
    
    # Store match details
    gameDetails <- data.frame()
    gameDetails[1, "week"] <- i
    gameDetails[1, "team"] <- match[1]
    gameDetails[1, "versus"] <- match[2]
    gameDetails[1, "local"] <- 1
    gameDetails[1, "gf"] <- match[5]
    gameDetails[1, "gc"] <- match[6]
    gameDetails[1, "win"] <- ifelse(localWin, 1, 0)
    gameDetails[1, "lose"] <- ifelse(!localWin & !draw, 1, 0)
    gameDetails[1, "draw"] <- ifelse(draw, 1, 0)
    gameDetails[1, "points"] <- ifelse(draw, 1, ifelse(localWin, 2, 0))
    
    gameDetails[2, "week"] <- i
    gameDetails[2, "team"] <- match[2]
    gameDetails[2, "versus"] <- match[1]
    gameDetails[2, "local"] <- 0
    gameDetails[2, "gf"] <- match[6]
    gameDetails[2, "gc"] <- match[5]
    gameDetails[2, "win"] <- ifelse(!localWin & !draw, 1, 0)
    gameDetails[2, "lose"] <- ifelse(localWin, 1, 0)
    gameDetails[2, "draw"] <- ifelse(draw, 1, 0)
    gameDetails[2, "points"] <- ifelse(draw, 1, ifelse(localWin, 0, 2))
    
    details <<- rbind(details, gameDetails)
}


computeRanking <- function(teams, details) {
    # Join team names with details
    joined <- left_join(teams, details, by = c("id" = "team"))

    # Compute ranking
    ranking <<- group_by(joined, teamName, id) %>%
        summarise(points = sum(points), pj = n(), pg = sum(win), pe = sum(draw), pp = sum(lose), gf = sum(gf), gc = sum(gc)) %>%
        mutate(diff = gf-gc) %>%
        ungroup() %>%
        arrange(desc(points, diff))
}

deleteWeeks <- function(i) {
    # Auth into drive
    gs_auth()
    
    # Read teams current information
    ss <<- gs_url("https://docs.google.com/spreadsheets/d/1_Vu1pKEjHWTlYiHPUW5GC5t4XTsHxs0Ni4Sjr44oAqk")
    teams <<- gs_read_csv(ss, "teams")
    details <<- gs_read_csv(ss, "details")
    ranking <<- gs_read_csv(ss, "ranking")
    
    # Delete weeks data
    i <- as.matrix(i)
    apply(i, 1, deleteWeek)
    
    # Update classification
    computeRanking(teams, details)
    
    # Upload ranking and details
    gs_edit_cells(ss, "ranking", ranking, "A1")
    details[is.na(details)] <- ""
    gs_edit_cells(ss, "details", details, "A1")
}

deleteWeek <- function(i) {
    # Remove affected rows and compute new ranking
    toRemove <- details[details$week == i, ]
    toRemove <- as.data.frame(matrix(nrow = nrow(toRemove), ncol = ncol(toRemove)))
    colnames(toRemove) <- colnames(details)
    details <<- details[!details$week == i, ]
    details <<- rbind(details, toRemove)
}