## CHECK EVERYTHING ##
computeGame <- function(x, y) {
    plot1 <<- resultsComparison(x, y)
    plot2 <<- goalsComparison(x, y)
}

# Compare results of people that played with each team
resultsComparison <- function(x, y) {
  dd <- filter(details, team %in% c(x, y)) %>% mutate(diff = gf-gc)
  
  dd <- left_join(dd, teams, by = c("team" = "id"))
  dd <- left_join(dd, teams, by = c("versus" = "id"))
  
  names(dd)[12] <- "localName"
  names(dd)[13] <- "visitName"
  
  max = max(dd$diff)
  
  ggplot(dd, aes(x=visitName, y=diff, fill=localName, group=localName)) + geom_bar(stat="identity", position="dodge") +
    scale_y_continuous(limits = c(-max, max), breaks = seq(-100, 100, by = 5), minor_breaks = seq(-100, 100, by=1)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(size=rel(2))) + ylab("Diferencia de gols") + xlab("rival") +
    labs(title = "Comparativa de diferencia de gols vs rivals") +
    geom_hline(yintercept=0)
}

# Compare goals data
goalsComparison <- function(x, y) {
    # get data for teams selected + get their names
    dd <- filter(details, team %in% c(x, y))
    dd <- left_join(dd, teams, by=c("team"="id"))
    
    # transform for better plotting
    dd <- gather(dd, "category", "goals", 5:6)
    
    # boxplot
    ggplot(dd, aes(teamName, goals)) + geom_boxplot(outlier.shape=NA) + geom_jitter() + facet_grid(category ~ win) +
        scale_y_continuous(breaks=seq(0, 100, by = 1), minor_breaks=seq(0, 100, by = 1)) +
        labs(title = "Gols en contra(gc) i a favor(gf), si Perdut(0) o Guanyat(1)") + ylab("Gols") +
        theme(plot.title = element_text(size=rel(2)))
}