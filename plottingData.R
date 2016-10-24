resultsComparison <- function(x, y) {
  dd <- filter(details, team %in% c(x, y)) %>% mutate(diff = gf-gc)
  
  dd <- left_join(dd, teams, by = c("team" = "id"))
  dd <- left_join(dd, teams, by = c("versus" = "id"))
  
  names(dd)[12] <- "localName"
  names(dd)[13] <- "visitName"
  
  ggplot(dd, aes(x=visitName, y=diff, fill=localName, group=localName)) + geom_bar(stat="identity", position="dodge") +
    scale_y_continuous(limits = c(-10, 10)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_hline(yintercept=0)
}