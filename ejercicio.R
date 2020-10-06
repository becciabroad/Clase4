# load data and packages:
library(polAr)
library(tidyverse)
library(janitor)

# Show available data
show_available_elections(viewer = T)

# Show presidential elections of 2003
argentina <- get_election_data("arg",	"presi",	"gral",	2003)

# Separate results for presidential elections in 2003 for province of Misiones:
misiones <- argentina %>%
  filter(name_prov == "MISIONES")

# Formate votes to precentages
misiones %>%
  ungroup() %>%
  select(votos, listas) %>%
  group_by(listas) %>%
  mutate(total = sum(votos),
         votos_pct = round(votos / total *100,2),
         pct_total = sum(votos_pct)) %>%
  arrange(votos_pct)

misiones %>%
  ungroup() %>%
  group_by(listas) %>%
  summarise(votos = sum(votos)) %>%
  arrange(desc(votos)) %>%
  mutate(votos = votos/sum(votos)*100)

# Show results for Misiones presidential election 2003
misiones %>%
  plot_results()

misiones %>%
  tabulate_results()

# Difference between first and second:
37.8-22.2 

# There was 15.6 procentage points difference
# between the first and the second in 2003 presidential election
# in Misiones