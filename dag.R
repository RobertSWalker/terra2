library(tidyverse)
#remotes::install_github("malcolmbarrett/causalworkshop") #https://github.com/r-causal/causalworkshop#
#library(causalworkshop)
#remotes::install_github("jtextor/dagitty/r")

#Draw assumptions using a causal diagram (using DAG)
library(ggdag)
dag <- dagify(
  ind_deaths ~ native_pop + nonnative_pop + homicide_rate + rural + space,
  homicide_rate ~ nonnative_pop + rural + space,
  nonnative_pop ~ space + rural,
  native_pop ~ space + rural + nonnative_pop,
  rural ~ space,
  exposure = "rural", #my model works for native pop, homicide_rate but not rural, space, or nonind pop
  outcome = "ind_deaths",
  #coords = list(x = c(malaria_risk = 7, net = 3, income = 4, health = 5,
  #                    temperature = 6, resistance = 8.5, eligible = 2, household = 1),
  #              y = c(malaria_risk = 2, net = 2, income = 3, health = 1,
  #                    temperature = 3, resistance = 2, eligible = 3, household = 2)),
  labels = c(ind_deaths = "Indigenous deaths",
             native_pop = "Native population",
             nonnative_pop ="Non-native population",
             homicide_rate = "Homicide rate",
             space = "Space",
             rural = "Rural")
)
library(dagitty)
adjustmentSets(dag, type = "all")

ggdag_status(dag, use_labels = "label", text = FALSE) +
  guides(fill = FALSE, color = FALSE) +  # Disable the legend
  theme_dag()

dag |> ggdag(layout = "time_ordered", text_size = 2.8) + theme_dag() #layouts: time_ordered, sugiyama

#look for open paths that connect exposure and outcome
dag |> ggdag_paths(shadow = TRUE, text = FALSE, use_labels = "label")

(dag_tidy <- dag |> tidy_dagitty()) #all paths

#visualize the minimal adjustment set 
#(the set(s) that can close all backdoor paths with the fewest number of variables possible)
ggdag_adjustment_set(dag, text = FALSE, use_labels = "label") + theme_dag()

#get full adjustment sets (every combination of variables that result in a valid set)
ggdag_adjustment_set(
  dag,
  text = FALSE,
  use_labels = "label",
  type = "all"
) + theme_dag()

  
#general DAG rules:
#1) Always adjust for confounders; even if you don't have it measured, then do a sensitivity analysis 
#2) Never adjust for colliders
#3 Adjust for mediators if you want the partial effect, but Don't adjust if you want the full effect


#what happens when we switch the x (intervention)?

temp_dag <- dagify(
  ind_deaths ~ native_pop + nonnative_pop + homicide_rate + rural + space,
  homicide_rate ~ nonnative_pop + rural + space,
  nonnative_pop ~ space,
  native_pop ~ space,
  rural ~ space,
  exposure = "homicide_rate",
  outcome = "ind_deaths",
  #coords = list(x = c(malaria_risk = 7, net = 3, income = 4, health = 5,
  #                    temperature = 6, resistance = 8.5, eligible = 2, household = 1),
  #              y = c(malaria_risk = 2, net = 2, income = 3, health = 1,
  #                    temperature = 3, resistance = 2, eligible = 3, household = 2)),
  labels = c(ind_deaths = "Indigenous deaths",
             native_pop = "Native population",
             nonnative_pop ="Non-native population",
             homicide_rate = "Homicide rate",
             space = "Space",
             rural = "Rural")
)

ggdag_status(temp_dag, use_labels = "label", text = FALSE) +
  guides(fill = FALSE, color = FALSE) +  # Disable the legend
  theme_dag()

#look for open paths that connect exposure and outcome
temp_dag |> ggdag_paths(shadow = TRUE, text = FALSE, use_labels = "label") + theme_dag()

#visualize the minimal adjustment set
ggdag_adjustment_set(temp_dag, text = FALSE, use_labels = "label")

adjustmentSets(dag, type = "all")

