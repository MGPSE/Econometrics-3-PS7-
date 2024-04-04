
library(did)
library(tidyverse)
library(plm)

data("mpdta")

# Question (a)
out <- att_gt(yname = "lemp",
              gname = "first.treat",
              idname = "countyreal",
              tname = "year",
              xformla = ~1,
              data = mpdta,
              est_method = "reg"
)

summary(out)

ggdid(out, ylim = c(-.25, .1))   # We don't really need the graph

es <- aggte(out, type = "dynamic")

summary(es)

ggdid(es)                       # We don't really need the graph

# Question (b)
#We need to count each county once, therefore we group by realcounty.

never_treated <- mpdta %>% filter(
    treat == 0
    ) %>%
    group_by(countyreal) %>%
    summarize()

n_never_treated <- as.integer(nrow(never_treated))
# Question (c)
out_nyt <- att_gt(yname = "lemp",
              gname = "first.treat",
              idname = "countyreal",
              tname = "year",
              xformla = ~1,
              data = mpdta,
              est_method = "reg",
              control_group = "notyettreated"
)

# Question (d)
mpdta <- mpdta %>%
    mutate(D = ifelse(first.treat <= year, 1, 0))

mpdta_pdf <- pdata.frame(mpdta, index = c("countyreal", "year"))

twfe <- plm(lemp ~ D, data = mpdta)

summary(twfe)

group_effects <- aggte(out, type = "group")

summary(group_effects)

# Question (e)

# Question (f)
library(TwoWayFEWeights)

Y = "lemp"
G = "countyreal"
T = "year"
D = "D"

twowayfeweights(mpdta, Y, G, T, D, type = "feTR")
