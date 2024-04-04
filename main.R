# Load the required libraries
library(did)
library(tidyverse)
library(plm)
library(stargazer)  # Add stargazer library

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

# Print the summary
summary(out)
es <- aggte(out, type = "dynamic")
summary(es)
# Question (b)
never_treated <- mpdta %>%
  filter(treat == 0) %>%
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

# Print the summary
summary(out_nyt)
es_nyt <- aggte(out_nyt, type = "dynamic")
summary(es_nyt)
# Question (d)
mpdta <- mpdta %>%
  mutate(D = ifelse(first.treat <= year, 1, 0))

mpdta_pdf <- pdata.frame(mpdta, index = c("countyreal", "year"))

twfe <- plm(lemp ~ D, data = mpdta)
stargazer(twfe, type = "latex",
                    out = "OUTPUT/2wfe.tex",
                    label = "2wfe",
                    title = "Two-Way Fixed Effects Estimate")
summary(twfe)

group_effects <- aggte(out, type = "group")

summary(group_effects)

# Question (e)

# Question (f)
library(TwoWayFEWeights)

Y <- "lemp"
G <- "countyreal"
T <- "year"
D <- "D"

twowayfeweights(mpdta, Y, G, T, D, type = "feTR")
