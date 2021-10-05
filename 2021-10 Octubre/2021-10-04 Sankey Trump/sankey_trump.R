# Librerias ----
library(tidyverse)
library(networkD3)

# Datos ----
bd <- tibble::tribble(
            ~source,             ~target, ~value,
            "Trump",         "Democrats",    38L,
            "Trump",               "GOP",    12L,
            "Trump",            "Others",    32L,
            "Trump",         "The media",   145L,
        "Democrats",   "Other democrats",    24L,
        "Democrats",     "Chuck & Nancy",     6L,
        "Democrats",           "Hillary",     8L,
              "GOP",            "Corker",     4L,
              "GOP", "Other republicans",     8L,
           "Others",     "Int´l figures",     3L,
           "Others",         "Companies",     4L,
           "Others",            "Sports",     8L,
           "Others",       "Celebrities",     6L,
           "Others",       "Publig Fig.",     4L,
           "Others",      "Other others",     7L,
        "The media",       "Fake News!'",    33L,
        "The media",  "Specific outlets",    56L,
        "The media",               "NBC",    12L,
        "The media",               "CNN",    13L,
        "The media",               "NYT",    17L,
        "The media",     "Other outlets",    14L)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(bd$source),
         as.character(bd$target)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
bd$IDsource <- match(bd$source, nodes$name)-1
bd$IDtarget <- match(bd$target, nodes$name)-1

bd <- bd %>% as.data.frame()

# Make the Network
p <- sankeyNetwork(Links = bd,
                   Nodes = nodes,
                   Source = "IDsource",
                   Target = "IDtarget",
                   Value = "value",
                   NodeID = "name",
                   sinksRight=FALSE)
p
