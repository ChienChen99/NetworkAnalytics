Network Analysis - Exercise 1
================
2024-03-19

## Import Libraries

``` r
knitr::opts_chunk$set(tidy.opts=list(width.cutoff=60), format='latex', echo=TRUE)
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.5.0     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(tidygraph)
```

    ## 
    ## Attaching package: 'tidygraph'
    ## 
    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

``` r
library(ggraph)
library(readr)
library(dplyr)
library(igraph)
```

    ## 
    ## Attaching package: 'igraph'
    ## 
    ## The following object is masked from 'package:tidygraph':
    ## 
    ##     groups
    ## 
    ## The following objects are masked from 'package:lubridate':
    ## 
    ##     %--%, union
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     as_data_frame, groups, union
    ## 
    ## The following objects are masked from 'package:purrr':
    ## 
    ##     compose, simplify
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     crossing
    ## 
    ## The following object is masked from 'package:tibble':
    ## 
    ##     as_data_frame
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     decompose, spectrum
    ## 
    ## The following object is masked from 'package:base':
    ## 
    ##     union

## Import Data

``` r
Connections <- read_csv("/Users/chien/Library/CloudStorage/OneDrive-McGillUniversity/5d_Network_Analysis/Exercise1/Connections.csv", skip = 2, show_col_types=FALSE)
```

## Employer Count

``` r
employer_counts <- Connections %>% group_by(Company) %>% summarise(Count = n())

# View the summary
print(employer_counts)
```

    ## # A tibble: 162 × 2
    ##    Company                                              Count
    ##    <chr>                                                <int>
    ##  1 A*STAR - Agency for Science, Technology and Research     2
    ##  2 ALDO Group                                               1
    ##  3 Accenture                                                1
    ##  4 Air Transat                                              2
    ##  5 Airbus                                                   1
    ##  6 AltaML                                                   1
    ##  7 Amazon Web Services (AWS)                                2
    ##  8 Ameresco                                                 1
    ##  9 Applied Materials                                        1
    ## 10 AtkinsRéalis                                             3
    ## # ℹ 152 more rows

## Total Count of Contacts

``` r
# Total count
total_contacts <- nrow(Connections)
print(total_contacts)
```

    ## [1] 196

## Creating Nodes and Edges

``` r
Connections$Label <- paste(Connections$`First Name`,substr(Connections$`Last Name`, 1, 1), sep = " ")

nodes_df <- distinct(Connections, Label, .keep_all = TRUE) %>%
  select(Label) %>%
  mutate(id = row_number())

# re-arrange label and names

# Assuming your DataFrame is named df and has columns 'label' and 'id'
nodes_df <- nodes_df[, c('id', 'Label')]

# Create edges based on shared current employer
edges_df <- Connections %>%
  select(Label, Company) %>%
  inner_join(Connections, by = "Company") %>%
  select(Label.x, Label.y) %>%
  filter(Label.x != Label.y) %>%
  unique()
```

    ## Warning in inner_join(., Connections, by = "Company"): Detected an unexpected many-to-many relationship between `x` and `y`.
    ## ℹ Row 4 of `x` matches multiple rows in `y`.
    ## ℹ Row 11 of `y` matches multiple rows in `x`.
    ## ℹ If a many-to-many relationship is expected, set `relationship =
    ##   "many-to-many"` to silence this warning.

``` r
# Map node ids to edges
edges_df <- edges_df %>%
  left_join(nodes_df, by = c("Label.x" = "Label")) %>%
  rename(source = id) %>%
  left_join(nodes_df, by = c("Label.y" = "Label")) %>%
  rename(target = id) %>%
  select(source, target)

# Introduce a count of occurrences as 'weight' for each (source, target) pair
edges_weighted <- edges_df %>%
  group_by(source, target) %>%
  summarise(weight = n(), .groups = "drop")

# 'edges_weighted' now contains the unique pairs of (source, target) with their weights
```

## Analyze Network with igraph

``` r
# Assuming edges_weighted and nodes_df are already defined and g is created
g <- graph_from_data_frame(d=edges_weighted, vertices=nodes_df, directed=FALSE)

# Assign names from the 'Label' column in nodes_df to the vertex attribute 'name'
V(g)$name <- nodes_df$Label

# Plot Graph
par(mar=c(1, 1, 1, 1))

plot(g,
     layout = layout_with_kk(g),
     vertex.size = 10,
     vertex.label.cex = 0.5)
```

![](Exercise1_files/figure-gfm/igraph-1.png)<!-- -->
