# Initialisation of libraries and dataset

## Import Libraries

    knitr::opts_chunk$set(tidy.opts=list(width.cutoff=60), format='latex', echo=TRUE)
    library(tidyverse)

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

    library(lubridate)
    library(arrow)

    ## 
    ## Attaching package: 'arrow'
    ## 
    ## The following object is masked from 'package:lubridate':
    ## 
    ##     duration
    ## 
    ## The following object is masked from 'package:utils':
    ## 
    ##     timestamp

    library(tibble)
    library(dplyr)

## Import Dataset

    data_path <- "/Users/chien/Library/CloudStorage/OneDrive-McGillUniversity/5d_Network_Analysis/Exercise3/app_data_sample.parquet" # change this to your path
    applications <- read_parquet(data_path)
    edges <- read_csv('/Users/chien/Library/CloudStorage/OneDrive-McGillUniversity/5d_Network_Analysis/Exercise3/edges_sample.csv', show_col_types = FALSE)

# Part 1. Processing of dataset to include gender, race, tenure for examiners

## Adding gender to dataset based on surnames library

The code uses the surnames library called ‘gender’ to add another column
to our data set “applications” by predicting the gender of the examiners
using their names.

    library(gender)
    examiner_names <- applications %>%
            distinct(examiner_name_first)

    examiner_names_gender <- examiner_names %>%
            do(results = gender(.$examiner_name_first, method = "ssa")) %>%
            unnest(cols = c(results), keep_empty = TRUE) %>%
            select(
                    examiner_name_first = name,
                    gender,
                    proportion_female)

    # remove extra colums from the gender table
    examiner_names_gender <- examiner_names_gender %>%
            select(examiner_name_first, gender)

    # joining gender back to the dataset
    applications <- applications %>%
            left_join(examiner_names_gender, by = "examiner_name_first")

    # cleaning up
    rm(examiner_names)
    rm(examiner_names_gender)
    gc()

    ##            used  (Mb) gc trigger  (Mb) limit (Mb) max used  (Mb)
    ## Ncells  4484932 239.6    7960404 425.2         NA  4504487 240.6
    ## Vcells 49597948 378.5   93106498 710.4      16384 79913773 609.7

## Adding race to dataset using race library

Using the ‘wru’ library, we add another column into the data set
“applications” by predicting the race of the examiner based on their
last names.

    library(wru)

    ## 
    ## Please cite as:
    ## 
    ## Khanna K, Bertelsen B, Olivella S, Rosenman E, Rossell Hayes A, Imai K
    ## (2024). _wru: Who are You? Bayesian Prediction of Racial Category Using
    ## Surname, First Name, Middle Name, and Geolocation_. R package version
    ## 3.0.1, <https://CRAN.R-project.org/package=wru>.
    ## 
    ## Note that wru 2.0.0 uses 2020 census data by default.
    ## Use the argument `year = "2010"`, to replicate analyses produced with earlier package versions.

    examiner_surnames <- applications %>%
            select(surname = examiner_name_last) %>%
            distinct()

    examiner_race <- predict_race(voter.file = examiner_surnames, surname.only = T) %>%
            as_tibble()

    ## Predicting race for 2020

    ## Warning: Unknown or uninitialised column: `state`.

    ## Proceeding with last name predictions...

    ## ℹ All local files already up-to-date!

    ## 701 (18.4%) individuals' last names were not matched.

    examiner_race <- examiner_race %>%
            mutate(max_race_p = pmax(pred.asi, pred.bla, pred.his, pred.oth, pred.whi)) %>%
            mutate(race = case_when(
                    max_race_p == pred.asi ~ "Asian",
                    max_race_p == pred.bla ~ "black",
                    max_race_p == pred.his ~ "Hispanic",
                    max_race_p == pred.oth ~ "other",
                    max_race_p == pred.whi ~ "white",
                    TRUE ~ NA_character_
            ))

    # removing extra columns
    examiner_race <- examiner_race %>%
            select(surname,race)

    applications <- applications %>%
            left_join(examiner_race, by = c("examiner_name_last" = "surname"))

    rm(examiner_race)
    rm(examiner_surnames)
    gc()

    ##            used  (Mb) gc trigger  (Mb) limit (Mb) max used  (Mb)
    ## Ncells  4690149 250.5    7960404 425.2         NA  6896941 368.4
    ## Vcells 51981740 396.6   93106498 710.4      16384 92188145 703.4

## Adding dates-related data to calculate tenure days

To calculate the days of tenure for the examiners, we use the library
‘lubridate’ to add another column into the data set “applications”.
Tenure days is calculated by using the difference between the first vs
last application they examined.

    library(lubridate) # to work with dates

    examiner_dates <- applications %>%
            select(examiner_id, filing_date, appl_status_date)

    examiner_dates <- examiner_dates %>%
            mutate(start_date = ymd(filing_date), end_date = as_date(dmy_hms(appl_status_date)))

    examiner_dates <- examiner_dates %>%
            group_by(examiner_id) %>%
            summarise(
                    earliest_date = min(start_date, na.rm = TRUE),
                    latest_date = max(end_date, na.rm = TRUE),
                    tenure_days = interval(earliest_date, latest_date) %/% days(1)
            ) %>%
            filter(year(latest_date)<2018)

    applications <- applications %>%
            left_join(examiner_dates, by = "examiner_id")

    rm(examiner_dates)
    gc()

    ##            used (Mb) gc trigger  (Mb) limit (Mb)  max used  (Mb)
    ## Ncells  4698936  251    7960404 425.2         NA   7960404 425.2
    ## Vcells 58057592  443  111807797 853.1      16384 111425975 850.2

## Extract the rows with unique examiner IDs

Given that the analysis is examiner-specific, we will filter out a list
of unique examiners. This list of unique examiners is dependent on
unique combinations of both examiner\_id and their art unit, as we are
concerned about the demographics of each art unit.

    # Load the dplyr package
    library(dplyr)

    unique_examiners <- applications %>%
      select(examiner_id, examiner_art_unit, race, gender, tenure_days) %>%  # 
      distinct(examiner_id, examiner_art_unit, .keep_all = TRUE)  # Keep unique rows based on both examiner_id and examiner_art_unit

    # View the unique examiners list
    print(head(unique_examiners))

    ## # A tibble: 6 × 5
    ##   examiner_id examiner_art_unit race  gender tenure_days
    ##         <dbl>             <dbl> <chr> <chr>        <dbl>
    ## 1       96082              1764 white female        5926
    ## 2       87678              1764 white <NA>          6093
    ## 3       63213              1752 white female        6344
    ## 4       73788              1648 white female        6331
    ## 5       77294              1762 white male          6332
    ## 6       68606              1734 white female        6345

## Check for NA values in the dataset

Checking the dataset for NA values showed that there are some examiners
with NA examiners ID. The decision is to drop the rows with NA examiner
ID. Thereafter, another check was done to look out for NA values.

    na_count_by_column <- colSums(is.na(unique_examiners))
    print(na_count_by_column)

    ##       examiner_id examiner_art_unit              race            gender 
    ##               209                 0                 0              1679 
    ##       tenure_days 
    ##               247

    na_examiner_id_data <- unique_examiners %>%
      filter(is.na(examiner_id))

    # View the filtered data
    print(head(na_examiner_id_data))

    ## # A tibble: 6 × 5
    ##   examiner_id examiner_art_unit race  gender tenure_days
    ##         <dbl>             <dbl> <chr> <chr>        <dbl>
    ## 1          NA              1615 white <NA>            NA
    ## 2          NA              1752 white <NA>            NA
    ## 3          NA              1643 white <NA>            NA
    ## 4          NA              1621 white <NA>            NA
    ## 5          NA              1741 white <NA>            NA
    ## 6          NA              1751 white <NA>            NA

    unique_examiners <- unique_examiners %>%
      filter(!is.na(examiner_id))

    # View the cleaned data
    print(unique_examiners)

    ## # A tibble: 10,304 × 5
    ##    examiner_id examiner_art_unit race  gender tenure_days
    ##          <dbl>             <dbl> <chr> <chr>        <dbl>
    ##  1       96082              1764 white female        5926
    ##  2       87678              1764 white <NA>          6093
    ##  3       63213              1752 white female        6344
    ##  4       73788              1648 white female        6331
    ##  5       77294              1762 white male          6332
    ##  6       68606              1734 white female        6345
    ##  7       89557              1627 black female        5634
    ##  8       97543              1645 white female          NA
    ##  9       98714              1637 white female        6331
    ## 10       65530              1723 Asian female        6345
    ## # ℹ 10,294 more rows

    # Check for NA values again
    na_count_by_column <- colSums(is.na(unique_examiners))
    print(na_count_by_column)

    ##       examiner_id examiner_art_unit              race            gender 
    ##                 0                 0                 0              1470 
    ##       tenure_days 
    ##                38

After removing examiners with NA values, there is still 1470 NA values
within gender. The following code calculates the percentage of NA values
in gender respective to the whole dataset.

    # Total number of rows in the dataset
    total_rows <- nrow(unique_examiners)

    # Number of missing values in the 'gender' column (as per your output)
    missing_values_gender <- 1470

    # Calculate the percentage of missing values
    percentage_missing_gender <- (missing_values_gender / total_rows) * 100

    # Print the result
    cat("Percentage of missing values in 'gender':", percentage_missing_gender, "%\n")

    ## Percentage of missing values in 'gender': 14.2663 %

Given that there is only 14.2% of missing values within gender, we will
proceed to drop the rows that has NA in the gender. We will ignore
tenure\_days with NA values as it is not crucial in our analysis.

    # Filter out rows where 'gender' is NA
    unique_examiners_clean <- unique_examiners %>%
      filter(!is.na(gender))

    # Check for NA values again
    na_count_by_column <- colSums(is.na(unique_examiners_clean))
    print(na_count_by_column)

    ##       examiner_id examiner_art_unit              race            gender 
    ##                 0                 0                 0                 0 
    ##       tenure_days 
    ##                35

# Part 2. Summary Statistics and Plots for Workgroups: 177 and 218

## Creating Workgroups

We extract the workgroups from the examiner art unit using the first
three numbers.

    unique_examiners_clean$workgroup <- substr(unique_examiners_clean$examiner_art_unit, 1, 3)

    head(unique_examiners_clean)

    ## # A tibble: 6 × 6
    ##   examiner_id examiner_art_unit race  gender tenure_days workgroup
    ##         <dbl>             <dbl> <chr> <chr>        <dbl> <chr>    
    ## 1       96082              1764 white female        5926 176      
    ## 2       63213              1752 white female        6344 175      
    ## 3       73788              1648 white female        6331 164      
    ## 4       77294              1762 white male          6332 176      
    ## 5       68606              1734 white female        6345 173      
    ## 6       89557              1627 black female        5634 162

To select the workgroup of interest, we will summarise the number of
examiners in each workgroup.

    examiners_per_workgroup <- unique_examiners_clean %>%
      filter(!is.na(examiner_id), !is.na(workgroup)) %>%
      count(workgroup, name = "n_examiners") %>%
      arrange(desc(n_examiners))

    # View the sorted results
    print(examiners_per_workgroup)

    ## # A tibble: 38 × 2
    ##    workgroup n_examiners
    ##    <chr>           <int>
    ##  1 179               912
    ##  2 218               399
    ##  3 216               383
    ##  4 177               362
    ##  5 217               350
    ##  6 212               320
    ##  7 215               314
    ##  8 161               303
    ##  9 213               298
    ## 10 174               280
    ## # ℹ 28 more rows

The decision to choose workgroup 217 and 212 should give us a good
comparison of examiners demographics as the number of examiners is large
enough and therefore representative. Also the same number of examiners
(350) makes it easier for cross-comparison.

    filtered_data <- unique_examiners_clean %>%
      filter(workgroup %in% c(217, 212))

## Summary Statistics and Plots

As tenure days is a continuous variable, we will cross-compare the
distribution of the mean, median, min and max tenure days of the
employees in both workgroups.

    numeric_summary <- filtered_data %>%
      group_by(workgroup) %>%
      summarise(
        mean_tenure = mean(tenure_days, na.rm = TRUE),
        median_tenure = median(tenure_days, na.rm = TRUE),
        min_tenure = min(tenure_days, na.rm = TRUE),
        max_tenure = max(tenure_days, na.rm = TRUE)
      )
    print(numeric_summary)

    ## # A tibble: 2 × 5
    ##   workgroup mean_tenure median_tenure min_tenure max_tenure
    ##   <chr>           <dbl>         <dbl>      <dbl>      <dbl>
    ## 1 212             4844.          5926        519       6349
    ## 2 217             4823.          5507        408       6347

It seems that the examiners in workgroup 212 is generally more senior
(with a higher higher median\_tenure days). Additionally, the minimum
tenure days for examiners in workgroup 217 is smaller than that of
workgroup 212.

    ## Warning: Removed 2 rows containing non-finite outside the scale range
    ## (`stat_bin()`).

![](Exercise3_files/figure-markdown_strict/Plot%20of%20Tenure%20Days-1.png)

From the plots, we can see that both workgroups have a
disproportionately large amount of examiners with significant number of
tenure days (&gt;5000 days).

    # For 'race'
    race_summary <- filtered_data %>%
      count(workgroup, race, sort = TRUE)

    # For 'gender'
    gender_summary <- filtered_data %>%
      count(workgroup, gender, sort = TRUE)

    print(race_summary)

    ## # A tibble: 9 × 3
    ##   workgroup race         n
    ##   <chr>     <chr>    <int>
    ## 1 217       white      208
    ## 2 212       white      192
    ## 3 217       Asian      101
    ## 4 212       Asian       84
    ## 5 212       Hispanic    28
    ## 6 217       Hispanic    25
    ## 7 217       black       16
    ## 8 212       black       14
    ## 9 212       other        2

    print(gender_summary)

    ## # A tibble: 4 × 3
    ##   workgroup gender     n
    ##   <chr>     <chr>  <int>
    ## 1 212       male     259
    ## 2 217       male     241
    ## 3 217       female   109
    ## 4 212       female    61

Numerical analysis of the categorical variables does not seem to show
much insights. We will proceed to plot both the data for gender and
race.

![](Exercise3_files/figure-markdown_strict/Plot%20of%20Gender-1.png)
Both workgroups have more males than females. However, we can see that
in workgroup 212, there is a significantly higher proportion of males
compared to females.

![](Exercise3_files/figure-markdown_strict/Plot%20of%20Race-1.png)

In both workgroups, there are a significantly more “white” examiners
compared to other races. Apart from that, the graphs are mostly similar.

# Part 3. Create advice network and calculate centrality scores

## Justification of Centrality Measure

Betweenness Centrality: Identifies nodes that serve as bridges between
other nodes in the network. High betweenness suggests an examiner is
critical in facilitating communication or advice flow across the
network. Given that we are interested in advice networks, betweeness
centrality should highlight examiners who play a crucial role in
connecting groups or individuals within the network.

## Prepare the edges data set

Removes the NA values first. Code creates a new data frame
edges\_transformed where each row represents a unique ego-alter pair
with a weight indicating the number of unique applications between them.

    library(dplyr)

    # Check for NA values
    na_count_by_column <- colSums(is.na(edges))
    print(na_count_by_column)

    ## application_number        advice_date    ego_examiner_id  alter_examiner_id 
    ##                  0                  0               2817               1659

    # Drop rows with NA ego_examiner_id
    edges_cleaned <- edges %>%
      filter(!is.na(ego_examiner_id))

    # Check for NA values
    na_count_by_column <- colSums(is.na(edges_cleaned))
    print(na_count_by_column)

    ## application_number        advice_date    ego_examiner_id  alter_examiner_id 
    ##                  0                  0                  0               1475

    # Drop rows with NA alter_examiner_id
    edges_cleaned <- edges_cleaned %>%
      filter(!is.na(alter_examiner_id))

    # Check for NA values
    na_count_by_column <- colSums(is.na(edges_cleaned))
    print(na_count_by_column)

    ## application_number        advice_date    ego_examiner_id  alter_examiner_id 
    ##                  0                  0                  0                  0

    # Transform the data to calculate weights
    edges_transformed <- edges_cleaned %>%
      group_by(ego_examiner_id, alter_examiner_id) %>%
      summarise(weight = n_distinct(application_number), .groups = "drop") %>%
      rename(ego = ego_examiner_id, alter = alter_examiner_id)

    # View the transformed data
    print(head(edges_transformed))

    ## # A tibble: 6 × 3
    ##     ego alter weight
    ##   <dbl> <dbl>  <int>
    ## 1 59030 59194      1
    ## 2 59030 60623      8
    ## 3 59030 66932      1
    ## 4 59030 67846      1
    ## 5 59030 85989      1
    ## 6 59108 61485      1

## Filter the examiners in the workgroup

Create two subsets of edges\_transformed: one for workgroup 212 and
another for workgroup 217, using the examiner IDs filtered in
filtered\_data.

    # Assuming `filtered_data` has been filtered for workgroup 212 already
    examiner_ids_212 <- filtered_data %>% 
      filter(workgroup == "212") %>% 
      select(examiner_id) %>% 
      unique()

    # Assuming `edges_transformed` contains columns `ego_examiner_id` and `alter_examiner_id`
    edges_212 <- edges_transformed %>%
      filter(ego%in% examiner_ids_212$examiner_id | alter %in% examiner_ids_212$examiner_id)

    # Repeat for workgroup 217
    examiner_ids_217 <- filtered_data %>% 
      filter(workgroup == "217") %>% 
      select(examiner_id) %>% 
      unique()

    edges_217 <- edges_transformed %>%
      filter(ego %in% examiner_ids_217$examiner_id | alter %in% examiner_ids_217$examiner_id)

## Calculate Centrality Scores

Calculate the betweenness centrality for the sub-networks corresponding
to workgroups 212 and 217.

    library(igraph)

    ## 
    ## Attaching package: 'igraph'

    ## The following objects are masked from 'package:lubridate':
    ## 
    ##     %--%, union

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     as_data_frame, groups, union

    ## The following objects are masked from 'package:purrr':
    ## 
    ##     compose, simplify

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     crossing

    ## The following object is masked from 'package:tibble':
    ## 
    ##     as_data_frame

    ## The following objects are masked from 'package:stats':
    ## 
    ##     decompose, spectrum

    ## The following object is masked from 'package:base':
    ## 
    ##     union

    # Create graphs for each sub-network
    g_212 <- graph_from_data_frame(d=edges_212, directed=TRUE)
    g_217 <- graph_from_data_frame(d=edges_217, directed=TRUE)

    # Calculate normalized betweenness centrality
    betweenness_212 <- betweenness(g_212, directed=TRUE, normalized=TRUE)
    betweenness_217 <- betweenness(g_217, directed=TRUE, normalized=TRUE)

    # Create data frames with the results
    centrality_scores_212 <- data.frame(examiner_id = V(g_212)$name, betweenness = betweenness_212)
    centrality_scores_217 <- data.frame(examiner_id = V(g_217)$name, betweenness = betweenness_217)

## Plot Network graph

    plot(g_212, 
         main = "Network for Workgroup 212", 
         vertex.size = 5, 
         vertex.color = "skyblue", 
         edge.arrow.size = 0.3, 
         layout = layout_with_fr(g_212),
         vertex.label = NA)  # Omitting labels

![](Exercise3_files/figure-markdown_strict/Network%20Analysis-1.png)

    plot(g_217, 
         main = "Network for Workgroup 217", 
         vertex.size = 5, 
         vertex.color = "lightgreen", 
         edge.arrow.size = 0.3, 
         layout = layout_with_fr(g_217),
         vertex.label = NA)  # Omitting labels

![](Exercise3_files/figure-markdown_strict/Network%20Analysis-2.png)

    # And ensure V(g_212)$name is also in the same format

    # Define a specific color for each race
    race_colors <- c('white' = 'skyblue', 'Asian' = 'yellow', 'Hispanic' = 'orange', 
                     'black' = 'black', 'other' = 'grey')

    # Filter for workgroup 212, ensuring unique examiner IDs
    filtered_data_212 <- filtered_data %>%
      filter(workgroup == 212) %>%
      select(examiner_id, race) %>%
      distinct()

    filtered_data_212$examiner_id <- as.character(filtered_data_212$examiner_id)
    V(g_212)$name <- as.character(V(g_212)$name)

    # Prepare node color vector for g_212 based on race
    node_colors_212 <- sapply(V(g_212)$name, function(id) {
      race <- filtered_data_212$race[filtered_data_212$examiner_id == as.numeric(id)]
      if(length(race) == 0 || is.na(race)) {
        return('grey') # Default color for unmatched or NA races
      } else {
        return(race_colors[as.character(race)])
      }
    }, USE.NAMES = FALSE)

    # Plot the network for workgroup 212
    plot(g_212, 
         vertex.color = node_colors_212, 
         main = "Network for Workgroup 212 by Race", 
         vertex.size = 5, 
         edge.arrow.size = 0.5, 
         layout = layout_with_fr(g_212), 
         vertex.label = NA)  # No labels for clarity

    legend("topright",                    # Position of the legend within the plot
           legend = names(race_colors),   # Labels for the legend, based on race
           col = race_colors,             # Color keys for the legend
           pch = 15,                      # Type of symbol to use (square by default)
           title = "Race",                # Title of the legend
           cex = 0.8,                     # Font size of the legend text
           bty = "n",                     # No box around the legend
           pt.cex = 2)                    # Size of the symbols in the legend

![](Exercise3_files/figure-markdown_strict/Network%20Plot%20for%20Workgroup%20212%20with%20Characteristics-1.png)

There seems to be some problems with the colouring the nodes to compare
the characteristics.
