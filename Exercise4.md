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

    data_path <- "/Users/chien/Library/CloudStorage/OneDrive-McGillUniversity/5d_Network_Analysis/Exercise4/app_data_sample.parquet" # change this to your path
    applications <- read_parquet(data_path)
    edges <- read_csv('/Users/chien/Library/CloudStorage/OneDrive-McGillUniversity/5d_Network_Analysis/Exercise4/edges_sample.csv', show_col_types = FALSE)

# Part 1. Processing of dataset to include gender, race, tenure, application days for examiners

## Adding gender to dataset based on surnames library

This R script enriches an existing dataset named `applications` by
assigning genders to examiner names based on their first names,
utilizing the `gender` library which infers gender from names using
historical data from the U.S. Social Security Administration. It begins
by extracting unique first names from the dataset and then employs the
`gender` function to estimate each name’s gender. The script further
simplifies the data by focusing on essential fields - the first name and
its inferred gender. This gender information is then merged back into
the `applications` dataset. Finally, the script cleans up by removing
temporary variables and calling the garbage collector to free up memory
resources. The outcome is an enhanced version of the `applications`
dataset that includes gender information for each examiner, facilitating
analyses related to gender diversity.

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
    ## Ncells  4484800 239.6    7960674 425.2         NA  4504355 240.6
    ## Vcells 49601111 378.5   93110294 710.4      16384 79916936 609.8

## Adding race to dataset using race library

This script utilizes the `wru` library to append racial background
estimations to an `applications` dataset based on examiner surnames. By
extracting unique surnames and applying the `predict_race` function, it
calculates the likelihood of each surname being associated with specific
racial groups. The script identifies the race with the highest
probability for each surname and simplifies the data to include just
surnames and their most likely racial categorization. These enriched
racial data are then merged back into the original dataset, offering a
nuanced layer of racial background information. The process concludes
with a cleanup of temporary variables and memory optimization, enhancing
the dataset’s utility for analyses concerning racial diversity or
related studies.

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
    ## Ncells  4690023 250.5    7960674 425.2         NA  6896986 368.4
    ## Vcells 51984986 396.7   93110294 710.4      16384 92192117 703.4

## Adding dates-related data to calculate tenure days

This script leverages the `lubridate` library for date manipulation
within an `applications` dataset, focusing on processing examiners’
filing and status update dates to derive their tenure. Initially, it
selects relevant date fields along with the examiner ID, converting
string date formats into `lubridate`-compatible date objects for
accurate computation. The script calculates each examiner’s earliest
filing date and latest status update date, determining their tenure in
days by measuring the interval between these dates. It further filters
the data to include only records where the latest date is before 2018,
ensuring the analysis pertains to a specific time frame. Finally, it
enriches the original `applications` dataset by merging these tenure
calculations back in, based on examiner IDs. The procedure concludes
with a cleanup step, removing temporary variables and calling the
garbage collector to manage memory efficiently. The outcome is an
augmented dataset with detailed tenure information, facilitating
analyses related to examiners’ durations of activity within the
dataset’s context.

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
    ## Ncells  4698810  251    7960674 425.2         NA   7960674 425.2
    ## Vcells 58060838  443  111812352 853.1      16384 111429221 850.2

## Adding application processing time

This R script efficiently processes an applications dataset to analyze
the timeline from filing to a final decision, either a patent issuance
or abandonment. Initially, it filters out applications that lack a
decision, retaining those with either a patent issue date or an abandon
date. Subsequently, it merges these two decision-related dates into a
singular `final_decision_date` column, prioritizing the patent issue
date when both are available. The script then calculates the processing
time for each application by determining the difference in days between
the filing date and the final decision date. This involves converting
date strings to date objects to ensure accurate arithmetic operations,
facilitating a comprehensive analysis of the application processing
duration within the dataset. This streamlined approach allows for a
detailed examination of the timelines associated with patent
applications, highlighting the efficiency of the patent examination
process.

    # first filter out rows without NA in both patent_issue_date and abandon_date

    filtered_applications <- applications %>%
      filter(!(is.na(patent_issue_date) & is.na(abandon_date)))

    # next, combine both the columns to form a new column called final_decision_date

    filtered_applications <- filtered_applications %>%
      mutate(
        final_decision_date = coalesce(patent_issue_date, abandon_date)
      )

    # calculate the differences in application processing days

    filtered_applications <- filtered_applications %>%
      mutate(
        filing_date = ymd(filing_date), # Convert filing_date to Date format if necessary
        final_decision_date = ymd(final_decision_date) # Convert final_decision_date to Date format if necessary
      ) %>%
      mutate(
        decision_day_difference = as.numeric(difftime(final_decision_date, filing_date, units = "days"))
      )

## Create a dataset: Unique Examiners

This segment of R code delves into analyzing the performance of patent
examiners by extracting unique identifiers and associated attributes,
then calculating and appending the average application processing time
for each. Initially, it isolates distinct examiner identities from a
pre-processed dataset (`filtered_applications`), selecting specific
attributes such as the examiner’s ID, art unit, race, gender, and tenure
days, to create a unique profile for each examiner. Following this, it
computes the average application processing time per examiner by
grouping the data by `examiner_id` and averaging the
`decision_day_difference`, which represents the time taken from filing
to a decision being made on each application, thus providing a metric of
examiner efficiency. This average processing time is then merged back
with the unique examiner profiles, creating a comprehensive dataset that
not only highlights the demographic and professional attributes of each
examiner but also includes a performance metric in the form of average
processing time. This enriched dataset is then displayed, offering
insights into the operational dynamics and diversity within the patent
examination process, combining performance analysis with demographic
data for a multifaceted view of examiner productivity and diversity.

    # Extract unique examiner attributes
    unique_examiners <- filtered_applications %>%
      select(examiner_id, examiner_art_unit, race, gender, tenure_days) %>%
      distinct(examiner_id, examiner_art_unit, .keep_all = TRUE)

    # Calculate the average application processing time for each unique examiner
    average_processing_time <- filtered_applications %>%
      group_by(examiner_id) %>%
      summarise(avg_app_proc_time = mean(decision_day_difference, na.rm = TRUE)) %>%
      ungroup()

    # Join the average processing time with the unique examiners data
    unique_examiners <- unique_examiners %>%
      left_join(average_processing_time, by = "examiner_id")

    # View the unique examiners list with average processing time
    print(head(unique_examiners))

    ## # A tibble: 6 × 6
    ##   examiner_id examiner_art_unit race  gender tenure_days avg_app_proc_time
    ##         <dbl>             <dbl> <chr> <chr>        <dbl>             <dbl>
    ## 1       96082              1764 white female        5926              594.
    ## 2       87678              1764 white <NA>          6093              752.
    ## 3       63213              1752 white female        6344              928.
    ## 4       73788              1648 white female        6331             1047.
    ## 5       77294              1762 white male          6332              795.
    ## 6       68606              1734 white female        6345              921.

## Check for NA values in the dataset

This R script segment focuses on identifying and handling missing values
(NA) within the `unique_examiners` dataset, ensuring data integrity and
cleanliness. Initially, it calculates and prints the total count of NA
values across each column, offering a preliminary overview of data
completeness. Following this, the script isolates and displays records
with missing `examiner_id` values, pinpointing entries that lack this
crucial identifier. To maintain dataset quality, these entries are
subsequently filtered out, retaining only those records with valid
`examiner_id` information. This filtration ensures that further analyses
or operations are based on complete and meaningful data. The cleaned
dataset is then presented to confirm the removal of the identified
incomplete records. Finally, a reevaluation of NA counts across columns
is conducted and displayed, verifying the effectiveness of the cleaning
process and ensuring that the dataset is devoid of entries missing the
essential `examiner_id` field. This meticulous approach to data cleaning
underscores the importance of data quality in analytical processes,
particularly in contexts where identifiers like `examiner_id` play a
critical role in linking and analyzing data accurately.

    na_count_by_column <- colSums(is.na(unique_examiners))
    print(na_count_by_column)

    ##       examiner_id examiner_art_unit              race            gender 
    ##               193                 0                 0              1641 
    ##       tenure_days avg_app_proc_time 
    ##               231                 0

    na_examiner_id_data <- unique_examiners %>%
      filter(is.na(examiner_id))

    # View the filtered data
    print(head(na_examiner_id_data))

    ## # A tibble: 6 × 6
    ##   examiner_id examiner_art_unit race  gender tenure_days avg_app_proc_time
    ##         <dbl>             <dbl> <chr> <chr>        <dbl>             <dbl>
    ## 1          NA              1615 white <NA>            NA              232.
    ## 2          NA              1752 white <NA>            NA              232.
    ## 3          NA              1643 white <NA>            NA              232.
    ## 4          NA              1621 white <NA>            NA              232.
    ## 5          NA              1741 white <NA>            NA              232.
    ## 6          NA              1751 white <NA>            NA              232.

    unique_examiners <- unique_examiners %>%
      filter(!is.na(examiner_id))

    # View the cleaned data
    print(unique_examiners)

    ## # A tibble: 10,196 × 6
    ##    examiner_id examiner_art_unit race  gender tenure_days avg_app_proc_time
    ##          <dbl>             <dbl> <chr> <chr>        <dbl>             <dbl>
    ##  1       96082              1764 white female        5926              594.
    ##  2       87678              1764 white <NA>          6093              752.
    ##  3       63213              1752 white female        6344              928.
    ##  4       73788              1648 white female        6331             1047.
    ##  5       77294              1762 white male          6332              795.
    ##  6       68606              1734 white female        6345              921.
    ##  7       97543              1645 white female          NA             1343.
    ##  8       98714              1637 white female        6331             1235.
    ##  9       65530              1723 Asian female        6345             1015.
    ## 10       77112              1755 white female        6347              988.
    ## # ℹ 10,186 more rows

    # Check for NA values again
    na_count_by_column <- colSums(is.na(unique_examiners))
    print(na_count_by_column)

    ##       examiner_id examiner_art_unit              race            gender 
    ##                 0                 0                 0              1448 
    ##       tenure_days avg_app_proc_time 
    ##                38                 0

After removing examiners with NA values, there is still 1448 NA values
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

    ## Percentage of missing values in 'gender': 14.41742 %

Given that there is only 14.4% of missing values within gender, we will
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
    ##       tenure_days avg_app_proc_time 
    ##                35                 0

# Part 2. Using Linear Regression to estimate relationship between centrality and app\_proc\_time

## Preparing the edges dataset

This R script meticulously prepares an `edges` dataset for network
analysis by first identifying and removing rows with missing values in
critical columns, specifically `ego_examiner_id` and
`alter_examiner_id`, which represent the primary and secondary examiners
in a relational network. After each step of cleaning, it reassesses the
dataset to ensure no NA values remain in these key columns.
Subsequently, the script transforms the cleaned dataset to calculate the
`weight` of each unique examiner pair, defined by the distinct count of
`application_number` associated with them, thus quantifying the strength
or frequency of their interactions. The final transformation includes
renaming the examiner ID columns for clarity and displaying a preview of
the transformed data. This process not only guarantees the data’s
integrity for accurate network analysis but also structures it into a
format that highlights the connections and interaction intensity among
the examiners, setting the stage for in-depth network dynamics studies.

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

## Calculating centrality values

This R code snippet utilizes the `igraph` package to calculate various
centrality measures for nodes in a directed network graph created from
the `edges_transformed` dataframe. The centrality measures calculated
are in-degree and out-degree centrality, betweenness centrality,
closeness centrality (both in and out), and eigenvector centrality,
incorporating edge weights where applicable. In-degree and out-degree
centralities are calculated to assess the popularity and outreach of
nodes, respectively, highlighting nodes that receive many connections
and those that initiate many connections within the network. Betweenness
centrality identifies nodes that frequently act as bridges or connectors
between other nodes, emphasizing their role in facilitating
communication or the flow within the network. Closeness centrality,
calculated for both incoming and outgoing connections, measures how
close a node is to all other nodes, indicating the efficiency of a node
in spreading information or reaching out to the network. Eigenvector
centrality reflects the influence of a node, considering not just the
quantity but the quality of its connections, by factoring in the
centrality of the nodes it is connected to. These centrality measures,
especially when calculated for both incoming and outgoing connections in
a directed network, provide a comprehensive understanding of the roles,
influences, and interactions of nodes, revealing the underlying dynamics
and structure of the network.

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

    # Create a directed igraph object from the edges_transformed dataframe
    g <- graph_from_data_frame(d=edges_transformed, directed=TRUE, vertices=NULL)

    # Calculate degree centrality (In-degree and Out-degree)
    in_degree_centrality <- degree(g, mode="in")
    out_degree_centrality <- degree(g, mode="out")

    # Calculate betweenness centrality, incorporating weights
    betweenness_centrality <- betweenness(g, directed=TRUE, weights=E(g)$weight)

    # Since closeness centrality in directed graphs can be different for in and out,
    # calculate them both, incorporating weights
    in_closeness_centrality <- closeness(g, mode="in", weights=E(g)$weight)
    out_closeness_centrality <- closeness(g, mode="out", weights=E(g)$weight)

    # Calculate eigenvector centrality, incorporating weights
    eigenvector_centrality <- eigen_centrality(g, directed=TRUE, weights=E(g)$weight)$vector

    ## Warning in eigenvector_centrality_impl(graph = graph, directed = directed, : At
    ## vendor/cigraph/src/centrality/eigenvector.c:304 : Weighted directed graph in
    ## eigenvector centrality.

    # Creating a dataframe to store the centrality measures
    centrality_measures <- data.frame(
      examiner_id = V(g)$name,
      in_degree = in_degree_centrality,
      out_degree = out_degree_centrality,
      betweenness = betweenness_centrality,
      in_closeness = in_closeness_centrality,
      out_closeness = out_closeness_centrality,
      eigenvector = eigenvector_centrality
    )

    # View the centrality measures
    head(centrality_measures)

    ##       examiner_id in_degree out_degree betweenness in_closeness out_closeness
    ## 59030       59030         0          5        0.00          NaN  0.0011402509
    ## 59108       59108         1         29    32021.63 0.0002129019  0.0001471887
    ## 59141       59141         0          4        0.00          NaN  0.0384615385
    ## 59156       59156         0          1        0.00          NaN  1.0000000000
    ## 59165       59165         1          7        8.00 1.0000000000  0.0555555556
    ## 59166       59166         1          4        0.00          NaN  0.0001026062
    ##        eigenvector
    ## 59030 0.000000e+00
    ## 59108 2.978536e-07
    ## 59141 0.000000e+00
    ## 59156 0.000000e+00
    ## 59165 0.000000e+00
    ## 59166 0.000000e+00

## Running Linear Regression Models to Estimate Relationship

This code first ensures that examiner\_id in both
`unique_examiners_clean` and `centrality_measures` is of type character
by using the mutate() and as.character() functions. Once both
examiner\_id columns are of the same type, the left\_join() operation
can proceed without type compatibility issues, successfully merging the
datasets based on the examiner\_id column.

    # first merge the two datasets unique_examiners_clean and centrality_measures
    library(dplyr)

    # Convert examiner_id in unique_examiners_clean to character if it's numeric
    unique_examiners_clean <- unique_examiners_clean %>%
      mutate(examiner_id = as.character(examiner_id))

    # Ensure examiner_id in centrality_measures is also character
    # This step may be redundant if it's already a character, but it's good practice to ensure compatibility
    centrality_measures <- centrality_measures %>%
      mutate(examiner_id = as.character(examiner_id))

    # Now perform the left join
    combined_data <- unique_examiners_clean %>%
      left_join(centrality_measures, by = "examiner_id")

    # View the first few rows of the combined dataset
    head(combined_data)

    ## # A tibble: 6 × 12
    ##   examiner_id examiner_art_unit race  gender tenure_days avg_app_proc_time
    ##   <chr>                   <dbl> <chr> <chr>        <dbl>             <dbl>
    ## 1 96082                    1764 white female        5926              594.
    ## 2 63213                    1752 white female        6344              928.
    ## 3 73788                    1648 white female        6331             1047.
    ## 4 77294                    1762 white male          6332              795.
    ## 5 68606                    1734 white female        6345              921.
    ## 6 97543                    1645 white female          NA             1343.
    ## # ℹ 6 more variables: in_degree <dbl>, out_degree <dbl>, betweenness <dbl>,
    ## #   in_closeness <dbl>, out_closeness <dbl>, eigenvector <dbl>

## Running Linear Regression Models to Estimate Relationship between Centrality and Average Processing Time

The results of the linear regression model indicate how various
centrality measures and network characteristics relate to the average
application processing time (`avg_app_proc_time`) for examiners. The
model includes centrality measures (in-degree, out-degree, betweenness,
in-closeness, out-closeness, and eigenvector) as predictors.

    linear_model <- lm(avg_app_proc_time ~ in_degree + out_degree + betweenness + in_closeness + out_closeness + eigenvector, 
                       data = combined_data)

    summary(linear_model)

    ## 
    ## Call:
    ## lm(formula = avg_app_proc_time ~ in_degree + out_degree + betweenness + 
    ##     in_closeness + out_closeness + eigenvector, data = combined_data)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -721.3 -205.2  -29.2  183.1  763.9 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    1.307e+03  1.380e+01  94.731  < 2e-16 ***
    ## in_degree      1.821e+00  1.279e+00   1.424   0.1546    
    ## out_degree     2.094e+00  8.175e-01   2.561   0.0105 *  
    ## betweenness    1.162e-03  6.405e-04   1.814   0.0699 .  
    ## in_closeness   4.378e+00  2.011e+01   0.218   0.8277    
    ## out_closeness -1.037e+02  2.175e+01  -4.769 2.05e-06 ***
    ## eigenvector    4.567e+02  2.634e+02   1.734   0.0832 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 265.4 on 1383 degrees of freedom
    ##   (7358 observations deleted due to missingness)
    ## Multiple R-squared:  0.05182,    Adjusted R-squared:  0.04771 
    ## F-statistic:  12.6 on 6 and 1383 DF,  p-value: 7.114e-14

<table>
<caption>Linear Regression Model Summary</caption>
<colgroup>
<col style="width: 20%" />
<col style="width: 18%" />
<col style="width: 17%" />
<col style="width: 15%" />
<col style="width: 27%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;"></th>
<th style="text-align: right;">Estimate</th>
<th style="text-align: right;">Std. Error</th>
<th style="text-align: right;">t value</th>
<th style="text-align: right;">Pr(&gt;|t|)</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">(Intercept)</td>
<td style="text-align: right;">1307.1963966</td>
<td style="text-align: right;">13.7990962</td>
<td style="text-align: right;">94.7305808</td>
<td style="text-align: right;">0.0000000</td>
</tr>
<tr class="even">
<td style="text-align: left;">in_degree</td>
<td style="text-align: right;">1.8209637</td>
<td style="text-align: right;">1.2786873</td>
<td style="text-align: right;">1.4240884</td>
<td style="text-align: right;">0.1546465</td>
</tr>
<tr class="odd">
<td style="text-align: left;">out_degree</td>
<td style="text-align: right;">2.0937596</td>
<td style="text-align: right;">0.8175173</td>
<td style="text-align: right;">2.5611195</td>
<td style="text-align: right;">0.0105389</td>
</tr>
<tr class="even">
<td style="text-align: left;">betweenness</td>
<td style="text-align: right;">0.0011618</td>
<td style="text-align: right;">0.0006405</td>
<td style="text-align: right;">1.8138471</td>
<td style="text-align: right;">0.0699179</td>
</tr>
<tr class="odd">
<td style="text-align: left;">in_closeness</td>
<td style="text-align: right;">4.3780133</td>
<td style="text-align: right;">20.1108157</td>
<td style="text-align: right;">0.2176945</td>
<td style="text-align: right;">0.8276993</td>
</tr>
<tr class="even">
<td style="text-align: left;">out_closeness</td>
<td style="text-align: right;">-103.7277273</td>
<td style="text-align: right;">21.7516160</td>
<td style="text-align: right;">-4.7687366</td>
<td style="text-align: right;">0.0000020</td>
</tr>
<tr class="odd">
<td style="text-align: left;">eigenvector</td>
<td style="text-align: right;">456.6581194</td>
<td style="text-align: right;">263.3633949</td>
<td style="text-align: right;">1.7339468</td>
<td style="text-align: right;">0.0831504</td>
</tr>
</tbody>
</table>

Linear Regression Model Summary

### Interpretation of Coefficients:

-   **Intercept (1.307e+03)**: The expected `avg_app_proc_time` when all
    predictors are zero is approximately 1307 days. This high intercept
    suggests that even in the absence of network effects, the processing
    time is substantial.
-   **In-Degree (1.821e+00, p=0.1546)**: Each additional in-degree point
    is associated with an increase in processing time by about 1.82
    days, although this effect is not statistically significant (p &gt;
    0.05).
-   **Out-Degree (2.094e+00, p=0.0105)**: Each additional out-degree
    point increases processing time by approximately 2.09 days, and this
    relationship is statistically significant (p &lt; 0.05), indicating
    that examiners who have more outgoing connections tend to have
    longer processing times.
-   **Betweenness (1.162e-03, p=0.0699)**: A higher betweenness
    centrality is associated with a slight increase in processing time,
    although this effect is marginally significant (p just above 0.05),
    suggesting a potential role of being a bottleneck in the information
    flow.
-   **In-Closeness (4.378e+00, p=0.8277)** and **Out-Closeness
    (-1.037e+02, p=2.05e-06)**: In-closeness centrality does not
    significantly affect processing time, whereas out-closeness has a
    significant negative impact, decreasing processing time by about
    103.7 days for each unit increase. This suggests that examiners who
    are closer (in terms of out-closeness) to others in the network
    process applications more quickly, likely due to more efficient
    information or resource access.
-   **Eigenvector (4.567e+02, p=0.0832)**: Having a higher eigenvector
    centrality, which indicates connections to well-connected nodes, is
    associated with an increase in processing time by about 456.7 days,
    although this effect is not statistically significant (p &gt; 0.05).

### Model Summary:

-   **Residual Standard Error**: The average difference between the
    observed and predicted `avg_app_proc_time` is about 265.4 days,
    indicating variability in processing times not captured by the
    model.
-   **R-squared (0.05182)**: Only about 5.18% of the variance in
    `avg_app_proc_time` is explained by the model, suggesting that while
    the centrality measures have some effect, other unmodeled factors
    are also at play.
-   **F-Statistic (p-value: 7.114e-14)**: The model is statistically
    significant overall, indicating that at least some of the centrality
    measures meaningfully relate to processing times, even if the effect
    size is small for some predictors.

# Part 3. Incorporating examiner gender into linear model

The linear regression model explores how centrality measures and their
interactions with gender influence the average application processing
time (`avg_app_proc_time`), revealing the nuanced interplay between an
examiner’s network position, gender, and processing efficiency.

    linear_model_with_interaction <- lm(avg_app_proc_time ~ in_degree * gender + out_degree * gender + betweenness * gender + 
                                        in_closeness * gender + out_closeness * gender + eigenvector * gender, 
                                        data = combined_data)

    summary(linear_model_with_interaction)

    ## 
    ## Call:
    ## lm(formula = avg_app_proc_time ~ in_degree * gender + out_degree * 
    ##     gender + betweenness * gender + in_closeness * gender + out_closeness * 
    ##     gender + eigenvector * gender, data = combined_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -648.58 -190.75  -37.23  171.40  738.82 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               1.232e+03  2.857e+01  43.111  < 2e-16 ***
    ## in_degree                 3.903e+00  1.847e+00   2.113  0.03474 *  
    ## gendermale                1.039e+02  3.271e+01   3.178  0.00152 ** 
    ## out_degree                3.882e+00  1.955e+00   1.986  0.04723 *  
    ## betweenness              -2.057e-03  1.423e-03  -1.445  0.14860    
    ## in_closeness             -1.231e+01  3.595e+01  -0.342  0.73206    
    ## out_closeness            -5.143e+01  4.548e+01  -1.131  0.25828    
    ## eigenvector               5.404e+02  2.639e+02   2.048  0.04078 *  
    ## in_degree:gendermale     -3.730e+00  2.571e+00  -1.451  0.14705    
    ## gendermale:out_degree    -2.049e+00  2.149e+00  -0.954  0.34040    
    ## gendermale:betweenness    4.054e-03  1.601e-03   2.532  0.01144 *  
    ## gendermale:in_closeness   2.944e+01  4.336e+01   0.679  0.49734    
    ## gendermale:out_closeness -6.688e+01  5.181e+01  -1.291  0.19694    
    ## gendermale:eigenvector    8.857e+02  1.971e+03   0.449  0.65328    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 262.6 on 1376 degrees of freedom
    ##   (7358 observations deleted due to missingness)
    ## Multiple R-squared:  0.07611,    Adjusted R-squared:  0.06738 
    ## F-statistic:  8.72 on 13 and 1376 DF,  p-value: < 2.2e-16

<table style="width:100%;">
<caption>Linear Regression Model Summary</caption>
<colgroup>
<col style="width: 30%" />
<col style="width: 16%" />
<col style="width: 16%" />
<col style="width: 13%" />
<col style="width: 23%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;"></th>
<th style="text-align: right;">Estimate</th>
<th style="text-align: right;">Std. Error</th>
<th style="text-align: right;">t value</th>
<th style="text-align: right;">Pr(&gt;|t|)</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">(Intercept)</td>
<td style="text-align: right;">1231.7542991</td>
<td style="text-align: right;">28.5713762</td>
<td style="text-align: right;">43.1114795</td>
<td style="text-align: right;">0.0000000</td>
</tr>
<tr class="even">
<td style="text-align: left;">in_degree</td>
<td style="text-align: right;">3.9025237</td>
<td style="text-align: right;">1.8465256</td>
<td style="text-align: right;">2.1134415</td>
<td style="text-align: right;">0.0347426</td>
</tr>
<tr class="odd">
<td style="text-align: left;">gendermale</td>
<td style="text-align: right;">103.9480118</td>
<td style="text-align: right;">32.7108188</td>
<td style="text-align: right;">3.1777869</td>
<td style="text-align: right;">0.0015170</td>
</tr>
<tr class="even">
<td style="text-align: left;">out_degree</td>
<td style="text-align: right;">3.8822048</td>
<td style="text-align: right;">1.9547819</td>
<td style="text-align: right;">1.9860041</td>
<td style="text-align: right;">0.0472310</td>
</tr>
<tr class="odd">
<td style="text-align: left;">betweenness</td>
<td style="text-align: right;">-0.0020573</td>
<td style="text-align: right;">0.0014235</td>
<td style="text-align: right;">-1.4452960</td>
<td style="text-align: right;">0.1486025</td>
</tr>
<tr class="even">
<td style="text-align: left;">in_closeness</td>
<td style="text-align: right;">-12.3105306</td>
<td style="text-align: right;">35.9471138</td>
<td style="text-align: right;">-0.3424623</td>
<td style="text-align: right;">0.7320553</td>
</tr>
<tr class="odd">
<td style="text-align: left;">out_closeness</td>
<td style="text-align: right;">-51.4338961</td>
<td style="text-align: right;">45.4795766</td>
<td style="text-align: right;">-1.1309229</td>
<td style="text-align: right;">0.2582846</td>
</tr>
<tr class="even">
<td style="text-align: left;">eigenvector</td>
<td style="text-align: right;">540.4018175</td>
<td style="text-align: right;">263.9092180</td>
<td style="text-align: right;">2.0476807</td>
<td style="text-align: right;">0.0407808</td>
</tr>
<tr class="odd">
<td style="text-align: left;">in_degree:gendermale</td>
<td style="text-align: right;">-3.7297482</td>
<td style="text-align: right;">2.5707088</td>
<td style="text-align: right;">-1.4508637</td>
<td style="text-align: right;">0.1470457</td>
</tr>
<tr class="even">
<td style="text-align: left;">gendermale:out_degree</td>
<td style="text-align: right;">-2.0493749</td>
<td style="text-align: right;">2.1488726</td>
<td style="text-align: right;">-0.9536977</td>
<td style="text-align: right;">0.3404042</td>
</tr>
<tr class="odd">
<td style="text-align: left;">gendermale:betweenness</td>
<td style="text-align: right;">0.0040538</td>
<td style="text-align: right;">0.0016008</td>
<td style="text-align: right;">2.5323448</td>
<td style="text-align: right;">0.0114407</td>
</tr>
<tr class="even">
<td style="text-align: left;">gendermale:in_closeness</td>
<td style="text-align: right;">29.4386767</td>
<td style="text-align: right;">43.3648010</td>
<td style="text-align: right;">0.6788611</td>
<td style="text-align: right;">0.4973400</td>
</tr>
<tr class="odd">
<td style="text-align: left;">gendermale:out_closeness</td>
<td style="text-align: right;">-66.8846482</td>
<td style="text-align: right;">51.8101087</td>
<td style="text-align: right;">-1.2909575</td>
<td style="text-align: right;">0.1969353</td>
</tr>
<tr class="even">
<td style="text-align: left;">gendermale:eigenvector</td>
<td style="text-align: right;">885.7175603</td>
<td style="text-align: right;">1971.2987616</td>
<td style="text-align: right;">0.4493066</td>
<td style="text-align: right;">0.6532813</td>
</tr>
</tbody>
</table>

Linear Regression Model Summary

### Main Effects:

-   **Intercept (1.232e+03)**: The baseline processing time is
    approximately 1232 days for the reference gender category (assuming
    female if “male” is specified).
-   **In-Degree (3.903, p=0.03474)**: Holding other variables constant,
    an increase in in-degree centrality is associated with a 3.903-day
    increase in processing time, indicating that more inbound
    connections slightly increase processing times.
-   **Gender (male) (1.039e+02, p=0.00152)**: Male examiners, on
    average, have a 103.9-day longer processing time than their
    counterparts, suggesting gender disparities in processing times.
-   **Out-Degree (3.882, p=0.04723)**: An increase in out-degree
    centrality marginally increases processing time by 3.882 days,
    suggesting that more outbound connections could slightly increase
    workload or processing times.
-   **Eigenvector (5.404e+02, p=0.04078)**: Higher eigenvector
    centrality, indicating influential positions within the network, is
    associated with a significant increase (540.4 days) in processing
    time, possibly due to involvement in more complex or numerous cases.

### Interaction Effects:

-   **In-Degree and Gender (Male) (-3.730, p=0.14705)**: The interaction
    term suggests that the relationship between in-degree centrality and
    processing time differs by gender, although this effect is not
    statistically significant.
-   **Betweenness and Gender (Male) (4.054e-03, p=0.01144)**: This
    significant interaction indicates that the effect of betweenness
    centrality on processing time is more pronounced for male examiners,
    suggesting gender differences in how being a network bridge affects
    workload.

### Model Summary:

-   **Residual Standard Error (262.6)**: Indicates the average
    difference between the observed and predicted processing times,
    suggesting variability not captured by the model.
-   **Multiple R-squared (0.07611)**: Only 7.611% of the variance in
    processing time is explained by the model, indicating that while
    centrality measures and gender do impact processing times, much of
    the variation is due to other unaccounted factors.
-   **F-statistic (p-value: &lt; 2.2e-16)**: Demonstrates that the model
    is statistically significant, meaning that there is a relationship
    between the combined predictors and processing time, even if
    individual predictors vary in their impact.

# Part 4. Findings and Implications for USPTO

## Implications from Network Position (Part 2):

**Efficiency and Network Position:** The relationship between an
examiner’s network centrality, particularly out-closeness centrality,
and application processing times indicates that examiners who are more
central in their outgoing connections within the network tend to process
applications more efficiently. This could be because centrally located
examiners have better access to information, resources, or support from
colleagues, facilitating quicker decision-making.

**Policy and Training:** USPTO might consider policies or training
programs aimed at enhancing the connectivity and resource sharing among
examiners. Encouraging a culture of collaboration and information
exchange could leverage the benefits of network centrality to improve
overall processing times. Additionally, identifying and supporting
examiners who are less central could help in balancing workloads and
efficiency across the board.

## Implications from Gender Differences (Part 3):

**Gender and Workload Efficiency:** The interaction between gender and
network centrality measures on processing times underscores that gender
plays a role in how network position affects workload management. This
suggests that male and female examiners might experience or leverage
their network positions differently, potentially due to varying access
to resources or differences in collaboration patterns.

**Addressing Gender Disparities:** Acknowledging and addressing any
underlying gender disparities is crucial. USPTO could benefit from
further investigating the causes behind these differences and
implementing measures to ensure equitable access to resources and
support for all examiners, regardless of gender. This could involve
mentorship programs, targeted training, or efforts to foster a more
inclusive network environment.

## Additional Considerations

The relatively low R-squared values from both parts suggest that the
models capture only a small portion of the variance in application
processing times at the USPTO. This indicates a complex interplay of
factors beyond network centrality and gender influencing processing
efficiency. Here are steps USPTO can take to address these findings and
improve their understanding and management of processing times:

**1. Expand Data Collection:** USPTO should consider broadening the
scope of data collection to include additional variables that could
influence processing times. This may include examiner workload,
complexity of applications, technological area differences, examiner
experience beyond tenure (such as training and educational background),
and external factors like changes in patent law or application trends.

**2. Conduct Qualitative Research:** In-depth interviews or surveys with
examiners could uncover qualitative factors that impact processing
times, such as personal work strategies, the role of teamwork and
collaboration, or administrative support. Understanding these nuances
can provide insights that quantitative data alone may not reveal.

**3. Implement Advanced Analytical Techniques:** Employing more
sophisticated statistical or machine learning models could help uncover
complex relationships between processing times and potential predictors.
Techniques like random forests, gradient boosting machines, or neural
networks can model non-linear relationships and interactions more
effectively than linear regression.
