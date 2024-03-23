## 1. Import Libraries

    knitr::opts_chunk$set(echo = TRUE)
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

    library(tidygraph)

    ## 
    ## Attaching package: 'tidygraph'
    ## 
    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

    library(ggraph)
    library(readr)
    library(dplyr)
    library(igraph)

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

## 2. Import Data

    data <- read.csv("/Users/chien/Library/CloudStorage/OneDrive-McGillUniversity/5d_Network_Analysis/Exercise2/Fakebook_bus_dataset.csv")

## 3. Calculate Centralilty

    # to take the first two columns, and graph is directed
    g <- graph_from_data_frame(data[,1:2], directed=TRUE) 

    # Calculate degree centrality
    degree_centrality <- degree(g)

    # Calculate closeness centrality
    closeness_centrality <- closeness(g, normalized=TRUE) # normalized=TRUE scales the values between 0 and 1

    # Calculate betweenness centrality
    betweenness_centrality <- betweenness(g, normalized=TRUE) # normalized=TRUE scales the values between 0 and 1

    # view centrality values
    print("Degree Centrality:")

    ## [1] "Degree Centrality:"

    print(degree_centrality[1:4])

    ## A B C D 
    ## 4 8 7 7

    print("Closeness Centrality:")

    ## [1] "Closeness Centrality:"

    print(closeness_centrality[1:4])

    ##         A         B         C         D 
    ## 0.5000000 0.7272727 0.7272727 0.6666667

    print("Betweeness Centrality:")

    ## [1] "Betweeness Centrality:"

    print(betweenness_centrality[1:4])

    ##          A          B          C          D 
    ## 0.05357143 0.13392857 0.07142857 0.06250000

## 4. Possible Consequences

The analysis of centrality measures for seats within the bus network,
particularly focusing on Node B, reveals significant implications for
interpersonal dynamics and information flow among the passengers,
presumably employees. Node B, distinguished by the highest degree
centrality of 8, facilitates unparalleled direct access to other
individuals. This position, therefore, affords maximum exposure to the
diverse array of passengers, potentially enhancing one’s understanding
of the organizational culture and employee interrelations swiftly.
Nonetheless, the predominance of Node B comes with potential drawbacks.
The immediate and extensive visibility afforded by this central seat
might not always contribute positively to the development of trust and
influence. Occupying such a pivotal position could inadvertently project
an image of overassertiveness or presumptuousness, which might not be
favorably received by all members of the organization.

Furthermore, the closeness centrality measures, particularly pronounced
for Nodes B and C, highlight the efficiency with which occupants of
these seats can reach out to or be reached by others within the network.
This characteristic is particularly beneficial for quick integration
into the social fabric of the organization and expedited acquisition of
situational awareness regarding the company’s dynamics. However, the
strategic advantage of such positions might encounter limitations during
periods of high communicative demand, where these nodes might become
inadvertent chokepoints or subjects of overwhelming attention.

The betweenness centrality of Node B, marked at approximately 0.134,
underscores its indispensable role in mediating the flow of dialogue
among the employees. This unique vantage point could enable an
individual to subtly influence or gauge the pulse of information
exchange across the network. Yet, the proximity to potentially sensitive
or confidential exchanges may place the occupant of this seat in awkward
situations, where their presence might deter open communication, thereby
inadvertently impeding the natural flow of information.

In summary, while the central position represented by Node B in the
network graph offers numerous advantages for rapid social integration
and influence within the organizational context, it also imposes certain
responsibilities and potential social dilemmas. The choice of this seat
could thus be seen as a double-edged sword, offering significant
opportunities for engagement and influence, albeit coupled with
challenges in navigating the complexities of workplace dynamics and
interpersonal communication.

## 5. Plot Network graph with labels and centrality values

    # adjust node_size for visualisation
    degree_centrality_norm <- degree_centrality / max(degree_centrality)
    node_sizes <- degree_centrality_norm * 50 

    # Prepare labels with degree centrality
    node_labels <- paste(V(g)$name, "\n(", round(degree_centrality),
                         round(closeness_centrality, 3), round(betweenness_centrality, 3),")", sep=" ")

    # Plot Graph
    par(mar=c(1, 1, 1, 1))

    # Plot the network
    plot(g,
         layout = layout_with_kk(g),
         vertex.size=node_sizes, # Change this for different centrality measures
         vertex.label=node_labels, # Assuming the nodes have names
         vertex.label.cex=0.8, # Adjust label size
         edge.arrow.size=0.3,
         vertex.label.dist = 0.5,
         main="Network Graph",
         vertex.color="skyblue")

    legend("bottomright", # Position of the legend. Other options: "topleft", "bottomleft", "topright"
           title="Centrality Measures", # Title of the legend
           cex=0.8, # Size of the legend text
           bty="n", # No box around the legend
           legend=c("Degree Centrality Closeness Centrality Betweenness Centrality"), # Legend text
           ) # Margin around the legend

![](Exercise2_files/figure-markdown_strict/Network%20Analysis-1.png)
