# This is a simple code demo for visualising scientific interest in key terms
# (adapted by Andy Cunliffe from https://twitter.com/EShekhova/status/1772925867603165413?s=09, who adapted it from elsewhere...)
# This can be really helpful for developing quantitative summaries to evidence 
# statements like "there is rising interest in topic X"
# The europepmc package accesses the Europe PMC database, an archive of > 45 Million
# scientific outputs. While this is not complete, it’s much larger than PubMed  
# and has enough for this application. 


### Step 1 - Load packages ------------------------------------------------

# install.packages("europepmc")
# install.packages("tidyverse")
# install.packages("viridis")

library(europepmc)  # This package is published by https://ropensci.org/, which is really high quality and has a thorough review process.
library(tidyverse)
library(viridis)


## Plotting theme (note essential but helps ensure pretty graphs)
theme_beautiful <- function() {
  theme_bw() +
    theme(
      text = element_text(family = "Helvetica"),
      axis.text = element_text(size = 8, color = "black"),
      axis.title = element_text(size = 8, color = "black"),
      axis.line.x = element_line(size = 0.3, color = "black"),
      axis.line.y = element_line(size = 0.3, color = "black"),
      axis.ticks = element_line(size = 0.3, color = "black"),
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
      plot.title = element_text(
        size = 8,
        vjust = 1,
        hjust = 0.5,
        color = "black"
      ),
      legend.text = element_text(size = 8, color = "black"),
      legend.title = element_text(size = 8, color = "black"),
      legend.position = c(0.9, 0.9),
      legend.key.size = unit(0.9, "line"),
      legend.background = element_rect(
        color = "black",
        fill = "transparent",
        size = 2,
        linetype = "blank"
      )
    )
}
windowsFonts("Helvetica" = windowsFont("Helvetica")) # Ensure font is mapped correctly


### Step 2 - Specify keywords ------------------------------------------------
# (e.g., categories of diet, land management practices, data sources, etc.)

keyword1 <- "Baobab"
keyword2 <- "Beaver"
keyword3 <- "Castor fiber"
keyword4 <- "hydrology"


### Step 3 - Fetch metrics ------------------------------------------------
# Use the europepmc package to query the Europe PubMed Central RESTful Web Service to 
# fetch the number of publications with specific keywords for each year. 
# Note that the search arguments can be much more nuanced through targeting particular 
# parts of source documents (e.g., titles, abstracts (via "abstract: my-keywords"),
# funders) and/or use of Boolean operators (e.g., and, or, etc.). 
# For more information see https://docs.ropensci.org/europepmc/.

trend_1 <- europepmc::epmc_hits_trend(query = keyword1,
                                        period = 1995:2025, synonym = FALSE)
trend_2 <- europepmc::epmc_hits_trend(query = keyword2,
                                         period = 1995:2025, synonym = FALSE)
trend_3 <- europepmc::epmc_hits_trend(query = keyword3,
                                          period = 1995:2025, synonym = FALSE)
trend_4 <- europepmc::epmc_hits_trend(query = keyword4,
                                         period = 1995:2025, synonym = FALSE)


### Step 4 - Combine data ------------------------------------------------

combined_data <- rbind(
  mutate(trend_1, keywords = keyword1),
  mutate(trend_2, keywords = keyword2),
  mutate(trend_3, keywords = keyword3),
  mutate(trend_4, keywords = keyword4)
)


### Step 5 - Visualise data ------------------------------------------------

# Absolute trends
keyword_1_plot_abs <- ggplot(trend_1, aes(x = factor(year), y = query_hits)) +
  geom_col(width = 0.6, alpha = 0.9) +
  # theme_minimal() +
  labs(x = "Year", y = "Number  of published articles") +
  ggtitle("Scientific Interest in Baobab") +
  # ylim(0, 0.85) + # NB. the ylim often needs tuning if enabled
  scale_fill_viridis_d() +
  theme_beautiful()+
  # theme(legend.position = "none",
  #       axis.text.x = element_text(size = 7, angle = 60, hjust = 1),
  #       plot.title = element_text(hjust = 0.5),
  #       legend.text = element_text(size = 12),
  #       panel.grid.major = element_blank(),
  #       panel.grid.minor = element_blank())+
  scale_x_discrete(labels = function(x) ifelse(seq_along(x) %% 2 == 1, x, ""))

keyword_1_plot_abs # Display output

ggsave("Utility 2 - Visualising scientific interest in key terms/key_term_1_abs.jpg",
       plot = keyword_1_plot_abs,
       width = 16,
       height = 16,
       units = "cm")

# Relative trends
keyword_1_plot_relative <- ggplot(trend_1, aes(x = factor(year), y = (query_hits / all_hits * 100))) +
  geom_col(width = 0.6, alpha = 0.9) +
  # theme_minimal() +
  labs(x = "Year", y = "% of all published articles") +
  ggtitle("Interest of scientists in studying XXXXXXXXXXXX") +
  # ylim(0, 0.85) + # NB. the ylim often needs tuning if enabled
  scale_fill_viridis_d() +
  theme_beautiful()+
  # theme(legend.position = "none",
  #       axis.text.x = element_text(size = 7, angle = 60, hjust = 1),
  #       plot.title = element_text(hjust = 0.5),
  #       legend.text = element_text(size = 12),
  #       panel.grid.major = element_blank(),
  #       panel.grid.minor = element_blank())+
  scale_x_discrete(labels = function(x) ifelse(seq_along(x) %% 2 == 1, x, ""))

keyword_1_plot_relative # Display output

ggsave("Utility 2 - Visualising scientific interest in key terms/key_term_1_relative.jpg",
       plot = keyword_1_plot_relative,
       width = 16,
       height = 16,
       units = "cm")


combined_plot_relative <- ggplot(combined_data, aes(x = factor(year), y = (query_hits / all_hits * 100), fill = keywords)) +
  geom_col(width = 0.6, alpha = 0.9) +
  # theme_minimal() +
  labs(x = "Year", y = "% of all published articles") +
  ggtitle("Interest of scientists in studying XXXXXXXXXXXX") +
  # ylim(0, 0.85) + # NB. the ylim often needs tuning if enabled
  scale_fill_viridis_d() +
  facet_wrap(~keywords, ncol = 2) +
  theme_beautiful()+
  # theme(legend.position = "none",
  #       axis.text.x = element_text(size = 7, angle = 60, hjust = 1),
  #       plot.title = element_text(hjust = 0.5),
  #       legend.text = element_text(size = 12),
  #       panel.grid.major = element_blank(),
  #       panel.grid.minor = element_blank())+
  scale_x_discrete(labels = function(x) ifelse(seq_along(x) %% 2 == 1, x, ""))

combined_plot_relative # Display output

ggsave("Utility 2 - Visualising scientific interest in key terms/combined_plot_relative.jpg", 
       plot = combined_plot,
       width = 16,
       height = 16,
       units = "cm")

