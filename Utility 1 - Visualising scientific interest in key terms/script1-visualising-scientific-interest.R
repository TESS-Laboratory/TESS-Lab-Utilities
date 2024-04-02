# This is a simple code demo for visualising scientific interest in key terms
# (adapted by Andy Cunliffe from https://twitter.com/EShekhova/status/1772925867603165413?s=09, who adapted it from elsewhere...)

# Step 1 - Load packages ------------------------------------------------

# install.packages("europepmc")
# install.packages("tidyverse")
# install.packages("viridis")

library(europepmc)
library(tidyverse)
library(viridis)


# Step 2 - Specify keywords ------------------------------------------------
# (e.g., categories of diet, land management practices, data sources, etc.)

keyword1 <- "vegetarian diet"
keyword2 <- "low carbohydrate diet"
keyword3 <- "intermittent fasting"
keyword4 <- "gluten free diet"


# Step 3 - Fetch metrics ------------------------------------------------
# Use the europepmc package to query the Europe PubMed Central RESTful Web Service to 
# fetch the number of publications with specific keywords for each year. 

trend_1 <- europepmc::epmc_hits_trend(query = keyword1,
                                        period = 1995:2023, synonym = FALSE)
trend_2 <- europepmc::epmc_hits_trend(query = keyword2,
                                         period = 1995:2023, synonym = FALSE)
trend_3 <- europepmc::epmc_hits_trend(query = keyword3,
                                          period = 1995:2023, synonym = FALSE)
trend_4 <- europepmc::epmc_hits_trend(query = keyword4,
                                         period = 1995:2023, synonym = FALSE)


# Step 4 - Combine data ------------------------------------------------

combined_data <- rbind(
  mutate(trend_1, keywords = keyword1),
  mutate(trend_2, keywords = keyword2),
  mutate(trend_3, keywords = keyword3),
  mutate(trend_4, keywords = keyword4)
)


# Step 5 - Visualise data ------------------------------------------------

combined_plot <- ggplot(combined_data, aes(x = factor(year), y = (query_hits / all_hits * 100), fill = keywords)) +
  geom_col(width = 0.6, alpha = 0.9) +
  theme_minimal() +
  labs(x = "Year", y = "% of all published articles") +
  ggtitle("Interest of scientists in studying different XXXXXXXXXXXX") +
  ylim(0, 0.85) +
  scale_fill_viridis_d() +
  facet_wrap(~keywords, ncol = 2) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 7, angle = 60, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_x_discrete(labels = function(x) ifelse(seq_along(x) %% 2 == 1, x, ""))

combined_plot

# NB. the ylim often needs tuning!

ggsave("combined_plot.png", plot = combined_plot)

