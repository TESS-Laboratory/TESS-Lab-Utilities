# This script (from Tapiwa Gumbo) is intended to fit a linear model using total 
# least squares (TLS) regression and allowing for the intercept to be constrained
# through zero. This can be useful and appropriate in situations like allometric 
# functions for modelling size-biomass relationships 


# It needs careful review to confirm that the matrices handling functions as
# expected and is suitable for research application. !!!!!!!!!!!!!!!!!!!!!!!!!!

# Load packages

library(tidyverse)
library(vegan)
library(multcompView)
library(patchwork)
library(lmerTest)
library(Matrix)
library(lme4)
library(emmeans)
library(here) 
library(robustbase)
library(sjPlot)
library(flextable)
library(officer)
library(glmmTMB)
library(performance)  # model diagnostics
library(stringi)
library(dataMaid)
library(MASS)
library(RobustLinearReg)
library(cowplot)   # for draw_* helpers
library(png)


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



######################### GRASS BIOMASS GRASS BIOMASS  USING TLS TLS TLS 

#LOAD DATA for TLS BIOMASS and intercept line - UNTRANSFORMED VARIABLES
WGtlsdata <- read_csv("DATA/DPM.csv")


################## DPM HEIGHT ~ OVEN DRIED WEIGHT. NO SQUARE ROOT

# 3.  Robust TLS helper
# ------------------------------------------------------------------
tls_fit <- function(x, y, through_origin = FALSE) {
  df <- na.omit(data.frame(x, y))
  if (nrow(df) < 2) stop("Need at least two points for TLS")
  
  # build the matrix for SVD
  if (through_origin) {
    A <- cbind(df$x, df$y)
  } else {
    A <- cbind(df$x - mean(df$x), df$y - mean(df$y))
  }
  
  V <- svd(A)$v
  if (ncol(V) < 2) stop("TLS failed: insufficient rank (x or y constant)")
  
  v2    <- V[, ncol(V)]            # always take the last right‑singular vector
  slope <- -v2[1] / v2[2]
  intercept <- if (through_origin) 0 else mean(df$y) - slope * mean(df$x)
  
  list(intercept = intercept, slope = slope)
}

# 4.  Fit the two TLS models
# ------------------------------------------------------------------
tls_free <- tls_fit(df_tls$DPH_Height, df_tls$Biomass_kg_ha)                # free intercept
tls_zero <- tls_fit(df_tls$DPH_Height, df_tls$Biomass_kg_ha, through_origin = TRUE)  # passes origin

n_obs    <- nrow(df_tls)
r2_proxy <- cor(df_tls$DPH_Height, df_tls$Biomass_kg_ha)^2                  # correlation² as proxy

# 4a.  Build annotation strings (include n)
# ------------------------------------------------------------------
eq_tls1 <- sprintf("y = %.2f + %.2fx\nn = %d", 
                   tls_free$intercept, tls_free$slope, as.integer (n_obs))

eq_tls0 <- sprintf("y = %.2f + %.2fx\nn = %d", 
                   tls_zero$intercept, tls_zero$slope, as.integer(n_obs))

# 5.  Plot
# ------------------------------------------------------------------

ggplot(df_tls, aes(x = DPH_Height, y = Biomass_kg_ha)) +
  geom_point() +
  geom_abline(intercept = tls_free$intercept, slope = tls_free$slope, colour = "red",  size = 1) +
  geom_abline(intercept = 0,                slope = tls_zero$slope, colour = "blue", size = 1) +
  annotate("text",
           x = min(df_tls$DPH_Height, na.rm = TRUE),
           y = max(df_tls$Biomass_kg_ha, na.rm = TRUE),
           label = eq_tls1, hjust = 0, size = 3, colour = "red") +
  annotate("text",
           x = min(df_tls$DPH_Height, na.rm = TRUE),
           y = 0.80 * max(df_tls$Biomass_kg_ha, na.rm = TRUE),
           label = eq_tls0, hjust = 0, size = 3, colour = "blue") +
  labs(x = "DPM Height (cm)", y = "Standing grass biomass (kg/ha)") +
  theme_beautiful()





