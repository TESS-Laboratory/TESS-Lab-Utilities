## R code for creating a bespoke theme in ggplot that makes it much easier to produce beautiful publication-quality plots.
## Step 1) Include the function definition at the top of your script, 
## Step 2) Call this function by including "theme_beautiful() +" within a ggplot2 call, and
## Step 3) Use ggsave to automate the export of high quality plots with correctly scaled text. 


## Create Plotting theme
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

## NB. if you want to combine panels in a figure, R's 'patchework' (https://patchwork.data-imaginist.com/) package is very powerful and easy to use, with easy panel labels.


## ggsave call (use this after you've defined you plot objects)
## NB. Raster-type files (e.g., the ".png" format is fine for inclusion in a Word document for a dissertation, thesis or manuscript for review.
## for final production of papers, it's usually preferable to submit vectorised figures e.g. ".pdf", easly switched by changing the file extention in the ggsave call). 

ggsave(
  yourggplotobject,  #repalce this with the name of your ggplot2 object
  filename = "plots/full_model/Figure 6 comparison_chm_idw_v2.png",  # use relative rather than absolute file paths.
  width = 16, height = 16,  # adjust these based on the intended display size - Portrait A4 documents typically have a ca. 16 cm wide display area.
  units = "cm"
)

