#install magick package
install.packages("magick")
install.packages("pdftools")
library("magick")

# creating magick-image class with a png for each page of the pdf

# NB change the working directory and inpiut/output directories and pdf location to your own in the following script
setwd("C:/Workspace/R_Scripts/PDF_Image_Conversion")

pages <- magick::image_read_pdf("C:/Workspace/R_Scripts/PDF_Image_Conversion/data_in/dsa-2023-0003.pdf")

# view the ppng file for each page 

pages

# saving each page of the pdf as a png 
# change the number of pages to the number of pages you have imaged above

j <- 1:20
for (i in j){
  pages[i] %>% image_write(., path = paste0("C:/Workspace/R_Scripts/PDF_Image_Conversion/data_out/dsa-2023-0003/image",i,".png"), format = "png")
  
}  

# You should now have a png file for each page of your PDF document ideal for inserting into a Thesis!