#install magick package
install.packages("magick")
install.packages("pdftools")
library("magick")

# creating magick-image class with a png for each page of the pdf

# NB change the working directory and input/output path directories and pdf name to your own in the following script

input_path = "C:/Workspace/R_Scripts/PDF_Image_Conversion/data_in"# change to your input data directory

output_path = "C:/Workspace/R_Scripts/PDF_Image_Conversion/data_out/dsa-2023-0003"# change to your output directory (where you want to save the images extracted)

pdf_name = "dsa-2023-0003"# change to your pdf name

pages <- magick::image_read_pdf(paste0(input_path,"/",pdf_name,".pdf"))

# view the ppng file for each page 

pages

# saving each page of the pdf as a png 
# change the number of pages to the number of pages you have imaged above

j <- 1:20
for (i in j){
  pages[i] %>% image_write(., path = paste0(output_path,"/image_page_",i,"_",pdf_name,".png"), format = "png")
  
}  

# You should now have a png file for each page of your PDF document ideal for inserting into a Thesis!