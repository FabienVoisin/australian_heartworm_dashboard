library(shiny); library(lubridate); library(DT); library(ggplot2); library(sf);
library(sp); library(cropgrowdays); library(RColorBrewer); library(scales);
#library(maptools); 
library(raster); library(sf); library(sp); 
#library(rgeos); library(rgdal); library(devtools)
library(terra); library(rasterVis); library(tmap); 
library(tmaptools); library(plyr); 
library(busdater); library(stringr); library(dplyr); library(leaflet); library(shinycssloaders); library(rmarkdown); library(knitr);
library(quarto); library(tinytex); library(shinyWidgets); library(plotly);library(graphics)


dseq <- seq(from = as.Date("01-01-2013", format = "%d-%m-%Y"), to = as.Date(Sys.Date()-5, format = "%d-%m-%Y"), by = 1)

#

yseq.df <- plyr::count(data.frame(dseq, year = strftime(dseq, "%Y")), "year")

#

poa.list <- readRDS("data/poa.list")

poa20132017max <- readRDS("data/poa20132017max.RDS")
poa20182022max <- readRDS("data/poa20182022max.RDS")
poa2023max <- readRDS("data/poa2023max.RDS")

postcodes.all <- bind_rows(poa20132017max, poa20182022max, poa2023max)
postcodes.all <- postcodes.all[1:(length(dseq)),]

rownames(postcodes.all) <- dseq

#currentmax.df <- readRDS("data/currentmax.RDS")

currentmax.df <- postcodes.all[(nrow(postcodes.all)-1),]

#postcodes.all <- data.frame(dseq, readRDS("poa20152021max.RDS"))

#column no. of capital cities
capital.codes <- c(35, 466, 671, 1365, 1798, 2139, 2523, 1)
capital.names <- c("Sydney", "Canberra", "Melbourne", "Brisbane", "Adelaide", "Perth", "Hobart", "Darwin")

capital.status.df <- postcodes.all[,((capital.codes))]

capital.chdu <- currentmax.df[,((capital.codes))]
capital.chdu <- round(capital.chdu, digits = 2)
preventatives <- c(0)
for (i in 1:8){
  capital.chdu[2,i] <- if_else(capital.chdu[1,i] >= 130, 'On', 'Off')
  preventatives[i] <- if_else(capital.chdu[2,i]=="On", "Preventatives may be required", "Preventatives not necessary")

}
capital.chdu <- rbind(capital.names, capital.chdu, preventatives)
capital.df <- as.data.frame(t(capital.chdu))
colnames(capital.df) <- c("Capital city", (paste(max(dseq), "'s", " cHDUs", sep="")), (paste(max(dseq), "'s", " status", sep="")), "Are preventatives necessary?")
