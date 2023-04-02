---
  
title:   "Migration flows between and within world regions in the period 2015 to 2020."
         "Annotated R script."
         
author:  "Andreas Genoni"
date:    "March 28, 2023"

mail:    "andreas.genoni@gmail.com"
twitter: "@a_genoni"

note:    "The script produces the plots shown in Sander & Genoni (2023): Data
          visualisation in migration research: The case of circular migration 
          plots, published in the Handbook of Research in Migration, 2nd edition,
          Elgar publishing. 
          
          The small arrows in Figure 4 at the end of bigger migration outflows 
          were implemented afterwards by using the open source programme Inkscape.
          
          Please refer to the article and/or github repository if you use this script."

data:    "Abel, Guy (2019): Bilateral international migration flow estimates. 
          figshare. Dataset. https://doi.org/10.6084/m9.figshare.7731233.v6

          The estimated numbers are based on the demographic accounting approach. 
          For more detail, see Azose, J. J., & Raftery, A. E. (2018). Estimation of 
          emigration, return migration, and transit migration between all pairs 
          of countries. Proceedings of the National Academy of Sciences, 201722334. 
          https://doi.org/10.1073/PNAS.1722334116"

---
  
 

################################################################################
################################### PACKAGES ###################################
################################################################################



# clear all
rm(list = ls())



# install packages (if you don't yet have them on your computer)
#install.packages("tidyverse")
#install.packages("circlize")
#install.packages("readxl")



# load packages
library(tidyverse)
library(circlize)
library(readxl)



################################################################################
############################## DATA PREPARATION ################################
################################################################################



# set your working directory
# (the path on your computer that leads to the place where you stored the files)
setwd("C:/your working directory")



# check for names of excel sheets if there are many
readxl::excel_sheets('data-abel 2019-v6.xlsx') 



# read in the downloaded data of the country codes and global migration flows
codes <- read.csv('country codes.csv')
gmig <- read_excel(path = 'data-abel 2019-v6.xlsx') 



# rename identifier variable in gmig for join with codes (origin perspective)
gmig <- rename(gmig, country = origin_name)



# first join of gmig and iso using (origin) country as identifier, keep selected variables
gmig_codes1 <-  left_join(gmig, codes, by = "country") %>%
  select(region2, country, destination_name, migration_2015)



# rename identifier variables for another join with iso3 (destination perspective)
gmig_codes1 <- rename(gmig_codes1, origin_name = country)
gmig_codes1 <- rename(gmig_codes1, country = destination_name)
gmig_codes1 <- rename(gmig_codes1, originregion_name = region2)




# second join of gmig and iso using (destination) country as identifier, keep selected variables
gmig_codes2 <-  left_join(gmig_codes1, codes, by = "country") %>%
  select(originregion_name, region2, origin_name, country, migration_2015)



# group origin countries in world regions and summarise outflows
gmig_out <- gmig_codes2 %>%
  group_by(originregion_name, region2) %>%
  summarise(outflow = sum(migration_2015))



# transform into adjacency matrix: flows from origin regions in column 1 to 
# destination regions in row 1
gmig_mat <- gmig_out %>% 
  pivot_wider(names_from = region2, values_from = outflow)



# use values of first column "country" as row names
gmig_mat <- column_to_rownames(gmig_mat, var = "originregion_name")



# explicitly transform migration data set into "real" matrix
gmig_rmat <-  gmig_mat %>%
  as.matrix()



################################################################################
################################ PRELIMINARIES #################################
################################################################################



# create matrix for link visibility (for all figures)
visible <- matrix(TRUE, nrow = nrow(gmig_rmat), ncol = ncol(gmig_rmat))
visible[gmig_rmat < 400000] = FALSE



# create another matrix for second link visibility version (fig 5)
visible2 <- matrix(TRUE, nrow = nrow(gmig_rmat), ncol = ncol(gmig_rmat))
visible2[gmig_rmat < 800000] = FALSE




# define main colour scheme (for all figures) 
# colour schemes orient on the left-to-right ranking of colnames in gmig_mat
colour_scheme <- c("#4a91fd","#9ad1ff","#74d7ac",
                   "#FFE44D","#4ea2c3","#056886",
                   "#026b47","#ffc763","#005bbe",
                   "#0a77e0","#3aa078","#efa23f","#74b1ff")
grid.col <- setNames(colour_scheme, rownames(gmig_mat))



# define another colour scheme for demonstrating lack of colour contrast (fig 5)
colour_scheme_cc = c("#0a77e0","#0a77e0","#3aa078",
                     "#FFE44D","#4ea2c3","#4ea2c3",
                     "#3aa078","#efa23f","#0a77e0",
                     "#0a77e0","#3aa078","#efa23f","#0a77e0")
grid.col_cc <- setNames(colour_scheme_cc, rownames(gmig_mat))



# define a second colour scheme (fig 6)
colour_scheme2 <- c("#4a91fd","#9ad1ff","#80dd68",
                    "#E4DB45","#EE5C16","#c63a00",
                    "#3e9e2c","#ffc763","#005bbe",
                    "#0a77e0","#5fbd4a","#efa23f","#74b1ff")
grid.col2 <- setNames(colour_scheme2, rownames(gmig_mat))



# create colour-blind version of first scheme (fig 6) 
colour_scheme_cb <- c("#5d8dfb","#bdc8fa","#CEC2A2",
                      "#ffe34e","#8D95BA","#575F7F",
                      "#655D43","#e6cf66","#135cbc",
                      "#3c73dc","#978D70","#c6b145","#91aafb")
grid.col_cb <- setNames(colour_scheme_cb, rownames(gmig_mat))



# create colour-blind version of second scheme (fig 6)
colour_scheme2_cb <- c("#5d8dfb","#bdc8fa","#dcc561",
                       "#968527","#9c8a27","#7a6c13",
                       "#9b8a2b","#e6cf66","#135cbc",
                       "#3c73dc","#bba746","#c6b145","#91aafb")
grid.col2_cb <- setNames(colour_scheme2_cb, rownames(gmig_mat))



################################################################################
################################### FIGURE 4 ###################################
################################################################################



# SVG graphics device (path towards the place where you want to store the graphics)
# important: to create an svg file, you need two commands, one that opens the svg
# device (line 205 below) and one that closes the svg device (for fig 4, line 322)
# note: if you run the svg code, r won't show the plot in the plot window
svg(file="C:/path where you want to save your plot/Figure-4.svg")



# in case the code is run a couple of times, the following command disables layout function
par(mfrow=c(1,1))



# initialize circular plot -----------------------------------------------------



# (re)set parameters
circos.clear()
circos.par(
  
  # where to start
  start.degree = 145, 
  
  # gaps between segments
  gap.degree = c(0.5, 0.5, 5, 5, 0.5, 0.5, 0.5, 0.5, 5, 0.5, 5, 0.5, 5), 
  
  # gaps between segments and links
  track.margin = c(0.001, 0.001), 
  
  # turn off warnings for plotting outside cell
  points.overflow.warning = FALSE)



# circular plot --------------------------------------------------------------------



chordDiagram(
  
  # matrix
  gmig_rmat, 
  
  # change order of segments
  order = c("Southern Europe", "Northern & Western Europe", "Eastern Europe", 
            "Fmr. USSR", 
            "East Asia", "Southeast Asia", 
            "South Asia", "Arabian Peninsula", "West Asia",   
            "Northern Africa", "Sub-Saharan Africa", 
            "Latin America", "North America"), 
  
  # grid and link colours 
  grid.col = grid.col, # see preliminaries
  
  # transparency of links
  transparency = 0.2,
  link.visible = visible, # see preliminaries
  
  # determine direction of links (i.e. direction of relationship between cells)
  directional = 1,
  diffHeight = -0.03,
  
  # how directionality is represented
  direction.type = "diffHeight", 
  
  # style and order of links
  link.sort = TRUE, 
  link.largest.ontop = TRUE,
  
  # adjust height of all links
  h.ratio = 0.85,
  
  # defining outer segments
  annotationTrack = "grid", 
  annotationTrackHeight = c(0.03, 0.01),
  preAllocateTracks = 0.5 # for customizing sector labels
  
)



# Add text and axis
circos.trackPlotRegion( # or short: circos.track()
  track.index = 1, 
  bg.border = NA, 
  panel.fun = function(x, y) { # applies plotting to a cell. it thus requires x and y values
    
    xlim = get.cell.meta.data("xlim") # get.cell.meta.data = obtain info of current cell
    sector.index = get.cell.meta.data("sector.index") 
    
    # parameters for text 
    circos.text(
      
      # text location
      x = mean(xlim),
      y = 3.2,
      
      # names of segments
      labels = sector.index,
      
      # facing of names
      facing = "bending",
      niceFacing = TRUE,
      
      # font size & boldness
      cex = 0.8,
      font = 2,
      
      # font color
      col=grid.col[sector.index]
      
      
      
    )
  }
)



# close .svg device
dev.off()



################################################################################
################################## FIGURE 5 ####################################
################################################################################



# SVG graphics device
svg(file="C:/path where you want to save your plot/Figure-5.svg")



# define layout first ----------------------------------------------------------



# 1x3 plot layout (smaller version)
layout(matrix(c(1,1,1,1,1,4,4,4,4,4, # create matrix for plotting
                1,1,1,1,1,4,4,4,4,4,
                0,0,0,0,0,4,4,4,4,4,
                2,2,2,2,2,4,4,4,4,4,
                2,2,2,2,2,4,4,4,4,4,
                0,0,0,0,0,4,4,4,4,4,
                3,3,3,3,3,4,4,4,4,4,
                3,3,3,3,3,4,4,4,4,4), nrow=8, byrow=TRUE),
       heights=c(0.5,0.5,0.2,0.5,0.5,0.2,0.5,0.5)) # determine relative height of rows



# 1x3 plot layout (bigger version)
layout(matrix(c(1,1,1,1,1,4,4,4,4,4, # create matrix for plotting
                1,1,1,1,1,4,4,4,4,4,
                2,2,2,2,2,4,4,4,4,4,
                2,2,2,2,2,4,4,4,4,4,
                3,3,3,3,3,4,4,4,4,4,
                3,3,3,3,3,4,4,4,4,4), nrow=6, byrow=TRUE),
       heights=c(0.5,0.5,0.5,0.5,0.5,0.5)) # determine relative height of rows



# layout preview
layout.show(n=4)



# check current margins
par()$oma # oma = margins for outer plot area with c(bottom, left, top, right) in number of lines of text
par()$mar # mar = margins for individual plots with c(bottom, left, top, right) in number of lines of text
par()$mai # mai = margins for individual plots with c(bottom, left, top, right) in inches



# set margins if needed
par(oma=c(0.1,0.1,2,0.1))
par(mar=c(0.1,0.1,0.1,0.1))



# layout preview
layout.show(n=4)



# Figure 5 - initialize Plot A -------------------------------------------------



# (re)set parameters
circos.clear()
circos.par(
  
  # where to start
  start.degree = 145, 
  
  # gaps between sectors
  gap.degree = c(0.5, 0.5, 5, 5, 0.5, 0.5, 0.5, 0.5, 5, 0.5, 5, 0.5, 5), 
  
  # gaps between grids and links
  track.margin = c(0.001, 0.001), 
  
  # turn off warnings for plotting outside cell
  points.overflow.warning = FALSE)


# circular plot --------------------------------------------------------------------


chordDiagram(
  
  # matrix
  gmig_rmat, 
  
  # change order of sectors
  order = c("Southern Europe", "Northern & Western Europe", "Eastern Europe", 
            "Fmr. USSR", 
            "East Asia", "Southeast Asia", 
            "South Asia", "Arabian Peninsula", "West Asia",   
            "Northern Africa", "Sub-Saharan Africa", 
            "Latin America", "North America"), 
  
  # grid and link colours
  grid.col = grid.col, # see preliminaries
  
  # transparency of links
  transparency = 0.2,
  
  # determine direction of links (i.e. direction of relationship between cells)
  directional = 1,
  diffHeight = -0.03,
  
  # how directionality is represented
  direction.type = "diffHeight", 
  
  # style and order of links
  link.sort = TRUE, 
  link.largest.ontop = TRUE,
  
  # adjust height of all links
  h.ratio = 0.85,
  
  # defining outer tracks
  annotationTrack = "grid", 
  annotationTrackHeight = c(0.03, 0.01)
  
)



# Figure 5 - initialize Plot B -------------------------------------------------



# (re)set parameters
circos.clear()
circos.par(
  
  # where to start
  start.degree = 145, 
  
  # gaps between sectors
  gap.degree = c(0.5, 0.5, 5, 5, 0.5, 0.5, 0.5, 0.5, 5, 0.5, 5, 0.5, 5), 
  
  # gaps between grids and links
  track.margin = c(0.001, 0.001), 
  
  # turn off warnings for plotting outside cell
  points.overflow.warning = FALSE)



# circular plot --------------------------------------------------------------------



chordDiagram(
  
  # matrix
  gmig_rmat, 
  
  # change order of sectors
  order = c("Southern Europe", "Northern & Western Europe", "Eastern Europe", 
            "Fmr. USSR", 
            "East Asia", "Southeast Asia", 
            "South Asia", "Arabian Peninsula", "West Asia",   
            "Northern Africa", "Sub-Saharan Africa", 
            "Latin America", "North America"), 
  
  # grid and link colours
  grid.col = grid.col, # see preliminaries
  
  # transparency of links
  transparency = 0.2,
  link.visible = visible2, # see preliminaries
  
  # determine direction of links (i.e. direction of relationship between cells)
  directional = 1,
  diffHeight = -0.03,
  
  # how directionality is represented
  direction.type = "diffHeight", 
  
  # style and order of links
  link.sort = TRUE, 
  link.largest.ontop = TRUE,
  
  # adjust height of all links
  h.ratio = 0.85,
  
  # defining outer tracks
  annotationTrack = "grid", 
  annotationTrackHeight = c(0.03, 0.01)
  
)



# Figure 5 - initialize Plot C -------------------------------------------------



# (re)set parameters
circos.clear()
circos.par(
  
  # where to start
  start.degree = 145, 
  
  # gaps between sectors
  gap.degree = c(0.5, 0.5, 5, 5, 0.5, 0.5, 0.5, 0.5, 5, 0.5, 5, 0.5, 5), 
  
  # gaps between grids and links
  track.margin = c(0.001, 0.001), 
  
  # turn off warnings for plotting outside cell
  points.overflow.warning = FALSE)



# circular plot --------------------------------------------------------------------



chordDiagram(
  
  # matrix
  gmig_rmat, 
  
  # change order of sectors
  order = c("Southern Europe", "Northern & Western Europe", "Eastern Europe", 
            "Fmr. USSR", 
            "East Asia", "Southeast Asia", 
            "South Asia", "Arabian Peninsula", "West Asia",   
            "Northern Africa", "Sub-Saharan Africa", 
            "Latin America", "North America"), 
  
  # grid and link colours
  grid.col = grid.col_cc, # see preliminaries
  
  # transparency of links
  transparency = 0.2,
  link.visible = visible,
  
  # determine direction of links (i.e. direction of relationship between cells)
  directional = 1,
  diffHeight = -0.03,
  
  # how directionality is represented
  direction.type = "diffHeight", 
  
  # style and order of links
  link.sort = TRUE, 
  link.largest.ontop = TRUE,
  
  # adjust height of all links
  h.ratio = 0.85,
  
  # defining outer tracks
  annotationTrack = "grid", 
  annotationTrackHeight = c(0.03, 0.01)
  
)



# close .svg device
dev.off()



################################################################################
################################## FIGURE 6 ####################################
################################################################################



# SVG graphics device
svg(file="C:/path where you want to save your plot/Figure-6.svg")



# define layout ----------------------------------------------------------------



# 2x2 plot layout (small version)
layout(matrix(c(1,1,1,1,1,2,2,2,2,2, # create matrix for plotting
                1,1,1,1,1,2,2,2,2,2,
                0,0,0,0,0,0,0,0,0,0,
                3,3,3,3,3,4,4,4,4,4,
                3,3,3,3,3,4,4,4,4,4), nrow=5, byrow=TRUE),
       heights=c(0.5,0.5,0.2,0.5,0.5)) # determine relative height of rows



# 2x2 plot layout (big version)
layout(matrix(c(1,1,1,1,1,2,2,2,2,2, # create matrix for plotting
                1,1,1,1,1,2,2,2,2,2,
                3,3,3,3,3,4,4,4,4,4,
                3,3,3,3,3,4,4,4,4,4), nrow=4, byrow=TRUE),
       heights=c(0.5,0.5,0.5,0.5)) # determine relative height of rows



# layout preview
layout.show(n=4)



# check current margins
par()$oma # oma = margins for outer plot area with c(bottom, left, top, right) in number of lines of text
par()$mar # mar = margins for individual plots with c(bottom, left, top, right) in number of lines of text
par()$mai # mai = margins for individual plots with c(bottom, left, top, right) in inches



# set margins if needed
par(oma=c(0.1,0.1,2.5,0.1))
par(mar=c(0.1,0.1,0.1,0.1))



# layout preview
layout.show(n=4)
   
  
  
# Figure 6 - initialize top left plot ------------------------------------------ 



# (re)set parameters
circos.clear()
circos.par(
  
  # where to start
  start.degree = 145, 
  
  # gaps between sectors
  gap.degree = c(0.5, 0.5, 5, 5, 0.5, 0.5, 0.5, 0.5, 5, 0.5, 5, 0.5, 5), 
  
  # gaps between grids and links
  track.margin = c(0.001, 0.001), 
  
  # turn off warnings for plotting outside cell
  points.overflow.warning = FALSE)


# circular plot --------------------------------------------------------------------


chordDiagram(
  
  # matrix
  gmig_rmat, 
  
  # change order of sectors
  order = c("Southern Europe", "Northern & Western Europe", "Eastern Europe", 
            "Fmr. USSR", 
            "East Asia", "Southeast Asia", 
            "South Asia", "Arabian Peninsula", "West Asia",   
            "Northern Africa", "Sub-Saharan Africa", 
            "Latin America", "North America"), 
  
  # grid and link colors
  grid.col = grid.col2, # see preliminaries
  
  # transparency of links
  transparency = 0.2,
  link.visible = visible, # see preliminaries
  
  # determine direction of links (i.e. direction of relationship between cells)
  directional = 1,
  diffHeight = -0.03,
  
  # how directionality is represented
  direction.type = "diffHeight", 
  
  # style and order of links
  link.sort = TRUE, 
  link.largest.ontop = TRUE,
  
  # adjust height of all links
  h.ratio = 0.85,
  
  # defining outer tracks
  annotationTrack = "grid", 
  annotationTrackHeight = c(0.03, 0.01)
  
)



# Figure 6 - initialize top right plot -----------------------------------------



# (re)set parameters
circos.clear()
circos.par(
  
  # where to start
  start.degree = 145, 
  
  # gaps between sectors
  gap.degree = c(0.5, 0.5, 5, 5, 0.5, 0.5, 0.5, 0.5, 5, 0.5, 5, 0.5, 5), 
  
  # gaps between grids and links
  track.margin = c(0.001, 0.001), 
  
  # turn off warnings for plotting outside cell
  points.overflow.warning = FALSE)



# circular plot ----------------------------------------------------------------



chordDiagram(
  
  # matrix
  gmig_rmat, 
  
  # change order of sectors
  order = c("Southern Europe", "Northern & Western Europe", "Eastern Europe", 
            "Fmr. USSR", 
            "East Asia", "Southeast Asia", 
            "South Asia", "Arabian Peninsula", "West Asia",   
            "Northern Africa", "Sub-Saharan Africa", 
            "Latin America", "North America"), 
  
  # grid and link colors
  grid.col = grid.col2_cb, # see preliminaries
  
  # transparency of links
  transparency = 0.2,
  link.visible = visible, # see preliminaries
  
  # determine direction of links (i.e. direction of relationship between cells)
  directional = 1,
  diffHeight = -0.03,
  
  # how directionality is represented
  direction.type = "diffHeight", 
  
  # style and order of links
  link.sort = TRUE, 
  link.largest.ontop = TRUE,
  
  # adjust height of all links
  h.ratio = 0.85,
  
  # defining outer tracks
  annotationTrack = "grid", 
  annotationTrackHeight = c(0.03, 0.01)
  
)



# Figure 6 - initialize bottom left plot ---------------------------------------



# (re)set parameters
circos.clear()
circos.par(
  
  # where to start
  start.degree = 145, 
  
  # gaps between sectors
  gap.degree = c(0.5, 0.5, 5, 5, 0.5, 0.5, 0.5, 0.5, 5, 0.5, 5, 0.5, 5), 
  
  # gaps between grids and links
  track.margin = c(0.001, 0.001), 
  
  # turn off warnings for plotting outside cell
  points.overflow.warning = FALSE)



# circular plot ----------------------------------------------------------------



chordDiagram(
  
  # matrix
  gmig_rmat, 
  
  # change order of sectors
  order = c("Southern Europe", "Northern & Western Europe", "Eastern Europe", 
            "Fmr. USSR", 
            "East Asia", "Southeast Asia", 
            "South Asia", "Arabian Peninsula", "West Asia",   
            "Northern Africa", "Sub-Saharan Africa", 
            "Latin America", "North America"), 
  
  # grid and link colors
  grid.col = grid.col, # see preliminaries
  
  # transparency of links
  transparency = 0.2,
  link.visible = visible, # see preliminaries
  
  # determine direction of links (i.e. direction of relationship between cells)
  directional = 1,
  diffHeight = -0.03,
  
  # how directionality is represented
  direction.type = "diffHeight", 
  
  # style and order of links
  link.sort = TRUE, 
  link.largest.ontop = TRUE,
  
  # adjust height of all links
  h.ratio = 0.85,
  
  # defining outer tracks
  annotationTrack = "grid", 
  annotationTrackHeight = c(0.03, 0.01)
  
)



# Figure 6 - initialize bottom right plot --------------------------------------



# (re)set parameters
circos.clear()
circos.par(
  
  # where to start
  start.degree = 145, 
  
  # gaps between sectors
  gap.degree = c(0.5, 0.5, 5, 5, 0.5, 0.5, 0.5, 0.5, 5, 0.5, 5, 0.5, 5), 
  
  # gaps between grids and links
  track.margin = c(0.001, 0.001), 
  
  # turn off warnings for plotting outside cell
  points.overflow.warning = FALSE)



# circular plot ----------------------------------------------------------------



chordDiagram(
  
  # matrix
  gmig_rmat, 
  
  # change order of sectors
  order = c("Southern Europe", "Northern & Western Europe", "Eastern Europe", 
            "Fmr. USSR", 
            "East Asia", "Southeast Asia", 
            "South Asia", "Arabian Peninsula", "West Asia",   
            "Northern Africa", "Sub-Saharan Africa", 
            "Latin America", "North America"), 
  
  # grid and link colors
  grid.col = grid.col_cb, # see preliminaries
  
  # transparency of links
  transparency = 0.2,
  link.visible = visible, # see preliminaries
  
  # determine direction of links (i.e. direction of relationship between cells)
  directional = 1,
  diffHeight = -0.03,
  
  # how directionality is represented
  direction.type = "diffHeight", 
  
  # style and order of links
  link.sort = TRUE, 
  link.largest.ontop = TRUE,
  
  # adjust height of all links
  h.ratio = 0.85,
  
  # defining outer tracks
  annotationTrack = "grid", 
  annotationTrackHeight = c(0.03, 0.01)
  
)



# close .svg device
dev.off()
