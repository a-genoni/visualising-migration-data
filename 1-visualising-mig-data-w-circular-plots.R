---
  
title:   "Annotated R script for creating circular migration plots with R"
author: "Andreas Genoni"
date:    "4th of February 2021"

mail:    "andreas.genoni@gmail.com"
twitter: "@a_genoni"

note:    "This script recreates the circular migration plots shown in Sander & Genoni (2021):
         Visualising migration data with circular migration plots"

         "After running the code, Figure 4 in the article has been edited with Inkscape"

---
  
  
  

################################################################################
############################## LOAD PACKAGES ###################################
################################################################################




library(tidyverse)
library(circlize)
library(readxl)
library(forestmangr)




################################################################################
############################## DATA PREPARATION ################################
################################################################################




# set your working directory (the path of the folder in which you saved/are going to save the files)
setwd("C:/Users/MaxMuster/visualising-migration-data")



# show names of excel sheets with data
readxl::excel_sheets('data-azose-raftery-2019.xlsx')



# choose sheet name and read in its data
data <- read_excel(path = 'data-azose-raftery-2019.xlsx', sheet = '2010-2015')



# Round estimated number of migrated people (package forestmangr)
data_r <- round_df(data, 0, rf = "round")



# read in file with country codes
## this file contains various country codes, allowing us to aggregate countries and corresponding flows at a higher regional level 
codes <- read.csv('country-codes.csv')



# select variables "cregion" and "country" from .csv file
## "cregion" contains aggregated regional information for each country
## "country" helps us to assign the regional information to the right country in our migration data
codes_s <- codes %>% 
  select(cregion, country)



# join variables "cregion" and "country" ("codes-s" data set) with our migration data (data-r)
data_codes <-  left_join(data_r, codes_s, by = "country")



# group origin countries by "cregion" and summarise the flows within each group
regflow1 <- data_codes %>% 
  group_by(cregion) %>%
  summarise_if(is.numeric, funs(sum)) %>%
  ungroup()



# outflow to long format (preparing data for calculating inflows)
regflow_long <- regflow1 %>%
  pivot_longer(Afghanistan:Zimbabwe, names_to = "destination", values_to = "flows")



# select variables "cregion" and "country" in "codes" and rename them
## we rename these variables because we now use them to summarise inflows of countries within the same region
## summarising inflows requires us to identify to which countries/regions migrants are moving
## hence, we name the variables "dest_region" and "destination"
codes_rn <- codes %>% dplyr::rename(dest_region = cregion, destination = country) %>%
  select(dest_region, destination)



# join "dest_region" variable ("codes-rn" data set) with outflow-long
## since we already prepared "outflow-long" by turning it to long format and listing the countries in a variable named "destination",
## the "destination" variable in "codes-rn" helps us assigning the information of regional destination ("dest_region") to our main data set
regflow_codes <- left_join(regflow_long, codes_rn, by = "destination")



# group countries by "cregion" and by "dest_region" and summarise the flows within each group
regflow2 <- regflow_codes %>% 
  group_by(cregion, dest_region) %>%
  dplyr::summarise(flows = sum(flows)) %>% # if you do not use "dplyr::" summarise command is not recognized
  ungroup()



# transform migration data into adjacency matrix by turning it to wide format
matrix <- regflow2 %>% 
  pivot_wider(names_from = dest_region, values_from = flows)



# use values of first column "cregion" as row names
mat <- column_to_rownames(matrix, var = "cregion")



# transform migration data set into real matrix
rmat <-  mat %>%
  as.matrix()




################################################################################
########################## FIGURE 4 - FINAL DESIGN #############################
################################################################################



# preliminaries ----------------------------------------------------------------



# SVG graphics device
svg(file="4-Figure-4.svg")


# in case the code is run a couple of times, the following command disables layout function
par(mfrow=c(1,1))


# create matrix for link visibility
visible <- matrix(TRUE, nrow = nrow(rmat), ncol = ncol(rmat))
visible[rmat < 400000] = FALSE



# initialize circular plot -----------------------------------------------------



# (re)set parameters
circos.clear()
circos.par(
  
  # where to start "drawing" the circle
  start.degree = 145, 
  
  # gaps between segments
  gap.degree = c(0.5, 0.5, 5, 5, 0.5, 0.5, 0.5, 0.5, 5, 0.5, 5, 0.5, 5), 
  
  # gaps between segments and links
  track.margin = c(0.001, 0.001), 
  
  # turn off warnings for plotting outside cell
  points.overflow.warning = FALSE)



# circular plot ----------------------------------------------------------------


chordDiagram(
  
  # matrix
  rmat, 
  
  # change order of segments
  order = c("Southern Europe", "Northern and Western Europe", "Rest of Eastern Europe", 
            "Post-Soviet States", 
            "Eastern Asia", "Southeast Asia and Oceania", 
            "Southern Asia", "Arabian Peninsula", "Rest of Western Asia",   
            "Northern Africa", "Sub-Saharan Africa", 
            "Latin America and Caribbean", "Northern America"), 
  
  # segment and link colours
  grid.col = c("#3aa078", "#026b47", "#74d7ac", 
               "#FFE44D",
               "#9ad1ff", "#0a77e0", "#005bbe", "#4a91fd", "#74b1ff",
               "#ffc763", "#efa23f", 
               "#4ea2c3", "#056886"),
  
  
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
  
  # defining outer segments
  annotationTrack = "grid", 
  annotationTrackHeight = c(0.03, 0.01)
  
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
      
      # font size
      cex = 0.8,
      
      # font color
      col = "black"
    )
  }
)

# close .svg device
dev.off()


# note: we edited Figure 4 with inkscape after plotting it
# inkscape can be downloaded for free: https://inkscape.org
# we used inkscape to adjust and colour the segment labels




################################################################################
######################### FIGURE 5 - SMALL MULTIPLES ###########################
################################################################################




# preliminaries ----------------------------------------------------------------



# SVG graphics device
svg(file="4-Figure-5.svg")

# 2x2 plot layout
layout(matrix(c(1,1,1,1,1,2,2,2,2,2, # create matrix for plotting
                1,1,1,1,1,2,2,2,2,2,
                0,0,0,0,0,0,0,0,0,0,
                3,3,3,3,3,4,4,4,4,4,
                3,3,3,3,3,4,4,4,4,4), nrow=5, byrow=TRUE),
       heights=c(0.5,0.5,0.2,0.5,0.5)) # determine relative height of rows

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



# Figure 5 - Plot A ------------------------------------------------------------



# initialize circular plot -----------------------------------------------------



# (re)set parameters
circos.clear()
circos.par(
  
  # where to start "drawing" the circle
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
  rmat, 
  
  # change order of sectors
  order = c("Southern Europe", "Northern and Western Europe", "Rest of Eastern Europe", 
            "Post-Soviet States", 
            "Eastern Asia", "Southeast Asia and Oceania", "Southern Asia", 
            "Arabian Peninsula", "Rest of Western Asia",   
            "Northern Africa", "Sub-Saharan Africa", 
            "Latin America and Caribbean", "Northern America"), 
  
  # grid and link colors
  grid.col = c("#3aa078", "#026b47", "#74d7ac", 
               "#FFE44D",
               "#9ad1ff", "#0a77e0", "#005bbe", "#4a91fd", "#74b1ff",
               "#ffc763", "#efa23f", 
               "#4ea2c3", "#056886"),
  
  
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

# add title
mtext("A) unfiltered", 
      line = 0.5, cex = 0.8)



# Figure 5 - Plot B ------------------------------------------------------------



# create matrix for link visibility
visible <- matrix(TRUE, nrow = nrow(rmat), ncol = ncol(rmat))
visible[rmat < 800000] = FALSE



# initialize circular plot -----------------------------------------------------



# (re)set parameters
circos.clear()
circos.par(
  
  # where to start "drawing" the circle
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
  rmat, 
  
  # change order of sectors
  order = c("Southern Europe", "Northern and Western Europe", "Rest of Eastern Europe", 
            "Post-Soviet States", 
            "Eastern Asia", "Southeast Asia and Oceania", 
            "Southern Asia", "Arabian Peninsula", "Rest of Western Asia",   
            "Northern Africa", "Sub-Saharan Africa", 
            "Latin America and Caribbean", "Northern America"), 
  
  # grid and link colors
  grid.col = c("#3aa078", "#026b47", "#74d7ac", 
               "#FFE44D",
               "#9ad1ff", "#0a77e0", "#005bbe", "#4a91fd", "#74b1ff",
               "#ffc763", "#efa23f", 
               "#4ea2c3", "#056886"),
  
  
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

# add title
mtext("B) overfiltered", 
      line = 0.5, cex = 0.8)



# Figure 5 - Plot C ------------------------------------------------------------



# create matrix for link visibility
visible <- matrix(TRUE, nrow = nrow(rmat), ncol = ncol(rmat))
visible[rmat < 400000] = FALSE



# initialize circular plot -----------------------------------------------------



# (re)set parameters
circos.clear()
circos.par(
  
  # where to start "drawing" the circle
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
  rmat, 
  
  # change order of sectors
  order = c("Southern Europe", "Northern and Western Europe", "Rest of Eastern Europe", 
            "Post-Soviet States", 
            "Eastern Asia", "Southeast Asia and Oceania", 
            "Southern Asia", "Arabian Peninsula", "Rest of Western Asia",   
            "Northern Africa", "Sub-Saharan Africa", 
            "Latin America and Caribbean", "Northern America"), 
  
  # grid and link colors
  grid.col = c("#3aa078", "#3aa078", "#3aa078", 
               "#FFE44D",
               "#0a77e0", "#0a77e0", "#0a77e0", "#0a77e0", "#0a77e0",
               "#efa23f", "#efa23f", 
               "#4ea2c3", "#4ea2c3"),
  
  
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

# add title
mtext("C) lack of
colour contrast", 
      line = 0.25, cex = 0.8)



# Figure 5 - Plot D ------------------------------------------------------------



# create matrix for link visibility
visible <- matrix(TRUE, nrow = nrow(rmat), ncol = ncol(rmat))
visible[rmat < 400000] = FALSE



# initialize circular plot -----------------------------------------------------



# (re)set parameters
circos.clear()
circos.par(
  
  # where to start "drawing" the circle
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
  rmat, 
  
  # change order of sectors
  order = c("Southern Europe", "Northern and Western Europe", "Rest of Eastern Europe", 
            "Post-Soviet States", 
            "Eastern Asia", "Southeast Asia and Oceania", 
            "Southern Asia", "Arabian Peninsula", "Rest of Western Asia",   
            "Northern Africa", "Sub-Saharan Africa", 
            "Latin America and Caribbean", "Northern America"), 
  
  # grid and link colors
  grid.col = c("#3aa078", "#026b47", "#74d7ac", 
               "#FFE44D",
               "#9ad1ff", "#0a77e0", "#005bbe", "#4a91fd", "#74b1ff",
               "#ffc763", "#efa23f", 
               "#4ea2c3", "#056886"),
  
  
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

# add title
mtext("D) final design (Figure 4)", 
      line = 0.5, cex = 0.8)

# close .svg device
dev.off()




################################################################################
######################### FIGURE 6 - SMALL MULTIPLES ###########################
################################################################################




# preliminaries ----------------------------------------------------------------



# SVG graphics device
svg(file="4-Figure-6.svg")

# 2x2 plot layout
layout(matrix(c(1,1,1,1,1,2,2,2,2,2, # create matrix for plotting
                1,1,1,1,1,2,2,2,2,2,
                0,0,0,0,0,0,0,0,0,0,
                3,3,3,3,3,4,4,4,4,4,
                3,3,3,3,3,4,4,4,4,4), nrow=5, byrow=TRUE),
       heights=c(0.5,0.5,0.2,0.5,0.5)) # determine relative height of rows

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



# Figure 6 - Plot A ------------------------------------------------------------



# create matrix for link visibility
visible <- matrix(TRUE, nrow = nrow(rmat), ncol = ncol(rmat))
visible[rmat < 400000] = FALSE



# initialize circular plot -----------------------------------------------------



# (re)set parameters
circos.clear()
circos.par(
  
  # where to start "drawing" the circle
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
  rmat, 
  
  # change order of sectors
  order = c("Southern Europe", "Northern and Western Europe", "Rest of Eastern Europe", 
            "Post-Soviet States", 
            "Eastern Asia", "Southeast Asia and Oceania", 
            "Southern Asia", "Arabian Peninsula", "Rest of Western Asia",   
            "Northern Africa", "Sub-Saharan Africa", 
            "Latin America and Caribbean", "Northern America"), 
  
  # grid and link colors
  grid.col = c("#5fbd4a", "#3e9e2c", "#80dd68", 
               "#E4DB45",
               "#0a77e0", "#0a77e0", "#0a77e0", "#0a77e0", "#0a77e0",
               "#efa23f", "#efa23f", 
               "#EE5C16", "#c63a00"),
  
  
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

# add title
mtext("A) colour blind (cb) unfriendly design
(normal vision)", 
      line = 0.25, cex = 0.8)


# Figure 6 - Plot B ------------------------------------------------------------



# create matrix for link visibility
visible <- matrix(TRUE, nrow = nrow(rmat), ncol = ncol(rmat))
visible[rmat < 400000] = FALSE



# initialize circular plot -----------------------------------------------------



# (re)set parameters
circos.clear()
circos.par(
  
  # where to start "drawing" the circle
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
  rmat, 
  
  # change order of sectors
  order = c("Southern Europe", "Northern and Western Europe", "Rest of Eastern Europe", 
            "Post-Soviet States", 
            "Eastern Asia", "Southeast Asia and Oceania", 
            "Southern Asia", "Arabian Peninsula", "Rest of Western Asia",   
            "Northern Africa", "Sub-Saharan Africa", 
            "Latin America and Caribbean", "Northern America"), 
  
  # grid and link colors
  grid.col = c("#bba746", "#9b8a2b", "#dcc561", 
               "#968527",
               "#bdc8fa", "#3c73dc", "#135cbc", "#5d8dfb", "#91aafb",
               "#e6cf66", "#c6b145", 
               "#9c8a27", "#7a6c13"),
  
  
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

# add title
mtext("B) cb unfriendly design
(cb vision)", 
      line = 0.25, cex = 0.8)



# Figure 6 - Plot C ------------------------------------------------------------



# create matrix for link visibility
visible <- matrix(TRUE, nrow = nrow(rmat), ncol = ncol(rmat))
visible[rmat < 400000] = FALSE



# initialize circular plot -----------------------------------------------------



# (re)set parameters
circos.clear()
circos.par(
  
  # where to start "drawing" the circle
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
  rmat, 
  
  # change order of sectors
  order = c("Southern Europe", "Northern and Western Europe", "Rest of Eastern Europe", 
            "Post-Soviet States", 
            "Eastern Asia", "Southeast Asia and Oceania", 
            "Southern Asia", "Arabian Peninsula", "Rest of Western Asia",   
            "Northern Africa", "Sub-Saharan Africa", 
            "Latin America and Caribbean", "Northern America"), 
  
  # grid and link colors
  grid.col = c("#3aa078", "#026b47", "#74d7ac", 
               "#FFE44D",
               "#9ad1ff", "#0a77e0", "#005bbe", "#4a91fd", "#74b1ff",
               "#ffc763", "#efa23f", 
               "#4ea2c3", "#056886"),
  
  
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

# add title
mtext("C) final design
(normal vision)", 
      line = 0.25, cex = 0.8)



# Figure 6 - Plot B ------------------------------------------------------------



# create matrix for link visibility
visible <- matrix(TRUE, nrow = nrow(rmat), ncol = ncol(rmat))
visible[rmat < 400000] = FALSE



# initialize circular plot -----------------------------------------------------



# (re)set parameters
circos.clear()
circos.par(
  
  # where to start "drawing" the circle
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
  rmat, 
  
  # change order of sectors
  order = c("Southern Europe", "Northern and Western Europe", "Rest of Eastern Europe", 
            "Post-Soviet States", 
            "Eastern Asia", "Southeast Asia and Oceania", 
            "Southern Asia", "Arabian Peninsula", "Rest of Western Asia",   
            "Northern Africa", "Sub-Saharan Africa", 
            "Latin America and Caribbean", "Northern America"), 
  
  # grid and link colors
  grid.col = c("#978D70", "#655D43", "#CEC2A2", 
               "#ffe34e",
               "#bdc8fa", "#3c73dc", "#135cbc", "#5d8dfb", "#91aafb",
               "#e6cf66", "#c6b145", 
               "#8D95BA", "#575F7F"),
  
  
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

# add title
mtext("D) final design
(cb vision)", 
      line = 0.25, cex = 0.8)


# close .svg device
dev.off()
