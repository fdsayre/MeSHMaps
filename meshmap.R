# creating maps from Mesh
# first will be a circular map of MeSH coocuurances
# second will be a heatmap of all mesh co-occurances

# data is from https://ii.nlm.nih.gov/MRCOC.shtml
# specifically: wget https://ii.nlm.nih.gov/MRCOC/summary_CoOccurs_2018.txt.gz

# load library

library(dplyr)
library(ggplot2)
library(data.table)

# Load DUI to tree number correlations from existing file 

duitrees <- read.csv("duiToTree.csv")

# Load summary correlations
# only need one of the following
# this is a huge file if you do it all

summarydf <- fread("summary_CoOccurs_2018.txt", sep = "|", select=c("DUI1", "DUI2", "Freq"), nrows = 300000000) #get everything

summarydf <- fread("summary_CoOccurs_2018.txt", sep = "|", select=c("DUI1", "DUI2", "Freq"), nrows = 300000) #get sample

summarydf <- fread("summary_CoOccurs_2018.txt", sep = "|", nrows = 3000)


# get MeSH correlations based on a specific DUI  -----

summarydf <- fread("grep D009157 summary_CoOccurs_2018.txt", sep = "|", nrows = 300000) #variable names all fucked up because of the grep. Also not sure if this is searching properly. Could run on full then use dplyr and compare numbers to figure out how accurate this is

summarydf <- summarydf %>% # fix col names
  select(V1, V3, V5)
colnames(summarydf)
colnames(summarydf) <- c("DUI1", "DUI2", "Freq")


# alternative way to do above

justmg <- df %>%
  filter(DUI1 == "D009157" | DUI2 == "D009157")


#add tree locations to DUIs ----
# this process massively increases the number of rows in the file. Not sure why this is happening. maybe because of multiple tree locations by DUI?

# add for DUI1
summarydf <- merge(summarydf, duitrees, by.x = "DUI1", by.y = "DUI")
colnames(summarydf)
colnames(summarydf) <- c("DUI1","DUI2","Freq","subLevelTree1","treeNumber1","topTree1","Name1")


# add for DUI2
summarydf <- merge(summarydf, duitrees, by.x = "DUI2", by.y = "DUI", allow.cartesian=TRUE)
colnames(summarydf)
colnames(summarydf) <- c("DUI2","DUI1","Freq","subLevelTree1","treeNumber1","topTree1","Name1","subLevelTree2","treeNumber2","topTree2","Name2")


# ChordDiagram ---------
# some resources
# https://rstudio-pubs-static.s3.amazonaws.com/145337_0ecf43312d7b42aaa6b4687649915879.html 

library(circlize)

#get correlations

treecorrelations <- summarydf %>%
  select(Freq, Name1, Name2)

treecorrelations <- summarydf %>%
  select(Freq, DUI1, DUI2)

# get summary? 

freqpairs <- treecorrelations %>% group_by(Name1, Name2) %>%
  summarize(freq = n())

freqpairs <- treecorrelations %>% group_by(DUI1, DUI2) %>%
  summarize(freq = n())

# create graphs

chordDiagram(freqpairs) 

# try to get text labels rotated so they are readable
# THIS WORKED

chordDiagram(freqpairs, annotationTrack = "grid", preAllocateTracks = list(track.height = 0.1))

circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  xplot = get.cell.meta.data("xplot")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
              niceFacing = TRUE, adj = c(0, 0.5), cex = 0.4)
}, bg.border = NA)

# try different 


# create heatmaps -----------

#df2 <- df %>%
#  select(DUI1, DUI2, Freq)

ggplot(df, aes(y=DUI1,x=DUI2, fill=Freq)) +
  geom_tile() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  scale_fill_continuous(high = "#132B43", low = "#56B1F7")

ggplot(justmg, aes(y=DUI1,x=DUI2, fill=Freq)) +
  geom_tile() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) 

ggplot(biggermg, aes(y=DUI1,x=DUI2, fill=Freq)) +
  geom_tile() +
  scale_fill_continuous(high = "#132B43", low = "#56B1F7")

# heatmap from base stats package that needs input to be matrix format -- 
# try melt? or acast?
require(reshape2)
casted <- acast(smalldf, DUI2 ~ DUI1, value.var = "Freq")


heatmap(casted, xlab = NULL, ylab = NULL, key.xlab = NA, key.ylab = NA)

heatmap(casted, xlab = NULL, ylab = NULL, key.xlab = NA, key.ylab = NA, Colv = NA, Rowv = NA)


## Heatmap.2
library(gplots)
heatmap.2(casted, xlab = NULL, ylab = NULL, dendrogram = NULL, key.xlab = NA, key.ylab = NA)


## Latice ----
# Lattice package
require(lattice)

#The lattice package provides a dataset named volcano. It's a square matrix looking like that :
head(df)

# The use of levelplot is really easy then :
levelplot(casted)

levelplot(casted, data = NULL)

####### OLD -----
# bigmemory

library(bigmemory)
library(biganalytics)
library(bigtabulate)
library(parallel)

df <- read.big.matrix("summary_CoOccurs_2018.csv", header = TRUE)


library(data.table)

df <- data.table("summary_CoOccurs_2018.csv")
