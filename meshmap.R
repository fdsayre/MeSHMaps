# creating maps from Mesh
# first will be a circular map of MeSH coocuurances
# second will be a heatmap of all mesh co-occurances

# data is from https://ii.nlm.nih.gov/MRCOC.shtml
# specifically: wget https://ii.nlm.nih.gov/MRCOC/summary_CoOccurs_2018.txt.gz

# load libraries

library(dplyr)
library(ggplot2)
library(data.table)

# Load DUI to tree number correlations from existing file 

duitrees <- read.csv("duiToTree.csv")

# Load summary correlations
# only need one of the following
# this is a huge file if you do it all

all <- fread("summary_CoOccurs_2018.txt", sep = "|", select=c("DUI1", "DUI2", "Freq"), nrows = 300000000) #get everything

summarydf <- fread("summary_CoOccurs_2018.txt", sep = "|", select=c("DUI1", "DUI2", "Freq"), nrows = 300000) #get sample

summarydf <- fread("summary_CoOccurs_2018.txt", sep = "|", select=c("DUI1", "DUI2", "Freq"), nrows = 3000)


# get MeSH correlations based on a specific DUI  -----

summarydf <- fread("grep D009157 summary_CoOccurs_2018.txt", sep = "|", nrows = 300000) #variable names all fucked up because of the grep. Also not sure if this is searching properly. Could run on full then use dplyr and compare numbers to figure out how accurate this is

summarydf <- summarydf %>% # fix col names
  select(V1, V3, V5)
colnames(summarydf)
colnames(summarydf) <- c("DUI1", "DUI2", "Freq")


# alternative way to do above
# librarians D016245
# mg D009157
#canada D002170

summarydf <- all %>%
  filter(DUI1 == "D009157" | DUI2 == "D009157")


#add tree locations to DUIs ----
# this process massively increases the number of rows in the file. Not sure why this is happening. maybe because of multiple tree locations by DUI?

# add for DUI1
summarymetadf <- merge(summarydf, duitrees, by.x = "DUI1", by.y = "DUI")
colnames(summarymetadf)
colnames(summarymetadf) <- c("DUI1","DUI2","Freq","subLevelTree1","treeNumber1","topTree1","Name1")


# add for DUI2
summarymetadf <- merge(summarymetadf, duitrees, by.x = "DUI2", by.y = "DUI", allow.cartesian=TRUE)
colnames(summarymetadf)
colnames(summarymetadf) <- c("DUI2","DUI1","Freq","subLevelTree1","treeNumber1","topTree1","Name1","subLevelTree2","treeNumber2","topTree2","Name2")


# ChordDiagram ---------
# some resources
# https://rstudio-pubs-static.s3.amazonaws.com/145337_0ecf43312d7b42aaa6b4687649915879.html 

library(circlize)

#get correlations

treecorrelations <- summarymetadf %>%
  select(Freq, Name1, Name2)

treecorrelations <- summarymetadf %>%
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

circos.clear()

chordDiagram(freqpairs, annotationTrack = "grid", preAllocateTracks = list(track.height = 0.1))

circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  xplot = get.cell.meta.data("xplot")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex = 0.6)
}, bg.border = NA)

# try another Chord Diagram utilizing D3

devtools::install_github("mattflor/chorddiag")
library(chorddiag)
library(reshape2)

matrix <- acast(summarymetadf, Name1 ~ Name2)
chorddiag(matrix, type = "bipartite", showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 90)

chorddiag(matrix, type = "bipartite", showTicks = F, groupnameFontsize = 14, groupnamePadding = 20, margin = 90)

# try removing low number results from summarymetadf and then re-using chorddiag to make prettier?

smallsummarymetadf <- summarymetadf %>%
  filter(Freq > 2)
matrix <- acast(smallsummarymetadf, Name1 ~ Name2)
chorddiag(matrix, type = "bipartite", showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 90)


# the following directional one does not work
chorddiag(t, type = "directional", showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 90)


# DUITREE Visualizations ------
# subLevelTree / Name correlations where they share the same DUI
# OR: Tree locations that have the same MeSH Terms

tree <- duitrees %>%
  select(DUI, Name) 

treetable <- as.data.frame(table(tree))

tree <- treetable %>%
  filter(Freq > 1)


treesfreqpairs <- duitrees %>% group_by(DUI, Name) %>%
  summarize(freq = n())


tree2 <- duitrees %>% group_by(subLevelTree, DUI) %>% tally()

tree2 <- duitrees %>% group_by(Name, DUI) %>% tally()

tree2 <- table(duitrees$DUI, duitrees$Name)

tree <- as.data.table(tree2)

tree <- tree %>%
  filter(N > 0)

tree2 <- crossprod(table(duitrees$DUI, duitrees$Name))
diag(tree2) <- 0

circos.clear()

chordDiagram(tree2, annotationTrack = "grid", preAllocateTracks = list(track.height = 0.1))

circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  xplot = get.cell.meta.data("xplot")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex = 0.6)
}, bg.border = NA)

library(reshape2)
tmatrix <- acast(tree, DUI ~ Name)
chorddiag(tree2, type = "bipartite", showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 90)
chorddiag(tree2, type = "directional", showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 90)


# create heatmaps -----------

ggplot(summarymetadf, aes(y=DUI1,x=DUI2, fill=Freq)) +
  geom_tile() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  scale_fill_continuous(high = "#132B43", low = "#56B1F7")

ggplot(summarydf, aes(y=DUI1,x=DUI2, fill=Freq)) +
  geom_tile() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) 

ggplot(summarydf, aes(y=DUI1,x=DUI2, fill=Freq)) +
  geom_tile() +
  scale_fill_continuous(high = "#132B43", low = "#56B1F7")

# heatmap from base stats package that needs input to be matrix format -- 
# try melt? or acast?
require(reshape2)
casted <- acast(summarydf, DUI2 ~ DUI1, value.var = "Freq")


heatmap(casted, xlab = NULL, ylab = NULL, key.xlab = NA, key.ylab = NA)

heatmap(matrix, xlab = NULL, ylab = NULL, key.xlab = NA, key.ylab = NA, Colv = NA, Rowv = NA)


## Heatmap.2
library(gplots)
heatmap.2(matrix, xlab = NULL, ylab = NULL, dendrogram = NULL, key.xlab = NA, key.ylab = NA)


## Latice ----
# Lattice package
require(lattice)

#The lattice package provides a dataset named volcano. It's a square matrix looking like that :
head(df)

# The use of levelplot is really easy then :
levelplot(matrix)

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
