# creating a big ass heatmap of all mesh co-occurances
# library

library(dplyr)
library(ggplot2)
library(data.table)

summarydf <- fread("summary_CoOccurs_2018.txt", sep = "|", select=c("DUI1", "DUI2", "Freq"), nrows = 300000000) #get everything

smallsummarydf <- fread("summary_CoOccurs_2018.txt", sep = "|", select=c("DUI1", "DUI2", "Freq"), nrows = 300000) #get sample

summaryallmeta <- fread("summary_CoOccurs_2018.txt", sep = "|", nrows = 3000)


# get MeSH correlations based on DUI  -----

# just get a disease state based on DUI for MeSH
# grep within fread seems to work but doesn't get header out so need to select variables and rename


summarymgdf <- fread("grep D009157 summary_CoOccurs_2018.txt", sep = "|", nrows = 300000) #variable names all fucked up

summarydf <- summarymgdf %>%
  select(V1, V3, V5)
colnames(summarydf)
colnames(summarydf) <- c("DUI1", "DUI2", "Freq")


# alternative way to do above

justmg <- df %>%
  filter(DUI1 == "D009157" | DUI2 == "D009157")


#add tree locations to DUIs ----
# this is convoluted but works

new <- merge(summarydf, duitrees, by.x = "DUI1", by.y = "dui")
colnames(new)
colnames(new)[4] <- "treedui1"
colnames(new)[5] <-"tdui1"

newdf <- merge(new, duitrees, by.x = "DUI2", by.y="dui", allow.cartesian=TRUE) 

treecorrelations <- newdf %>%
  select(Freq, tdui1, t)

# graph relationship between DUIs and table locations ---
# both below come from https://rstudio-pubs-static.s3.amazonaws.com/145337_0ecf43312d7b42aaa6b4687649915879.html 
# this works! 
library(circlize)
freqpairs <- treecorrelations %>% group_by(tdui1, t) %>%
  summarize(freq = n())
chordDiagram(freqpairs) # works, don't change above

# try a different circlize

chordDiagram(freqpairs, annotationTrack = "grid", preAllocateTracks = list(track.height = 0.1))
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  xplot = get.cell.meta.data("xplot")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  if(abs(xplot[2] - xplot[1]) < 10) {
    circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                niceFacing = TRUE, adj = c(0, 0.5))
  } else {
    circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
                niceFacing = TRUE, adj = c(0.5, 0))
  }
}, bg.border = NA)

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
