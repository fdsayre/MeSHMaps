#Get clean exported CSV file of MeSH DUIs with multiple levels of MeSH tree numbers

#load libraries

library(stringr)

#get basic file out and converted to just DUIs and full tree numbers
df <- read.csv("duiToTree.txt", sep="")
colnames(df) <- c("V1","V2","V3","V4")
duitrees <- as.data.frame(str_extract(df$V1, "D[0-9]*"))
colnames(duitrees) <- "dui"
tree1 <- str_extract(df$V3, "[A-Z].*")
tree2 <- gsub(">", "", tree1)
treedf <- as.data.frame(tree2)
colnames(treedf) <- "tree"
duitrees$tree <- treedf$tree


# get first character of tree locations into its own column

duitrees$t <- substring(duitrees$tree, 1, 1)

# get first three character of tree locations into its own column

duitrees$sublevelt <- substring(duitrees$tree, 1, 3)

# import second level tree numbers and add them to file

treenumbers <- read.csv("treenumbers.csv")

# add tree names to duitrees

duitrees <- merge(duitrees, treenumbers, by.x = "sublevelt", by.y = "A")

# fix names
colnames(duitrees)
colnames(duitrees) <- c("subLevelTree", "DUI", "treeNumber", "topTree","Name")

# save the duitrees as csv
write.csv(duitrees, file = "duiToTree.csv", row.names=FALSE)

# visualize the multiple location in tree relationships for MeSH
#copied below from meshmap.R so need to modify


library(circlize)

#get correlations

duitreecorrelations <- duitrees %>%
  select(DUI, Name)

duitreecorrelations <- duitrees %>%
  select(DUI, Name)

# get summary? 

duitreefreqpairs <- duitreecorrelations %>% group_by(DUI, Name) %>%
  summarize(freq = n())

duitreefreqpairs <- duitreecorrelations %>% group_by(DUI, Name) %>%
  summarize(freq = n())


# create graphs

chordDiagram(duitreefreqpairs) 

# try to get text labels rotated so they are readable
# THIS WORKED

chordDiagram(duitreecorrelations, annotationTrack = "grid", preAllocateTracks = list(track.height = 0.1))

circos.par(cell.padding = c(0, 0, 0, 0), clock.wise = FALSE, track.margin=c(0,0.1),
           gap.degree = 4, start.degree =90)

circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  xplot = get.cell.meta.data("xplot")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex = 0.6)
}, bg.border = NA)


# REST IS PLAYING MAYBE JUNK ------
# LOOK and see if you should delete below
# map tree locations for multiple trees

table <- table(duitrees$dui, duitrees$t)
table2 <- as.table(table)

table <- table(duitrees$t)
head(table)

count(duitrees, 't')

duit <- duitrees %>%
  select(dui, t)

table <- as.data.frame(table(duit))


# graph relationship between DUIs and table locations ----

# Charge the circlize library
library(circlize)
freqpairs <- treecorrelations %>% group_by(tdui1, t) %>%
  summarize(freq = n())
chordDiagram(freqpairs)




# playing ------
# Make the circular plot
chordDiagram(freqpairs, transparency = 0.5)

set.seed(999)
mat = matrix(sample(18, 18), 3, 6) 
rownames(mat) = paste0("S", 1:3)
colnames(mat) = paste0("E", 1:6)

df = data.frame(from = rep(rownames(mat), times = ncol(mat)),
                to = rep(colnames(mat), each = nrow(mat)),
                value = as.vector(mat),
                stringsAsFactors = FALSE)

chordDiagram(mat)
chordDiagram(freqpairs)
circos.clear()

# geom_tile heatmap

ggplot(duitrees, aes(dui, tree)) +
  geom_tile()

ggplot(duitrees, aes(dui, t)) + geom_raster()

library(circlize)
library(dplyr)
freqpairs <- treecorrelations %>% group_by(tdui1, t) %>%
  summarize(freq = n())

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

