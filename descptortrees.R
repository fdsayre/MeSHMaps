# extract descrptors with tree numbers
library(stringr)

df <- read.csv("duiToTree.txt", sep="")
colnames(df) <- c("V1","V2","V3","V4")
duitrees <- as.data.frame(str_extract(df$V1, "D[0-9]*"))
colnames(duitrees) <- "dui"

tree1 <- str_extract(df$V3, "[A-Z].*")
tree2 <- gsub(">", "", tree1)
treedf <- as.data.frame(tree2)
colnames(treedf) <- "tree"

duitrees$tree <- treedf$tree

# save the duitrees as csv
write.csv(duitrees, file = "duiToTree.csv")


# get first character of tree locations into its own column

duitrees$t <- substring(treedf$tree, 1, 1)

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
chordDiagram(treecorrelations, transparency = 0.5)

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

