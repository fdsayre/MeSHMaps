# Loan Libraries
library(easyPubMed)

# Import XML via query

query_string <- "Myasthenia Gravis[Mesh]"
pubmed_ids <- get_pubmed_ids(query_string)
papers <- fetch_pubmed_data(pubmed_ids)
papers_list <- articles_to_list(papers)
Mesh_list <- custom_grep(papers_list, tag = "DescriptorName", format = "list") #way better


## get general count of MeSH
counttable <- as.data.frame(as.table(table(unlist(Mesh_list)))) # WORKS

noNAs <- na.omit(counttable)

### get both qualifiers and headings into same df

library(XML)
result <- xmlParse(file = "easyPubmed_data_071.xml")
names(result)
print(result)
rootnode <- xmlRoot(result)
rootsize <- xmlSize(rootnode)
print(rootnode[1])


# random playing



# Parse XML to read numbers of MeSH


# Add MeSH Numbers to Dataframe