# Query PubMed, retrieve a selected citation and format it as a data frame
dami_query <- "Damiano Fantini[AU]"
dami_on_pubmed <- get_pubmed_ids(dami_query)
dami_abstracts_xml <- fetch_pubmed_data(dami_on_pubmed)
dami_abstracts_list <- articles_to_list(dami_abstracts_xml)
test_df <- article_to_df(pubmedArticle = dami_abstracts_list[[4]], autofill = FALSE, max_chars = 100)
article_to_df(pubmedArticle = dami_abstracts_list[[4]], autofill = TRUE, max_chars = 300)[1:2,]


data <- xmlParse("pubmed_result_mg.xml")
