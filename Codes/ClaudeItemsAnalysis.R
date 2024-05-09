library(readr)
ClaudeTest <- read_csv("Data/ClaudeTest.csv")
ClaudeTest$doc_id <- 1:nrow(ClaudeTest)
library(quanteda)
ClaudeItems <- corpus(ClaudeTest$item)
docvars(ClaudeItems, "Dimension") <- ClaudeTest$dimension
docvars(ClaudeItems, "Generating.Source") <- ClaudeTest$item.source
summary(ClaudeItems)
ItemsData <- data.frame(summary(ClaudeItems, n = length(ClaudeItems)))

boxplot(data = ItemsData, Types ~ Generating.Source)
Items <- tokens(ClaudeItems, 
                   remove_numbers = TRUE, 
                   remove_punct = TRUE, 
                   remove_url = TRUE, 
                   remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("english"))

existing.items <- corpus_subset(ClaudeItems, Generating.Source == "existing.scale")
ExistingItems <- dfm(tokens(existing.items), tolower = TRUE)
tmod <- textmodel_ca(ExistingItems)
tmod$rowcoord

LLM.items <- corpus_subset(ClaudeItems, Generating.Source == "LLM.generated")
LLMITEMS <- dfm(tokens(LLM.items), tolower = TRUE)

# Semantic Analysis ----
library("quanteda.textmodels")
SA.ExistingItems <- textmodel_lsa(ExistingItems)
SA.ExistingItems$features
