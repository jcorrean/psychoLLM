library(readr)
ClaudeTest <- read_csv("Data/ClaudeTest.csv")
ClaudeTest$doc_id <- 1:nrow(ClaudeTest)
library(quanteda)
ClaudeItems <- corpus(ClaudeTest$item)
docvars(ClaudeItems, "Dimension") <- ClaudeTest$dimension
docvars(ClaudeItems, "Generating.Source") <- ClaudeTest$item.source
summary(ClaudeItems)
ItemsData <- data.frame(summary(ClaudeItems, n = length(ClaudeItems)))

boxplot(data = ItemsData, Tokens ~ Generating.Source)
Items <- tokens(ClaudeItems, 
                   remove_numbers = TRUE, 
                   remove_punct = TRUE, 
                   remove_url = TRUE, 
                   remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("english"))

ClaudeITEMS <- corpus(ClaudeItems)
CLAUDEITEMS <- dfm(Items, tolower = TRUE)
existing.items <- corpus_subset(ClaudeItems, Generating.Source == "existing.scale")
ExistingItems <- dfm(tokens(existing.items), tolower = TRUE)
LLMgeneratedItems <- corpus_subset(ClaudeItems, Generating.Source == "LLM.generated")
LLMGeneratedItems <- dfm(tokens(LLMgeneratedItems), tolower = TRUE)


# let's see if the eight first items
# are clusterable or not.
library(hopkins)
Hopkins.Items <- hopkins(as.matrix(ExistingItems))
Hopkins.Items
hopkins.pval(Hopkins.Items, n = 8)

# The Hopkins statistics suggests that these eight items
# are indeed measuring at least two dimensions. If this is correct
# we should be able to corroborate this as follows:

library("quanteda.textmodels")
tmod <- textmodel_ca(ExistingItems)
tmod$rowcoord

LLM.items <- corpus_subset(ClaudeItems, Generating.Source == "LLM.generated")
LLMITEMS <- dfm(tokens(LLM.items), tolower = TRUE)
tmod2 <- textmodel_ca(LLMITEMS)
tmod2
tmod$rowinertia
