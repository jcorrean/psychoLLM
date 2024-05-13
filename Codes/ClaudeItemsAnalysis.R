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


library(hopkins)
Hopkins <- hopkins(ItemsData[c(2:4)])
Hopkins
hopkins.pval(Hopkins, n = 28)
# This result suggests that all items 
# are clusterable around two or more clusters.
# Nonetheless, this interpretation is 
# misleading, because it uses the number of 
# unique words (types) and total words (tokens).
# A more accurate evaluation can be gauged 
# by assessing how clusterable the document-term 
# frequency (from the items) is:

Hopkins.Items <- hopkins(as.matrix(ExistingItems))
Hopkins.Items
hopkins.pval(Hopkins.Items, n = 8)

# Now, the Hopkins statistics suggests that these eight items
# are indeed measuring at least two dimensions. If this is correct
# we should be able to corroborate this as follows:

library(coefficientalpha)
EI <- as.matrix(ExistingItems)
tau.test(as.matrix(ExistingItems))
colnames(EI) <- 1:ncol(EI)
rownames(EI) <- 1:nrow(EI)
tau.test(t(as.matrix(ExistingItems)))

library("quanteda.textmodels")
tmod <- textmodel_ca(ExistingItems)
tmod$sv

LLM.items <- corpus_subset(ClaudeItems, Generating.Source == "LLM.generated")
LLMITEMS <- dfm(tokens(LLM.items), tolower = TRUE)

# Semantic Analysis ----

SA.ExistingItems <- textmodel_lsa(ExistingItems, nd = 1)
SA.ExistingItems$sk
SA.ExistingItems$docs
SA.ExistingItems$features
SA.ExistingItems$matrix_low_rank

SA.ExistingItems$data
DATA <- as.matrix(SA.ExistingItems$data)
