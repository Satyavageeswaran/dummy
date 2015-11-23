###SECTION 0 - Basic Setup
#-------------------------
rm(list=ls())

ifelse (Sys.getenv('computername') == "51150001_NEW",     # Setting WD on either computers 
        setwd("D:\\Cloud\\G-Drive\\Work\\Research\\My Research\\PhD\\Projects Related\\All Projects\\Active Projects\\Fortune 1000\\WD"), 
        setwd("E:\\Cloud\\Drive\\Work\\Research\\My Research\\PhD\\Projects Related\\All Projects\\Active Projects\\Fortune 1000\\WD")
)

#data <- readRDS("F1K.RDS")
data <- readRDS("F1K_NonNull.RDS") ; attach(data)
class.of.data <- as.matrix(lapply(data,class)) # generating a view of the class of each of the variables in the dataset


#----------------------------
### SECTION 1 - OLS / I model
#----------------------------

F1 <- Mkt.Val ~ log(Assets)+Leverage+factor(Services)+factor(Bas_Mat)+factor(Fin)+factor(Con_Goods)+factor(Tech)+factor(Healthcare)+
  factor(Ind_Good)+factor(Utilities)+factor(Conglomerates)+factor(Others)+ Profit + log(Revenue)
F2 <- tobinsQ.2013 ~ log(Assets)+Leverage+factor(Services)+factor(Bas_Mat)+factor(Fin)+factor(Con_Goods)+factor(Tech)+factor(Healthcare)+
  factor(Ind_Good)+factor(Utilities)+factor(Conglomerates)+factor(Others)+ Profit + log(Revenue)
F3 <- mc.tobinsQ.2013 ~ 0 + log(Assets)+Leverage+factor(Services)+factor(Bas_Mat)+factor(Fin)+factor(Con_Goods)+factor(Tech)+factor(Healthcare)+
  factor(Ind_Good)+factor(Utilities)+factor(Conglomerates)+factor(Others)+ Profit + log(Revenue)
F4 <- log(tobinsQ.2013) ~ log(Assets)+Leverage+factor(Services)+factor(Bas_Mat)+factor(Fin)+factor(Con_Goods)+factor(Tech)+factor(Healthcare)+
  factor(Ind_Good)+factor(Utilities)+factor(Conglomerates)+factor(Others)+ log(Revenue)

summary(lm(F4, data = data))


#------------------------------------------------------------------------
### SECTION 2 - Text Manipulation to Construct W (Based on Pandey's Code) 
#------------------------------------------------------------------------
#Re-RUN ONLY if a new W needs to be created. Otherwise, move to next section.

library(tm)
library(RWeka)
library(wordcloud)
library(lsa)

# User defined functions

text.clean = function(x)                          # text data
{ require("tm")
  x  =  gsub("<.*?>", "", x)                  # regex for removing HTML tags
  x  =  iconv(x, "latin1", "ASCII", sub="")   # Keep only ASCII characters
  x  =  gsub("[^[:alnum:]]", " ", x)       # keep only alpha numeric 
  x  =  tolower(x)                          # convert to lower case characters
  x  =  removeNumbers(x)                    # removing numbers
  x  =  stripWhitespace(x)                  # removing white space
  x  =  gsub("^\\s+|\\s+$", "", x)          # remove leading and trailing white space
  return(x)
}

text.clean1 = function(x)                          # text data
{ require("tm")
  x  =  gsub("<.*?>", "", x)                  # regex for removing HTML tags
  x  =  iconv(x, "latin1", "ASCII", sub="")   # Keep only ASCII characters
  x  =  gsub("[^[:alnum:]-]", " ", x)       # keep only alpha numeric 
  x  =  tolower(x)                          # convert to lower case characters
  x  =  removeNumbers(x)                    # removing numbers
  x  =  stripWhitespace(x)                  # removing white space
  x  =  gsub("^\\s+|\\s+$", "", x)          # remove leading and trailing white space
  return(x)
}

remove.blank.docs <- function(tdm.new){
  a0 = NULL; 
  for (i1 in 1:ncol(tdm.new)){ if (sum(tdm.new[, i1]) == 0) {a0 = c(a0, i1)} }
  length(a0)    # no. of empty docs in the corpus
  if (length(a0) >0) { tdm.new1 = tdm.new[, -a0]} else {tdm.new1 = tdm.new};
  
  
  x2mat = t(tdm.new1)		# x2mat is DTM. Don't do tfidf, not mentioned anywhere for topic modeling.
  
  test = colnames(x2mat); 
  test1 = gsub(" ",".", test);  # replace spaces with dots
  colnames(x2mat) = test1
  
  return(x2mat)   
}                         # remove.blank.docs func ends

nonunique.terms <- function(tdm){
  a1 = apply(tdm, 1, sum);   # retain only terms that occur >1 in corpus
  sum(a1 > 1);	  
  a2 = ((a1 > 1))
  tdm.new = tdm[a2, ]
  return(tdm.new) 
}                              # func ends

