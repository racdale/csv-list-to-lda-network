library(plyr)
library(tm)
library(lda)
library(date)
library(igraph)
setwd('/Users/rickdale/Dropbox/duties/finance/csv-list-to-lda-network')

weekDays = c("*SUNDAYS","*MONDAYS","*TUESDAYs","*WEDNESDAYS","*THURSDAYS","*FRIDAYS","*SATURDAYS")

a = read.table('~/Downloads/transactions (5).csv',sep=',',quote = "\"",header=T)
a$Description = tolower(as.character(a$Description))
a$Date = as.character(a$Date)

a = a[a$Transaction.Type=="debit"&a$Category!="Hide from Budgets & Trends",]

dts = unique(a$Date) 
docs = c()
for (dt in dts) {
  #newstr = c(paste(a[a$Date==dt,]$Description,collapse=';'),paste(a[a$Date==dt,]$Category,collapse=';'))
  newstr = c(paste(a[a$Date==dt,]$Description,collapse=';'))
  wkday = date.mdy(as.date(dt,year=2014),weekday=T)$weekday - 1
  if (wkday==0) { wkday = 7 }
  newstr = paste(c(newstr,weekDays[wkday]),collapse=';')
  docs = c(docs,newstr)
  
}
corpus = Corpus(VectorSource(docs))
spaceToDash = function(x) { return(gsub(" ","-",x)) }
corpus = tm_map(corpus, spaceToDash)
scToSpace = function(x) { return(gsub(";"," ",x)) }
corpus = tm_map(corpus, scToSpace)
docs = PlainTextDocument(corpus)

corpus = lexicalize(docs) # yup
to.keep = corpus$vocab[word.counts(corpus$documents, corpus$vocab) >= 2] # only stuff that happens, actually
corpus = lexicalize(docs, vocab=to.keep)

result = lda.collapsed.gibbs.sampler(corpus,nclusts<-8,to.keep,25,.1,.1) # out of the box run
top.words = top.topic.words(result$topics, nwords<-15, by.score=TRUE) # out of the box run
edges = data.frame() # let's build the edges for our igraph plot
for (i in 1:nclusts) {
  edges = rbind(edges,expand.grid(top.words[,i],top.words[,i])) # cartesian
}
edges[,1] = as.character(edges[,1])
edges[,2] = as.character(edges[,2])
alphabetorder = function(x) { 
  s = edges[x,]
  edges[x,] <<- edges[x,order(s)]
}
sapply(1:dim(edges)[1],alphabetorder)
edges = unique(edges)
edges = edges[edges[,1]!=edges[,2],]
net = graph.data.frame(edges,directed=F)
plot(net,vertex.shape='none',vertex.size=0,vertex.color='white',layout=layout.fruchterman.reingold,
     vertex.label.color='black',vertex.label.cex=1,vertex.label.family='Arial',
     vertex.label.font=2)




