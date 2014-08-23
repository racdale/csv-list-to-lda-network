library(plyr)
library(tm)
library(lda)
library(date)
library(igraph)
setwd('/Users/rickdale/Dropbox/duties/finance/csv-list-to-lda-network')

weekDays = c("*sundays","*mondays","*tuesdays","*wednesdays","*thursdays","*fridays","*saturdays")

a = read.table('~/Downloads/transactions (5).csv',sep=',',quote = "\"",header=T)
a$Description = tolower(as.character(a$Description))
a$Category = tolower(as.character(a$Category))
a$Date = as.character(a$Date)

a = a[a$Transaction.Type=="debit"&a$Category!="hide from budgets & trends"&a$Category!="uncategorized",]

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
corpus = lexicalize(docs)
to.keep = corpus$vocab[word.counts(corpus$documents, corpus$vocab) >= 3]

edges = data.frame() # let's build the edges for our igraph plot
for (i in 1:length(docs)) {
  words = unlist(strsplit(docs[i]," "))
  words = words[words %in% to.keep]
  edges = rbind(edges,expand.grid(words,words)) # cartesian
}
edges[,1] = as.character(edges[,1])
edges[,2] = as.character(edges[,2])
alphabetorder = function(x) { 
  s = edges[x,]
  edges[x,] <<- edges[x,order(s)]
}
sapply(1:dim(edges)[1],alphabetorder)
counted = count.rows(edges)
counted = counted[counted[,1]>4 & counted[,2]!=counted[,3],]
edges = counted[,2:3]
occs1 = unlist(lapply(edges[,1],function (x) { return(sum(edges[,1]==x))}))
occs2 = unlist(lapply(edges[,2],function (x) { return(sum(edges[,2]==x))}))

lineWeights = counted[,1]
lineWeights = lineWeights/(.5*occs1+.5*occs2)

net = graph.data.frame(edges,directed=F)
plot(net,vertex.shape='none',vertex.size=0,vertex.color='white',layout=layout.fruchterman.reingold,
     vertex.label.color='black',vertex.label.cex=1,edge.width=lineWeights*4,vertex.label.family='Arial',
     vertex.label.font=2)

count.rows <-
  function(x)
  {
    order.x <- do.call(order,as.data.frame(x))
    equal.to.previous <-
      rowSums(x[tail(order.x,-1),] != x[head(order.x,-1),])==0
    tf.runs <- rle(equal.to.previous)
    counts <- c(1,
                unlist(mapply( function(x,y) if (y) x+1 else (rep(1,x)),
                               tf.runs$length, tf.runs$value )))
    counts <- counts[ c(diff(counts) <= 0, TRUE ) ]
    unique.rows <- which( c(TRUE, !equal.to.previous ) )
    cbind( counts, x[order.x[ unique.rows ], ,drop=F] )
  }
# https://stat.ethz.ch/pipermail/r-help/2008-January/151489.html


