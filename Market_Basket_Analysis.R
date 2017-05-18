# install.packages(c("arules","arulesViz","datasets))
library(arules)
library(arulesViz)
library(datasets)

data("Groceries")
#Since 'arules' package is designed to work with  'transactions' class, 
#it is desirable to convert your dataframe to this class.

#transDat <- as (myDataFrame, "transactions")

# Create an item frequency plot for the top 20 items
itemFrequencyPlot(Groceries,topN=20,type="absolute")

# Get the rules
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8, maxlen = 4))

#rules
options(digits=2)
inspect (rules[1:5])
summary(rules)
rules<-sort(rules, by="confidence", decreasing=TRUE)

#Convert rules into data frame
df = as(rules, "data.frame")
write.csv(df, file = "D:\\All_Rules.csv",row.names=FALSE)

#What are customers likely to buy before buying Product A
rules_milk<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.08), 
               appearance = list(default="lhs",rhs="whole milk"),
               control = list(verbose=F))
rules_milk<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules_milk[1:5])

#What are customers likely to buy if they purchase Product A?
rules2_milk<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.15,minlen=2), 
               appearance = list(default="rhs",lhs="whole milk"),
               control = list(verbose=F))
rules2_milk<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules2_milk[1:5])

# Interactive Plot
plot (rules[1:10],method="graph",interactive=TRUE,shading="confidence") 
plot (rules, measure=c("support", "lift"), shading="confidence")
