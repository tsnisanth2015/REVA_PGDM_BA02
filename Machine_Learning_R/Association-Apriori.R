library(arules)

data(Groceries)
Groceries

inspect(Groceries[1:3])

itemFrequencyPlot(Groceries,topN=10)

rules=apriori(Groceries,parameter=list(support=0.01,confidence=0.25,minlen = 2))

summary(rules)

inspect(rules)

inspect(rules[101:103])

sorted_rules=sort(rules,by="lift")

inspect(sorted_rules[1:2])

sodarules=subset(rules, rhs %in% "soda")
inspect(sodarules)
