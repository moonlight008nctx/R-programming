
# data type transformation
## number to category

# cut() helps to cut a numeric column into a specified number of category. 
lawsch_tb$rank_cg <- cut(lawsch_tb$rank,5,labels=FALSE)