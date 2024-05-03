library(bibliometrix)



file <- "D:/Mirna/ENCE/DISSERTACÃO/revisão bibliográfica/scopus.csv"

M <- convert2df(file, dbsource = "scopus", format = "csv")

head(M["TC"])

biblioshiny()
