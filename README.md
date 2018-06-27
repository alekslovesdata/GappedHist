GappedHist
_______________________________________________
This R package was inspired by the work of Dr. Fushing Hsieh and Dr. Tania Roy on gapped histograms and analysis of histogram (ANOHT) and developed by Aleksandra Taranov, M.S. student at UC Davis. The academic article it is based on is located here: https://arxiv.org/abs/1702.05879
_______________________________________________
To install within Rstudio, open Rstudio and in it, run:

<<<<<<< HEAD
install.packages("devtools")

devtools::install_github("taranov2007/GappedHist")
=======
# install.packages("devtools")
# devtools::install_github("taranov2007/GappedHist")
>>>>>>> 1307431605d8d8c93d66aa2e15dc0700088b2d24
_______________________________________________
To test out the functions, run:

library(GappedHist)
X <- iris[,1:4]
head(X)
 M3=histbyDESS(values=X[,3],epsilon=1,graph=TRUE);M3
par(mfrow=c(1,3))
cdffromtable(X[,3],M3$table)
histfromtable(M3$table)
plotDESS(X[,3],M3)
