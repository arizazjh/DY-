library(xlsx)
Data <- read.xlsx("C:/Users/Ariza/Desktop/EPU_SZ_EX_COM.xlsx", sheetIndex = 3) 


library(zoo)
data = zoo(Data, as.Date(Data[,1], origin="1899-12-30"))  ## it works well for some cases..note date column shoul have "Date" name not "date"
data=data[,2:5]

library(vars)
library(urca)
library(frequencyConnectedness)
library(PerformanceAnalytics)
library(nortest)
library(tseries)
library(stats)



plot(data)
data=na.omit(data)
table.Stats(data)



#####################################################
# VAR with liberary vars 
#####################################################
lags=VARselect(data, lag.max = 30, type = "const")  ## "const", "trend", "both", "none"
p=lags$selection[[1]]  ## it selects based on AIC to chnage for example to SIC replace 1 by 3 because: AIC(n)  HQ(n)  SC(n) FPE(n) 
p

############################################################################
### Then compute a system estimate on which the computation of connectedness is based:
# Compute the VAR(2) estimate with constant and save results
est <- VAR(data, p = p, type = "const")

###########################################################
################# 
## The spillover table has no frequency bands, standard Diebold & Yilmaz.
spilloverDY12(est, n.ahead = 24, no.corr = T)

###########################################################
DYsp <- spilloverDY12(est, n.ahead = 24, no.corr = T)
# Get the connectedness tables
overall(DYsp)
DYsp
write.csv(DYsp$tables,"gbltable.csv")

### Contribution PAIRWISE spillover 
nps=pairwise(DYsp)
nps
write.csv(nps,"gblnps.csv")

### Contribution NET spillover 
net(DYsp)



####################################################################################
#########  Rolling Window DY Model 
####################################################################################
params_est = list(p = p, type = "const")
DYspR <- spilloverRollingDY12(data, n.ahead = 24, no.corr = T, "VAR", params_est = params_est, window = 24)

#######################################################################
plotOverall(DYspR)
grid()
#box()
DY.Overall=head(overall(DYspR))


plotTo(DYspR)
grid()
DY.TO=head(to(DYspR))


plotFrom(DYspR)
grid()
DY.From=head(from(DYspR))


plotNet(DYspR)
grid()
DY.Net=head(net(DYspR))
write.csv(DY.Net,"gblns.csv")

plotPairwise(DYspR, which=1:2)
grid()
DY.Pairwise=head(pairwise(DYspR))
write.csv(DY.Pairwise,"gbldnps.csv")
