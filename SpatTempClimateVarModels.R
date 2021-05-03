library(nlme)
library(MASS)
library(lme4)
library(cAIC4)
library(glmmTMB)
library(stlplus)
library(forecast)
library(lubridate)
library(scales)
library(gam)
library(devtools)
library(itsadug)
library(BINCOR)
library(seasonal)
library(MuMIn)

devtools::install_github("vr-vr/itsadug", build_vignettes=TRUE)

###dataframe for G3 and G4 combined
datatable = read.csv('/Users/eschall/Documents/MATLAB/Results_SpatioTemporal/StatsTableGreenwichHW.csv')
#datatable = read.csv('/Users/eschall/Documents/MATLAB/Results_SpatioTemporal/StatsTableG3G4adHW.csv')

dt_wnan = datatable[!is.nan(datatable$PHours),]

SICdifG34 = datatable$SIC[datatable$Site == 'G3'] - datatable$SIC[datatable$Site == 'G4']
range(SICdifG34)

dotchart(datatable$PHours,groups = factor(datatable$SAMc))
boxplot(PHours ~ factor(ONIc),varwidth=T,data=datatable)

hist(datatable$PHours)

dtG3G4 = datatable[datatable$Site == 'G3',] 
PHours = datatable$PHours[datatable$Site == 'G4']
PA = datatable$PA[datatable$Site == 'G4']
for(i in 1:length(dtG3G4$PHours))
{
  if (is.nan(dtG3G4$PHours[i]))  
  {dtG3G4$PHours[i] = PHours[i]
  dtG3G4$PA[i] = PA[i]}
  if (is.nan(PHours[i]))  next 
  else {dtG3G4$PHours[i] = mean(c(dtG3G4$PHours[i], PHours[i]))
        dtG3G4$PA[i] = as.logical(dtG3G4$PHours[i])}
}

dtG3G4$Date = seq(as.Date('2011-01-01'),as.Date('2018-12-31'),by='days')
#dtG3G4 = dtG3G4[dtG3G4$Month < 7,]
#dtG3G4$Year = as.factor(dtG3G4$Year) 
#dtG3G4$Month = as.factor(dtG3G4$Month)
dtG3G4$DOY = as.numeric(dtG3G4$DOY)
dtG3G4$SIC = as.numeric(scale(dtG3G4$SIC))
dtG3G4$ONI = as.numeric(scale(dtG3G4$ONI))
dtG3G4$SAM = as.numeric(scale(dtG3G4$SAM))
dtG3G4$group =factor(rep(1,nrow(dtG3G4)))
dtG3G4$DOY2 = factor(1:nrow(dtG3G4))
#dtG3G4 = dtG3G4[dtG3G4$Year != 2014,]
dtG3G4$Jul = as.numeric(dtG3G4$Date)

write.csv(dtG3G4,'/Users/eschall/Documents/MATLAB/Results_SpatioTemporal/dtG3G4.csv')

acf(dtG3G4$SIC, na.action = na.pass)

dtG2 = datatable[datatable$Site == 'G2',,drop=F] 
dtG2$Date = seq(as.Date('2011-01-01'),as.Date('2018-12-31'),by='days')
dtG2 = dtG2[dtG2$Year <2017,]

###de-seasonalize with applying gams and subtracting predicted values from datasets
par(mfrow=c(3,1))
plot(dtG3G4$PHours, type= 'l')
b = gam::gam(PHours ~ lo(DOY,span=0.4),data=dtG3G4, family = quasibinomial(),na.action = na.omit)
E = resid(b)
pp <- predict(b,type = "terms")
I1 <- !is.nan(dtG3G4$PA)
spred <- vector(length = length(dtG3G4$PA))
spred <- NA
spred[I1] <- pp[,1]
spred = spred - mean(spred,na.rm = TRUE)
spred = rescale(spred,to=c(0,0.4))
plot(spred,type='l')
PAds = dtG3G4$PHours - spred
plot(PAds,type='l')

#SIC G3/4
plot(dtG3G4$SIC, type= 'l')
b = gam(SIC ~ s(DOY),data=dtG3G4,na.action = na.omit,correlation=corARMA(form=~1,p=2,q=1))
E = resid(b)
pp <- predict(b, type = "terms")
spred <- pp[,1]
plot(spred,type='l')
spred = ((spred-min(spred,na.rm=T))/(max(spred,na.rm=T)-min(spred,na.rm=T)))*100
SICds = dtG3G4$SIC - spred
plot(SICds,type='l')

#SIC G2
plot(dtG2$SIC, type= 'l')
b = gam(SIC ~ s(DOY),data=dtG2,na.action = na.omit,correlation=corARMA(form=~1,p=2,q=1))
E = resid(b)
pp <- predict(b, type = "terms")
spred <- pp[,1]
plot(spred,type='l')
spred = ((spred-min(spred,na.rm=T))/(max(spred,na.rm=T)-min(spred,na.rm=T)))*100
SICds2 = dtG2$SIC - spred
plot(SICds2,type='l')

plot(dtG3G4$ONI, type= 'l')
b = gam(ONI ~ s(DOY),data=dtG3G4,na.action = na.omit,correlation=corARMA(form=~1,p=2,q=1))
E = resid(b)
pp <- predict(b, type = "terms")
spred <- pp[,1]
plot(spred,type='l')
spred = ((spred-min(spred,na.rm=T))/(max(spred,na.rm=T)-min(spred,na.rm=T)))
ONIds = dtG3G4$ONI - spred
plot(ONIds,type='l')

plot(dtG3G4$SAM, type= 'l')
b = gam(SAM ~ s(DOY),data=dtG3G4,na.action = na.omit,correlation=corARMA(form=~1,p=2,q=1))
E = resid(b)
pp <- predict(b, type = "terms")
spred <- pp[,1]
plot(spred,type='l')
spred = ((spred-min(spred,na.rm=T))/(max(spred,na.rm=T)-min(spred,na.rm=T)))
SAMds = dtG3G4$SAM - spred
plot(SAMds,type='l')

###de-seasonalizing using stlplus (season and trend decomposition with loess)
ls = data.frame(dtG3G4$Date)
ls_PHours = stlplus(dtG3G4$PHours,dtG3G4$Date,n.p=365,s.window ='periodic',outer = 10)
plot(ls_PHours)
ls$PHours = ls_PHours$data$remainder + ls_PHours$data$trend
#ls$PHourss = as.numeric(scale(ls$PHours))
ls_PA = stlplus(dtG3G4$PA,dtG3G4$Date,n.p=365,s.window = 'periodic',outer = 10)
plot(ls_PA)
ls$PA = ls_PA$data$remainder + ls_PA$data$trend
#ls$PAs = as.numeric(scale(ls$PA))
#ls$PAs2 = (ls$PA-min(ls$PA,na.rm=T))/(max(ls$PA,na.rm=T)-min(ls$PA,na.rm=T))
#ls_AAO = stlplus(dtG3G4$AAOd,dtG3G4$Date,n.p=365,s.window = 363,t.window=3000,outer = 10)
#ls$AAO = sgolayfilt(ls_AAO$data$remainder)
#ls$AAOs = as.numeric(scale(ls$AAO))
ls_SAM = stlplus(dtG3G4$SAM,dtG3G4$Date,n.p=365,s.window = 'periodic',outer = 10)
plot(ls_SAM)
ls$SAM = ls_SAM$data$remainder + ls_SAM$data$trend
#ls$SAMs = as.numeric(scale(ls$SAM))
#ls_NINO = stlplus(dtG3G4$NINO34,dtG3G4$Date,n.p=365,s.window = 363,t.window=3000,outer = 10)
#ls$NINO = sgolayfilt(ls_NINO$data$remainder)
#ls$NINOs = as.numeric(scale(ls$NINO))
ls_ONI = stlplus(dtG3G4$ONI,dtG3G4$Date,n.p=365,s.window = 'periodic',outer = 10)
plot(ls_ONI)
ls$ONI = ls_ONI$data$remainder + ls_ONI$data$trend
#ls$ONIs = as.numeric(scale(ls$ONI))
ls_SIC = stlplus(dtG3G4$SIC,dtG3G4$Date,n.p=365,s.window = 'periodic',outer = 10)
plot(ls_SIC)
ls$SIC = ls_SIC$data$remainder + ls_SIC$data$trend
#ls$SICs = as.numeric(scale(ls$SIC))

lsG2 = stlplus(x=dtG2[,6],t=dtG2[,15],n.p=365,s.window = 'periodic')
lsG2 = lsG2$data$remainder
plot(ls_PHours)

colnames(ls)[1] <- "Date"
ls$DOY = dtG3G4$DOY
ls$Year = dtG3G4$Year
ls$Month = dtG3G4$Month
#ls$group = factor(rep(1,nrow(ls)))
ls$fMonth = as.factor(ls$Month) 
ls$Jul = julian(as.Date(ls$Date))
write.csv(ls,'/Users/eschall/Documents/MATLAB/Results_SpatioTemporal/StatsTableGreenwichHW_stlwo.csv')

acf(ls$PHourss, na.action = na.pass)
lsPa = arima(ls$PHourss,order = c(1, 1, 1))
E = resid(lsPa)
acf(E,na.action = na.pass)

ls = read.csv('/Users/eschall/Documents/MATLAB/Results_SpatioTemporal/StatsTableGreenwichHW_stl.csv')

par(mfrow=c(1,3))
plot(ls$SIC,ls$PHours)
plot(ls$ONI,ls$PHours)
plot(ls$SAM,ls$PHours)


###dataframe of daily de-seasonalized data
dt = data.frame(dtG3G4$Date,dtG3G4$Year,dtG3G4$Month,dtG3G4$DOY,PAds,SICds,ONIds,SAMds)
colnames(dt)[1:4] = c('Date','Year','Month','DOY')
dt = dt[dt$Year != 2014,]
dt$PAds = ls$PHours
#dt$PAds = (ls$PHours-min(ls$PHours,na.rm=T))/(max(ls$PHours,na.rm=T)-min(ls$PHours,na.rm=T))
dt$PA = dtG3G4$PA[dtG3G4$Year != 2014]
dt$DOY2 = 1:nrow(dt)
dt = start_event(dt, column="DOY2", event=c("Year"))
dt$SAMc = dtG3G4$SAMc
dt$Jul = julian(as.Date(dt$Date))
dt$fMonth = as.factor(dt$Month)

write.csv(dt,'/Users/eschall/Documents/MATLAB/Results_SpatioTemporal/StatsTableGreenwichHW_deaseasonalized.csv')
dt = read.csv('/Users/eschall/Documents/MATLAB/Results_SpatioTemporal/StatsTableGreenwichHW_deaseasonalized.csv')

#dataframe G2 de-seasonalized daily data
dt2 = data.frame(dtG2$Date,dtG2$Year,dtG2$Month,dtG2$DOY,PAds[1:2192],SICds2[1:2192],ONIds[1:2192],SAMds[1:2192])
colnames(dt2)[1:4] = c('Date','Year','Month','DOY')
dt2$PAds = lsG2
#dt$PAds = (ls$PHours-min(ls$PHours,na.rm=T))/(max(ls$PHours,na.rm=T)-min(ls$PHours,na.rm=T))
dt2$PA = dtG2$PA
dt2 = dt2[dt2$Year != 2014,]
dt2 = dt2[dt2$Year != 2012,]

###dataframe of monthly de-seasonalized data
dtm = data.frame(matrix(NA,length(unique(dt$Year))*length(unique(dt$Month)),1))
c = 84
for(i in 1:length(unique(dt2$Year)))
{
  for(j in 1:length(unique(dt2$Month)))
  {
    c = c+1
    dtm[c,1] = unique(dt2$Year)[i]
    dtm$Month[c] = unique(dt2$Month)[j]
    dtm$PDays[c] = sum(dt2$PA[dt2$Year == unique(dt2$Year)[i] & dt2$Month == unique(dt2$Month)[j]],na.rm=T)/length(dt2$PA[!is.na(dt2$PA) &dt2$Year == unique(dt2$Year)[i] & dt2$Month == unique(dt2$Month)[j]])
    dtm$SIC[c] = mean(dt2$SICds[dt2$Year == unique(dt2$Year)[i] & dt2$Month == unique(dt2$Month)[j]])
    dtm$ONI[c] = mean(dt2$ONIds[dt2$Year == unique(dt2$Year)[i] & dt2$Month == unique(dt2$Month)[j]])
    dtm$SAM[c] = mean(dt2$SAMds[dt2$Year == unique(dt2$Year)[i] & dt2$Month == unique(dt2$Month)[j]])
    dtm$PAds[c] = mean(dt2$PAds[dt2$Year == unique(dt2$Year)[i] & dt2$Month == unique(dt2$Month)[j]],na.rm=T)
    dtm$Site[c] = 'G2'
  }
}
colnames(dtm)[1] = 'Year'
dtm$Jul = c(1:36,49:96)
dtm$group = rep(1,nrow(dtm))
dtm$SIC = as.numeric(scale(dtm$SIC))
dtm$ONI = as.numeric(scale(dtm$ONI))
dtm$SAM = as.numeric(scale(dtm$SAM))
dtm$Site = rep('G3.4',nrow(dtm))

write.csv(dtm[dtm$Site=='G3.4',],'/Users/eschall/Documents/MATLAB/Results_SpatioTemporal/StatsTableGreenwichHW_monthly.csv')

par(mfrow=c(4,1))
plot(dt$PAds)
plot(dtm$SIC)
plot(dtm$ONI)
plot(dtm$SAM)

###GLS implementation on deseasonalized data
control.list <- lmeControl(maxIter = 5000, msMaxIter = 1000, msMaxEval=500,tolerance = 0.1) #,sing.tol=1e-20

#M0 = gls(PAds ~ (SIC + ONI + SAM)^2 , data = dtm, na.action = na.omit,correlation = corARMA(p = 1, q = 1))
M0 = gls(PHours ~ SIC + ONI + SAM,correlation = corARMA(coefs,form=~Jul,p = 5, q = 0),weight= varComb(varIdent(form=~1|fMonth),varExp(form =~ SIC),varExp(form =~ ONI)),data = ls, na.action = na.omit)
#M02 = lme(PHours ~ SICs + ONIs + SAMs,random=~1|fMonth,correlation = corARMA(coefs,form=~Jul,p = 5, q = 0),weights= varComb(varIdent(form=~1|fMonth),varExp(form =~ SICs),varExp(form =~ ONIs)),data = ls, na.action = na.omit)

#correlation = corAR1(valRho,form=~Jul|Year)
#,correlation = corAR1(valRho)
#correlation = corARMA(coefs,form=~Jul|Year,p = 1, q = 1),
#control=lmeControl(msMaxIter = 400, msMaxEval = 1000, msVerbose = TRUE)
#I(ls_SIC^2) + I(ls_ONI^2) + I(ls_SAM^2)

summary(M0)
E = resid(M0, type='normalized')
I1 <- !is.na(ls$PHours)
Efull <- vector(length = length(ls$PHours))
Efull <- NA
Efull[I1] <- E

#Save model output
saveRDS(M0,file='/Users/eschall/Documents/Humpback_Presence/Stats/gls_LS_PHourssSICsONIsSAMsINTfMonthcorARMA50varExpS&OvarIfM.rds')

#Open saved models
GLS1 = readRDS('/Users/eschall/Documents/Humpback_Presence/Stats/glsPAdsSICds2ONIdsSAMdsfMonthcorARMA50varIM.rds')
summary(GLS1)
AIC(GLS1)
GLS2 = readRDS('/Users/eschall/Documents/Humpback_Presence/Stats/glsPAdsSICds2ONIdsSAMdscorARMA11varExpS&O.rds')
summary(GLS2)
GLS3 = readRDS('/Users/eschall/Documents/Humpback_Presence/Stats/glsPAdsSICds2ONIdsSAMdsINTfMonthcorARMA11varExpS&OvarIfM.rds')
summary(GLS3)
GLS4 = readRDS('/Users/eschall/Documents/Humpback_Presence/Stats/glsPAdsSICdsONIds2SAMds2fMonthcorARMA50varExpS&OvarIfM.rds')
summary(GLS4)
GLS5 = readRDS('/Users/eschall/Documents/Humpback_Presence/Stats/glsPAdsSICdsONIds2SAMdsINTfMonthcorARMA50varExpS&OvarIfM.rds')
summary(GLS5)
E = resid(GLS5, type='normalized')
I1 <- !is.na(dt$PA)
Efull <- vector(length = length(dt$PA))
Efull <- NA
Efull[I1] <- E

#Plot residuals for model validation
par(mfrow = c(2, 2), mar = c(5, 4, 1, 2))
plot(M0$fitted,E)
plot(ls$SIC,Efull)
plot(ls$ONI,Efull)
plot(ls$SAM,Efull)

plot(ls$fMonth,Efull)

#Residual distribution
hist(E, xlab = "Residuals", main = "")
hist(ls$PA)

#Plot QQ-plot for normality validation
qqnorm(M0)
qqline(resid(GLS1, type='normalized'))

 #Residual auto-correlation
acf(Efull, na.action = na.pass, main = "Auto-correlation plot for residuals")

###GLM implementation for raw data with AR1 autocorrelation

#M1 = glmmPQL(PA ~ (SIC + ONI + SAM)^2, data=dtG3G4, family = binomial(link="cloglog"),na.action = na.omit,correlation = corAR1(form=~unique(Day)))
#M1 = glmer(PA ~ Year + (SIC + ONI + SAM)^2 + (1 | Month), data=dtG3G4, family = binomial(link="cloglog"),na.action = na.omit)
#M1 = update(M1,correlation = corAR1(form=~unique(Day)))
#M1 = glmmML(PA ~ Year + (SIC + ONI + SAM)^2 , cluster = Month, data=dtG3G4, family = binomial(link="cloglog"),na.action = na.omit,correlation = corAR1(form=~unique(Day)))

M1 = glmmTMB(PA ~ SIC + ONI + SAM + I(SIC^2) + Month + ar1(DOY2+0|group),data=dtG3G4, family = binomial(link="logit"),na.action = na.omit)
M1 = glmmTMB(PHourss ~ SICs + ONIs + SAMs + ar1(Jul+0|group), data = ls,na.action = na.omit)
M1 = glmmTMB(PDays ~ scale(SIC) + scale(ONI) + scale(SAM) + ar1(Jul+0|group), data = dtm,na.action = na.omit, family = binomial(link='logit'))

summary(M1)
#cAIC(M1)
#drop1(M1)

E = resid(M1)

###GAM/GAMM implementation for raw and deseasonalized data

load('/Users/eschall/Documents/Humpback_Presence/Stats/GAMMs_Ahmed/HW_Data2.RData')

control.list <- lmeControl(maxIter = 5000, msMaxIter = 1000, msMaxEval=500,tolerance = 0.1) #,sing.tol=1e-20

M2 = mgcv::gam(PHours ~ s(SIC, bs='cc') + s(ONI,bs='cc') + s(SAM,bs='cc') + s(Jul,bs='fs',m=1), na.action = na.omit, family =quasibinomial,data=dtG3G4)
M2 = mgcv::gamm(PAds ~ s(SICds,bs='cs') + s(ONIds,bs='cs') + s(SAMds,bs='cs') + s(ONIds,by=SAMc) + s(Jul) + s(Month,bs='re'), correlation = corARMA(coefs,p = 1, q = 1), na.action = na.omit,data=dt)
M2 = mgcv::gamm(PA ~ s(SICds,bs='cs') + s(ONIds,bs='cs') + s(SAMds,bs='cs'),  family = binomial, na.action = na.omit,data=dt,correlation = corARMA(coefs,form=~Jul|Year,p = 3, q = 0))
M2 = mgcv::bam(PAds ~ s(SICds,bs='cs') + s(ONIds,bs='cs') + s(SAMds,bs='cs') + s(ONIds,by=SAMc) + s(Jul) + s(Month,bs='re'), data=dt,AR.start=dt$start.event, rho=valRho)
M2 = mgcv::gamm(PA ~ s(SIC,bs='cr') + ONIc + s(SAM,bs='cr') + s(Month, bs='cc'),correlation = corARMA(coefs,form=~Jul,p = 1, q = 0), family= binomial, control=control.list,niterPQL = 50,na.action = na.omit,data=dtG3G4)
M2 = mgcv::gamm(PHours ~ s(SIC,bs='c') + s(ONI,bs='cs') + s(SAM,bs='cs'),correlation = corARMA(coefs[1:5],form=~Jul,p = 5, q = 0), weights=varIdent(form=~1|fMonth),na.action = na.omit,data=ls,control=control.list,niterPQL = 50)

M2 <- gamm(PA ~ s(SIC, bs = "cr") + ONIc + s(SAM, bs='cr') + s(Month, bs = "cc"),
  method = "REML", data = HW_Data2, family = binomial, na.action = "na.omit", niterPQL = 30, control = control.list,
  correlation = corARMA(value = c(coefs,0.1), form = ~ DOY2, p = 2))

#,niterPQL = 50,control=control.list
#s(ONIds,by=SAMc) +  s(Month,bs='re')
#,correlation = corARMA(coefs,form=~Jul|Year,p = 3, q = 0)
#family = binomial,

summary(M2$gam)
acf_resid(M2) #for bam only
E = residuals(M2$lme,type='normalized')
AIC(M2$lme)

#Save model output
saveRDS(M2,file='/Users/eschall/Documents/Humpback_Presence/Stats/gamm_PAsSICONIcsSAMsMonthcorARMA20_DataAhmed.rds')
PASICONISAM = readRDS('/Users/eschall/Documents/Humpback_Presence/Stats/gammPASICONISAMcorARMA21.rds')
summary(PASICONISAM$gam)
M2 = readRDS('/Users/eschall/Documents/Humpback_Presence/Stats/gamm_PAsSICONIcsSAMsMonthcorARMA20_DataAhmed.rds')
summary(M2$gam)
dredge(M2)

#Use auto.arima to get setup for corARMA
arma_res = auto.arima(Efull,stationary=T,seasonal=F)
coefs = as.vector(arma_res$coef)

#Use autocorrelation value at lag 1 from acf for AR1 setup
valRho <- acf(Efull, plot=FALSE,na.action = na.pass)$acf[2]

#Residual distribution
hist(E, xlab = "Residuals", main = "")

#Residual auto-correlation
I1 <- !is.na(HW_Data2$PA)
Efull <- vector(length = length(HW_Data2$PA))
Efull <- NA
Efull[I1] <- E
acf(Efull, na.action = na.pass, main = "Auto-correlation plot for residuals")

#Plot Model output
par(mfrow = c(1,4),oma = c(4,4,4,4), mai = c(0.7, 0.3, 0.1, 0))
mgcv::plot.gam(M2$gam, select = 1, shade = T, cex.lab = 1.5, cex.axis = 1.5, las = 1, shade.col = "lightgrey", rug = F, xlab = "SIC",
               trans = plogis, all.terms = T, cex.main = 1.5,ylab = "Predicted value")
mtext('Predicted value', side=2,line=4,cex=1.1)

mgcv::plot.gam(M2$gam, select = 3, shade = T, cex.lab = 1.5, cex.axis = 1.5, las = 1, shade.col = "lightgrey", rug = F, xlab = "Month",
               trans = plogis, all.terms = T, ylab = "", cex.main = 1.5,yaxt="n")
axis(side = 2, at = c(0,0.2,0.4,0.6,0.8,1), labels = FALSE, tck = -0.04)

#mgcv::plot.gam(M2$gam, select = 4, shade = T, cex.lab = 2, cex.axis = 1.5, las = 1, shade.col = "lightgrey", rug = F, xlab = "ONIc",
#               trans = plogis, all.terms = T, ylab = "", cex.main = 1.5)

Pred2 <- HW_Data2 %>% 
  mutate(Pred = predict.gam(
      object = M2$gam,
      newdata = ., se.fit = T)$fit,Pred = plogis(Pred))
Pred2$ONIc = factor(Pred2$ONIc,levels=c('Negative','Neutral','Positive'))

boxplot(Pred2$Pred ~ Pred2$ONIc, xlab = "ONI",  las = 1, cex.axis = 1.5,cex.lab = 1.5,ylim=c(0,1),yaxt="n")
axis(side = 2, at = c(0,0.2,0.4,0.6,0.8,1), labels = FALSE, tck = -0.04)

mgcv::plot.gam(M2$gam, select = 2, shade = T, cex.lab = 1.5, cex.axis = 1.5, las = 1, shade.col = "lightgrey", rug = F, xlab = "SAM",
               trans = plogis, all.terms = T, ylab = "", cex.main = 1.5,yaxt="n")
axis(side = 2, at = c(0,0.2,0.4,0.6,0.8,1), labels = FALSE, tck = -0.04)


gamtabs(M2$gam, caption = " ", label = "tab.gam", pnames = NA,snames = NA, ptab = NA, stab = NA)

###Timeseries plots
par(mfrow=c(2,1))
plot(dtG3G4$Date,dtG3G4$PHours,'l')
plot(dtG3G4$Date,ls$PHours,'l')

par(mfrow=c(2,1))
plot(dtG3G4$Date,dtG3G4$SIC,'l')
plot(dtG3G4$Date,ls$SICs,'l')

par(mfrow=c(2,1))
plot(dtG3G4$Date,dtG3G4$ONI,'l')
plot(dtG3G4$Date,ls$ONIs,'l')

par(mfrow=c(2,1))
plot(dtG3G4$Date,dtG3G4$SAM,'l')
plot(dtG3G4$Date,ls$SAMs,'l')

par(mfrow=c(2,2))
plot(dt$SICds,dt$PA)
plot(dt$ONIds,dt$PA)
plot(dt$SAMds,dt$PA)

par(mfrow=c(2,2))
plot(dt$SICds,dt$PAds)
plot(dt$ONIds,dt$PAds)
plot(dt$SAMds,dt$PAds)

###Timeseries correlation analysis
PAds = cbind(as.numeric(dt$Date[!is.na(dt$PA)]),dt$PAds[!is.na(dt$PA)])
SICds = cbind(as.numeric(dt$Date),dt$SICds)
bincor.tmp = bin_cor(PAds,SICds,FLAGTAU=3,'PAdsSICds')
binnedts <- bincor.tmp$Binned_time_series
plot_ts(SICds,PAds,binnedts[,c(1,3)],binnedts[,1:2], "SICds","PAds", colts1=1, colts2=2, colbints1=3, colbints2=4, device="screen")
cor_ts(binnedts[,c(1,3)], binnedts[,1:2],"SICds","PAds", KoCM="pearson", rmltrd="y", device="pdf", Hpdf=6, Wpdf=9, resfig=300, ofilename="scatterplot_PAds_SICds")
   
ONIds = cbind(as.numeric(dt$Date),dt$ONIds)
bincor.tmp = bin_cor(PAds,ONIds,FLAGTAU=3,'PAdsONIds')
binnedts <- bincor.tmp$Binned_time_series
plot_ts(PAds,ONIds,binnedts[,1:2], binnedts[,c(1,3)], "PAds", "ONIds", colts1=1, colts2=2, colbints1=3, colbints2=4, device="screen")
cor_ts(binnedts[,c(1,3)],binnedts[,1:2],"ONIds","PAds", KoCM="pearson", rmltrd="y", device="pdf", Hpdf=6, Wpdf=9, resfig=300, ofilename="scatterplot_PAds_ONIds")

##Create Model output for manuscript
load('/Volumes/public/eschall/GAMMs_Ahmed/GAM_Cat_ARMA.RData')
summ = summary(GAM_Cat_ARMA$gam)
print(summ)     
