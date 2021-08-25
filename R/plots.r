plotCols <- c("#AEC441", "#008875", "#0094D6", "#F2E000", "#C362A6", "#F78F1E", "#2E62CC", "#FFC024")

# ------ RF importance score ------
load("C:/Users/jesso/OneDrive/Desktop/Baby/covidStateSird/Output/2021-08-19/randomForestDeathModel.Rdata")
lagDays <- 21

# par(mai = c(1.5,0,0,0))
# nVar <- length(rownames(randomForestDeathModel$importance))
# barplot(t(randomForestDeathModel$importance), col = plotCols[2], border = NA,
# ylab = "", yaxt = "n", xaxt = "n")
# 
# varnames <- c("Time", "Location", "Male", "Age 0-24", "Age 25-34", "Age 35-44", "Age 45-54", "Age 55-64",
#   "Age 65+", "Heart Disease", "Lung Cancer", "Diabetes", "COPD", "Urbanization", "Population", paste0("New Cases on t-",1:lagDays),
#   paste0("New Deaths on t-",1:lagDays))
# 
# 
# axis(1, at = 1.2 * (1:nVar) -.5 , labels = varnames, las = 3, tick = F,
#    cex.axis = 1, line = -1)
# 
# mtext("Normalized Variable Importance", side=2, line=-2, col="grey33")
outputPath <- 'C:/Users/jesso/OneDrive/Desktop/Baby/covidStateSird/Output/2021-08-19'
# --- log scale
pdf(file = paste0(outputPath, "/Plots/Death_Model_Log_Importance.pdf"), width = 12, height = 7)
par(mai = c(1.5,0,0,0))
nVar <- length(rownames(randomForestDeathModel$importance))

barplot(c(t(log(as.numeric(randomForestDeathModel$importance)))), border = NA,
ylab = "", yaxt = "n", xaxt = "n",
col = c(rep(paste0(plotCols[5],"40"),2), paste0(plotCols[3],"40"),
      rep(paste0(plotCols[2],"40"),6), rep(paste0(plotCols[1],"40"),4),
      rep(paste0(plotCols[4],"40"),2), rep(paste0(plotCols[7],"40"),1),
      rep(paste0(plotCols[6],"40"),lagDays), rep(paste0(plotCols[2],"40"),lagDays))
)

varnames <- c("Time", "Location", "Male", "Age 0-24", "Age 25-34", "Age 35-44", "Age 45-54", "Age 55-64",
 "Age 65+", "Heart Disease", "Lung Cancer", "Diabetes", "COPD", "Urbanization", "Population", "Vaccination", paste0("New Cases on t-",1:lagDays), paste0("New Deaths on t-",1:lagDays))
axis(1, at = 1.2 * (1:nVar) -.5 , labels = varnames, las = 3, tick = F,
cex.axis = 1, line = -.75, col.axis = "grey33")

mtext("Normalized Variable Importance (Log Scale)", side=2, line=-1.5, col="grey33")
dev.off()





# --- prediction error ---

# things needed:
lagDays <- 21
states <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DE", "FL", "GA",
"HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD",
"ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH",
"NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC",
"SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")

stateInterventions <- read.csv( "C:/Users/jesso/OneDrive/Desktop/Baby/covidStateSird/Data/StateInterventionDates.csv",  stringsAsFactors = FALSE, header = TRUE)

plotCols <- c("#AEC441", "#008875", "#0094D6", "#F2E000",
              "#C362A6", "#F78F1E", "#2E62CC", "#FFC024")


sirdPredError <- function(testDays, dataDir) {
  caseError <- deathError <- cumulCase <- predCumulCase <-
    cumulDeath <- predCumulDeath <- newCase <- newDeath <-
    predNewCase <- predNewDeath <- matrix(NA, nrow = 50, ncol = length(testDays))

  names(caseError) <- names(deathError) <- c(as.character(testDays))

  maseDenomCase <- maseDenomDeath <- rep(NA, 50)

  for(j in 1:length(states)) {
    stateAbbrev <- states[j]

    statePop <- stateInterventions$statePopulation[stateInterventions$stateAbbreviation ==   stateAbbrev]

    load(paste0(dataDir, stateAbbrev, ".Rdata"))
    
    stateDataCSV <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
    fips <- read.table('C:/Users/jesso/OneDrive/Desktop/Baby/covidStateSird/state.txt', header = TRUE, sep = "|")
    stateLong <- read.csv(stateDataCSV)
    stateLong <- merge(x = stateLong, y = fips, by.x = "fips", by.y = "STATE", all.x = TRUE)

    stateLong <- stateLong[which(stateLong$state == stateAbbrev),]
  
    n.day <- nrow(stateLong)
    daysAll <- NULL
    for(i in 1:n.day) {
      daysAll[i] <-(paste(substr(stateLong$date[i],1,4),
                          substr(stateLong$date[i],5,6),
                          substr(stateLong$date[i],7,8), sep = "-") )
    }
    daysAll <- as.Date(daysAll)
  
    stateLong <- stateLong[order(daysAll),]
    daysAll   <- daysAll[order(daysAll)]
  
    predDays <- which(stateFit$times %in% testDays)
    obsDays <- which(daysAll %in% testDays)
  
    newCase[j,]  <- c(NA,diff(stateLong$positive))[obsDays]
    newDeath[j, ]<- c(NA,diff(stateLong$death))[obsDays]
  
    predNewCase[j,] <- c(NA,diff(statePop - stateFit$S))[predDays]
    predNewDeath[j,]  <- c(NA, diff(stateFit$D))[predDays]
  
    cumulCase[j,] <- stateLong$positive[obsDays]
    predCumulCase[j,] <- (statePop - stateFit$S)[predDays]
  
    cumulDeath[j,] <- stateLong$death[obsDays]
    predCumulDeath[j,] <- stateFit$D[predDays]
  
    maseDenomCase[j] <- mean(abs(diff(newCase[j,])), na.rm = T)
    maseDenomDeath[j] <- mean(abs(diff(newDeath[j,])), na.rm = T)
    
    caseError[j,]   <- abs(newCase[j,] - predNewCase[j,]) / maseDenomCase[j]
    deathError[j,]  <- abs(newDeath[j,] - predNewDeath[j,]) / maseDenomDeath[j]
  }

  nTestDays <- length(testDays)

  meanCaseError <- rep(NA, nTestDays)
  meanDeathError <- rep(NA, nTestDays)
  for(k in 1:length(testDays)) {
    meanCaseError[k] <- mean(as.numeric(caseError[,k][caseError[,k] != Inf]), na.rm = T)
    meanDeathError[k] <- mean(as.numeric(deathError[,k][deathError[,k] != Inf]), na.rm = T)
  }

  deathErrNum <- caseErrNum <- matrix(NA, nrow = 50, ncol = nTestDays)
  for(k in 1:length(testDays)) {
    caseErrNum[,k] <- as.numeric(caseError[,k])
    deathErrNum[,k] <- as.numeric(deathError[,k])
  }
  caseErrNum[which(caseErrNum == Inf)] <- NA
  deathErrNum[which(deathErrNum == Inf)] <- NA
  
  return(list(caseError = caseError, deathError = deathError))
}


mase4_30 <- sirdPredError(as.Date("2020-05-01") + 0:20, "/Users/gregw/Dropbox/Projects/covidStateSird/Output/2021-01-06/4_30/Data/")

mase5_30 <- sirdPredError(as.Date("2020-06-01") + 0:20,
    "/Users/gregw/Dropbox/Projects/covidStateSird/Output/2021-01-06/5_31/Data/")

mase6_30 <- sirdPredError(as.Date("2020-07-01") + 0:20, "/Users/gregw/Dropbox/Projects/covidStateSird/Output/2021-01-06/6_30/Data/")

mase7_30 <- sirdPredError(as.Date("2020-08-01") + 0:20,
    "/Users/gregw/Dropbox/Projects/covidStateSird/Output/2021-01-06/7_31/Data/")

mase8_30 <- sirdPredError(as.Date("2020-09-01") + 0:20, "/Users/gregw/Dropbox/Projects/covidStateSird/Output/2021-01-06/8_31/Data/")

mase9_30 <- sirdPredError(as.Date("2020-10-01") + 0:20, "/Users/gregw/Dropbox/Projects/covidStateSird/Output/2021-01-06/9_30/Data/")

mase10_30 <- sirdPredError(as.Date("2020-11-01") + 0:20, "/Users/gregw/Dropbox/Projects/covidStateSird/Output/2021-01-06/10_31/Data/")

mase11_30 <- sirdPredError(as.Date("2020-12-01") + 0:20, "/Users/gregw/Dropbox/Projects/covidStateSird/Output/2021-01-06/11_30/Data/")

medianCaseError <- loCaseError <- upCaseError <-
  medianDeathError <- loDeathError <- upDeathError <-
  matrix(NA, nrow = 4, ncol = lagDays)

for(j in 1:lagDays) {
  medianCaseError[1,j] <- median(mase9_30$caseError[,j], na.rm = T)
  loCaseError[1,j]   <- quantile(mase9_30$caseError[,j], probs = .25, na.rm = T)
  upCaseError[1,j]   <- quantile(mase9_30$caseError[,j], probs = .75, na.rm = T)
  medianCaseError[2,j] <- median(mase10_30$caseError[,j], na.rm = T)
  loCaseError[2,j]   <- quantile(mase10_30$caseError[,j], probs = .25, na.rm = T)
  upCaseError[2,j]   <- quantile(mase10_30$caseError[,j], probs = .75, na.rm = T)
  medianCaseError[3,j] <- median(mase10_30$caseError[,j], na.rm = T)
  loCaseError[3,j]   <- quantile(mase10_30$caseError[,j], probs = .25, na.rm = T)
  upCaseError[3,j]   <- quantile(mase10_30$caseError[,j], probs = .75, na.rm = T)
  medianCaseError[4,j] <- median(mase11_30$caseError[,j], na.rm = T)
  loCaseError[4,j]   <- quantile(mase11_30$caseError[,j], probs = .25, na.rm = T)
  upCaseError[4,j]   <- quantile(mase11_30$caseError[,j], probs = .75, na.rm = T)
  
  medianDeathError[1,j] <- median(mase9_30$deathError[,j], na.rm = T)
  loDeathError[1,j]   <- quantile(mase9_30$deathError[,j], probs = .25, na.rm = T)
  upDeathError[1,j]   <- quantile(mase9_30$deathError[,j], probs = .75, na.rm = T)
  medianDeathError[2,j] <- median(mase10_30$deathError[,j], na.rm = T)
  loDeathError[2,j]   <- quantile(mase10_30$deathError[,j], probs = .25, na.rm = T)
  upDeathError[2,j]   <- quantile(mase10_30$deathError[,j], probs = .75, na.rm = T)
  medianDeathError[3,j] <- median(mase10_30$deathError[,j], na.rm = T)
  loDeathError[3,j]   <- quantile(mase10_30$deathError[,j], probs = .25, na.rm = T)
  upDeathError[3,j]   <- quantile(mase10_30$deathError[,j], probs = .75, na.rm = T)
  medianDeathError[4,j] <- median(mase11_30$deathError[,j], na.rm = T)
  loDeathError[4,j]   <- quantile(mase11_30$deathError[,j], probs = .25, na.rm = T)
  upDeathError[4,j]   <- quantile(mase11_30$deathError[,j], probs = .75, na.rm = T)
}

outDir <- "/Users/gregw/Dropbox/Projects/covidStateSird/Output/2021-01-06/"

pdf(file = paste0(outDir, "CaseMASE.pdf"), width = 9, height = 6)
par(mai = c(.8,.8,0,.2), mgp = c(3,.75,0))
plot(NA, xlim = c(1, lagDays), yaxs = "i", ylim = c(0, 2.6),
 #ylim = range(c(loCaseError, upCaseError), na.rm = T),
     xlab = "", ylab = "",
     yaxt = "n", xaxt = "n", bty = "n")
for(k in c(1,2,4)) {
  polygon(c(1:lagDays, lagDays:1), c(loCaseError[k,], rev(upCaseError[k,])), col = paste0(plotCols[k+1], "33"), border = F)
  points(1:lagDays, medianCaseError[k,], col = plotCols[k+1], type = "o", pch = 16)
}
abline(h = 1, lty = 2, col = "grey67")
axis(2, at = c(-9999999, 99999999), col = "grey33")
axis(1, at = c(-9999999, 99999999), col = "grey33")
axis(2, at = seq(0,5,.5), col = "grey33", las = 2, col.axis = "grey33",
labels = seq(0,5,.5), cex = .9, tick = F, hadj = .75)
axis(1, at = seq(7,lagDays,7), col.axis = "grey33", col = "grey33", tick = F, padj = -1.5)
 mtext("Mean Absolute Scaled Error", side=2, line=2.5, col="grey33", cex=1)
  mtext("Days from End of Training Data", side=1, line=1.5, col="grey33", cex=1)
dev.off()

pdf(file = paste0(outDir, "DeathMASE.pdf"), width = 9, height = 6)
par(mai = c(.8,.8,0,.2), mgp = c(3,.75,0))
plot(NA, xlim = c(1, lagDays), yaxs = "i",  ylim = c(0,2.6),
#ylim = range(c(loDeathError, upDeathError),na.rm = T),
     xlab = "", ylab = "",
     yaxt = "n", xaxt = "n", bty = "n")
for(k in c(1,2,4)) {
  polygon(c(1:lagDays, lagDays:1), c(loDeathError[k,], rev(upDeathError[k,])),
          col = paste0(plotCols[k+1], "33"), border = F)
  points(1:lagDays, medianDeathError[k,], col = plotCols[k+1], type = "o", pch = 16)
}
abline(h = 1, lty = 2, col = "grey67")
axis(2, at = c(-9999999, 99999999), col = "grey33")
axis(1, at = c(-9999999, 99999999), col = "grey33")
axis(2, at = seq(0,5,.5), col = "grey33", las = 2, col.axis = "grey33",
labels = seq(0,5,.5), cex = .9, tick = F, hadj = .75)
axis(1, at = seq(7,lagDays,7), col.axis = "grey33", col = "grey33", tick = F, padj = -1.5)
 mtext("Mean Absolute Scaled Error", side=2, line=2.5, col="grey33", cex=1)
  mtext("Days from End of Training Data", side=1, line=1.5, col="grey33", cex=1)
dev.off()





rbPal <- colorRampPalette(c("#5954A4", plotCols[c(3,2,1,4,6,5)]))
stCols <- rbPal(14)

# ---
pdf(file = paste0("/Users/gregw/Dropbox/COVID-19/Reports/2020-04-15 States/CaseMASE.pdf"), width = 9, height = 6)
par(mai = c(.8,.8,0,.2), mgp = c(3,.75,0))

boxplot(sqrt(caseError), outline = F, col = paste0(stCols, "40"), #paste0(c("#5954A4", plotCols[c(3,2,1,4,6,5)],"#5954A4", plotCols[c(3,2,1,4,6,5)]), c("40","80")), #paste0(c("#5954A4", plotCols[c(3,2,1,4,6,5)]), "40"),
        ylab = "", xaxt = "n", yaxt = "n", ylim = c(0, 1.2), border = "grey33")
axis(2, at = seq(0,5,.2), col = "grey33", las = 2, col.axis = "grey33",
 labels = seq(0,5,.2), cex = .9, tick = F, hadj = .75)

 axis(1, at = 1:length(testDays), labels = gsub(" 0", " ", format(testDays, "%b %d")), col.axis = "grey33", col = "grey33", tick = F, padj = -2, cex.axis= .75)

abline(h = 1, col = "grey82", lwd = 2)
 mtext("Mean Absolute Scaled Error", side=2, line=2.5, col="grey33", cex=1)
dev.off()
# ---
pdf(file = paste0("/Users/gregw/Dropbox/COVID-19/Reports/2020-04-15 States/DeathMASE.pdf"), width = 9, height = 6)
par(mai = c(.8,.8,0,.2), mgp = c(3,.75,0))

boxplot(sqrt(deathError), outline = F, col = paste0(stCols, "40"),  #paste0(c("#5954A4", plotCols[c(3,2,1,4,6,5)]), "40"),
        ylab = "", xaxt = "n", yaxt = "n", ylim = c(0, 1.2), border = "grey33")
axis(2, at = seq(0,5,.2), col = "grey33", las = 2, col.axis = "grey33",
 labels = seq(0,5,.2), cex = .9, tick = F, hadj = .75)

 axis(1, at = 1:length(testDays), labels = gsub(" 0", " ", format(testDays, "%b %d")), col.axis = "grey33", col = "grey33", tick = F, padj = -2, cex.axis= .75)
abline(h = 1, col = "grey82", lwd = 2)
 mtext("Mean Absolute Scaled Error", side=2, line=2.5, col="grey33", cex=1)
dev.off()
# ---


# --- Fig 1

ny <- which(velocLogCases$loc == 34)
co <- which(velocLogCases$loc == 6)
wv <- which(velocLogCases$loc == 49)

options(scipen=999)
# raw cases

pdf(file = paste0(outDir, "Cases.pdf"), width = 6, height = 6)
par(mai = c(.8,.8,0,.2))
plot((velocLogCases$u[ny]), col = plotCols[3], type = "l",
     ylab = "", xlab = "", xaxt = "n", yaxt = "n", bty = "n",
     lwd = 2, ylim = c(0, (max(velocLogCases$u[ny]))))
points((velocLogCases$u[co]), col = plotCols[2], type = "l", lwd = 2)
points((velocLogCases$u[wv]), col = plotCols[1], type = "l", lwd = 2)

axis(2, at = c(-9999999, 99999999), col = "grey33")
axis(1, at = c(-9999999, 99999999), col = "grey33")

axis(1, at = seq(0,300,50), col = "grey33", col.axis = "grey33", tick = F,
     padj = -1)
axis(2, at = seq(0,1000000,100000), labels = c("0", paste0(seq(100,1000,100), "k")), col = "grey33", col.axis = "grey33", tick = F, las = 2, hadj = .8)

 mtext("Cumulative Confirmed Cases", side=2, line=3, col="grey33", cex=1)
 mtext("Days since 100+ Confirmed Cases", side=1, line=2.5, col="grey33", cex=1)
dev.off()

# log cases

pdf(file = paste0(outDir, "LogCases.pdf"), width = 6, height = 6)
par(mai = c(.8,.8,0,.2))
plot(log(velocLogCases$u[ny]), col = plotCols[3], type = "l",
     ylab = "", xlab = "", xaxt = "n", yaxt = "n", bty = "n",
     lwd = 2, ylim = c(0, log(max(velocLogCases$u[ny]))))
points(log(velocLogCases$u[co]), col = plotCols[2], type = "l", lwd = 2)
points(log(velocLogCases$u[wv]), col = plotCols[1], type = "l", lwd = 2)

axis(2, at = c(-9999999, 99999999), col = "grey33")
axis(1, at = c(-9999999, 99999999), col = "grey33")

axis(1, at = seq(0,300,50), col = "grey33", col.axis = "grey33", tick = F,
     padj = -1)
axis(2, at = log(c(1,10,100,1000,10000, 100000, 1000000)), labels = c("1","10","100","1,000","10,000", "100,000", "1,000,000"), col = "grey33", col.axis = "grey33", tick = F, las = 2, hadj = .8)

 mtext("Log Cumulative Confirmed Cases", side=2, line=3, col="grey33", cex=1)
 mtext("Days since 100+ Confirmed Cases", side=1, line=2.5, col="grey33", cex=1)
dev.off()

 # d/dt log cases
 pdf(file = paste0(outDir, "VelocLogCases.pdf"), width = 6, height = 6)
 par(mai = c(.8,.8,0,.2))
 plot(lowess(velocLogCases$y[ny], f = .15), col = plotCols[3], type = "l",
      ylab = "", xlab = "", xaxt = "n", yaxt = "n", bty = "n",
      ylim = c(0, 0.25), lwd = 2)
 points(lowess(velocLogCases$y[co], f = .15), col = plotCols[2], type = "l", lwd = 2)
 points(lowess(velocLogCases$y[wv], f = .15), col = plotCols[1], type = "l", lwd = 2)

 axis(2, at = c(-9999999, 99999999), col = "grey33")
 axis(1, at = c(-9999999, 99999999), col = "grey33")

 axis(1, at = seq(0,300,50), col = "grey33", col.axis = "grey33", tick = F,
      padj = -1)
 axis(2, at = seq(0,.5,.05), col = "grey33", col.axis = "grey33", tick = F, las = 2, hadj = .5)

  mtext("Smoothed Velocity of Log Cumulative Confirmed Cases", side=2, line=2.5, col="grey33", cex=1)
  mtext("Days since 100+ Confirmed Cases", side=1, line=2.5, col="grey33", cex=1)
 dev.off()
