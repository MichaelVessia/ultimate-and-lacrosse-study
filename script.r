# Data
years <- c(2013, 2014, 2015, 2016)

USAU.totals <- c(47138, 48914, 53362, 54849)
USAU.youth <- c(703, 781, 1407, 1705)
USAU.high <- c(11913, 12983, 14180, 14875)
USAU.college <- c(16755, 17036, 18173, 18415)
USAU.post <- c(17418, 17807, 18956, 17222)

USL.totals <- c(746859, 772772, 799874, 824577)
USL.youth <- c(403770, 424836, 444580, 454527)
USL.high <- c(290046, 297238, 305122, 315877)
USL.college <- c(36515, 38383, 38383, 42384)
USL.post <- c(16288, 12075, 11789, 11789)

year1 <- c(703, 11913, 16755, 17418)
year2 <- c(781, 12983, 17036, 17807)
year3 <- c(1407, 14180, 18173, 18956)
year4 <- c(1705, 14875, 18415, 17222)

USAU.2016.totals <- c(728, 1096, 3090, 3269, 8516, 18415, 2725, 6797, 2335, 2366, 1103, 1676, 1741, 815, 95, 4)
USAU.2016.categories <- c(12, 14, 16, 17, 19, 23, 24, 28, 30, 33, 35, 40, 50, 60, 70, 80)

# Total Participation
# Line Graphs
plot(years, USAU.totals, type="l", col="red", main="USAU Total Participation", xlab="Year", ylab="Total")
plot(years, USL.totals, type="l", col="blue", main="USL Total Participation", xlab="Year", ylab="Total")

# Growth
cat("USAU Total Growth: ", ((54849-47138)/47138)*100, "%\n")
cat("USL Total Growth: ", ((824577-746859)/746859)*100, "%\n\n")

# Youth Participation
# Line Graphs
plot(years, USAU.youth, type="l", col="red", main="USAU Youth Participation", xlab="Year", ylab="Total")
plot(years, USL.youth, type="l", col="blue", main="USL Youth Participation", xlab="Year", ylab="Total")

#Growth
cat("USAU Youth Growth: ", ((1705-703)/703)*100, "%\n")
cat("USL Youth Growth: ", ((454527-403770)/403770)*100, "%\n\n")

# High School Participation
# Line Graphs
plot(years, USAU.high, type="l", col="red", main="USAU High School Participation", xlab="Year", ylab="Total")
plot(years, USL.high, type="l", col="blue", main="USL High School Participation", xlab="Year", ylab="Total")

#Growth
cat("USAU High School Growth: ", ((14875-11913)/11913)*100, "%\n")
cat("USL High School Growth: ", ((315877-290046)/290046)*100, "%\n\n")

# College Participation
# Line Graphs
plot(years, USAU.college, type="l", col="red", main="USAU College Participation", xlab="Year", ylab="Total")
plot(years, USL.college, type="l", col="blue", main="USL College Participation", xlab="Year", ylab="Total")

# Growth
cat("USAU College Growth: ", ((18415-16755)/16755)*100, "%\n")
cat("USL College Growth: ", ((42384-36515)/36515)*100, "%\n\n")

# Post-Collegiate Participation
# Line Graphs
plot(years, USAU.post, type="l", col="red", main="USAU Post-Collegiate Participation", xlab="Year", ylab="Total")
plot(years, USL.post, type="l", col="blue", main="USL Post-Collegiate Participation", xlab="Year", ylab="Total")

# Growth
cat("USAU Post-Collegiate Growth: ", ((17222-17418)/17222)*100 , "%\n")
cat("USL Post-Collegiate Growth: ", ((11789-16288)/16288)*100 , "%")

# Linear Regression
# Original
totals.per.year <- cbind(USAU.2016.totals, USAU.2016.categories)
plot(x=USAU.2016.categories, y=USAU.2016.totals, col="red", main="USAU Participation by Age Group, 2016", xlab="Age Group", ylab="Total")
abline(lm(USAU.2016.totals~USAU.2016.categories), col="blue")
Cor <- cor(USAU.2016.totals,USAU.2016.categories)
print(Cor)

LinMod <- lm(USAU.2016.categories~USAU.2016.totals)
print(summary(LinMod))

LinMod.res <- resid(LinMod)
plot(y=LinMod.res, x=USAU.2016.categories, ylab="Residuals", xlab="Age Groups", main="USAU Participation Residuals by Age Group, 2016")
abline(0, 0, col="red")
abline(sd(LinMod.res), 0, col="blue") # 1 standard deviation above 0
abline(-sd(LinMod.res), 0, col="blue") # 1 standard deviation below 0
abline(2*sd(LinMod.res), 0, col="green") # 2 standard deviations above 0
abline(-2*sd(LinMod.res), 0, col="green") # 2 standard deviations below 0

#Removing outliers
USAU.2016.totals.fixed <- c(3090, 3269, 8516, 18415, 2725, 6797, 2335, 2366, 1103, 1676, 1741)
USAU.2016.categories.fixed <- c(16, 17, 19, 23, 24, 28, 30, 33, 35, 40, 50)

totals.per.year.fixed <- cbind(USAU.2016.totals.fixed, USAU.2016.categories.fixed)
plot(x=USAU.2016.categories.fixed, y=USAU.2016.totals.fixed, col="red", main="USAU Participation by Age Group, 2016", xlab="Age Group", ylab="Total")
abline(lm(USAU.2016.totals.fixed~USAU.2016.categories.fixed), col="blue")

Cor2 <- cor(USAU.2016.totals.fixed,USAU.2016.categories.fixed)
print(Cor2)

LinMod2 <- lm(USAU.2016.categories.fixed~USAU.2016.totals.fixed)
print(summary(LinMod2))

LinMod2.res <- resid(LinMod2)
plot(y=LinMod2.res, x=USAU.2016.categories.fixed, ylab="Residuals", xlab="Age Groups", main="USAU Participation Residuals by Age Group, 2016")
abline(0, 0, col="red")
abline(sd(LinMod.res), 0, col="blue") # 1 standard deviation above 0
abline(-sd(LinMod.res), 0, col="blue") # 1 standard deviation below 0
abline(2*sd(LinMod.res), 0, col="green") # 2 standard deviations above 0
abline(-2*sd(LinMod.res), 0, col="green") # 2 standard deviations below 0