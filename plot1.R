NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# prepare data
NEI$year <- as.factor(NEI$year)
NEI$Pollutant <- as.factor(NEI$Pollutant)
NEI$type <- as.factor(NEI$type)

# calculate mean emission per year
totalEmissionsByYear = tapply(NEI$Emissions, NEI$year, sum)

# construct graph
years <- names(totalEmissionsByYear)

plot(years, totalEmissionsByYear, 
     xlab="Year", ylab="Total PM2.5 (millions)", 
     main="Total U.S. PM2.5 Emissions by Year", 
     pch=5, 
     col="blue")

abline(lm(totalEmissionsByYear ~ as.integer(years)), col="red", lwd=1.5)

legend(x="topright", 
       legend=c("Emissions","Regression Line"), 
       pch=c(5,NA), 
       lwd=c(NA,1.5), 
       col=c("blue", "red"), 
       cex=0.8)

dev.copy(png, "plot1.png")
dev.off()