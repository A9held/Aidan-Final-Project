SnowData = read.csv("/cloud/project/HMSBurlingtonVt.csv")
EData = read.csv("/cloud/project/WorldCO2Emissions.csv")

install.packages("ggplot2")
install.packages("dplyr")
install.packages("PerformanceAnalytics")
install.packages("patchwork")
library(ggplot2)
library(dplyr)
library(PerformanceAnalytics)
library(patchwork)

# Convert year from being hyphenated to just first of two years. NOTE: Season is now identified by the first of the two years listed for 
# each season. For example the 1892-1893 winter season is now labeled 1892 (the first of the two years in the original data).
SnowData$Year = as.numeric(sub("-.*","", SnowData$Year))

# Plot annual seasonal totals
Splot = ggplot(data = SnowData, # data for plot. Entire plot names Splot for future patchwork with CO2 graphs 
       aes(x = Year, y= Season))+ # aes, x and y
        geom_point()+ # make points at data point
         geom_line()+ # use lines to connect data points
        geom_smooth(method = "lm", se = TRUE, color = "red")+ #Plot red trend line with confidence interval
        labs(title = "Annual Snow Totals In Burlington, Vt (1892-2023)",x="Year", y="Total Seasonal Snowfall (Inches)")+ #axis labels
  theme_classic()
Splot

# Create Regression for Snow data 
Snowreg = lm(Season ~ Year, data = SnowData)
summary(Snowreg)

# Standardize residuals and fitted values at each observation
res.full = rstandard(Snowreg)
fit.full = fitted.values(Snowreg)

#qq plot to check normality of residuals 
qqnorm(res.full, pch=19, col="grey50")
qqline(res.full)

#shapiro-wilks test
shapiro.test(res.full)

#Residuals plot
plot(fit.full, res.full, pch=19, col="grey50")
abline(h=0)


## Emissions plot 

# Change column name to more manageable name 
colnames(EData)[4] = "Diox"

# Log transform Carbon emissions to be more manageable  
EData$log.Diox = EData$Diox + 1
EData$log.Diox = log(EData$log.Diox)

# Plot Carbon Dioxide emissions over time with trend line
Dplot = ggplot(data = EData, # data for plot
       aes(x = Year, y= log.Diox))+ # aes, x and y
  geom_point()+ # make points at data point
  geom_line()+ # use lines to connect data points
  geom_smooth(method = "lm", se = TRUE, color = "red")+  #Plot red trend line with confidence interval
  labs(title = "CO2 Emissions Over Time (1892-2023)", x="Year", y="Emissions (In Billions of tons of CO2)")+ #labels
  theme_classic()
Dplot 

# Create regression for Carbon Dioxide emissions 
Dreg = lm(log.Diox ~ Year, data = EData)
summary(Dreg)

# Standardize residuals and fitted values at each observation
Dres.full = rstandard(Dreg)
Dfit.full = fitted.values(Dreg)

#qq plot to check normality of residuals 
qqnorm(Dres.full, pch=19, col="grey50")
qqline(Dres.full)

#shapiro-wilks test
shapiro.test(Dres.full)

#Residuals plot
plot(Dfit.full, Dres.full, pch=19, col="grey50")
abline(h=0)



# Combine the two data frames to analyze together
AllData = full_join(SnowData, EData)
AllData

# Multiple regression combining the two data sets 
Cmod.full = lm(Season ~ Year +
                log.Diox, data = AllData)
summary(Cmod.full)

#Checking assumptions 
Cres.full = rstandard(Cmod.full)
Cfits.full = fitted.values(Cmod.full)

# Check normality with QQ plot
qqnorm(Cres.full, pch= 19, col="grey50")
qqline(Cres.full)

#Shapiro-wilks test
shapiro.test(Cres.full)

#Plot residuals 
plot(fit.full,res.full, pch=19, col="grey50")
abline(h=0)

# Isolate continuous model variables into data frame:
reg.data = data.frame(AllData$Season, AllData$log.Diox)
chart.Correlation(reg.data, histogram=TRUE, pch = 19)


# Plot both Carbon Emissions and Snowfall totals together  
Dplot + Splot + plot_layout(ncol = 1)

