NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


## Plot 1

meanpm <- tapply(NEI$Emissions, NEI$year, mean)

plot(names(meanpm),meanpm,type="l", xlab = "Year", ylab = expression ("Total" ~ PM[2.5] ~"Emissions (tons)"), 
     main = expression("Total US" ~ PM[2.5] ~ "Emissions by Year"), col="Purple")

## Plot 2

baltimore <- NEI[NEI$fips == "24510",]
baltmean <- tapply(baltimore$Emissions, baltimore$year, mean)

plot(names(baltmean),baltmean,type="l", xlab = "Year", ylab = expression ("Total" ~ PM[2.5] ~"Emissions (tons)"), 
     main = expression("Total US" ~ PM[2.5] ~ "Emissions by Year"), col="Purple")

## PLot 3

library(ggplot2)

baltimore <- NEI[NEI$fips == "24510",]
balt_year_source <- aggregate(Emissions ~ year + type, baltimore, sum)

qplot(year, Emissions, data=balt_year_source, color=type, geom ="line") +
        ggtitle(expression("Baltimore City" ~ PM[2.5] ~ "Emmission by source, type and year")) + 
        xlab("Year") + ylab(expression("Total" ~ PM[2.5] ~ "Emissions (in tons)"))

## Plot 4 

data <- merge(NEI,SCC,by="SCC")
coal = grepl("coal", data$Short.Name, ignore.case=TRUE)
coaldata = data[coal,]

emission <- aggregate(Emissions ~ year, coaldata, sum)

g <- ggplot(emission, aes(factor(year), Emissions))
g <- g + geom_bar(stat="identity") +
        xlab("year") +
        ylab(expression('Total PM'[2.5]*" Emissions")) +
        ggtitle('Total Emissions from coal sources from 1999 to 2008')
print(g)

## Plot 5 

motor <- NEI[NEI$fips=="24510" & NEI$type=="ON-ROAD",  ]
emission_motor <- aggregate(Emissions ~ year, data, sum)

g <- ggplot(emission_motor, aes(factor(year), Emissions))
g <- g + geom_bar(stat="identity") +
        xlab("year") +
        ylab(expression('Total PM'[2.5]*" Emissions")) +
        ggtitle('Total Emissions from motor vehicle (type = ON-ROAD) in Baltimore City, Maryland (fips = "24510") from 1999 to 2008')
print(g)

## Plot 6

motor <- NEI[(NEI$fips=="24510" | NEI$fips == "06037") & NEI$type=="ON-ROAD", ]

agg <- aggregate(Emissions ~ year + fips, motor, sum)
agg$fips[agg$fips=="24510"] <- "Baltimore, MD"
agg$fips[agg$fips=="06037"] <- "Los Angeles, CA"

g <- ggplot(agg, aes(factor(year), Emissions))
g <- g + facet_grid(. ~ fips)
g <- g + geom_bar(stat="identity")  +
        xlab("year") +
        ylab(expression('Total PM'[2.5]*" Emissions")) +
        ggtitle('Total Emissions from motor vehicle (type=ON-ROAD) in Baltimore City, MD (fips = "24510") vs Los Angeles, CA (fips = "06037")  1999-2008')
print(g)




