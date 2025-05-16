## Testing and preparing data for forecasting 

work.dir <- getwd()

dir.create("functions_testing_120525_forecast")
setwd("functions_testing_120525_forecast")

library(data.table)
library(dplyr)
library(ggplot2)
library(forecast) 

#set up functions#

## Index generator
## A function that creates a data frame containing the index information of
## different contents of a vector -> Useful for speed-ups
index.generator <- function(in.vector){
unique.entries <- unique(in.vector)
index.match <- match(unique.entries, in.vector)
index.matrix <- data.frame(start = index.match, end = c(index.match[2:length(index.match)] - 1, length(in.vector)))
rownames(index.matrix) <- unique.entries
return(index.matrix)
}

## Reformat a "long" data frame into a "wide" one -> This is useful for generating certain plots i.e. barplots
entity.format.local <- function(in.quant, in.entity.name, in.run.name, in.quant.name){
in.quant <- data.frame(setorderv(data.table(in.quant), in.run.name))

unique.ent <- unique(in.quant[, in.entity.name])
unique.columns <- unique(as.character(in.quant[, in.run.name]))

ent.matrix <- matrix(nrow = length(unique.ent), ncol = length(unique.columns), data = NA)
rownames(ent.matrix) <- unique.ent
colnames(ent.matrix) <- unique.columns

## Get index of start and end of each run
run.index <- index.generator(as.character(in.quant[, in.run.name]))

for(i in 1:nrow(run.index)){
tmp.matrix <- in.quant[run.index[i, "start"]:run.index[i, "end"], ]
ent.matrix[as.character(tmp.matrix[, in.entity.name]), rownames(run.index)[i]] <- tmp.matrix[, in.quant.name]
rm(tmp.matrix)
}
rm(i)

return(ent.matrix)
}

## Data processing

## This function reads in all files within a user-specified folder
## It will then process the data:
#
# - Reads all data files
# - Combines them together
# - Removes a few columns (deemed not useful for this analysis -> This can be retained if customer needs them) i.e. Questions, Answers, Names, Emails and phone numbers
# - Binds together the user-specified activities
# - Filters down the data to only those where the 'List.Price' is not empty (these were empty due to re-scheduling of tours so removed to avoid multi-counting)
# - Calculates various metrics e.g. 'Number of People' which equates to number of tickets

data.processing.tour <- function(path.location){

## Need to automate reading in of many spread sheets
## Assumes they are in csv or tsv format
#data.23.df <- data.frame(fread("../../data/2023_UMBIZO.tsv"))
#data.24.df <- data.frame(fread("../../data/2024_UMBIZO.tsv"))
#data.25.df <- data.frame(fread("../../data/2025_UMBIZO.tsv"))

avail.files <- list.files(path.location, recursive = TRUE, full.names = TRUE)

file.list <- apply(as.matrix(avail.files),1,function(x){
data.frame(fread(x))
})

table.names <- table(unlist(lapply(file.list, colnames)))
con.names <- names(table.names)[which(table.names == length(file.list))]


data.df <- do.call(rbind, lapply(file.list, function(x){
x[,con.names]
}))

data.df <- data.df[,grep("Question", fixed = TRUE, invert = TRUE, colnames(data.df))]
data.df <- data.df[,grep("Answer", fixed = TRUE, invert = TRUE, colnames(data.df))]
data.df <- data.df[,grep("Name", fixed = TRUE, invert = TRUE, colnames(data.df))]
data.df <- data.df[,grep("Email", fixed = TRUE, invert = TRUE, colnames(data.df))]
data.df <- data.df[,grep("Phone", fixed = TRUE, invert = TRUE, colnames(data.df))]
data.df <- data.df[,grep("Customer.Notes", fixed = TRUE, invert = TRUE, colnames(data.df))]

act.check <- table(c(
    grep("Activity", colnames(data.df), fixed = TRUE),
    grep("Item", colnames(data.df), fixed = TRUE)
))

colnames(data.df)[as.numeric(names(act.check)[which(act.check == 2)])] <- "Tour_Name"

tour.info <- data.frame(
Tours = c(
    "Ultimate Tour of Ireland (Jewel Tour)",
    "Wild West and North Tour (Jewel Tour)",
    "Emerald Explorer Tour (Jewel Tour)",
    "Magical Southern Explorer (Jewel Tour)",
    "The Full Irish",
    "Ireland to Island Tour (Jewel and Zest Tour)",
    "Great Atlantic Adventure Tour (Zest Tour)",
    "Spectacular South & West Tour (Zest Tour)"
),
TourType = c(rep("Relaxed", 4), rep("Active", 4))
)

data.df[,"Tours"] <- "Other"
data.df[which(is.na(match(data.df[,"Tour_Name"], tour.info[,"Tours"])) == FALSE), "Tours"] <- "Interest"
data.df <- data.df[which(data.df[,"Booking.ID"] != "Total"),]
data.df <- data.df[which(data.df[,"Payment.Status"] == "Paid"),]

type.vector <- tour.info[,"TourType"]
names(type.vector) <- tour.info[,"Tours"]

data.int.df <- data.df[which(data.df[,"Tours"] == "Interest"),]
data.int.df[,"Type"] <- NA
data.int.df[,"Type"] <- type.vector[data.int.df[,"Tour_Name"]]

## Activity.Date
data.int.df[,"DateOnly"] <- apply(as.matrix(data.int.df[,"Activity.Date"]),1,function(x){strsplit(x,split = " ", fixed = TRUE)[[1]][[1]]})
data.int.df[,"Purchase_DateOnly"] <- apply(as.matrix(data.int.df[,"Purchase.Date"]),1,function(x){strsplit(x,split = " ", fixed = TRUE)[[1]][[1]]})
data.int.df[,"Month"] <- apply(as.matrix(data.int.df[,"DateOnly"]),1,function(x){
    tmp.split <- strsplit(x,split = "/", fixed = TRUE)[[1]]
    paste.out <- tmp.split[1]
    return(paste.out)
    })
data.int.df[,"Year"] <- apply(as.matrix(data.int.df[,"DateOnly"]),1,function(x){
    tmp.split <- strsplit(x,split = "/", fixed = TRUE)[[1]]
    paste.out <- tmp.split[3]
    return(paste.out)
    })

data.int.df <- data.int.df[which(data.int.df[,"List.Price"] != "€0.00"),]
data.int.df.2 <- data.int.df
data.int.df.2[,"Number_Of_People"] <- as.numeric(apply(as.matrix(data.int.df.2[,"Ticket.List"]),1,function(x){strsplit(x,split="x ", fixed = TRUE)[[1]][[1]]}))

data.int.df.2[,"Time"] <- NA
data.int.df.2[grep("8:00am", data.int.df.2[,"Activity.Date"]),"Time"] <- 8
data.int.df.2[grep("9:00am", data.int.df.2[,"Activity.Date"]),"Time"] <- 9

data.int.df.2[,"DateOnly"] <- as.Date(data.int.df.2[,"DateOnly"], "%m/%d/%Y")
data.int.df.2[,"Day"] <- weekdays(data.int.df.2[,"DateOnly"])
data.int.df.2[,"LeadTime"] <- difftime(as.Date(data.int.df.2[,"DateOnly"], "%m/%d/%Y"), as.Date(data.int.df.2[,"Purchase_DateOnly"], "%m/%d/%Y"), units = "weeks")

return(data.int.df.2)

}

## Calculates the popular days of each tour available
popular_days <- function(in.df, output.name){

day.count <- aggregate(Number_Of_People ~ Day + Type + Tour_Name, in.df , sum)
day.count[,"Day"] <- factor(day.count[,"Day"], levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Saturday", "Sunday"))

p1 <- ggplot(arrange(day.count[which(day.count[,"Type"] == "Relaxed"),],Day),aes(x = Day, y = Number_Of_People))  +
  geom_bar(stat="identity") + facet_wrap(vars(Tour_Name)) + ylab("Number Of Tickets")

p2 <- ggplot(arrange(day.count[which(day.count[,"Type"] == "Active"),],Day),aes(x = Day, y = Number_Of_People))  +
  geom_bar(stat="identity") + facet_wrap(vars(Tour_Name)) + ylab("Number Of Tickets")

pdf(paste(output.name, ".pdf", sep = ""), width = 12)
plot(p1)
plot(p2)
dev.off()

}

## Compares the performance of each tour over time
tour_comparison_plot <- function(in.df, output.name)
{
perf.table <- data.frame(setorder(data.table(aggregate(Number_Of_People ~ Month + Year + Type + Tour_Name, in.df, sum)), Year, Month))

## Aggregate and draw scatterlines
## Split according to year, then two coloured lines representing active and relaxed

library(ggplot2)
sum.table <- data.frame(setorder(data.table(aggregate(Number_Of_People ~ Month + Year + Type, perf.table, sum)), Year, Month))

perf.relaxed.table <- data.frame(setorder(data.table(aggregate(Number_Of_People ~ Month + Year + Tour_Name, in.df[which(in.df[,"Type"] == "Relaxed"),], sum)), Year, Month))
perf.active.table <- data.frame(setorder(data.table(aggregate(Number_Of_People ~ Month + Year + Tour_Name, in.df[which(in.df[,"Type"] == "Active"),], sum)), Year, Month))

sum.relaxed.table <- data.frame(setorder(data.table(aggregate(Number_Of_People ~ Month + Year + Tour_Name, perf.relaxed.table, sum)), Year, Month))
sum.active.table <- data.frame(setorder(data.table(aggregate(Number_Of_People ~ Month + Year + Tour_Name, perf.active.table, sum)), Year, Month))

colnames(sum.relaxed.table)[3] <- "Tour"
colnames(sum.active.table)[3] <- "Tour"

p1 <- ggplot(sum.table, aes(x=Month, y=Number_Of_People, group=Type)) +
  geom_line(aes(color=Type), size = 2)+
  geom_point() + facet_wrap(vars(Year)) + ylab("Number Of Tickets") +
  guides(color=guide_legend(title="Summary Tour Comparison"))

p2 <- ggplot(sum.relaxed.table, aes(x=Month, y=Number_Of_People, group=Tour)) +
  geom_line(aes(color = Tour), size = 2)+
  geom_point() + facet_wrap(vars(Year)) + ylab("Number Of Tickets") +
  guides(color=guide_legend(title="Relaxed Tour Comparison"))

p3 <- ggplot(sum.active.table, aes(x=Month, y=Number_Of_People, group=Tour)) +
  geom_line(aes(color = Tour), size = 2)+
  geom_point() + facet_wrap(vars(Year)) + ylab("Number Of Tickets") +
  guides(color=guide_legend(title="Active Tour Comparison"))


pdf(paste(output.name, "_Tour_Comparison_plots.pdf", sep = ""), width = 12)
plot(p1)
plot(p2)
plot(p3)
dev.off()
}


## Calculates the lead time -> Defined here as the time of activity - time of purchase
lead_time_plot <- function(in.df, output.name){
pdf(paste(output.name, "_Time_Lead_boxplots.pdf", sep = ""), width = 12)
boxplot(LeadTime ~ Type, in.df, outline = FALSE, xlab = "Tour Type", ylab = "Lead Time (Weeks)")
boxplot(LeadTime ~ Tour_Name, in.df[which(in.df[,"Type"] == "Relaxed"),], outline = FALSE, ylab = "Lead Time (Weeks)", xlab = "Tour", cex.axis = 0.7)
boxplot(LeadTime ~ Tour_Name, in.df[which(in.df[,"Type"] == "Active"),], outline = FALSE, ylab = "Lead Time (Weeks)", xlab = "Tour", cex.axis = 0.7)
dev.off()
}


## Calculates 'clashing tours'
## The question here is that if you have multiple tours on the same day, does one tour take customers away from another tour?
## The purpose here is that if a tour does outcompete another -> How many typically?
## Important to know incase there are many customers in which case, more transport might be needed
clashing_tours <- function(in.df, output.name){

tour.count <- data.frame(setorder(data.table(aggregate(Number_Of_People ~ DateOnly + Type, in.df, sum)), DateOnly))

tour.compare <- t(entity.format.local(tour.count, "Type", "DateOnly", "Number_Of_People"))
tour.compare[is.na(tour.compare)] <- 0

# apply(tour.compare,1,sum) - apply(tour.compare,1,max)

## Only 3 instances where there was a competeting tour type
# 2023-10-01 2023-10-08 2024-06-30

## Not really a problem for across tour types

relaxed.count <- data.frame(setorder(data.table(aggregate(Number_Of_People ~ DateOnly + Tour_Name, in.df[which(in.df[,"Type"] == "Relaxed"),], sum)), DateOnly))
active.count <- data.frame(setorder(data.table(aggregate(Number_Of_People ~ DateOnly + Tour_Name, in.df[which(in.df[,"Type"] == "Active"),], sum)), DateOnly))

relaxed.compare <- t(entity.format.local(relaxed.count, "Tour_Name", "DateOnly", "Number_Of_People"))
relaxed.compare[is.na(relaxed.compare)] <- 0
active.compare <- t(entity.format.local(active.count, "Tour_Name", "DateOnly", "Number_Of_People"))
active.compare[is.na(active.compare)] <- 0

## Ranking competition
## If there are competing cases, then which one typically wins out?

rank.comp.abs <- function(in.compare){
# in.compare <- relaxed.compare
comp.df <- in.compare[which(apply(in.compare,1,sum) - apply(in.compare,1,max) > 0),]

## Compare the highest versus the next highest and fill in that particular slot
abs.df <- matrix(nrow = nrow(comp.df), ncol = ncol(comp.df), data = 0)
rownames(abs.df) <- rownames(comp.df)
colnames(abs.df) <- colnames(comp.df)

for(i in 1:nrow(comp.df)){
tmp.i <- comp.df[i,]
tmp.i <- sort(tmp.i, decreasing = TRUE)
abs.df[i,names(tmp.i)[1]] <- tmp.i[1] - tmp.i[2]
rm(tmp.i)
}
rm(i)

abs.df[which(abs.df == 0)] <- NA

abs.vector <- colMeans(abs.df, na.rm = TRUE)
abs.vector[is.nan(abs.vector)] <- 0

rank.df <- data.frame(
Tour = gsub("(", '\n(', fixed = TRUE, names(abs.vector)),
Abs_Score = as.numeric(abs.vector)
)

return(rank.df)
}

relaxed.comp <- rank.comp.abs(relaxed.compare)
active.comp <- rank.comp.abs(active.compare)

p1 <- ggplot(relaxed.comp ,aes(x = Tour, y = Abs_Score, fill = Tour))  +
  geom_bar(stat="identity") + ylab("Average Difference in Tickets") + xlab("Tour") + ggtitle("Comparison of clashing relax tours")

p2 <- ggplot(active.comp ,aes(x = Tour, y = Abs_Score, fill = Tour))  +
  geom_bar(stat="identity") + ylab("Average Difference in Tickets") + xlab("Tour") + ggtitle("Comparison of clashing active tours")

pdf(paste(output.name, "_Clashing_Comparisons_Absolute.pdf"), width = 12)
plot(p1)
plot(p2)
dev.off()
}


testing_1 <- function(){
dir.create("testing_1")
setwd("testing_1")


data.df <- data.processing.tour("../../data/")

## Which days are popular
popular_days(data.df, "popular_day_function_test")

## Comparing tours
tour_comparison_plot(data.df, "testing")

## Comparing Lead times
lead_time_plot(data.df, "lead_test")

## Clashing days
clashing_tours(data.df, "clash_test")


}


analysis1 <- function(){
dir.create("analysis1")
setwd("analysis1")


tour.info <- data.frame(
Tours = c(
    "Ultimate Tour of Ireland (Jewel Tour)",
    "Wild West and North Tour (Jewel Tour)", 
    "Emerald Explorer Tour (Jewel Tour)",
    "Magical Southern Explorer (Jewel Tour)",      
    "The Full Irish",
    "Ireland to Island Tour (Jewel and Zest Tour)",
    "Great Atlantic Adventure Tour (Zest Tour)",
    "Spectacular South & West Tour (Zest Tour)"
),
TourType = c(rep("Relaxed", 4), rep("Active", 4))
)
data.df <- data.processing.tour.v2("../../data/", tour.info)

perf.table <- data.frame(setorder(data.table(aggregate(Number_Of_People ~ Month + Year + Type + Tour_Name, data.df[which(data.df[,"Type"] == "Relaxed"),], sum)), Year, Month))
sum.table <- data.frame(setorder(data.table(aggregate(Number_Of_People ~ Month + Year + Type, perf.table, sum)), Year, Month))

summary.df <- entity.format.local(sum.table, "Year", "Month", "Number_Of_People")
summary.df <- summary.df[which(rownames(summary.df) != "2025"),]

summary.df <- cbind(
data.frame(
Jan = rep(NA, 2),
Feb = rep(NA, 2)
),
summary.df, 
data.frame(
Nov = rep(NA, 2),
Dec = rep(NA, 2)
))

colnames(summary.df) <- c(
"Jan",
"Feb",
"Mar",
"Apr",
"May",
"Jun",
"Jul",
"Aug",
"Sep",
"Oct",
"Nov",
"Dec"
)


pre.ts <- as.numeric(as.character(as.matrix(t(summary.df))))
pre.ts[is.na(pre.ts)] <- 0
summary.ts <- ts(pre.ts, frequency = 12, start = c(2023, 1))

# Fit an ARIMA model automatically
arima_model <- auto.arima(summary.ts)

# Generate forecasts
arima.forecast <- forecast(arima_model, h = 12)

}

