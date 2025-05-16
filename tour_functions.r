## Functions for customer

library(data.table)
library(dplyr)
library(ggplot2)

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

data.processing.tour <- function(file_paths) {
  # Read the three specified files directly
  data.23.df <- data.frame(fread(file_paths[1]))
  data.24.df <- data.frame(fread(file_paths[2]))
  data.25.df <- data.frame(fread(file_paths[3]))

  # Combine into list (maintaining original structure)
  file.list <- list(data.23.df, data.24.df, data.25.df)

  # Get common column names across all files
  table.names <- table(unlist(lapply(file.list, colnames)))
  con.names <- names(table.names)[which(table.names == length(file.list))]

  # Combine the data frames keeping only common columns
  data.df <- do.call(rbind, lapply(file.list, function(x) {
    x[, con.names]
  }))

  # Remove unwanted columns (original functionality preserved)
  data.df <- data.df[, grep("Question", fixed = TRUE, invert = TRUE, colnames(data.df))]
  data.df <- data.df[, grep("Answer", fixed = TRUE, invert = TRUE, colnames(data.df))]
  data.df <- data.df[, grep("Name", fixed = TRUE, invert = TRUE, colnames(data.df))]
  data.df <- data.df[, grep("Email", fixed = TRUE, invert = TRUE, colnames(data.df))]
  data.df <- data.df[, grep("Phone", fixed = TRUE, invert = TRUE, colnames(data.df))]
  data.df <- data.df[, grep("Customer.Notes", fixed = TRUE, invert = TRUE, colnames(data.df))]

  # Identify and rename the tour name column
  act.check <- table(c(
    grep("Activity", colnames(data.df), fixed = TRUE),
    grep("Item", colnames(data.df), fixed = TRUE)
  ))

  colnames(data.df)[as.numeric(names(act.check)[which(act.check == 2)])] <- "Tour_Name"

  # Define tour information (unchanged from original)
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

  # Classify tours (original logic preserved)
  data.df[,"Tours"] <- "Other"
  data.df[which(is.na(match(data.df[,"Tour_Name"], tour.info[,"Tours"])) == FALSE), "Tours"] <- "Interest"
  data.df <- data.df[which(data.df[,"Booking.ID"] != "Total"),]
  data.df <- data.df[which(data.df[,"Payment.Status"] == "Paid"),]

  # Add tour type information
  type.vector <- tour.info[,"TourType"]
  names(type.vector) <- tour.info[,"Tours"]

  data.int.df <- data.df[which(data.df[,"Tours"] == "Interest"),]
  data.int.df[,"Type"] <- NA
  data.int.df[,"Type"] <- type.vector[data.int.df[,"Tour_Name"]]

  # Process dates and times (original processing preserved)
  data.int.df[,"DateOnly"] <- apply(as.matrix(data.int.df[,"Activity.Date"]), 1, function(x) {
    strsplit(x, split = " ", fixed = TRUE)[[1]][1]
  })
  data.int.df[,"Purchase_DateOnly"] <- apply(as.matrix(data.int.df[,"Purchase.Date"]), 1, function(x) {
    strsplit(x, split = " ", fixed = TRUE)[[1]][1]
  })
  data.int.df[,"Month"] <- apply(as.matrix(data.int.df[,"DateOnly"]), 1, function(x) {
    tmp.split <- strsplit(x, split = "/", fixed = TRUE)[[1]]
    paste.out <- tmp.split[1]
    return(paste.out)
  })
  data.int.df[,"Year"] <- apply(as.matrix(data.int.df[,"DateOnly"]), 1, function(x) {
    tmp.split <- strsplit(x, split = "/", fixed = TRUE)[[1]]
    paste.out <- tmp.split[3]
    return(paste.out)
  })

  # Filter and process additional fields
  data.int.df <- data.int.df[which(data.int.df[,"List.Price"] != "€0.00"),]
  data.int.df.2 <- data.int.df
  data.int.df.2[,"Number_Of_People"] <- as.numeric(apply(
    as.matrix(data.int.df.2[,"Ticket.List"]), 1, function(x) {
      strsplit(x, split="x ", fixed = TRUE)[[1]][1]
    }))

  data.int.df.2[,"Time"] <- NA
  data.int.df.2[grep("8:00am", data.int.df.2[,"Activity.Date"]), "Time"] <- 8
  data.int.df.2[grep("9:00am", data.int.df.2[,"Activity.Date"]), "Time"] <- 9

  data.int.df.2[,"DateOnly"] <- as.Date(data.int.df.2[,"DateOnly"], "%m/%d/%Y")
  data.int.df.2[,"Day"] <- weekdays(data.int.df.2[,"DateOnly"])
  data.int.df.2[,"LeadTime"] <- difftime(
    as.Date(data.int.df.2[,"DateOnly"], "%m/%d/%Y"),
    as.Date(data.int.df.2[,"Purchase_DateOnly"], "%m/%d/%Y"),
    units = "weeks"
  )

  return(data.int.df.2)
}

popular_days <- function(in.df, output.name) {
  # Load required packages
  require(ggplot2)
  require(dplyr)

  # Data processing
  day.count <- in.df %>%
    group_by(Day, Type, Tour_Name) %>%
    summarise(Number_Of_People = sum(Number_Of_People), .groups = "drop") %>%
    mutate(Day = factor(Day, levels = c("Monday", "Tuesday", "Wednesday",
                                        "Thursday", "Saturday", "Sunday")))

  # Create color palette for tours
  tour_colors <- scales::hue_pal()(length(unique(day.count$Tour_Name)))
  names(tour_colors) <- unique(day.count$Tour_Name)

  # Plot 1: Relaxed Tours (PNG)
  p_relaxed <- day.count %>%
    filter(Type == "Relaxed") %>%
    ggplot(aes(x = Day, y = Number_Of_People, color = Tour_Name, group = Tour_Name)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    scale_color_manual(values = tour_colors) +
    labs(title = "Relaxed Tours: Daily Ticket Sales",
         y = "Number of Tickets",
         x = "Day of Week",
         color = "Tour Name") +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 14)
    )

  ggsave(paste0(output.name, "_relaxed.png"),
         plot = p_relaxed,
         width = 10,
         height = 6,
         dpi = 300,
         units = "in")

  # Plot 2: Active Tours (PNG)
  p_active <- day.count %>%
    filter(Type == "Active") %>%
    ggplot(aes(x = Day, y = Number_Of_People, color = Tour_Name, group = Tour_Name)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    scale_color_manual(values = tour_colors) +
    labs(title = "Active Tours: Daily Ticket Sales",
         y = "Number of Tickets",
         x = "Day of Week",
         color = "Tour Name") +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 14)
    )

  ggsave(paste0(output.name, "_active.png"),
         plot = p_active,
         width = 10,
         height = 6,
         dpi = 300,
         units = "in")
}

## Compares the performance of each tour over time
tour_comparison_plot <- function(in.df, output.name) {
  require(ggplot2)
  require(data.table)
  require(RColorBrewer)

  # Create aggregated tables
  perf.table <- setorder(data.table(aggregate(Number_Of_People ~ Month + Year + Type + Tour_Name, in.df, sum)), Year, Month)
  sum.table <- setorder(data.table(aggregate(Number_Of_People ~ Month + Year + Type, perf.table, sum)), Year, Month)

  perf.relaxed.table <- setorder(data.table(aggregate(Number_Of_People ~ Month + Year + Tour_Name,
                                                      in.df[which(in.df[,"Type"] == "Relaxed"),], sum)), Year, Month)
  perf.active.table <- setorder(data.table(aggregate(Number_Of_People ~ Month + Year + Tour_Name,
                                                     in.df[which(in.df[,"Type"] == "Active"),], sum)), Year, Month)

  # Custom theme with white background and grid
  my_theme <- function() {
    theme_minimal(base_size = 12) +
      theme(
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_line(colour = "grey95"),
        strip.text = element_text(size = 14, face = "bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title = element_text(size = 12)
      )
  }

  # Distinct color palettes (using ColorBrewer qualitative palettes)
  type_colors <- c("Active" = "#E41A1C", "Relaxed" = "#377EB8")  # Set1 colors

  # For tours, we'll use more distinct colors
  relaxed_colors <- c("#1B9E77", "#66A61E", "#7570B3", "#D95F02", "#E7298A")  # Dark2 colors
  active_colors <- c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99")   # Paired colors

  # Plot 1: Summary comparison
  p1 <- ggplot(sum.table, aes(x = Month, y = Number_Of_People, group = Type)) +
    geom_line(aes(color = Type), size = 1.5) +
    geom_point(aes(color = Type), size = 3) +
    facet_wrap(vars(Year)) +
    ylab("Number Of Tickets") +
    ggtitle("Summary Tour Comparison") +
    my_theme() +
    scale_color_manual(values = type_colors) +
    guides(color = guide_legend(override.aes = list(size = 3)))

  # Plot 2: Relaxed tours comparison
  p2 <- ggplot(perf.relaxed.table, aes(x = Month, y = Number_Of_People, group = Tour_Name)) +
    geom_line(aes(color = Tour_Name), size = 1.5) +
    geom_point(aes(color = Tour_Name), size = 3) +
    facet_wrap(vars(Year)) +
    ylab("Number Of Tickets") +
    ggtitle("Relaxed Tour Comparison") +
    my_theme() +
    scale_color_manual(values = relaxed_colors) +
    guides(color = guide_legend(nrow = 2, override.aes = list(size = 3)))

  # Plot 3: Active tours comparison
  p3 <- ggplot(perf.active.table, aes(x = Month, y = Number_Of_People, group = Tour_Name)) +
    geom_line(aes(color = Tour_Name), size = 1.5) +
    geom_point(aes(color = Tour_Name), size = 3) +
    facet_wrap(vars(Year)) +
    ylab("Number Of Tickets") +
    ggtitle("Active Tour Comparison") +
    my_theme() +
    scale_color_manual(values = active_colors) +
    guides(color = guide_legend(nrow = 2, override.aes = list(size = 3)))

  # Save as PNG files
  ggsave(paste0(output.name, "_summary_comparison.png"), plot = p1,
         width = 12, height = 8, dpi = 300, units = "in", bg = "white")
  ggsave(paste0(output.name, "_relaxed_comparison.png"), plot = p2,
         width = 12, height = 8, dpi = 300, units = "in", bg = "white")
  ggsave(paste0(output.name, "_active_comparison.png"), plot = p3,
         width = 12, height = 8, dpi = 300, units = "in", bg = "white")
}
lead_time_plot <- function(in.df, output.name) {
  require(ggplot2)
  require(patchwork)
  require(stringr)

  # First remove anything in brackets (including the brackets)
  in.df$Tour_Clean <- str_remove_all(in.df$Tour_Name, "\\s*\\([^\\)]+\\)")

  # Then create labels from first letters of each remaining word
  in.df$Tour_Short <- sapply(strsplit(in.df$Tour_Clean, " "), function(x) {
    paste(substr(x, 1, 1), collapse = "")
  })

  # Set colors
  relaxed_colors <- colorRampPalette(c("#4E79A7", "#A0CBE8"))(length(unique(in.df$Tour_Name)))
  active_colors <- colorRampPalette(c("#E15759", "#F28E2B"))(length(unique(in.df$Tour_Name)))

  # Shared plot theme
  boxplot_theme <- function() {
    theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        plot.margin = margin(1, 1, 1, 1, "cm")
      )
  }

  # Plot 1: Relaxed Tours
  p_relaxed <- ggplot(in.df[in.df$Type == "Relaxed",],
                      aes(x = reorder(Tour_Short, LeadTime, median),
                          y = LeadTime,
                          fill = Tour_Name)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.8, width = 0.7) +
    scale_fill_manual(values = relaxed_colors) +
    labs(title = "Lead Time: Relaxed Tours",
         y = "Weeks",
         x = "") +
    boxplot_theme() +
    ylim(0, 100)

  # Plot 2: Active Tours
  p_active <- ggplot(in.df[in.df$Type == "Active",],
                     aes(x = reorder(Tour_Short, LeadTime, median),
                         y = LeadTime,
                         fill = Tour_Name)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.8, width = 0.7) +
    scale_fill_manual(values = active_colors) +
    labs(title = "Lead Time: Active Tours",
         y = "Weeks",
         x = "") +
    boxplot_theme() +
    ylim(0, 100)

  # Combine plots
  combined_plot <- p_relaxed / p_active +
    plot_layout(heights = c(1, 1))

  # Save output
  ggsave(paste0(output.name, "_Lead_Time.png"),
         plot = combined_plot,
         width = 9,
         height = 7,
         dpi = 300,
         units = "in")
}

## Calculates 'clashing tours'
## The question here is that if you have multiple tours on the same day, does one tour take customers away from another tour?
## The purpose here is that if a tour does outcompete another -> How many typically?
## Important to know incase there are many customers in which case, more transport might be needed
clashing_tours <- function(in.df, output.name) {
  require(ggplot2)
  require(data.table)
  require(patchwork)
  require(gridExtra)

  # Data processing (keeping your original calculations)
  tour.count <- data.frame(setorder(data.table(aggregate(Number_Of_People ~ DateOnly + Type, in.df, sum)), DateOnly))

  relaxed.count <- data.frame(setorder(data.table(aggregate(Number_Of_People ~ DateOnly + Tour_Name,
                                                            in.df[which(in.df[,"Type"] == "Relaxed"),], sum)), DateOnly))
  active.count <- data.frame(setorder(data.table(aggregate(Number_Of_People ~ DateOnly + Tour_Name,
                                                           in.df[which(in.df[,"Type"] == "Active"),], sum)), DateOnly))

  relaxed.compare <- t(entity.format.local(relaxed.count, "Tour_Name", "DateOnly", "Number_Of_People"))
  relaxed.compare[is.na(relaxed.compare)] <- 0
  active.compare <- t(entity.format.local(active.count, "Tour_Name", "DateOnly", "Number_Of_People"))
  active.compare[is.na(active.compare)] <- 0

  # Your ranking competition function
  rank.comp.abs <- function(in.compare){
    comp.df <- in.compare[which(apply(in.compare,1,sum) - apply(in.compare,1,max) > 0),]
    abs.df <- matrix(nrow = nrow(comp.df), ncol = ncol(comp.df), data = 0)
    rownames(abs.df) <- rownames(comp.df)
    colnames(abs.df) <- colnames(comp.df)

    for(i in 1:nrow(comp.df)){
      tmp.i <- comp.df[i,]
      tmp.i <- sort(tmp.i, decreasing = TRUE)
      abs.df[i,names(tmp.i)[1]] <- tmp.i[1] - tmp.i[2]
    }

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

  # Create plots with clean white background
  p1 <- ggplot(relaxed.comp, aes(x = Tour, y = Abs_Score)) +
    geom_bar(stat = "identity", fill = "#4E79A7") +
    ylab("Average Difference in Tickets") +
    xlab("Tour") +
    ggtitle("Comparison of Clashing Relaxed Tours") +
    theme_minimal() +
    theme(
      panel.background = element_blank(),
      plot.background = element_blank(),
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, face = "bold")
    )

  p2 <- ggplot(active.comp, aes(x = Tour, y = Abs_Score)) +
    geom_bar(stat = "identity", fill = "#E15759") +
    ylab("Average Difference in Tickets") +
    xlab("Tour") +
    ggtitle("Comparison of Clashing Active Tours") +
    theme_minimal() +
    theme(
      panel.background = element_blank(),
      plot.background = element_blank(),
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, face = "bold")
    )

  # Combine plots side-by-side
  combined_plot <- grid.arrange(p1, p2, ncol = 2)

  # Save as PNG
  ggsave(paste0(output.name, "_Clashing_Comparisons.png"),
         plot = combined_plot,
         width = 14,
         height = 6,
         dpi = 300,
         units = "in",
         bg = "white")
}







testing_1 <- function(){

input_files <- c(
  "2023_UMBIZO.tsv",
  "2024_UMBIZO.tsv",
  "2025_UMBIZO.tsv"
)
data.df <- data.processing.tour(input_files)

## Which days are popular
popular_days(data.df, "popular_day_function_test")

## Comparing tours
tour_comparison_plot(data.df, "testing")

## Comparing Lead times
lead_time_plot(data.df, "lead_test")

## Clashing days
clashing_tours(data.df, "clash_test")


}



