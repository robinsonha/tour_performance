# Wrapper function to run the tour analysis
data_paths <- c(
  "2023_UMBIZO.tsv",
  "2024_UMBIZO.tsv",
  "2025_UMBIZO.tsv"
)

run_tour_analysis <- function(data_paths) {

  library(data.table)
  library(dplyr)
  library(ggplot2)


  # Process each input file
 # processed_data <-# lapply(data_paths, function(path) {
   # data.processing.tour(path)
    combined_data<-data.processing.tour(data_paths)
 # })

  # Combine all processed data
 # combined_data <- do.call(rbind, processed_data)

  # Popular days analysis
  popular_days(combined_data, "Popular_Days_Analysis")

  # Tour comparison analysis
  tour_comparison_plot(combined_data, "Tour_Comparison")

  # Lead time analysis
  lead_time_plot(combined_data, "Lead_Time_Analysis")

  # Clashing tours analysis
  clashing_tours(combined_data, "Clashing_Tours_Analysis")

  # Return the combined data in case you want to use it further
  return(combined_data)
}

# Example usage:
# Define your input paths (replace with your actual file paths)

# Run the analysis
result <- run_tour_analysis(data_paths)
write.csv(result,"tour_results.csv")
