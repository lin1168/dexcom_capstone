library(data.table)

# List all CSV files in the directory
csv_files <- list.files("/home/dexcom/cmu_exports/", pattern = "*.csv", full.names = TRUE)

# Read all files in parallel using lapply and fread (fastest method)
data_list <- lapply(csv_files, fread)

# Combine all datasets into one large data.table
full_data <- rbindlist(data_list, use.names = TRUE, fill = TRUE)

# Convert usage_date to Date type for efficiency
full_data[, usage_date := as.Date(usage_date)]

str(full_data)

fwrite(full_data, "/home/dexcom/merged_data.csv")


# # Count rows in each individual file
row_counts <- sapply(csv_files, function(file) nrow(fread(file, fill = TRUE, showProgress = FALSE)))
# 
# # Total expected rows
expected_rows <- sum(row_counts)
# 
# # Actual rows in merged dataset
# actual_rows <- nrow(full_data)
# 
# # Compare row counts
# print(paste("Expected rows:", expected_rows))
# print(paste("Actual rows:", actual_rows))
# 
# if (expected_rows == actual_rows) {
#   print("All data is present!")
# } else {
#   print(" Some data might be missing!")
# }
