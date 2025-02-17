library(dplyr)
library(ggplot2)
library(data.table)

full_data <- fread("~/merged_data.csv", showProgress = TRUE)

#remove IT proj and NA project id rows
filtered_data <- full_data[!grepl("^it-", project_id, ignore.case = TRUE) & project_id != "" & !is.na(project_id)]


# -----------------------Aggregate total cost per project------------------------
project_costs <- filtered_data[, .(total_project_cost = sum(total_cost)), by = project_id]

# Sort projects by total cost (descending)
setorder(project_costs, -total_project_cost)

# Compute cumulative sum and percentage
project_costs[, cum_cost := cumsum(total_project_cost)]
project_costs[, cum_pct := cum_cost / sum(total_project_cost) * 100]

# Add project index (to use for ranking)
project_costs[, project_rank := .I]










# ----# of proj accounted for % of cost----
# Define cost thresholds
cost_thresholds <- c(50, 60, 70, 80, 90, 100)

# Calculate the number of projects required to reach each threshold
project_counts <- sapply(cost_thresholds, function(thresh) {
  min(project_costs[cum_pct >= thresh, project_rank])  
  # Find the first project index that exceeds the threshold
})

# Create a summary table
summary_table <- data.table(
  `Cumulative Cost (%)` = cost_thresholds,
  `Number of Projects` = project_counts
)

# Print the summary table
print(summary_table) 

# ------------------ key takeaways------------
# some proj no $
# some proj total cost < 0 153 proj 
# why negative? 
# ----------------------------------------------------------

length(unique(filtered_data$project_id))

# 5 it projects
length(unique(full_data$project_id))


# Just 33 projects (≈ 10% of total 310) contribute 80% of total cost.










# ---viz1----

# Plot histogram (bar chart)
ggplot(project_costs, aes(x = project_rank, y = total_project_cost)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_vline(xintercept = 33, linetype = "dashed", color = "red")
  labs(title = "Total Cost Distribution Across Projects",
       x = "Number of Projects (Ordered by Cost)",
       y = "Total Cost") +
  theme_minimal()

  
# ---viz2----
# Plot cumulative cost distribution
ggplot(project_costs, aes(x = project_rank, y = cum_pct)) +
  geom_step(color = "blue", size = 1.2) +  # Use step plot for cumulative effect
  geom_hline(yintercept = 80, linetype = "dashed", color = "red") +  # Mark 80% threshold
  geom_vline(xintercept = 33, linetype = "dashed", color = "red")
  labs(title = "Cumulative Cost Contribution Across Projects",
       x = "Number of Projects (Ordered by Cost)",
       y = "Cumulative % of Total Cost") +
  theme_minimal()


  
  
  
  
  
  
  
  

  
# ----time spent for top 33 and full----
  
# Select the top 33 projects contributing to 80% of total cost
top_33_projects <- project_costs[1:33, project_id]
  
# Filter the dataset for only these top 33 projects
top_33_data <- filtered_data[project_id %in% top_33_projects]
  
# Convert usage_date to Date format if not already
top_33_data[, usage_date := as.Date(usage_date)]

# Calculate duration stats (min, max, and number of unique active days)
project_durations <- top_33_data[, .(
  start_date = min(usage_date),
  end_date = max(usage_date),
  active_days = uniqueN(usage_date)
), by = project_id]
  
# Compute summary statistics
summary_stats <- project_durations[, .(
  avg_duration = mean(active_days),
  min_duration = min(active_days),
  max_duration = max(active_days)
)]
  
# Print results
print(project_durations)
print(summary_stats)
  











# --------Calculate duration stats (min, max, and number of unique active days)-------
project_durations_full <- filtered_data[, .(
  start_date = min(usage_date),
  end_date = max(usage_date),
  active_days = uniqueN(usage_date)
), by = project_id]

# Compute summary statistics
summary_stats_full <- project_durations_full[, .(
  avg_duration = mean(active_days),
  min_duration = min(active_days),
  max_duration = max(active_days)
)]

# Print results
print(project_durations_full)
print(summary_stats_full)



# -----------good stuff----------------
# consistent on active days per projects (at least 50% of the porj runs for more than 50 days)
# top 33 do not differ much to the rest (more than 75% more than 126 days)
# time isnt a huge factor to the cost driver 






# ----sort by usage unit cost per project per day----
top_33_data[, usage_amount := as.numeric(usage_amount)]

# Aggregate cost and usage by project, date, and unit
top_33_data_unit <- top_33_data[, .(
  total_usage = sum(usage_amount, na.rm = TRUE),  # Sum total usage per unit
  total_cost = sum(total_cost, na.rm = TRUE)      # Sum total cost per unit
), by = .(project_id, usage_date, usage_unit)]
# only 4 or fewer row per project per day

# Pivot data to have separate columns for each usage unit
top_33_data_wide <- dcast(top_33_data_unit, project_id + usage_date ~ usage_unit, 
                          value.var = c("total_usage", "total_cost"), 
                          fun.aggregate = sum, fill = 0)

# we want to preserve as much usage amount informaiton (sku services) and we dont want to make the model too complex
# we turned usage amount information into their usage unit info 

# Rename columns: replace hyphens with underscores (prevent error later)
setnames(top_33_data_wide, 
         old = c("total_usage_byte-seconds", "total_cost_byte-seconds"), 
         new = c("total_usage_byte_seconds", "total_cost_byte_seconds"))




# # Compute total summed cost from all cost components
# top_33_data_wide[, total_cost := total_cost_byte_seconds + total_cost_requests + total_cost_bytes + total_cost_seconds]
# 
# # Compare with actual total cost
# top_33_data_wide[, cost_difference := abs(total_cost_check - total_cost)]
# 
# # Print rows where the sum does NOT match the total cost
# mismatches <- top_33_data_wide[cost_difference > 0.01]  # Small threshold to ignore floating-point precision errors
# print(mismatches)















# ----------------------------------------------------------------------------------NEW FEATURES----------------------------------------------------------------


# ----------------------------------------------------------------------------------add lag features ------------------------------------------------------------

# Ensure correct ordering
setorder(top_33_data_wide, project_id, usage_date)

# Add lagged features (2-day, 3-day, and 7-day lags)
lag_days <- c(2, 3, 7)

for (lag in lag_days) {
  top_33_data_wide[, paste0("cost_byte_seconds_lag_", lag) := shift(total_cost_byte_seconds, lag, type="lag"), by = project_id]
  top_33_data_wide[, paste0("cost_bytes_lag_", lag) := shift(total_cost_bytes, lag, type="lag"), by = project_id]
  top_33_data_wide[, paste0("cost_requests_lag_", lag) := shift(total_cost_requests, lag, type="lag"), by = project_id]
  top_33_data_wide[, paste0("cost_seconds_lag_", lag) := shift(total_cost_seconds, lag, type="lag"), by = project_id]
}




# ----------------------------------------------------------add moving averages ------------------------------------------------------------------------------------------------------------------
# Compute 7-day moving average **only up to lag_2 (latest available cost)
top_33_data_wide[, `:=` (
  ma7_cost_byte = frollmean(shift(total_cost_byte_seconds, 2, type="lag"), 7, align="right"),
  ma7_cost_bytes = frollmean(shift(total_cost_bytes, 2, type="lag"), 7, align="right"),
  ma7_cost_requests = frollmean(shift(total_cost_requests, 2, type="lag"), 7, align="right"),
  ma7_cost_seconds = frollmean(shift(total_cost_seconds, 2, type="lag"), 7, align="right")
), by = project_id]



# -------------------------------------------------------------------add growth rate-----------------------------------------------------------------------------------------------------------------
# Compute growth rate: (Current Cost - Cost N Days Ago) / Cost N Days Ago
# Compute growth rate using `lag_2` as the most recent known cost
top_33_data_wide[, `:=` (
  growth_cost_byte_3d = (shift(total_cost_byte_seconds, 2, type="lag") - shift(total_cost_byte_seconds, 5, type="lag")) / shift(total_cost_byte_seconds, 5, type="lag"),
  growth_cost_bytes_3d = (shift(total_cost_bytes, 2, type="lag") - shift(total_cost_bytes, 5, type="lag")) / shift(total_cost_bytes, 5, type="lag"),
  growth_cost_requests_3d = (shift(total_cost_requests, 2, type="lag") - shift(total_cost_requests, 5, type="lag")) / shift(total_cost_requests, 5, type="lag"),
  growth_cost_seconds_3d = (shift(total_cost_seconds, 2, type="lag") - shift(total_cost_seconds, 5, type="lag")) / shift(total_cost_seconds, 5, type="lag")
), by = project_id]

# Compute 7-day growth rate using `lag_2` instead of today's cost
top_33_data_wide[, `:=` (
  growth_cost_byte_7d = (shift(total_cost_byte_seconds, 2, type="lag") - shift(total_cost_byte_seconds, 9, type="lag")) / shift(total_cost_byte_seconds, 9, type="lag"),
  growth_cost_bytes_7d = (shift(total_cost_bytes, 2, type="lag") - shift(total_cost_bytes, 9, type="lag")) / shift(total_cost_bytes, 9, type="lag"),
  growth_cost_requests_7d = (shift(total_cost_requests, 2, type="lag") - shift(total_cost_requests, 9, type="lag")) / shift(total_cost_requests, 9, type="lag"),
  growth_cost_seconds_7d = (shift(total_cost_seconds, 2, type="lag") - shift(total_cost_seconds, 9, type="lag")) / shift(total_cost_seconds, 9, type="lag")
), by = project_id]



# ----------------------------------------------------------add day of week, month -----------------------------------------------------------------------------------------------------------------
library(lubridate) 

# Extract day of the week (1 = Monday, 7 = Sunday)
top_33_data_wide[, day_of_week := wday(usage_date, week_start = 1)]

# Extract month (to capture seasonal cost variations)
top_33_data_wide[, month := month(usage_date)]




# ------------------------------------------------------Cost Spike Flag-----------------------------------------------------------------------------------------------------------------
# compare the most recent known cost (lag_2) to the moving average up to lag_2.
# Compute cost spike flags for each cost type separately
top_33_data_wide[, `:=` (
  cost_spike_byte_flag = ifelse(cost_byte_seconds_lag_2 > 1.5 * ma7_cost_byte, 1, 0),
  cost_spike_bytes_flag = ifelse(cost_bytes_lag_2 > 1.5 * ma7_cost_bytes, 1, 0),
  cost_spike_requests_flag = ifelse(cost_requests_lag_2 > 1.5 * ma7_cost_requests, 1, 0),
  cost_spike_seconds_flag = ifelse(cost_seconds_lag_2 > 1.5 * ma7_cost_seconds, 1, 0)
)]


# cost spike total cost for that day > 1.5 sum(ma7 of all 4 unit )




# ------------------------------------------------------ save csv ------------------------------------------------------------------------------------------------------------
# Define the file path
output_file <- "~/top_33_data.csv"

# Save the cleaned data to CSV
fwrite(top_33_data_wide, output_file)


