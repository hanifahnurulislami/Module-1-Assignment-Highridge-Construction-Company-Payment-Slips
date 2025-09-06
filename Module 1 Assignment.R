# Highridge Construction Company Worker Generator in R

# Set up output folder
output_dir <- "highridge_output_R"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Define worker attributes
first_names <- c("James","Mary","Robert","Patricia","John","Jennifer",
                 "Michael","Linda","David","Elizabeth","William","Susan",
                 "Richard","Jessica","Charles","Sarah","Joseph","Karen",
                 "Thomas","Nancy")

last_names <- c("Smith","Johnson","Williams","Brown","Jones","Garcia",
                "Miller","Davis","Rodriguez","Martinez","Hernandez",
                "Lopez","Gonzalez","Wilson","Anderson","Thomas","Taylor",
                "Moore","Jackson","Martin","Lee","Perez","Thompson",
                "White","Harris","Sanchez","Clark","Ramirez","Lewis",
                "Robinson","Walker","Young","Allen","King","Wright",
                "Scott","Torres","Nguyen","Hill","Flores","Green",
                "Adams","Nelson","Baker","Hall","Rivera","Campbell",
                "Mitchell","Carter","Roberts")

positions <- c("Engineer","Technician","Manager","Supervisor","Clerk")

# Create workers dataframe
workers <- data.frame(
  ID = 1:1000,
  Name = paste(sample(first_names, 1000, replace = TRUE),
               sample(last_names, 1000, replace = TRUE)),
  Gender = sample(c("Male","Female"), 1000, replace = TRUE),
  Salary = sample(3000:35000, 1000, replace = TRUE),
  Position = sample(positions, 1000, replace = TRUE),
  stringsAsFactors = FALSE
)

# Function to assign level
assign_level <- function(salary, gender) {
  if (salary > 10000 & salary < 20000) {
    return("A1")
  } else if (salary > 7500 & salary < 30000 & gender == "Female") {
    return("A5-F")
  } else if (salary >= 20000) {
    return("B1")
  } else {
    return("B2")
  }
}

workers$Level <- mapply(assign_level, workers$Salary, workers$Gender)

# Save to CSV with error handling
tryCatch({
  write.csv(workers, file = file.path(output_dir, "workers_with_levels.csv"), row.names = FALSE)
  cat("✅ CSV file successfully saved!\n")
}, error = function(e) {
  cat("❌ Failed to save CSV:", e$message, "\n")
})

# Generate payment slips
for (i in 1:nrow(workers)) {
  slip_text <- paste(
    "Highridge Construction Company\n",
    "-------------------------------\n",
    paste("Employee ID:", workers[i, "ID"]), "\n",
    paste("Name:", workers[i, "Name"]), "\n",
    paste("Gender:", workers[i, "Gender"]), "\n",
    paste("Position:", workers[i, "Position"]), "\n",
    paste("Salary: $", workers[i, "Salary"]), "\n",
    paste("Level:", workers[i, "Level"]), "\n",
    "-------------------------------\n"
  )
  
  slip_file <- file.path(output_dir, paste0("slip_", workers[i, "ID"], ".txt"))
  
  tryCatch({
    writeLines(slip_text, slip_file)
  }, error = function(e) {
    cat("❌ Failed to write slip for ID", workers[i, "ID"], ":", e$message, "\n")
  })
}