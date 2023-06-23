pacman::p_load(pacman,readxl, base,
               tidyverse, dplyr, tidyr, stringr,
               listviewer, purrr, rlist,
               lubridate, zoo,
               treemapify, 
               rmarkdown)

setwd("~/") # i.e., setwd("/Users/kuba/")
data <- read_excel("~/Desktop/ASS.xlsx", 
                   sheet = "$zl€_K(t) --> R",
                   range = "A2:K42")  %>% print(n=50)

data %>%
  filter(IN_OUT == "Gain")  #Gain, Costs, Spese, Crypto
 # distinct(Source_Sink)

addElementToList <- function(list, source_sink, date, amount, note) {
  element <- list(
    Source = source_sink,
    Date = as.yearmon(date),
    Amount = amount,
    Note = note
  )
  list <- list %>% list.append(element)
  return(list)
}

Gain <-  list()
Costs <-  list()
Spese <-  list()
Inv_Crypto <- list()
Inv_Stock <- list()

Gain <- addElementToList(Gain,'IRR_Stipendio', '2022-10',1600, '-')
Gain <- addElementToList(Gain,'IRR_Stipendio','2022-12',2400,'-')
Gain <- addElementToList(Gain,'TuttoAmb_Stipendio','2023-02',290,'-')
Gain <- addElementToList(Gain,'TuttoAmb_Stipendio','2023-03',1866,'-')
Gain <- addElementToList(Gain,'TuttoAmb_Stipendio','2023-04',1578,'-')
Gain <- addElementToList(Gain,'TuttoAmb_Stipendio','2023-05',1577,'-')
Gain <- addElementToList(Gain,'TuttoAmb_Stipendio','2023-06',426,'-')
Gain <- addElementToList(Gain,'TuttoAmb_Rimborso','2023-03',246,'-')
Gain <- addElementToList(Gain,'TuttoAmb_Rimborso','2023-04',137,'-')
Gain <- addElementToList(Gain,'TuttoAmb_Rimborso','2023-05',374,'-')
Gain <- addElementToList(Gain,'TuttoAmb_Rimborso','2023-06',64,'-')
Gain <- addElementToList(Gain,'TuttoAmb_BuoniPasto','2023-02',28,'-')
Gain <- addElementToList(Gain,'TuttoAmb_BuoniPasto','2023-03',154,'-')
Gain <- addElementToList(Gain,'TuttoAmb_BuoniPasto','2023-04',112,'-')
Gain <- addElementToList(Gain,'TuttoAmb_BuoniPasto','2023-05',126,'-')
Gain <- addElementToList(Gain,'Poker','2023-01',40,'-')
Gain <- addElementToList(Gain,'Poker','2023-03',40,'-')
Gain <- addElementToList(Gain,'Poker','2023-05',40,'-')
Gain <- addElementToList(Gain,'Croiz','2023-05',10,'-')
Gain <- addElementToList(Gain,'Miscellaneous','2022-11',663,'-')
Gain <- addElementToList(Gain,'Miscellaneous','2022-12',150,'-')
Gain <- addElementToList(Gain,'Miscellaneous','2023-02',62,'-')
Gain <- addElementToList(Gain,'Miscellaneous','2023-03',123,'-')

str(Gain)
length(Gain)
lapply(Gain, function(x) x$Amount)
unique(sapply(Gain, function(x) x$Source))
Gain[[1]]
Gain[[1]]$Source

# Save list as .RData
saveRDS(Gain, file="~/Desktop/ASS.RData")
Gain <- readRDS("~/Desktop/ASS.RData")

# K ----

my_list <- list(a = 10, b = 15, c = 5)
barplot(unlist(my_list))
my_list <- list(a = c(1, 2, 3), b = c(4, 5, 6), c = c(7, 8, 9))
boxplot(my_list)
my_list <- list(x = c(1, 2, 3), y = c(4, 5, 6))
plot(my_list$x, my_list$y)
my_list <- list(matrix(1:9, nrow = 3))
heatmap(my_list[[1]])

# Create a list of structures
my_list <- list(
  structure1 = list(feature1 = 10, feature2 = "abc"),
  structure2 = list(feature1 = 20, feature2 = "def"),
  structure3 = list(feature1 = 30, feature2 = "ghi"))
# Select element
my_list[[2]]$feature2
# Add element
my_list[[7]] <- list(feature1 = 40, feature2 = "jkl")
new_element7 <- my_list[[7]]

# Example 2: Using FUN in the sapply() function
my_list <- list(a = 1:3, b = 4:6, c = 7:9)
element_sums <- sapply(my_list, FUN = sum)  # Apply the sum() function to each element of the list


?lapply()
MoneyFlow <- list(Gain = list(Source,Date,Amount,Note),
                  Costs = list(),
                  Spese = list(),
                  Crypto_inv = list())


barplot(unlist(Gain))
pie(unlist(MoneyFlow))

my_list <- list(A = c(1, 2, 3, 4, 5), B = c(2, 4, 6, 8, 10), C = c(3, 6, 9, 12, 15))
plot(1:length(unlist(my_list[[1]])), unlist(my_list[[1]]), type = "l", ylim = range(unlist(my_list)), xlab = "Index", ylab = "Value")
lines(1:length(unlist(my_list[[2]])), unlist(my_list[[2]]), type = "l", col = "red")
lines(1:length(unlist(my_list[[3]])), unlist(my_list[[3]]), type = "l", col = "blue")
legend("topright", legend = names(my_list), col = c("black", "red", "blue"), lty = 1)

                          
# Creazione della lista dell'asset nel tempo a fine mese in tre valute diverse


  last_updated = Sys.time()


calculate_asset_value <- function(asset) {
  asset$amount * asset$price_usd}

# Use map() to apply the function to each asset in the portfolio
asset_values_usd <- map(portfolio$assets, calculate_asset_value)
# Print the resulting list of asset values
print(asset_values_usd)

# Function to add income or expense data
add_transaction <- function(month, type, amount, description) {
  # Create a new transaction object
  transaction <- list(
    month = month,
    type = type,
    amount = amount,
    description = description
  )
  
  # Add the transaction to the financial data list
  if (is.null(financial_data[[month]])) {
    financial_data[[month]] <- list(transaction)
  } else {
    financial_data[[month]] <- c(financial_data[[month]], transaction)
  }
}

# Add some sample transactions
add_transaction("February", "Expense", 300, "Utilities")

# Step 1: Generate a sequence of dates
dates <- seq(as.Date("2022-01-01"), as.Date("2022-12-31"), by = "month")

# Step 3: Loop over each month and add income/output data
for (i in 1:length(dates)) {
  # Create a new object for the month
  month_data <- list()
  
  # Add income/output data as separate structures with text data
  month_data$income <- income_values_for_month_i
  month_data$output <- output_values_for_month_i
  month_data$text_data <- text_data_for_month_i
  
  # Add the month data to the portfolio list
  portfolio_list[[format(dates[i], "%Y-%m")]] <- month_data
}

# Step 4: Access and modify the data for a specific month
selected_month <- "2022-05"
selected_data <- portfolio_list[[selected_month]]

# Add/update income data for the selected month
selected_data$income <- new_income_data

# Add/update output data for the selected month
selected_data$output <- new_output_data

# Add/update text data for the selected month
selected_data$text_data <- new_text_data


IRR_Stipendio <- subset(data[1,], select = Ott:Giu)
seq(as.Date("2022-10-01"), as.Date("2023-06-30"), by = "month")


# Example list with numeric and date features
my_list <- list(values = c(10, 15, 5),
                dates = as.Date(c("2022-01-01", "2022-02-01", "2022-03-01")))
# Convert dates to a readable format
formatted_dates <- format(my_list$dates, "%b %Y")
# Create a data frame from the list
df <- data.frame(value = my_list$values, 
                 date = formatted_dates)
# Create the plot using ggplot2
ggplot(df, aes(x = date, y = value)) +
  geom_bar(stat = "identity", fill = "blue", width = 0.5) +
  labs(title = "Values Over Time", x = "Date", y = "Value") +
  theme_minimal()



# Example list of structures ----
my_list <- list(structure(list(date = as.Date("2022-01-01"), value = 10), class = "my_structure"),
                structure(list(date = as.Date("2022-02-01"), value = 15), class = "my_structure"),
                structure(list(date = as.Date("2022-03-01"), value = 5), class = "my_structure"))

# Extract date and value information from each structure
dates <- sapply(my_list, function(x) x$date)
values <- sapply(my_list, function(x) x$value)
# Create a data frame
df <- data.frame(date = dates, value = values)
# Create the plot using ggplot2
ggplot(df, aes(x = date, y = value)) +
  geom_point() +
  labs(title = "Values Over Time", x = "Date", y = "Value") +
  theme_minimal()

# OPPURE 

# Use map to extract date and value from each structure
extract_info <- function(x) {
  tibble::tibble(date = x$date, value = x$value)
}
# Apply the extract_info function to each structure in the list
df <- purrr::map_dfr(my_list, extract_info)
# Create the plot using ggplot2
ggplot(df, aes(x = date, y = value)) +
  geom_point() +
  labs(title = "Values Over Time", x = "Date", y = "Value") +
  theme_minimal()
# CSV ----

write.csv(DataFrameName, "Path to export the DataFrame\\File Name.csv", row.names=FALSE)

# Convert the list to a data frame
data.frame(Gain) %>% view()

# Save the data frame to a CSV file
write.csv(data.frame(Gain), "~/Desktop/listASS.csv", row.names = FALSE)

data2 <- read.csv("~/Desktop/listASS.csv")

lines <- readLines("list_data.csv")
myList <- as.list(data2) # NO!!!

