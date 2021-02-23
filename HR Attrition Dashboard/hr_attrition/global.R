library(dashboardthemes)
library(DT)
library(inTrees)
library(glue)
library(plotly)
library(randomForest)
library(scales)
library(shiny)
library(shinydashboard)
library(tidyverse)

source("util.R")

employee <- read.csv("HR-Employee-Attrition.csv")

# Daftar kolom yang tidak dibutuhkan
drop_colnames <- c("Employee_Count", "Employee_Number", "Over_18", 
                   "Standard_Hours")

# Drop kolom yang tidak dibutuhkan
employee <- employee %>%
  select(-drop_colnames) 

# Ganti karakter & menjadi and
employee$Department <- str_replace_all(employee$Department, "&", "and")
# Membuat feature baru
employee$Role <- paste0("[", employee$Department, "] ", employee$Job_Role)
employee$Age_Category <- sapply(employee$Age, convert_age)
employee$Education <- sapply(employee$Education,
                             convert_education)
employee$Job_Involvement <- sapply(employee$Job_Involvement,
                                   convert_level)
employee$Job_Level <- sapply(employee$Job_Level,
                             convert_level)
employee$Stock_Option_Level <- sapply(employee$Stock_Option_Level,
                                      convert_stock)
employee$Years_At_Company_Category <- sapply(employee$Years_At_Company,
                                             convert_year)
employee$Years_In_Current_Role_Category <- sapply(employee$Years_In_Current_Role,
                                                  convert_year)
# Ubah nilai attrition menjadi numerik
employee$Attrition <- sapply(employee$Attrition, switch,
                             "Yes" = 1,
                             "No" = 0)
# Ganti column-type menjadi factor
factor_col <- c("Age_Category", "Business_Travel", "Department", "Education",
                "Education_Field", "Gender", "Job_Involvement", "Job_Level",
                "Job_Role", "Marital_Status", "Over_Time", "Role",
                "Stock_Option_Level", "Years_At_Company_Category",
                "Years_In_Current_Role_Category")
employee[, factor_col] <- lapply(employee[, factor_col], as.factor)

colnames(employee) <- str_replace_all(colnames(employee), "_", " ")

X <- subset(employee, select = -Attrition)
y <- as.factor(employee$Attrition)

model.rf <- randomForest(X, y, ntree = 50)

X.colnames <- sort(colnames(X))
checkbox.color <- rep(c("white"), times = length(X.colnames))
checkbox.names <- function() {
  res <- list()
  for (opt in X.colnames) {
    res[[length(res)+1]] <- tags$span(opt, style = "color: white;")
  }
  res
}
