convert_age <- function(x){
  if(x < 20){
    x <- "< 20 Years Old"
  }
  else if(x >= 20 & x < 30){
    x <- "20 - 29 Years Old"
  } 
  else if(x >= 30 & x < 40){
    x <- "30 - 39 Years Old"
  }
  else if(x >= 40 & x < 50){
    x <- "40 - 49 Years Old"
  } 
  else{
    x <- ">= 50 Years Old"
  }
}

convert_color <- function(x){
  x <- tags$span(x, style = "color: white;")
}

convert_education <- function(x){
  if(x == 1){
    x <- "Below College"
  }
  else if(x == 2){
    x <- "College"
  } 
  else if(x == 3){
    x <- "Bachelor"
  }
  else if(x == 4){
    x <- "Master"
  } 
  else{
    x <- "Doctor"
  }
}

convert_level <- function(x){
  if(x == 1){
    x <- "Low"
  }
  else if(x == 2){
    x <- "Medium"
  }
  else if(x == 3){
    x <- "High"
  }
  else{
    x <- "Very High"
  }
}

convert_stock <- function(x){
  if(x == 1){
    x <- "No stocks"
  }
  else if(x == 2){
    x <- "Less Stocks"
  }
  else if(x == 3){
    x <- "Moderate Stocks"
  }
  else{
    x <- "A lot of Stocks"
  }
}

convert_year <- function(x){
  if(x <= 1){
    x <- "<= 1 Years"
  }
  else if(x >= 2 & x <= 5){
    x <- "2 - 5 Years"
  } 
  else if(x >= 6 & x <= 10){
    x <- "6 - 10 Years"
  }
  else{
    x <- "> 10 Years"
  }
}

format_condition <- function(x){
  x <- str_replace_all(x, "%in%", "IN")
  x <- str_replace_all(x, "c\\(", "(" )
  x <- str_remove_all(x, "'")
}

