library(dplyr)
library(plotly)

# Create function that reads in data and create a plot displaying the total number of students
# using each operating system
os_user_total_info <- function(data) {
  # Read in data
  data <- read.csv("https://raw.githubusercontent.com/INFO-498F/a7-survey-data/master/intro_survey_data.csv")
  # Change columnn names
  names(data) <- c("class_standing", "info_interest", "os", "Cmd Line Terminal", "
                   Git Version Control familiar", "R Markdown familiar", "R Lang familiar", 
                   "Program Experience", "Num Countries Visited", "Cat vs. Dog", "Seahawks fan?")
  # Summarise total number of students using each operating system
  os_num_data <- data %>% select(class_standing, os) %>% group_by(os) %>% 
    summarise(num_students = n())
  
  # Create bar graph comparing number of students using each operating system
  plot_ly(os_num_data,
          x = os,
          y = num_students,
          type = "bar") %>%
    layout(title = "Number of students using each OS",
           xaxis = list(title = "Operating System"),
           yaxis = list(title = "Number of Students"))
}

# Now that we know how many total students use what operating system, let's compare
# the number of students using each operating system by class standing!
os_user_by_class <- function(data) {
  # Read in data
  data <- read.csv("https://raw.githubusercontent.com/INFO-498F/a7-survey-data/master/intro_survey_data.csv")
  # Change columnn names
  names(data) <- c("class_standing", "info_interest", "os", "Cmd Line Terminal", "
                 Git Version Control familiar", "R Markdown familiar", "R Lang familiar", 
                 "Program Experience", "Num Countries Visited", "Cat vs. Dog", "Seahawks fan?")
  
  # Here are the number of students in each class using each operation system
  mac_by_class <- data %>% select(class_standing, os) %>% group_by(class_standing) %>%
    filter(os %in% "Mac") %>% summarise(num_student = n())
  windows_by_class <- data %>% select(class_standing, os) %>% group_by(class_standing) %>%
    filter(os %in% "Windows") %>% summarise(num_student = n())
  linux_by_class <- data %>% select(class_standing, os) %>% group_by(class_standing) %>%
    filter(os %in% "Linux") %>% summarise(num_student = n())
  others_by_class <- data %>% select(class_standing, os) %>% group_by(class_standing) %>%
    filter(!os %in% c("Mac", "Windows", "Linux")) %>% summarise(num_student = n())
  
  # Join all data frames together into one
  mac_and_windows <- left_join(mac_by_class, windows_by_class, linux_by_class,
                               by = 'class_standing')
  add_linux <- left_join(mac_and_windows, linux_by_class, by = 'class_standing')
  os_by_class <- left_join(add_linux, others_by_class, by = 'class_standing')
  os_by_class[is.na(os_by_class)] <- 0
  names(os_by_class) <- c("class_standing", "num_mac", "num_windows", "num_linux", "num_others")
   
  # Create grouped bar plot 
  plot_ly(os_by_class, 
          x = class_standing, 
          y = num_mac, 
          type = "bar", 
          marker = list(color = 'gray'),
          name = "# Mac users") %>% 
    add_trace(y = num_windows, marker = list(color = 'blue'), name = "# Windows users") %>% 
    add_trace(y = num_linux, marker = list(color = 'green'), name = "# Linux users") %>%
    add_trace(y = num_others, marker = list(color = 'orange'), name = "Others") %>%
    layout(title = "Number of students using each <br> Operating System by Class Standings",
           xaxis = list(title = "Class Standings"),
           yaxis = list(title = "Number of students"))
}