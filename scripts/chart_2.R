library(plotly)
library(dplyr)

# Chart Two
# The total amount of students who identified their programming level experience in INFO498f

chart_2 <- function(data) {
  # Read in data 
  data <- read.csv("https://raw.githubusercontent.com/INFO-498F/a7-survey-data/master/intro_survey_data.csv")
  
  # Group the students by their programming experience
  program_experience_data <- data %>% select(What.is.your.programming.experience.)
  names(program_experience_data) <- c("program_experience")
  summarized_data <- program_experience_data %>% group_by(program_experience) %>% 
    summarise(num_students = n())
  
  plot_ly(summarized_data, x = program_experience,
          y = num_students,
          name = "Programming Experience",
          type = "bar"
          ) %>% 
    layout(color = "orange",
           title = "INFO498f - Program Experience Level", 
            xaxis = list(title = "Program Experience Level"),
            yaxis = list(title = "Number of Students"))
}
