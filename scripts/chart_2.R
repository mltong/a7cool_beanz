library(jsonlite)
library(plotly)
library(dplyr)

# Chart Two
# The total amount of students who identified their programming level experience in INFO498f

chart_2 <- function(data) {


  # Group the students by their programming experience
<<<<<<< HEAD
  summarized_data <- select(data) %>% 
                     group_by(data$What.is.your.programming.experience.) %>% 
                     summarise(
                     num_students = n()

=======
  summarized_data <- data %>% 
    group_by(What.is.your.programming.experience.) %>% 
    summarise(
    num_students = n()
>>>>>>> 287cd373c54ae677a96ad8df1f6400dcc7a6b565
  )

plot_ly(summarized_data,
        x = What.is.your.programming.experience., 
        y = num_students,
        name = "Programming Experience",
        type = "bar",
        color = "Red"
) %>% layout(
  barmode = "group",
  bargap = 0.1,
  title = "INFO498f - Program Experience Level",
  xaxis = list(title = "Program Experience Level"),
  yaxis = list(title = "Number of Students")
)
}
