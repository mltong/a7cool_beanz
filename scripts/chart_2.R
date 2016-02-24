library(jsonlite)
library(plotly)
library(dplyr)

# Chart Two
# The total amount of students who identified their programming level experience in INFO498f
chart_2 <- function(data) {

summarized_data <- select(data) %>% 
  group_by(What.is.your.programming.experience.) %>% 
  summarise(
    num_students = n()
  )

plot_ly(summarized_data,
        x = What.is.your.programming.experience., 
        y = num_students,
        name = "Programming Experience",
        type = "bar",
        color = What.is.your.programming.experience.
) %>% layout(
  barmode = "group",
  bargap = 0.1,
  title = "INFO498f - Program Experience Level",
  xaxis = list(title = "Program Experience Level"),
  yaxis = list(title = "Number of Students")
)
}
