#The following function takes in a data_frame containing information about students survey before
#taking info 498f. It will return a data_frame with information about the original data_frame.
write_summary <- function(data) {
  num_freshman <- sum(as.vector(data$What.is.your.current.class.standing.) == "Freshman")
  num_sophomore <- sum(as.vector(data$What.is.your.current.class.standing.) == "Sophomore")
  num_junior <- sum(as.vector(data$What.is.your.current.class.standing.) == "Junior")
  num_interested <- sum(as.vector(data$Are.you.interested.in.applying.to.the.Informatics.major.) 
                    == "Yes")
  num_mac <- sum(as.vector(data$What.operating.system.do.you.typically.use.) == "Mac")
  num_windows <- sum(as.vector(data$What.operating.system.do.you.typically.use.) == "Windows")
  return(data_frame(num_freshman = num_freshman, num_sophomore = num_sophomore, 
                    num_junior = num_junior, num_interested = num_interested, 
                    num_mac = num_mac, num_windows = num_windows))
}