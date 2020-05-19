# Functions used here. 


# For selecting columns that are NOT all NA (good for cleaning up columns that are wasting space and have no info, just NA)
all_na <- function(x) any(!is.na(x))
# e.g. 
# df %>% select_if(all_na) 

prop_na <-function(x, prop) sum(!is.na(x))>0.3



# Extract the code (levels, key, labels and question) from a DTA converted into R
code <- function(x) {
  a <-get_label(mepro)[x]
  b <-get_labels(mepro)[x]
  c <-get_values(mepro)[x]
  print(c(a,b,c))
}


# Turn times (645, 1120) into times (6:45 (<time>))
turn_to_time <- function(x) {
  mins  <-  substr(x, nchar(x)-1, nchar(x))
  hour  <-  substr(x, 0, nchar(x)-2)
  time  <-  paste0(hour, ':', mins)
  hms::parse_hm(time)
}