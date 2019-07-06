# Function to generate ggplot colors (Original hcl: hues, 65 ,100)
ggcolors <- function(n){
  hues = seq(15, 375, length = n+2)
  hcl(h = hues, l = 65, c = 100)[2:(n+1)]
}

# Function to get a Date value
get_date <- function(year, month, day){
  ymd(paste(year, month, day, sep = "-"))
}
