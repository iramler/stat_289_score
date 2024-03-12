# Cleaning process of Kaggle's UFC Data

# This prep file assumes you have already downloaded 
# the data from https://www.kaggle.com/datasets/rajeevw/ufcdata

library(tidyverse)
library(lubridate)
fight_data <- read.csv("raw_fighter_details.csv") # change to your local path

ufc_data <- fight_data |>
  mutate(Str_Acc = parse_number(Str_Acc),
         Weight = parse_number(Weight),
         Reach = parse_number(Reach),
         Str_Def = parse_number(Str_Def),
         TD_Acc = parse_number(TD_Acc),
         TD_Def = parse_number(TD_Def),
         birthyear = year(mdy(DOB)), 
         feet = as.numeric(str_extract(Height, "\\d+(?=')")), 
         inches = as.numeric(str_extract(Height, "\\d+(?=\")")),
         height_inches = (feet * 12) + inches 
         ) |>
  filter(Str_Acc != 0) |> # dump errors
  filter(Reach != "") |> # dump errors
  select(-feet, -inches, -DOB, -Height) # remove unnecessary columns

# save file
write.csv(clean_data, "UFC_data.csv")