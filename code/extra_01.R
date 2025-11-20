pacman::p_load(tidyverse,
               patchwork,
               janitor,
               stringdist,
               here)

url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_messy.csv"
df_messy <- read_csv(url)


# check data entries before analysis --------------------------------------

## check class of each column
## - sapply & class
sapply(df_messy,
       FUN = class)

## check unique elements in each column
## - sapply & unique
sapply(df_messy,
       FUN = unique)

## check possible type
## stringdistmatrix()
collector <- unique(df_messy$collector)

stringdistmatrix(collector)


# text cleaning in R ------------------------------------------------------

## remove white space
## str_squish()
a <- c( " a ", "a  b", "b ", "a", "b")
str_squish(a)

##align test case
## str_to_lower() & str_to_upper()
b <- c("A", "a", "bB", "BB")
str_to_lower(b)
str_to_upper(b)

## replace text
## str_replace() & str_replace_all()
v <- c("a b", "a.b", "a b.c")

str_replace(v, "\\s", "_") %>%
  str_replace("\\.", "_")

str_replace(v, "\\s|\\.", "_")

str_replace_all(v, "\\s|\\.", "_")

## remove text
## str_remove() & str_remove_all()
x <- c("abc", "dd", "abd")
str_remove_all(x, "ab")

## extract text
## str_extract()
z <- c("abc", "dd", "abd")
str_extract(z, "ab")

## detect text
## str_detect()
str_detect(z, "a")

ifelse(str_detect(z, "a"),
       yes = 1,
       no = 0)

y <- c("ab", "Ab")
str_detect(y, "^[Aa]")

# data object -------------------------------------------------------------
d <- c("2024/06/01", "June 4 2024")

lubridate::parse_date_time(d,
                           tz = "EST",
                           orders = c("Y/m/d",
                                      "B d Y"))

# clean data --------------------------------

df_messy <- df_messy %>%
  mutate(collector = str_to_lower(collector),
         species = str_to_lower(species) %>%
           str_squish() %>%
           str_replace_all("\\s|\\.", "_") %>%
           str_remove("_$"),
         length_mm = str_replace(length_mm,
                                ",", ".") %>%
           str_remove("\\smm") %>%
           as.numeric(),
         sample_date = lubridate::parse_date_time(sample_date,
                                                  tz = "EST",
                                                  orders = c("Y-m-d",
                                                             "Y/m/d",
                                                             "B d Y",
                                                             "Y.m.d",
                                                             "d B Y")) %>%
           as.Date(),
         recaptured = ifelse(str_detect(recaptured, "^[Yy]"),
                             yes = 1,
                             no = 0))


