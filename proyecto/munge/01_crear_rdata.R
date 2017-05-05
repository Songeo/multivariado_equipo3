library(forcats)

# Example preprocessing script.
df.cpj <- read.csv("data/cpj.csv") %>% 
  as_tibble() %>% 
  mutate_if(is.factor, tolower)
df.cpj %>% head
df.cpj %>% dim

df.cpj.17 <- readxl::read_excel("data/cpj-database_mod.xls") %>% 
  as_tibble() %>% 
  mutate_if(is.factor, tolower) %>% 
  mutate_all(str_trim) %>% 
  separate(col = Date, into = c("day-month", "year"), sep = ",") %>% 
  mutate(year = parse_number(year)) %>% 
  mutate(country_killed = fct_recode(`Country Killed`, 
                                     israel = "israel and the occupied palestinian territory"),
         year = ifelse(year < 92, year + 2000, year))
df.cpj.17

df.cpj.17 %>% head
df.cpj.17 %>% dim
str(df.cpj.17)


cache("df.cpj.17")
