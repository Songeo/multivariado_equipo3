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

df.cpj.17 %>% data.frame() %>% head
df.cpj.17 %>% dim
str(df.cpj.17)



# Binarias de categoricas multiples
aux <- df.cpj.17 %>% 
  dplyr::select(Medium, Job, Coverage) %>% 
  rownames_to_column() %>% 
  gather(var.lab, var.val, -rowname) %>% 
  separate( var.val , c("1", "2", "3", "4", "5", "6", "7", "8"), sep = ",") %>% 
  gather(col.num, col.val, c(-rowname, -var.lab)) %>% 
  na.omit() %>% 
  mutate(ind = 1,
         col.val = str_trim(col.val)) %>% 
  dplyr::select(-col.num) %>% 
  unite_("var_col", c('var.lab', 'col.val'), sep = "_") %>% 
  spread(var_col, ind, fill = 0)
aux
dim(aux)
apply(is.na(aux), 2, sum)

aux %>% data.frame() %>% head

table(df.cpj.17$motive)
df.cpj.17 <- df.cpj.17 %>% 
  rownames_to_column() %>% 
  left_join(aux) 
df.cpj.17 %>% dim


cache("df.cpj.17")
