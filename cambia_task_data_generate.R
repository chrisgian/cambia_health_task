# This r script generates data used in a cambia health solutions application task.

# Libraries Used
require(dplyr)
require(data.table)
require(magrittr)
require(plyr)
require(lubridate)
require(xlsx)

# Generate Project Listing and Tasks
task_frame <- list(rep('Alpha', 5),
                   rep('Beta', 6),
                   rep('Charlie', 3)) %>% 
  lapply(., function(x) x %>% data.frame(project = ., task = 1:length(.))) %>% 
  ldply

# Generate List of Dates for reporting period as well as project start/end
reporting_dates <- 
  c('03/26/2017', '03/19/2017', '03/12/2017',
    '03/05/2017', '02/26/2017', '02/19/2017',
    '02/12/2017', '02/05/2017', '01/29/2017',
    '01/22/2017', '01/15/2017', '01/08/2017', '01/01/2017'
) %>% sort

start_dates <- 
  c('02/22/2017','01/17/2017','02/01/2017',
    '02/21/2017','02/16/2017','03/13/2017',
    '03/01/2017','01/25/2017','01/13/2017',
    '03/06/2017','03/04/2017','02/20/2017',
    '01/14/2017','01/01/2017')

end_dates <- 
  c('03/09/2017','02/05/2017','02/13/2017',
    '03/03/2017','02/19/2017','03/29/2017',
    '03/03/2017','01/30/2017','01/29/2017',
    '03/18/2017','03/08/2017','02/27/2017',
    '01/20/2017','01/14/2017')


duration <- c('16','19','13','10','3','17','2','5','16','13','5','7','6','14') %>% 
  as.numeric

# create "dates" table using vectors from above.
dates <- data.frame(start_dates, end_dates, duration)

# Combine "tasks" and dates into one table
task_frame %<>% cbind(., dates)

# Create every combination of reporting dates for each project
table_out <- expand.grid(reporting_dates, task_frame$project) %>% setnames(names(.), c('reporting','project'))

# join expanded table using tasks
table_out <- join(table_out, task_frame, by = 'project') %>% unique

# transform dates into "date" data type
table_out %<>% 
  mutate(reporting = reporting %>% gsub('/',',',.) %>% mdy, 
         start_dates = start_dates %>% gsub('/',',',.) %>% mdy,
         end_dates = end_dates %>% gsub('/',',',.) %>% mdy)

# create financial data placeholders
table_out %<>%
  mutate(
    reporting_data_available = ifelse( (start_dates <= reporting) &
                                         (end_dates >= reporting), 1, 0))
# Enter 0 for non-sensical records
table_out$total_budget <- 0

# Generate Finance Data
table_out %<>% 
  ddply(.(project, task),
        transform,
        total_budget = rnorm(n = 1, mean = 180333, 115511) %>% 
          abs %>%
          round)

# Create expected vs actual rate where actual rate is using skewed binomial dist
finance_data <- table_out %>% 
  filter(reporting_data_available == 1) %>%
  ddply(.(project,task), summarise, 
        expected_rate = 100/length(total_budget)/100,
        records_reported = length(total_budget))

finance_data <- do.call("rbind", replicate(18, finance_data, simplify = FALSE))

finance_data %<>% 
  arrange(project, task) %>% 
  dlply(., c("project",'task')) %>%
  lapply(., 
         function(x) cbind(x, record_count = 1:nrow(x))) %>% 
  ldply

finance_data %<>% 
  filter(record_count <= records_reported) %>% 
  select(-`.id`)

for (i in 1:nrow(finance_data)){
  finance_data[i,'actual_rate'] <-   finance_data[i,'expected_rate']*rbeta(1,9,2)/.85
}

# Create Final table
table_out <- rbind(
  cbind(
    table_out[table_out$reporting_data_available == 1,],
    finance_data[,c('actual_rate','expected_rate')]),
  table_out[table_out$reporting_data_available == 0,] %>% mutate(actual_rate = 0 , expected_rate = 0)) %>%
  mutate(
    expected_expenditure = (total_budget*expected_rate),
    actual_expenditure = (total_budget*actual_rate))

# QA 
table_out %>% 
  ddply(.(project, task), 
        summarise, 
        a = sum(expected_expenditure), 
        b = sum(actual_expenditure)) %>% 
  mutate(c = a-b)
