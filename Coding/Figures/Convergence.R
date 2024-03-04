require(tidyverse)
require(lubridate)


#LOAD THE DATA IN
load("C:/Users/giles/Creative Cloud Files/MBA/Dissertation/Data/RollingBetaSensitivty.Rdata")

#FIX A CODING BUG IN THE CREATOR
collectRegressionData = collectRegressionData %>%
                        distinct();


#TARGET YEARS
targetYears = 10;

#INITIAL PROCESSING
DATA_PROC1 = collectRegressionData %>%
             filter(term == "OilAbnormalReturnAnnualized") %>%
             select(-statistic,-p.value,-std.error,-Model,-R2) %>%
             mutate(start = ymd(paste(start,"01",sep="")))  %>%
             mutate(end   = ymd(paste(end,"01",sep=""))) %>%
             mutate(StartYear = year(start)) %>%
             mutate(EndYear = year(end)) %>%
             mutate(YearsBetween = EndYear - StartYear) %>%
             arrange(Symbol,Regression,start,end) %>%
             distinct();

#WHAT ARE PERCEIVED AS CONVERGED VALUES
DATA_TARGET_1 = DATA_PROC1 %>%
                select(Symbol,Regression,end,YearsBetween,estimate) %>%
                filter(YearsBetween == targetYears) %>%
                distinct() %>%
                rename(Target = estimate) %>%
                select(-YearsBetween);

#COMBINE THEM AND CAL DELTAS
DATA_TARGET_2 = DATA_PROC1 %>%
                left_join(DATA_TARGET_1,by=c("Symbol","Regression","end")) %>%
                filter(!is.na(Target)) %>%
                mutate(Diff = estimate - Target ) %>%
                mutate(DiffPerc = 100*(estimate - Target)/Target);

                    