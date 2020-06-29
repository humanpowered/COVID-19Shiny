data(state_laea)
data(county_laea)
data(fips_codes)


cntypop <- fread('https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv')
cntypop <- cntypop[, c('STNAME', 'COUNTY', 'CTYNAME', 'POPESTIMATE2019')]


stpop <- cntypop[cntypop$COUNTY == 0,]
stpop$granularity <- 'State'
stpop$County <- 'N/A'
stpop$State <- stpop$STNAME

cntypop2 <- cntypop[cntypop$COUNTY != 0,]
cntypop2$granularity <- 'County'
cntypop2$County <- gsub(" County", "", cntypop2$CTYNAME, fixed = T)
cntypop2$State <- cntypop2$STNAME

uspop <- aggregate(POPESTIMATE2019 ~ granularity, stpop, sum)
uspop$granularity <- 'Nation'
uspop$State <- 'N/A'
uspop$County <- 'N/A'

pop <- bind_rows(uspop, stpop, cntypop2)
pop <- pop %>% rename(Population = POPESTIMATE2019)
pop$COUNTY <- NULL
pop$STNAME <- NULL
pop$CTYNAME <- NULL

pop <- pop %>% rename(`Nation/State/County` = granularity)
pop$County <- gsub('Anchorage Municipality', 'Anchorage', pop$County, fixed = T)
pop$County <- gsub(' Parish', '', pop$County, fixed = T)

saveRDS(pop, 'population_data.rds')
