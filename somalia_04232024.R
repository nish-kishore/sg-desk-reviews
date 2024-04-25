
# loading branch from sirtools with init_dr. NOTE: this branch is unstable and needs to be 
# changed once init_dr() and other functions in progress are in the main branch
library(devtools)
repo <- "nish-kishore/sirfunctions"
ref <- "59-dr-modularization-data-loading"
install_github(repo, ref)

# load required libraries
library(sirfunctions)
library(tidyverse)
library(readxl)
library(sf)
library(osmdata)
library(scales)
library(lubridate)
library(ggh4x)
library(ggpubr)
library(kableExtra)
library(flextable)
library(ggrepel)
library(ggforce)
library(knitr)
library(officer)
library(cluster)
library(cowplot)
library(stringr)
library(readxl)
library(zoo)
library(rvg)
devtools::install_github("yutannihilation/ggsflabel")
library(ggsflabel)
library(janitor)
library(gridExtra)
library(xlsx)
library(writexl)
library(vctrs)
library(here)
if (!webshot::is_phantomjs_installed()) {
  webshot::install_phantomjs()
}

# Allow console to print more files
options(dplyr.print_max = 1e9)
options(max.print = 1e9)


# PARAMS
country <- "SOMALIA"
start_date <- as_date("2022-01-01") # NOTE: changes in GUIDS from 2021-2022 so using 2022 as start
end_date <- as_date("2024-03-15")
current_date <- Sys.Date()
polis_date <- floor_date(Sys.Date(), "week", 1) # Date that data was pulled from POLIS (previous friday)
local_dr_path <- "C:/Users/XRG9/Desktop/local_dr"

# get country data
ctry.data <- read_rds(file.path(local_dr_path, str_to_lower(country), 
                                year(Sys.Date()), 
                                "data", "somalia_04232024.rds"))


# convert date columns from char to date-time and add necessary columns
ctry.data$afp.all.2 <-ctry.data$afp.all.2 |>
  mutate(across(contains("date"), as.Date)) |>
  mutate(daysstooltolab = case_when(
    is.na(datestool2) == FALSE ~ as.numeric(difftime(stooltolabdate, datestool2), units = "days"),
    is.na(datestool2) ~ as.numeric(difftime(stooltolabdate, datestool1), units = "days")
  )) |>
  mutate(noti.7d.on = if_else(date.notify - date.onset <= 7, T, F),
         inv.2d.noti = if_else(date.invest - date.notify <= 2, T, F),
         coll.3d.inv = if_else(datestool1 - date.invest <= 3, T, F),
         ship.3d.coll = if_else(stool.date.sent.to.lab - datestool2 <= 3, T, F)
  )

# Check: missing guids 
count(ctry.data$afp.all.2, dist)
count(ctry.data$afp.all.2, adm2guid)

# FIX
missing_data = filter(ctry.data$afp.all.2, is.na(dist))
missing_data$epid

# Pull out any geographic info from the epid ----
missing_data2 = ctry.data$afp.all.2 %>%
  filter(epid %in% missing_data$epid)%>%
  mutate(epid.check = substr(epid, 1, 11))
count(missing_data2, epid.check)

# Match epid.check with epid and year onset to pull out district and guid ----
missing_data2$adm2guid.2=NA
for(i in 1:nrow(missing_data2)){
  x = filter(ctry.data$afp.all.2, year== missing_data2$year[i] &
               grepl(missing_data2$epid.check[i],ctry.data$afp.all.2$epid)==T &
               is.na(adm2guid)==F)
  #print(dim(x))
  if(dim(x)[1]>0 & length(unique(x$adm2guid))==1){
    # print(length(unique(x$adm2guid)))
    # print(x$adm2guid)
    missing_data2$adm2guid.2[i]=unique(x$adm2guid)
  }
  if(dim(x)[1]>0 & length(unique(x$adm2guid))>1){
    print(length(unique(x$adm2guid)))
    print(unique(x$adm2guid))
  }
}

# back fill to adm2guid
missing_data2 = missing_data2 %>%
  mutate(adm2guid = ifelse(is.na(adm2guid), adm2guid.2, adm2guid))
count(missing_data2, adm2guid.2, adm2guid)

missing_data3 = filter(missing_data2, is.na(adm2guid))
for(i in 1:nrow(missing_data3)){
  x = filter(ctry.data$afp.all.2,
             grepl(missing_data3$epid.check[i],ctry.data$afp.all.2$epid)==T &
               is.na(adm2guid)==F)
  #print(dim(x))
  if(dim(x)[1]>0 & length(unique(x$adm2guid))==1){
    # print(length(unique(x$adm2guid)))
    # print(x$adm2guid)
    missing_data3$adm2guid.2[i]=unique(x$adm2guid)
  }
  if(dim(x)[1]>0 & length(unique(x$adm2guid))>1){
    print(length(unique(x$adm2guid)))
    print(unique(x$adm2guid))
    print(unique(x$prov))
    print(missing_data3$prov[i])
  }
}

# Backfill missing_data2 with new data ----
missing_data2$dist[match(missing_data3$epid, missing_data2$epid)] = missing_data3$dist
missing_data2$adm2guid[match(missing_data3$epid, missing_data2$epid)] = missing_data3$adm2guid.2

# For those that have multiple adm2guids ------
# Match province and take the adm2guid that goes with the province
missing_data3 = filter(missing_data2, is.na(adm2guid))
for(i in 1:nrow(missing_data3)){
  x = filter(ctry.data$afp.all.2,
             grepl(missing_data3$epid.check[i],ctry.data$afp.all.2$epid)==T &
               is.na(adm2guid)==F)
  y = filter(x, prov == missing_data3$prov[i])
  #print(dim(x))
  if(dim(x)[1]>0 & dim(y)[1]>0){
    print(dim(y))
    # print(length(unique(x$adm2guid)))
    # print(x$adm2guid)
    missing_data3$adm2guid.2[i]=unique(y$adm2guid)
  }
}


count(missing_data2, adm2guid.2, adm2guid)
count(missing_data3, adm2guid.2, adm2guid)
missing_data2$adm2guid[match(missing_data3$epid, missing_data2$epid)] = missing_data3$adm2guid.2

count(missing_data2, adm2guid, dist)
# add dist in to missing_data2 ----
missing_data2 = missing_data2 %>%
  mutate(dist = case_when(
    is.na(dist) & is.na(adm2guid) == F ~ 
      ctry.data$afp.all.2$dist[match(missing_data2$adm2guid,
                                     ctry.data$afp.all.2$adm2guid)],
    T~dist
  ))

count(missing_data2, adm2guid, dist)
missing_data2$epid
# Update dist and adm2guid -----
ctry.data$afp.all.2$dist[match(missing_data2$epid, ctry.data$afp.all.2$epid)] = missing_data2$dist
ctry.data$afp.all.2$adm2guid[match(missing_data2$epid, ctry.data$afp.all.2$epid)] = missing_data2$adm2guid

#!!! no NAs should be present anymore
count(ctry.data$afp.all.2,dist)
count(ctry.data$afp.all.2,adm2guid)

# missing 11 guids...will need to drop these
# before analysis can continue
ctry.data$afp.all.2 <- ctry.data$afp.all.2 |>
  filter(!is.na(dist))


# AFP Tables ----
## Full AFP line list in analysis period
afp.by.month <- ctry.data$afp.all.2 |>
  drop_na(date.onset) |>
  filter(between(date.onset, start_date, end_date)) |>
  mutate(mon.year = floor_date(date, "month"))

## AFP cases by month, year, and province (adm1guid)
### Adds case categories

afp.by.month.prov <- afp.by.month |>
  group_by(adm1guid, mon.year, year) |> #changed to guid
  summarise(cases = n()) |>
  ungroup() |>
  complete(mon.year = seq(floor_date(start_date, "month"), 
                          end_date, by = "month"), 
           adm1guid = unique(ctry.data$prov.pop$adm1guid),
           fill = list(cases = 0)) |>
  ungroup()|>
  mutate(year = year(mon.year))|>
  # join this to the original  province population dataset
  left_join(ctry.data$prov.pop[,c("adm1guid", "year", "prov", "u15pop")], 
            by = c("adm1guid" = "adm1guid", 
                   "year" = "year"))|>
  mutate(cases = ifelse(is.na(cases), 0, cases)) |>
  mutate(
    mon.year2 = as_date(as.yearmon(mon.year, "%b-%y")),
    case.cat = case_when(
      cases == 0 ~ "0",
      cases == 1 ~ "1",
      cases > 1 & cases < 6 ~ "2-5",
      cases >= 6 & cases < 10 ~ "6-9",
      cases >= 10 ~ "10+"
    ),
    year = year(mon.year2),
    mononset = month(mon.year2)
  ) |>
  mutate(
    case.cat = factor(case.cat,
                      levels = c("0", "1", "2-5", "6-9", "10+"),
                      labels = c("0", "1", "2-5", "6-9", "10+")
    )
  )

## AFP cases by month, year, and district (adm2guid)
### Adds case categories

afp.by.month.dist <- afp.by.month |>
  group_by(dist, adm2guid, mon.year) |> #changed to guid
  summarise(cases = n()) |>
  ungroup() |>
  complete(mon.year = seq(floor_date(start_date, "month"), 
                          end_date, by = "month"), 
           adm2guid = unique(ctry.data$dist.pop$adm2guid),
           fill = list(cases = 0)) |>
  ungroup()|>
  mutate(year = year(mon.year))|>
  left_join(ctry.data$dist.pop[,c("adm2guid", "year", "prov", "u15pop")], 
            by = c("adm2guid" = "adm2guid", 
                   "year" = "year"))|>
  mutate(cases = ifelse(is.na(cases), 0, cases)) |>
  mutate(
    mon.year2 = as_date(as.yearmon(mon.year, "%b-%y")),
    case.cat = case_when(
      cases == 0 ~ "0",
      cases == 1 ~ "1",
      cases > 1 & cases < 6 ~ "2-5",
      cases >= 6 & cases < 10 ~ "6-9",
      cases >= 10 ~ "10+"
    ),
    year = year(mon.year2),
    mononset = month(mon.year2)
  ) |>
  mutate(
    case.cat = factor(case.cat,
                      levels = c("0", "1", "2-5", "6-9", "10+"),
                      labels = c("0", "1", "2-5", "6-9", "10+")
    )
  )

## AFP cases by year
afp.case <- summarize(group_by(filter(afp.by.month, 
                                      as.Date(date.onset) >= start_date &
                                        as.Date(date.onset) <= end_date), 
                               year), afp.case = n())

# 7.3) NPAFP --------------------------------------------------------
# LABEL: DATA ANALYSIS
### District 

# !!! there are NAs after running this, from the ADM2 looks like KAMBIA
dis.extract <- f.npafp.rate.01(afp.data = ctry.data$afp.all.2, 
                               pop.data = ctry.data$dist.pop,
                               start.date = start_date, 
                               end.date = end_date,
                               spatial.scale = "dist", 
                               pending = T,
                               rolling = F)

d.p.cases <- summarize(group_by(ctry.data$afp.all.2 |>
                                  filter(between(date.onset, start_date, end_date)),
                                adm2guid, year), afp.case = n(),
                       num.wpv.cases = sum(wild.1 == TRUE, wild.3 == TRUE, na.rm = T),
                       num.vdpv1.cases	= sum(vdpv.1  == TRUE,na.rm = T),
                       num.vdpv2.cases	= sum(vdpv.2 == TRUE, na.rm = T),
                       num.vdpv3.cases = sum(vdpv.3  == TRUE, na.rm = T))

dis.case.ind <- full_join(dis.extract, d.p.cases, by = c("adm2guid" = "adm2guid",
                                                         "year" = "year")) |>
  select(-adm1guid, -adm2guid)

### Province 
# only 6 provinces extracted here...
prov.extract <- f.npafp.rate.01(afp.data = ctry.data$afp.all.2, 
                                              pop.data = ctry.data$prov.pop,
                                              start.date = start_date, 
                                              end.date = end_date,
                                              spatial.scale = "prov", 
                                              pending = T,
                                              rolling = F)

p.p.cases <- summarize(group_by(ctry.data$afp.all.2 |>
                                  filter(date >= start_date & date <= end_date),
                                adm1guid, year), afp.case = n(),
                       num.wpv.cases = sum(wild.1 == TRUE, wild.3 == TRUE, na.rm = T),
                       num.vdpv1.cases	= sum(vdpv.1  == TRUE,na.rm = T),
                       num.vdpv2.cases	= sum(vdpv.2 == TRUE, na.rm = T),
                       num.vdpv3.cases = sum(vdpv.3  == TRUE, na.rm = T))


prov.case.ind = full_join(prov.extract, p.p.cases, by = c("adm1guid" = "adm1guid",
                                                          "year" = "year")) |>
  select(-adm1guid)

### Country 
ctry.extract <- f.npafp.rate.01(afp.data = ctry.data$afp.all.2, 
                                              pop.data = ctry.data$ctry.pop,
                                              start.date = start_date, 
                                              end.date = end_date,
                                              spatial.scale = "ctry", 
                                              pending = T,
                                              rolling = F)

c.p.cases <- summarize(group_by(ctry.data$afp.all.2|>
                                  filter(date >= start_date & date <= end_date),
                                year, ctry), afp.case = n(),
                       num.wpv.cases = sum(wild.1 == TRUE, wild.3 == TRUE, na.rm = T),
                       num.vdpv1.cases	= sum(vdpv.1  == TRUE,na.rm = T),
                       num.vdpv2.cases	= sum(vdpv.2 == TRUE, na.rm = T),
                       num.vdpv3.cases = sum(vdpv.3  == TRUE, na.rm = T))

ctry.case.ind <- left_join(ctry.extract, c.p.cases, by = c("ctry" = "ctry",
                                                           "year" = "year"))

# 7.4) Stool adequacy --------------------------------------------------------
# LABEL: DATA ANALYSIS
## District
dstool <- f.stool.ad.01(afp.data = ctry.data$afp.all.2, 
                                      admin.data = ctry.data$dist.pop,
                                      start.date = start_date, 
                                      end.date = end_date,
                                      spatial.scale = "dist",
                                      missing = "good", 
                                      bad.data = "inadequate",
                                      rolling = F)
## Province
pstool <- f.stool.ad.01(afp.data = ctry.data$afp.all.2, 
                        admin.data = ctry.data$prov.pop,
                        start.date = start_date, 
                        end.date = end_date,
                        spatial.scale = "prov",
                        missing = "good", 
                        bad.data = "inadequate",
                        rolling = F)
## Country
cstool <- f.stool.ad.01(afp.data = ctry.data$afp.all.2, 
                        admin.data = ctry.data$ctry.pop,
                        start.date = start_date, 
                        end.date = end_date,
                        spatial.scale = "ctry",
                        missing = "good", 
                        bad.data = "inadequate",
                        rolling = F)

# 7.5) Shapefiles --------------------------------------------------------------
# Set shapefiles to be most recent GUIDs
ctry.shape <- ctry.data$ctry %>%
  filter(yr.end == max(yr.end))

ctry.shape$yr.end

prov.shape <- ctry.data$prov %>%
  filter(yr.end == max(yr.end))

prov.shape$yr.end

dist.shape <- ctry.data$dist %>%
  filter(yr.end == max(yr.end))

dist.shape$yr.end


# RECOMMEND A MANUAL CHECK ON WHAT THE YR.END IS HERE - 9999 is present day ----
# 8.) Assumptions for data analysis - WILL NEED TO BE UPDATED MANUALLY----------
pptx.assumptions <- c('Data sources:',
                      paste0('POLIS (Data as of ', format(start_date, '%d-%b-%Y'), 
                             ' to ',format(end_date, '%d-%b-%Y') ,')'),
                      'Missing population estimated from the UNDP growth factor from previous year’s population.',
                      paste0('Timeframe for analysis: ', format(start_date, '%d-%b-%Y'), 
                             ' to ',format(end_date, '%d-%b-%Y')),
                      paste0('Some selected figures include additional data (', 
                             format(start_date, "%b %Y"),
                             "-",format(Sys.Date(), "%b %Y"), ")"), # UPDATE AS NEEDED
                      'NPAFP Assumptions',
                      'NPAFP cases with all pending cases included (Pending Lab and Pending Classification)',
                      'Stool Adequacy Assumptions',
                      'All AFP cases',
                      'Samples with missing stool condition were considered good quality',
                      'Samples with bad date data (e.g. collection before onset) were considered inadequate')

assump = unordered_list(
  level_list = c(1, 2, 2, 1,2,1,2,1,2,2,2), #Indentation level for each bullet
  str_list = pptx.assumptions,
  style = fp_text(color = "black", font.size = 18))

# EXCEL FILE WRITE OUT FOR AFP AND POPULATION (YOU MUST RUN THESE BEFORE RUNNING THE DESK REVIEW CODE) ----------------------------------
## AFP line list with stool adequacy (remove calculation variables) -----------------------------------------
# LABEL: DATA QUALITY CHECK

afp.data <- ctry.data$afp.all.2 |>
  filter(year>=year(start_date) & year<=year(end_date))


stool.data <- afp.data
stool.data$adequacy.final <- NA

stool.data <- afp.data |> # IF FUNCTION CHANGES, THIS WILL NEED TO CHANGE AS WELL
  as_tibble() |>
  filter(cdc.classification.all2 != "NOT-AFP") |> 
  mutate(adequacy.final = case_when(#Conditions for Bad Data
    bad.stool1 == "data entry error" | 
      bad.stool1 == "date before onset" | 
      bad.stool1 == "date onset missing" ~ 77
  )) %>%
  mutate(adequacy.final = case_when(#Conditions for Bad Data
    is.na(adequacy.final)==TRUE & (bad.stool2 == "data entry error" | 
                                     bad.stool2 == "date before onset" | 
                                     bad.stool2 == "date onset missing") ~ 77,
    TRUE ~ adequacy.final
  )) %>%
  mutate(adequacy.final = case_when(#Conditions for Poor Adequacy
    is.na(adequacy.final)==TRUE & (ontostool1 > 13 | ontostool1 < 0 | 
                                     is.na(stool1tostool2) == T |
                                     ontostool2 > 14 | ontostool2 < 1 | stool1tostool2 < 1 |
                                     stool.1.condition == "Poor" | stool.2.condition == "Poor") ~ 0,
    TRUE ~ adequacy.final)) %>% 
  mutate(adequacy.final = case_when(#Conditions for Good Adequacy
    is.na(adequacy.final)==TRUE & (ontostool1 <= 13 & ontostool1 >= 0 & 
                                     ontostool2 <= 14 & ontostool2 >= 1 & 
                                     stool1tostool2 >= 1 & stool.1.condition == "Good" & 
                                     stool.2.condition == "Good") ~ 1,
    TRUE ~ adequacy.final
  )) %>%
  mutate(adequacy.final = case_when(#Conditions for Missing Adequacy
    is.na(adequacy.final)==TRUE & (is.na(stool.1.condition) == T | 
                                     is.na(stool.2.condition) == T | 
                                     stool.1.condition == "Unknown" | stool.2.condition == "Unknown") ~ 99,
    TRUE ~ adequacy.final
  )) |>
  mutate(adequacy.final = case_when(
    adequacy.final == 0 ~ "Inadequate",
    adequacy.final == 1 ~ "Adequate",
    adequacy.final == 77 ~ "Bad data",
    adequacy.final == 99 ~ "Missing",
  ))|>
  mutate(adequacy.final2 = ifelse(adequacy.final == "Missing", "Adequate", adequacy.final)
  )

count(stool.data, adequacy.final) # Check stool classifications without any modification
count(stool.data, adequacy.final2) # Check stool classifications once assumptions applied
stool.data %>% count(duplicated(epid)) # Check for duplicated EPIDs - If TRUE - duplicate

## Filter to only variables needed ----
stool.data.export <- stool.data%>%
  mutate(nvaccine.2 = NA) |>
  select(c("polis.case.id","epid","followup.date","followup.findings",
           "investigation.date","notification.date","stool.1.collection.date",
           "stool.2.collection.date","stool.date.sent.to.ic.lab",
           "classification","source.advanced.notification",
           "advanced.notification","datenotificationtohq",
           "stool.1.condition","stool.2.condition",
           "specimen.date","paralysis.asymmetric","paralysis.confirmed",
           "paralysis.hot.case","paralysis.left.arm","paralysis.left.leg",
           "paralysis.onset.fever","paralysis.rapid.progress","paralysis.right.arm",
           "paralysis.right.leg","paralysis.site","paralysis.sudden",
           "person.age.in.months","person.age.in.years","sex","ctry","prov",
           "dist","admin0guid","admin1guid","admin2guid","country.iso3","whoregion",
           "ist","doses.date.of.1st","doses.date.of.2nd","doses.date.of.3rd",
           "doses.date.of.4th","doses.ipv.number","doses.ipv.date.of.last",
           "doses.ipv.routine","doses.ipv.sia","doses.opv.date.of.last",
           "doses.opv.routine","doses.opv.sia","doses.total","calcdosesrisi",
           "results.seq.date.to.program","last.opv.sia.date","last.ipv.sia.date",
           "created.date","last.updated.date","vaccine.1","vaccine.2","vaccine.3",
           "vdpv.1","vdpv.2","vdpv.3","wild.1","wild.3","nvaccine.2","calculated.age.(months)",
           "polis.longitude","polis.latitude","surveillancetypename",
           "case.date","provisional.diagnosis","adequate.stool","stool.adequacy.with.condition",
           "final.cell.culture.result","is.breakthrough","poliovirustypes",
           "virus.cluster","emergence.group","nt.changes","classificationvdpv",
           "vdpv.classification.id(s)","anonymousepid","epidateisolationresultsreceived",
           "epidateitdresultsreceived","admin0id","admin1id","admin2id",
           "admin0officialname","admin1officialname","admin2officialname",
           "followupfindingscode","stool1conditioncode","stool2conditioncode",
           "publishdate","surveillancetypecode","virustypeids","date",
           "year","date.notify","date.invest","datestool1","datestool2","ontostool2",
           "ontostool1","age.months","ontonot","ontoinvest","nottoinvest",
           "investtostool1","stool1tostool2","stooltolabdate",
           "stooltoiclabdate","clinicadmitdate","vtype","vtype.fixed","cdc.class",
           "hot.case","sabin1","sabin2","sabin3","lon","lat","bad.onset","bad.notify",
           "bad.invest","bad.stool1","bad.stool2","bad.followup","stool2missing",
           "stool1missing","stoolmissing","need60day","got60day","timeto60day",
           "ontime.60day","adm0guid","adm1guid","adm2guid","totalnumberofdosesipvopv",
           "cdc.classification.all2","daysstooltolab","noti.7d.on",
           "inv.2d.noti","coll.3d.inv","ship.3d.coll","adequacy.final",
           "adequacy.final2"
  ))

# BASIC POPULATION CHECKS ----------
# LABEL: DATA QUALITY CHECK
pop.check <- count(ungroup(filter(ctry.data$dist.pop, year>=year(start_date))), prov, dist, 
                  adm1guid,adm2guid, year, u15pop)

# compare difference in country vs. prov vs. dist roll up

ctry.data$ctry.pop
ppoppy = summarize(group_by(ctry.data$prov.pop, year), tot.freq = sum(u15pop, na.rm = T))
dpoppy = summarize(group_by(ctry.data$dist.pop, year), tot.freq = sum(u15pop, na.rm = T))

poppy = left_join(ctry.data$ctry.pop, ppoppy, by = c("year" = "year"))
poppy = left_join(poppy, dpoppy, by = c("year" = "year"))
poppy$ctryvprov_diff = poppy$u15pop - poppy$tot.freq.x
poppy$ctryvdist_diff = poppy$u15pop - poppy$tot.freq.y
poppy$ctryvprov_per = round(100*(poppy$ctryvprov_diff/poppy$u15pop),1)
poppy$ctryvdist_per = round(100*(poppy$ctryvdist_diff/poppy$u15pop),1)

poppy = poppy %>%
  mutate(across(c(tot.freq.x, tot.freq.y, ctryvprov_diff, ctryvdist_diff),as.integer))

poppy = poppy %>%
  rename("Country pop" = "u15pop",
         "Total pop from province rollup" = "tot.freq.x",
         "Total pop from district rollup" = "tot.freq.y",
         "Difference in country and province rollup" = "ctryvprov_diff",
         "Difference in country and district rollup" = "ctryvdist_diff",
         "Percent difference in country and province rollup" = "ctryvprov_per",
         "Percent difference in country and district rollup" = "ctryvdist_per")

poppy = poppy[,c(1,4,3,6,7,8,9,10,11,5,2)]

# !!! Inserting Liz's code 1326-1785
# LAB Data-----------------------------------------------------------------------------
# # Read in and deal with lab data --------------
laba <- read_excel(file.path(here(), "madagascar", year(Sys.Date()), "data", "polio_lab_2019_jan22_2024.xlsx"))
# updated lab data
laba <- laba |>
  filter(ctry.code2==ctry.data$ctry.code)
#filtering out negative time intervals
# !!! laba2 is that main dataset we will be working with
laba2 = laba %>%
  filter((days.collect.lab >= 0 | is.na(days.collect.lab)) &
           (days.lab.culture >= 0 | is.na(days.lab.culture)) &
           (days.seq.ship >= 0 | is.na(days.seq.ship)) &
           (days.lab.seq >= 0 | is.na(days.lab.seq)) &
           (days.itd.seqres >= 0 | is.na(days.itd.seqres)) &
           (days.itd.arriveseq >= 0 | is.na(days.itd.arriveseq)) &
           (days.seq.rec.res >= 0 | is.na(days.seq.rec.res))
  ) |>
  filter(year >= year(start_date) & year <= year(end_date),
         CaseOrContact == "1-Case")

# Missing year ----
# LABEL: DATA QUALITY CHECK AND CLEANING
names(laba2)
miss = filter(laba2, is.na(year))
count(miss, CaseOrContact)

# !!! some have duplicate entries, with slight differences (i.e, days.collect.lab)
count(miss, EpidNumber)

miss = miss %>%
  mutate(year2 = substr(EpidNumber, 13,14)) %>%
  mutate(year2 = as.numeric(paste0("20", year2)))

laba2 = laba2 %>%
  mutate(year = case_when(
    is.na(year) ~ miss$year2[match(laba2$EpidNumber, miss$EpidNumber)],
    T ~ year
  ))

# !!! after operation no more missing years
count(laba2, year)
# !!! District in the original dataset and distict names are incorrect
count(laba2, District)

# Adding guids ----
laba2$Province = str_to_upper(laba2$Province)
laba2$District = str_to_upper(laba2$District)
laba2$Province = iconv(laba2$Province, to='ASCII//TRANSLIT')
laba2$District = iconv(laba2$District, to='ASCII//TRANSLIT')
# !!! missing values for province in 13 entries for the date range specified
count(laba2, Province, year)

# Match province and district by epid number ----
# DONT FORGET TO ADD COUNTRY AND ADM0GUID
# create new columns
laba2$prov = NA
laba2$dist = NA
laba2$adm1guid = NA
laba2$adm2guid = NA

# !!! changed place.admin.1 = prov and place.admin.2 = dist from original code
# reason is that those were renamed at the top of the script
laba2$prov = ctry.data$afp.all.2$prov[match(laba2$EpidNumber, ctry.data$afp.all.2$epid)]
laba2$dist = ctry.data$afp.all.2$dist[match(laba2$EpidNumber, ctry.data$afp.all.2$epid)]

laba2$adm1guid = ctry.data$afp.all.2$adm1guid[match(laba2$EpidNumber, ctry.data$afp.all.2$epid)]
laba2$adm2guid = ctry.data$afp.all.2$adm2guid[match(laba2$EpidNumber, ctry.data$afp.all.2$epid)]


# matching by Epid
# !!! first pass at reconciling missing values
count(laba2, prov, year)

ugh = laba2
ugh %>% count(EpidNumber %in% ctry.data$afp.all.2$epid)
ugh = laba2 %>%
  filter(is.na(prov))
dim(ugh)
ugh$EpidNumber
ugh$Province

# By province name
ugh$adm1guid = ctry.data$afp.all.2$adm1guid[match(ugh$Province, ctry.data$afp.all.2$prov)]
ugh$prov = ctry.data$afp.all.2$prov[match(ugh$Province, ctry.data$afp.all.2$prov)]


count(ugh, prov, adm1guid)

ugh2 = ugh %>%
  filter(is.na(prov))

ugh2$Province
ugh2$EpidNumber

# Totally random cases in ugh2; use ugh ----
laba2 = laba2 %>%
  mutate(prov = ifelse(is.na(prov), ugh$prov[match(laba2$EpidNumber, 
                                                   ugh$EpidNumber)], prov)) %>%
  mutate(adm1guid = ifelse(is.na(adm1guid), ugh$adm1guid[match(laba2$EpidNumber, 
                                                               ugh$EpidNumber)], adm1guid))
count(laba2, prov, adm1guid)

count(laba2, prov) # 85 missing province
count(laba2, dist) # 227 missing districts

#---- Additional data cleaning steps outside of original Liberia code
geo_lookup_table <- ctry.data$afp.all.2 |>
  select(epid, matches("guid"), contains("$adm"), ctry, prov, dist, year) |>
  separate_wider_delim(cols=epid, delim = "-", 
                       names = c("epid_ctry", "epid_prov", "epid_dist", 
                                 "epid_04", "epid_05"),
                       too_few = "debug",
  ) |>
  select(contains("epid"), ctry, prov, dist, matches("adm[0-3]guid"), year) |>
  distinct()

prov_lookup_table <- geo_lookup_table |>
  select(epid_prov, prov, adm1guid, year) |>
  distinct() |>
  # doing manual filter for incorrect mapping
  filter(!(epid_prov == "NOR" & prov == "NORTH WESTERN"))

dist_lookup_table <- geo_lookup_table |>
  select(epid_dist, dist, adm2guid, year) |>
  distinct() |>
  # doing manual filter
  # KAMBIA has two adm2guid for the same year ({16... looks wrong and need to be removed for 2021)
  # KOI has two adm2guids, but switched GUID after 2019 so keeping the mapping
  # MOY = BO incorrect mapping, so need to be removed
  # PORT has two admguids in 2019,2020 but no overlap in years
  # WAR = WESTERN RURAL not mapped correctly
  filter(!(epid_dist == "BON" & dist == "BOMBALI"),
         !(epid_dist == "KAM" & adm2guid == "{16FAC340-B054-43D4-9089-E51664302A43}"),
         !(epid_dist == "MOY" & dist == "BO"),
         !(epid_dist == "WAU" & dist == "WESTERN RURAL")
  )

# geomatching algorithm
laba2 <- laba2 |>
  separate_wider_delim(cols=EpidNumber, delim = "-", 
                       names = c("epid_ctry", "epid_prov", "epid_dist", 
                                 "epid_04", "epid_05"), too_many = "debug", too_few = "debug")
test <- laba2 |>
  mutate(epid_prov = if_else(epid_prov == "WAE", "WEA", epid_prov),
         epid_dist = if_else(epid_dist == "PLT", "PTL", epid_dist))

# the missing values are due to WAE in the epid_prov...should be WEA and needs to be fixed before join operations
# also PLT in province, should be PTL...fixed above

test <- test |>
  left_join(prov_lookup_table) |>
  left_join(prov_lookup_table, by = join_by(epid_prov, year)) |>
  mutate(prov.x = if_else(is.na(prov.x) & !is.na(prov.y), prov.y, prov.x),
         adm1guid.x = if_else(is.na(adm1guid.x) & !is.na(adm1guid.y), adm1guid.y, adm1guid.y)
  ) |>
  left_join(dist_lookup_table) |>
  left_join(dist_lookup_table, by = join_by(epid_dist, year)) |>
  mutate(dist.x = if_else(is.na(dist.x) & !is.na(dist.y), dist.y, dist.x),
         adm2guid.x = if_else(is.na(adm2guid.x) & !is.na(adm2guid.y), adm2guid.y, adm2guid.y)
  ) |>
  rename(adm1guid = adm1guid.x,
         adm2guid = adm2guid.x,
         prov = prov.x,
         dist = dist.x) |>
  select(-ends_with(".y"))

# check for correctness
check <- test |>
  select(starts_with("epid_"), matches("adm[1-2]"), prov, dist, EpidNumber, year)
mismatch_dist <- anti_join(check, dist_lookup_table) 
# 14 mismatches in prov
mismatch_prov <- anti_join(check, prov_lookup_table)

laba2 <- test

count(laba2, dist)
count(laba2, prov)

# province and district remains problematic

# Some outputs for the powerpoint----




# Creating timeliness subsets -----
ugh = laba2
labbish1 = ugh %>%
  filter(as.Date(DateOfOnset)>=start_date & as.Date(DateOfOnset)<=end_date) %>%
  group_by(year) %>%
  summarize(medi = median(days.collect.lab, na.rm = T),
            freq = n()) %>%
  ungroup() %>%
  mutate(type = "days.collect.lab") %>%
  mutate(medi = as.numeric(medi))

# Quick question on onset to arrival in lab -----
labbishy = ugh %>%
  filter(as.Date(DateOfOnset)>=start_date & as.Date(DateOfOnset)<=end_date) %>%
  mutate(ontolab = difftime(as.Date(DateStoolReceivedinLab),as.Date(DateOfOnset))) %>%
  group_by(year) %>%
  summarize(medi = median(ontolab, na.rm = T),
            freq = n()) %>%
  ungroup() #%>%
#mutate(type = "days.collect.lab") %>%
#mutate(medi = as.numeric(medi))
labbishy

summarize(group_by(ctry.data$afp.all.2, year), medi = median(ontonot, na.rm = T))
summarize(group_by(ctry.data$afp.all.2, year), medi = median(nottoinvest, na.rm = T))
summarize(group_by(ctry.data$afp.all.2, year), medi = median(investtostool1, na.rm = T))
summarize(group_by(ctry.data$afp.all.2, year), medi = median(daysstooltolab, na.rm = T))

summarize(group_by(ctry.data$afp.all.2, year, prov), 
          medi = median(daysstooltolab, na.rm = T),
          freq = n()) %>%
  filter(year == 2023)


ugh %>%
  filter(as.Date(DateOfOnset)>=start_date & as.Date(DateOfOnset)<=end_date) %>%
  mutate(ontolab = difftime(as.Date(DateStoolReceivedinLab),as.Date(DateOfOnset))) %>%
  group_by(year) %>%
  summarize(medi = median(ontolab, na.rm = T),
            freq = n()) %>%
  ungroup()
#----

labbish2 = ugh %>%
  filter(as.Date(DateOfOnset)>=start_date & as.Date(DateOfOnset)<=end_date) %>%
  group_by(year) %>%
  summarize(medi = median(days.lab.culture, na.rm = T),
            freq = n()) %>%
  ungroup() %>%
  mutate(type = "days.lab.culture") %>%
  mutate(medi = as.numeric(medi))


labbish3 = ugh %>%
  filter(as.Date(DateOfOnset)>=start_date & as.Date(DateOfOnset)<=end_date) %>%
  group_by(year) %>%
  summarize(medi = median(days.seq.ship, na.rm = T),
            freq = n()) %>%
  ungroup() %>%
  mutate(type = "days.seq.ship") %>%
  mutate(medi = as.numeric(medi))

labbish4 = ugh %>%
  filter(as.Date(DateOfOnset)>=start_date & as.Date(DateOfOnset)<=end_date) %>%
  group_by(year) %>%
  summarize(medi = median(days.lab.seq, na.rm = T),
            freq = n()) %>%
  ungroup() %>%
  mutate(type = "days.lab.seq") %>%
  mutate(medi = as.numeric(medi))

labbish5 = ugh %>%
  filter(as.Date(DateOfOnset)>=start_date & as.Date(DateOfOnset)<=end_date) %>%
  group_by(year) %>%
  summarize(medi = median(days.itd.seqres, na.rm = T),
            freq = n()) %>%
  ungroup() %>%
  mutate(type = "days.itd.seqres") %>%
  mutate(medi = as.numeric(medi))

labbish6 = ugh %>%
  filter(as.Date(DateOfOnset)>=start_date & as.Date(DateOfOnset)<=end_date) %>%
  group_by(year) %>%
  summarize(medi = median(days.itd.arriveseq, na.rm = T),
            freq = n()) %>%
  ungroup() %>%
  mutate(type = "days.itd.arriveseq") %>%
  mutate(medi = as.numeric(medi))

labbish7 = ugh %>%
  filter(as.Date(DateOfOnset)>=start_date & as.Date(DateOfOnset)<=end_date) %>%
  group_by(year) %>%
  summarize(medi = median(days.seq.rec.res, na.rm = T),
            freq = n()) %>%
  ungroup() %>%
  mutate(type = "days.seq.rec.res") %>%
  mutate(medi = as.numeric(medi))

labbish = bind_rows(labbish1, labbish2, labbish3, labbish4, labbish5, labbish6, labbish7)
labbish

afp.data1 = ctry.data$afp.all.2 %>%
  filter(date.onset>= start_date & date.onset<= end_date)

# admin.data <- ungroup(admin.data) %>%
#   select(adm0guid, year) %>%
#   distinct(.)

int.data <- afp.data1 |>
  mutate(year = year(date)) |>
  group_by(adm0guid, year) |>
  select(
    epid,
    ontonot,
    nottoinvest,
    investtostool1,
    stool1tostool2,
    #daysstooltolab,
    year,
    adm0guid,
    ctry
  ) %>%
  mutate(across(c(ontonot, nottoinvest, investtostool1, stool1tostool2), as.numeric))%>%
  #daysstooltolab), as.numeric)) %>%
  pivot_longer(!c(epid, year, adm0guid, ctry), names_to = "type", values_to = "value") %>%
  group_by(year, type, adm0guid, ctry) %>%
  summarize(medi = median(value, na.rm = T), freq = n())

int.data = bind_rows(labbish, int.data)

levs <- c(
  "ontonot" = "Paralysis onset to notification",
  "nottoinvest" = "Case notification to investigation",
  "investtostool1" = "Case investigation to stool 1 collection",
  "stool1tostool2" = "Stool 1 collection to stool 2 collection",
  "days.collect.lab" = "Last stool collection to received in lab",
  "days.lab.culture" = "Stool received lab to final culture results",
  "days.seq.ship" = "Isolate received for sequencing to sequence results available",
  "days.lab.seq" = "Stool received in lab to sequence result",
  "days.itd.seqres" = "Final rRT-PCR results to sequence result",
  "days.itd.arriveseq" = "Final rRT-PCR results to isolate received for sequencing",
  "days.seq.rec.res" = "Isolate received for sequencing to sequence result"
)

int.data$type = ordered(int.data$type, levels = names(levs), labels = levs)
#int.data$type = ordered(int.data$type, levels = names(levs), labels = levs)

int.data = filter(int.data, type %in% c("Paralysis onset to notification",
                                        "Case notification to investigation",
                                        "Case investigation to stool 1 collection",
                                        "Stool 1 collection to stool 2 collection",
                                        "Last stool collection to received in lab",
                                        "Stool received lab to final culture results"))
# 
timely_nation <- ggplot() +
  geom_bar(data = int.data, aes(x = factor(year), y = medi, fill = fct_rev(type)),
           position = "stack",
           stat = "identity"
  ) +
  geom_text(
    data = filter(int.data, medi != 0),
    aes(x = factor(year), y = medi, label = medi, group = fct_rev(type)),
    position = position_stack(vjust = 0.5)
  ) +
  coord_flip() +
  ylab("Median Days") +
  xlab("Year of Paralysis Onset") +
  #scale_y_continuous(breaks = seq(0, max(pretty(tot.time$tot))+1)) +
  scale_x_discrete(labels = labs) +
  scale_fill_manual(
    name = "Interval",
    drop = F,
    values = f.color.schemes("timeliness.col.vars"),
    guide = guide_legend(reverse = TRUE)
  ) +
  theme(
    legend.position = "bottom",
    legend.background = element_blank()
  )

timely_nation
# 
# 
# 
# ## 20. Timeliness at provincial level 
# (IF AN ENTIRE PROVINCE HAS NO CASES, DOES NOT APPEAR)  -----------

afp.prov.year.lab = ctry.data$afp.all.2 %>%
  filter(date.onset>=start_date & date.onset<=end_date) %>%
  count(prov, adm1guid,year)


afp.prov.year.lab$labs = paste0(afp.prov.year.lab$year,
                                " (N=",afp.prov.year.lab$n,")")


labbish1 = ugh %>%
  filter(as.Date(DateOfOnset)>=start_date & as.Date(DateOfOnset)<=end_date) %>%
  group_by(year, adm1guid) %>%
  summarize(medi = median(days.collect.lab, na.rm = T),
            freq = n()) %>%
  ungroup() %>%
  mutate(type = "days.collect.lab") %>%
  mutate(medi = as.numeric(medi))

labbish2 = ugh %>%
  filter(as.Date(DateOfOnset)>=start_date & as.Date(DateOfOnset)<=end_date) %>%
  group_by(year, adm1guid) %>%
  summarize(medi = median(days.lab.culture, na.rm = T),
            freq = n()) %>%
  ungroup() %>%
  mutate(type = "days.lab.culture") %>%
  mutate(medi = as.numeric(medi))


labbish3 = ugh %>%
  filter(as.Date(DateOfOnset)>=start_date & as.Date(DateOfOnset)<=end_date) %>%
  group_by(year, adm1guid) %>%
  summarize(medi = median(days.seq.ship, na.rm = T),
            freq = n()) %>%
  ungroup() %>%
  mutate(type = "days.seq.ship") %>%
  mutate(medi = as.numeric(medi))

labbish4 = ugh %>%
  filter(as.Date(DateOfOnset)>=start_date & as.Date(DateOfOnset)<=end_date) %>%
  group_by(year, adm1guid) %>%
  summarize(medi = median(days.lab.seq, na.rm = T),
            freq = n()) %>%
  ungroup() %>%
  mutate(type = "days.lab.seq") %>%
  mutate(medi = as.numeric(medi))

labbish5 = ugh %>%
  filter(as.Date(DateOfOnset)>=start_date & as.Date(DateOfOnset)<=end_date) %>%
  group_by(year, adm1guid) %>%
  summarize(medi = median(days.itd.seqres, na.rm = T),
            freq = n()) %>%
  ungroup() %>%
  mutate(type = "days.itd.seqres") %>%
  mutate(medi = as.numeric(medi))

labbish6 = ugh %>%
  filter(as.Date(DateOfOnset)>=start_date & as.Date(DateOfOnset)<=end_date) %>%
  group_by(year, adm1guid) %>%
  summarize(medi = median(days.itd.arriveseq, na.rm = T),
            freq = n()) %>%
  ungroup() %>%
  mutate(type = "days.itd.arriveseq") %>%
  mutate(medi = as.numeric(medi))

labbish7 = ugh %>%
  filter(as.Date(DateOfOnset)>=start_date & as.Date(DateOfOnset)<=end_date) %>%
  group_by(year, adm1guid) %>%
  summarize(medi = median(days.seq.rec.res, na.rm = T),
            freq = n()) %>%
  ungroup() %>%
  mutate(type = "days.seq.rec.res") %>%
  mutate(medi = as.numeric(medi))



labbish = bind_rows(labbish1, labbish2, labbish3, labbish4, labbish5, labbish6, labbish7)
labbish

afp.data1 = ctry.data$afp.all.2 %>%
  filter(date.onset>= start_date & date.onset<= end_date) 

# admin.data <- ungroup(admin.data) %>%
#   select(adm0guid, year) %>%
#   distinct(.)

int.data <- afp.data1 |>
  mutate(year = year(date)) |>
  group_by(adm1guid, year) |>
  select(
    epid,
    ontonot,
    nottoinvest,
    investtostool1,
    stool1tostool2,
    #daysstooltolab,
    year,
    adm0guid,
    adm1guid,
    prov,
    ctry
  ) %>%
  mutate(across(c(ontonot, nottoinvest, investtostool1, stool1tostool2), as.numeric))%>%
  #daysstooltolab), as.numeric)) %>%
  pivot_longer(!c(epid, year, adm0guid,adm1guid,prov, ctry), names_to = "type", values_to = "value") %>%
  group_by(year, type, adm1guid, prov,ctry) %>%
  summarize(medi = median(value, na.rm = T), freq = n())

int.data = bind_rows(labbish, int.data)

int.data$prov = ctry.data$prov.pop$prov[match(int.data$adm1guid, ctry.data$prov.pop$adm1guid)]

levs <- c(
  "ontonot" = "Paralysis onset to notification",
  "nottoinvest" = "Case notification to investigation",
  "investtostool1" = "Case investigation to stool 1 collection",
  "stool1tostool2" = "Stool 1 collection to stool 2 collection",
  "days.collect.lab" = "Last stool collection to received in lab",
  "days.lab.culture" = "Stool received lab to final culture results",
  "days.seq.ship" = "Isolate received for sequencing to sequence results available",
  "days.lab.seq" = "Stool received in lab to sequence result",
  "days.itd.seqres" = "Final rRT-PCR results to sequence result",
  "days.itd.arriveseq" = "Final rRT-PCR results to isolate received for sequencing",
  "days.seq.rec.res" = "Isolate received for sequencing to sequence result"
)

int.data$type = ordered(int.data$type, levels = names(levs), labels = levs)
#int.data$type = ordered(int.data$type, levels = names(levs), labels = levs)

int.data = filter(int.data, type %in% c("Paralysis onset to notification",
                                        "Case notification to investigation",
                                        "Case investigation to stool 1 collection",
                                        "Stool 1 collection to stool 2 collection",
                                        "Last stool collection to received in lab",
                                        "Stool received lab to final culture results"))
prov.time.2 = left_join(int.data, afp.prov.year.lab, by = c("year" = "year",
                                                            "adm1guid" = "adm1guid",
                                                            "prov" = "prov"))

province_names <- prov.time.2 |>
  select(prov) |> unique() |> arrange(prov) |>
  pull(prov)
province_names_pt1 <- province_names[1:7]
province_names_pt2 <- province_names[8:14]
province_names_pt3 <- province_names[15:23]

timely_prov <- ggplot(prov.time.2|>
                        filter(is.na(medi)==F & is.na(prov)==F,
                               prov %in% province_names))+#med.p3) +
  geom_bar(aes(x = as.character(labs), y = medi, fill = fct_rev(type)),
           position = "stack", stat = "identity") +
  geom_text(
    aes(x = labs, y = medi, label = medi, group = fct_rev(type)),
    position = position_stack(vjust = 0.5)
  ) +
  coord_flip() +
  ylab("Median Days") +
  xlab("Year of Paralysis Onset") +
  # scale_y_continuous(breaks = seq(0, max(pretty(tot.time.p$tot))+1)) +
  scale_x_discrete() +
  ylab("Days") +
  xlab("Year") +
  scale_fill_manual(
    name = "Interval",
    values = f.color.schemes("timeliness.col.vars"),
    guide = guide_legend(reverse = TRUE), drop = F
  ) +
  facet_grid(prov~. ,scales = "free_y" , space = "free",
             switch = "y")+
  theme(
    legend.position = "bottom",
    legend.background = element_blank()
  ) +
  theme(strip.text.y = element_text(size = 5))

timely_prov

# Uncomment if ISS/eSURV data is available
# # # Read in and deal with the ISS data ----
# issy = read.csv("Current Sierra_Leone.csv")
# # 
# names(issy)
# count(issy, priority_level)
# # 
# # Cleaning -----------------------------------
# ## Priority level
# issy2 = issy %>%
#   mutate(priority_level = case_when(
#     hf_rating=="high" ~ "High",
#     hf_rating== "highest" ~ "High",
#     hf_rating=="medium" ~ "Medium",
#     hf_rating=="low" ~ "Low",
#     hf_rating=="n/a" ~ "Not Focal Site",
#     hf_rating=="none" ~ "Not Focal Site",
#     T ~ hf_rating
#   )) %>%
#   mutate(priority_level = factor(priority_level, levels = c(
#     "High", "Medium", "Low", "Not Focal Site"
#   )))
# count(issy2, priority_level)
# 
# 
# # add month
# issy2$monyear = as.yearmon(as.Date(issy2$starttime))
# issy2$month = month(as.Date(issy2$starttime))
# issy2$year = year(as.Date(issy2$starttime))
# count(issy2, year)
# count(issy2, monyear)
# # Unreported AFP
# # !!! not on iss data for SL
# issy2$unrep_afp = as.numeric(issy2$num_unreportedcases)
# count(issy2, num_unreportedcases, unrep_afp)
# # Province
# count(issy2, states_province)
# issy2$prov = toupper(issy2$states_province)
# count(issy2, prov)
# 
# # District
# count(issy2, district)
# issy2$dists = iconv(issy2$district,to="ASCII//TRANSLIT")
# issy2$dists = toupper(issy2$dists)
# count(issy2, dists)
# count(issy2, district)
# 
# # !!! Convert "n/a" characters to actual null values
# issy2 <- issy2 |>
#   mutate(dists = if_else(dists == "N/A", NA, dists),
#          prov = if_else(prov == "N/A", NA, prov)
#   )
# # !!! Clean district names for consistency
# issy2 <- issy2 |>
#   mutate(dists = case_when(
#     dists == "WESTERNAREARURAL" ~ "WESTERN RUR",
#     dists == "WESTERNAREAURBAN" ~ "WESTERN URB",
#     T ~ dists
#   )
#   )
# count(issy2, dists)
# 
# #unique(issy2$dist) %in% unique(shape.dist.pop$ADM2_NAME)
# #issy2$dist[which(!(issy2$dist %in% unique(shape.dist.pop$ADM2_NAME)))]
# 
# # # Facility Name
# count(issy2, hf)
# # # Remove accents
# issy2 = issy2 %>%
#   mutate(facility_name2 = iconv(issy2$hf,
#                                 to="ASCII//TRANSLIT"))
# 
# count(issy2, facility_name2)
# 
# #
# # # make all capital letters and remove extra whitespace
# issy2 <- issy2 %>%
#   mutate(facility_name2 = toupper(facility_name2)) %>%
#   mutate(facility_name2 = str_squish(facility_name2))
# count(issy2, facility_name2)
# 
# # ISS graphs ------
# wrongsy = count(issy2, today_date>=end_date)
# 
# wrongsy
# 
# issy2.1 = issy2 %>%
#   filter(today_date<=Sys.Date() & 
#            today_date>= start_date)
# 
# count(issy2, issy2$year<=as.yearmon(Sys.Date()))
# 
# issy3 = issy2.1 %>%
#   #filter(is_priority_afp=="Yes") %>%
#   group_by(month, year, priority_level) %>%
#   summarize(freq = n()) %>%
#   filter(year<=year(Sys.Date()) & 
#            year>= year(start_date))
# 
# count(issy3, month, year) # One is 2033
# 
# issy3$labs = month.abb[issy3$month] %>%
#   factor(., levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
#                        "Aug", "Sep", "Oct", "Nov", "Dec"))
# issy3 <- issy3 |>
#   filter(between(year, year(start_date), year(end_date)))
# # red = "#d73027"
# # orange = "#fdae61"
# # blue = #4575b4
# # grey = #878787
# 
# totty = issy3 %>%
#   group_by(year, month) %>%
#   summarize(totty = sum(freq)) 
# 
# mtot = max(totty$totty)
# 
# 
# issy.vis = ggplot(data = issy3) +
#   geom_bar(aes(x = factor(labs), y = freq, 
#                fill = priority_level), stat = "identity",
#            position = "stack", col = "black")+
#   scale_y_continuous(name = "Visit Number", limits = c(0, max(pretty(mtot))),
#                      breaks = seq(0, max(pretty(mtot)), max(pretty(mtot))/5),
#                      labels = seq(0, max(pretty(mtot)), max(pretty(mtot))/5))+
#   scale_x_discrete(name = "Time")+
#   scale_fill_manual(name = "Priority", 
#                     values = c(
#                       "High" = "#d73027",
#                       "Medium" = "#fdae61",
#                       "Low" = "#4575b4",
#                       "Not Focal Site" = "#878787"
#                       
#                     ))+
#   facet_wrap(~year) +
#   theme_bw()
# 
# issy.vis
# 
# # Number of high priority sites per district? province? ----
# # probably not worth it ----
# issy2 %>% count(prov %in% ctry.data$afp.all.2$prov)
# pryr = count(issy2, priority_level,year) %>%
#   filter(priority_level == "High")
# 
# issy4 = full_join(issy2, pryr)
# 
# issy4$labs = paste0(issy2$year, "\n(n = ",issy4$n,")")
# 
# issy.map = ggplot()+
#   geom_sf(data = prov.shape, color = "black", fill = NA, size = .5) +
#   geom_point(data = issy4|>
#                filter(year<=year(end_date) & 
#                         year>= year(start_date) &
#                         priority_level=="High"), 
#              aes(x = as.numeric(X_gps_longitude),
#                  y = as.numeric(X_gps_latitude), 
#                  col = priority_level))+
#   sirfunctions::f.plot.looks("epicurve") +
#   scale_color_manual("Priority level", values = c("High" = "#d73027"))+
#   facet_wrap(~labs) +
#   theme(
#     axis.text.x = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks = element_blank()
#   )
# issy.map


# -----------------------------------------------------------------------------

excel_output_path <- file.path(here(), str_to_lower(country), year(Sys.Date()), "data")
# NPAFP excel file write out ---------
sheets <- list("country_npafp" = ctry.case.ind,
               "province_npafp" = prov.case.ind,
               "district_npafp" = dis.case.ind) 
# !!! need to be put into a condition where it would create the folder if it doesn't already exist
write_xlsx(sheets, file.path(excel_output_path, paste0(Sys.Date(), "_", "npafp_indicators.xlsx")))

# Stool adequacy excel file write out ---------
sheets <- list("country_npafp" = cstool,
               "province_npafp" = pstool,
               "district_npafp" = dstool) 
write_xlsx(sheets, file.path(excel_output_path, paste0(Sys.Date(), "_", "stool_adequacy_indicators.xlsx")))

# AFP line list excel file write out ---------
write_xlsx(stool.data.export, 
           file.path(excel_output_path, paste0(Sys.Date(), "_",
                  "AFP Linelist_",country, ".xlsx")))

# Population checks excel file write out ---------
pop.check1 = as.data.frame(pivot_wider(pop.check, 
                                       names_from = year, 
                                       values_from = u15pop))
sheets <- list("districts_by_year" = pop.check1,
               "population_comparison" = poppy) 

write_xlsx(sheets,  file.path(excel_output_path, paste0(Sys.Date(), "_", 
                          "population_check_", 
                          country, ".xlsx")))
# FIGURES ---------------------------------------------------------
# 1. Reference map of country (pop.map)-------------------------
# - colored by under 15 population
# major roads = black
# major cities = blue
proppy = filter(ctry.data$prov.pop, year == year(end_date))
dim(proppy) # If dim is 0, may be missing some population

# Filter population to be the last year of analysis
# Note that if the population data isn't there for your last year of analysis, 
# you will need to manually set this
prov.pop <- ctry.data$prov.pop %>% 
  filter(year == year(end_date) & ctry==country)

# Merge with province
shape.prov.pop <- left_join(prov.shape, prov.pop, by = c("GUID" = "adm1guid"))

pop.map <- ggplot() +
  geom_sf(data = ctry.shape, color = "black", fill = NA, size = 1) +
  geom_sf(
    data = shape.prov.pop,
    aes(fill = u15pop)
  ) +
  geom_sf(data = st_crop(ctry.data$roads, ctry.data$ctry)) +
  geom_sf(data = filter(ctry.data$cities, toupper(CNTRY_NAME) == ctry.data$name), 
          size = 3, color = "blue") +
  geom_sf(data = ctry.shape, color = "black", fill = NA,
          size = 15) +
  geom_sf_label_repel(data = filter(ctry.data$cities, 
                                    toupper(CNTRY_NAME) == ctry.data$name), 
                      aes(label = CITY_NAME)) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, 
                       labels = scales::comma) +
  ggtitle(paste0("Major Cities and Roads - Province Level Population - ", year(end_date)))+
  labs(fill = "Under-15 pop") +
  sirfunctions::f.plot.looks("epicurve") +
  scale_size_identity()+
  labs(caption = "- Under 15 population is shown at the province level\n- Major roads are shown in black\n- Population centers are shown in blue") +
  theme(plot.title=element_text(size=15, face="bold", hjust=0.5), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "right",
        plot.caption = element_text(hjust=0, size = 11),
        legend.background = element_blank()
  )

pop.map

# 1.1. Province level map with district population(pop.map.provn) -------------------
# Filter population to be the last year of analysis
# Note that if the population data isn't there for your last year of analysis, 
# you will need to manually set this
dist.pop <- ctry.data$dist.pop %>% 
  filter(year == year(end_date) & ctry==country)

shape.dist.pop <- left_join(dist.shape, dist.pop, by = c("GUID" = "adm2guid"))


pop.map.provn <- ggplot() +
  geom_sf(data = ctry.shape, color = "black", fill = NA, size = 1) +
  geom_sf(
    data = shape.dist.pop,
    aes(fill = u15pop), color = NA
  ) +
  geom_sf(data = prov.shape, color = "black", fill = NA) +
  geom_sf_label_repel(data = shape.prov.pop, 
                      aes(label = ADM1_NAME), force=80) +
  scale_fill_distiller(palette = "YlOrRd", direction = "both", 
                       labels = scales::comma) +
  ggtitle(paste0("Province Names - District Level Population - ", year(end_date)))+
  labs(fill = "Under-15 pop") +
  sirfunctions::f.plot.looks("epicurve") +
  scale_size_identity()+
  labs(caption = "- Under 15 population is shown at the district level\n- Labels are province names\n- Black lines are province borders") +
  theme(plot.title=element_text(size=15, face="bold", hjust=0.5), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "right",
        plot.caption = element_text(hjust=0, size = 11),
        legend.background = element_blank()
  )

pop.map.provn
# 2. Paralytic polio and compatible cases map - date range (afp.case.map) -----------------
afp.case.map.filter <- ctry.data$afp.all %>%
  filter(as.Date(date.onset) >= start_date & as.Date(date.onset) <= end_date) %>%
  mutate(year = as.factor(year))

# Quick visual check of types of cases...
count(afp.case.map.filter, cdc.classification.all2, year)

# ADJUST AS NEEDED BASED ON DATA
#afp.map.legend = "No cases or compatible cases in 2022"


afp.case.map <- ggplot() +
  geom_sf(data = ctry.shape, color = "black", fill = NA, size = 1) +
  geom_sf(data = prov.shape, color = "black", fill = NA, size = .5) +
  geom_sf(
    data = afp.case.map.filter |>
      filter(!(cdc.class %in% c("PENDING", "NPAFP", "UNKNOWN", "NOT-AFP", "LAB PENDING"))),
    aes(color = cdc.classification.all2), size = 1
  ) +
  scale_color_manual(
    values = sirfunctions::f.color.schemes("para.case"), name = "Case type",
    drop = F
  ) +
  ggtitle(paste("Paralytic Polio and Compatible Cases", 
                year(start_date), "-", year(end_date))) +
  # NOTE: IF THERE ARE NONE IT NEEDS TO THROW AN ERROR
  sirfunctions::f.plot.looks("epicurve") +
  facet_wrap(~year, ncol = 4) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )

afp.case.map

# 5. AFP Epi-curve - date range (afp.epi.curve1)-----------------------------------------------
afp.epi.date.filter <- ctry.data$afp.epi %>% # This limits analysis to specified dates - use up to current date (below)
  filter(yronset >= as.numeric(year(start_date)) & yronset <= as.numeric(year(end_date)))
# Stephanie wants this one as up to date as possible usually
# Use current date instead of last date of analysis - COMMENT OUT IF DO NOT WANT TO USE UP TO PRESENT DAY -------------------------
afp.epi.date.filter <- ctry.data$afp.epi %>%
  filter(yronset >= as.numeric(year(start_date)) & yronset <= as.numeric(year(Sys.Date())))

case.num.labs <- reframe(group_by(afp.epi.date.filter, yronset),
                         labs = paste0(yronset," (N = ", sum(afp.cases), ")")) %>%
  distinct(.)

afp.epi.date.filter1 <- left_join(afp.epi.date.filter, 
                                  case.num.labs, 
                                  by = c("yronset" = "yronset"))

afp.epi.curve1 <- ggplot(
  afp.epi.date.filter1,
  aes(fill = cdc.classification.all2, 
      y = afp.cases, x = epi.week)
) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(
    values = sirfunctions::f.color.schemes(type = "epicurve"), 
    name = "Classification",
    drop = FALSE
  ) +
  sirfunctions::f.plot.looks(type = "epicurve") +
  facet_wrap(~labs, ncol = 3, drop = F)

afp.epi.curve1
# 7. National surveillance indicators by year - Table (surv.ind.tab) -----------
## AFP cases per year - afp.case
## NPAFP rate - ctry.extract
## Stool adequacy - cstool

ctry.ind.afp <- left_join(ctry.extract, cstool, 
                          by = c("year", "adm0guid"))
ctry.ind.afp <- left_join(ctry.ind.afp, afp.case, 
                          by = c("year"))

ctry.ind.afp

## District NPAFP (including non reporting) - dist.extract
## Stool adequacy at district level - dstool

dist.ind.afp <- left_join(dis.extract, dstool, 
                          by = c("prov" = "prov",
                                 "dist" = "dist",
                                 "year" = "year"
                          ))


# population meeting both >=2 NPAFP rate and >=80% stool adequacy
tot.dist.pop <- dist.ind.afp %>%
  group_by(year) %>%
  summarize(tot.dist.pop = sum(u15pop, na.rm = T))

dist.adeq.ind <- dist.ind.afp %>%
  filter(npafp_rate >= 2 & per.stool.ad >= 80) %>%
  group_by(year) %>%
  summarize(tot.dist.adeq = sum(u15pop, na.rm = T))

meet.ind <- left_join(tot.dist.pop, dist.adeq.ind, 
                      by = c("year" = "year")) %>%
  mutate(across(tot.dist.adeq, ~ replace_na(.x, 0)))

meet.ind$prop.dist.adeq <- 100 * meet.ind$tot.dist.adeq / meet.ind$tot.dist.pop


meet.ind # Percentage of population meeting indicators

temp.ind.tab <- left_join(ctry.ind.afp, meet.ind, by = c("year"))

## Districts with population over 100K ------
dist.100k <- dis.extract %>%
  filter(u15pop >= 100000)

unique.dist.100k <- ctry.data$dist.pop %>%
  filter(ctry== country & u15pop > 100000) %>%
  unique()%>%
  group_by(year, u15pop, adm2guid) %>%
  filter(u15pop > 100000) %>%
  filter(year >= year(start_date) & 
           year <= year(end_date))

num.dists.100k <- unique.dist.100k %>%
  group_by(year) %>%
  summarize(dist.100k.num = n()) # total number of districts with pop >100K u15


ad.dists.100k <- left_join(dstool, dist.100k, by = c("year"= "year",
                                                     "adm2guid" = "adm2guid")) %>% 
  filter(npafp_rate >= 2 & per.stool.ad >= 80) %>%
  group_by(year)%>%
  summarize(ad.dist.100k.num = n()) 
# number of dists meeting both requirements with pop >100K u15

adeq.dists <- left_join(num.dists.100k, ad.dists.100k, by = c("year")) %>%
  mutate(across(everything(), ~ replace_na(.x, 0))) %>%
  mutate(prop = paste0(ad.dist.100k.num, "/", dist.100k.num))

temp.ind.tab1 <- left_join(temp.ind.tab, adeq.dists, by = c("year" = "year"))

temp.ind.tab1
## Making a flextable for this
temp.ind.tab2 <- temp.ind.tab1 %>%
  mutate(across(c(afp.cases, npafp_rate, per.stool.ad, prop.dist.adeq), ~ as.numeric(.))) %>%
  mutate(across(c(afp.cases, npafp_rate, per.stool.ad, prop.dist.adeq), ~ round(.,1)))

temp.ind.tab.flex <- as.data.frame(t(temp.ind.tab2)) %>%
  row_to_names(row_number = 1) %>%
  rownames_to_column("type") %>%
  filter(type %in% c("afp.cases", "npafp_rate", "per.stool.ad", "prop.dist.adeq", "prop")) %>%
  mutate(type = case_when(
    type == "npafp_rate" ~ "NPAFP rate*",
    type == "afp.cases" ~ "AFP cases",
    type == "per.stool.ad" ~ "Stool adequacy**",
    type == "prop.dist.adeq" ~ "Population U15 living in districts that met both indicators",
    type == "prop" ~ "Districts >= 100,000 U15 that met both indicators",
    FALSE ~ type
  ))

temp.ind.tab.flex <- temp.ind.tab.flex[c(2, 1, 3, 4, 5), ] # Reorder the table to be in the correct order

temp.ind.tab.flex
## Flextable of surveillance indicators (surv.ind.tab) --------------------
surv.ind.tab <- flextable(temp.ind.tab.flex) %>%
  theme_booktabs() %>%
  bold(bold = TRUE, part = "header") %>%
  colformat_double(j = 2:ncol(temp.ind.tab.flex), digits = 1, na_str = "---") %>%
  set_header_labels(
    type = ""
  ) %>%
  add_footer_row(top = F, "*Pending included\n**Stool adequacy defined as per Certification Indicator, i.e., 2 stools collected at least 24h apart AND ≤14d of onset AND received in good condition at a WHO-accredited laboratory (missing condition assumed good)", 
                 colwidths = ncol(temp.ind.tab.flex)) %>%
  autofit()

surv.ind.tab
# 8.1 AFP detections by Province and Year - Figure (afp.dets.prov.year; HIGHEST POP ON TOP) -----------
afp.month.prov.g = afp.by.month.prov |> filter(year >= year(start_date) &
                                                 year <= year(Sys.Date())) 



afp.month.prov.g$case.cat = factor(afp.month.prov.g$case.cat, levels = c(
  c("0", "1", "2-5", "6-9", "10+")
))

# changed to u15pop.prov instead
# prov is not a column at afp.month.prov.g (fixed by adding to the groupby)
# !!! address
afp.dets.prov.year <- ggplot(afp.month.prov.g|> 
                               filter(!is.na(prov)) |>
                               arrange(u15pop),
                             aes(x = mon.year2, 
                                 y = fct_inorder(prov), 
                                 fill = case.cat)) +
  geom_tile(color = "black") +
  sirfunctions::f.plot.looks("geomtile")+
  scale_fill_manual(
    values = sirfunctions::f.color.schemes("afp.prov"), name = "AFP Cases",
    drop = F
  ) +
  ggtitle("Number of AFP Cases by Province")+
  sirfunctions::f.plot.looks("geomtile")+
  theme(plot.caption = element_text(hjust = 0))+
  labs(caption = "Provinces are ordered by under 15 population, with highest on top")



afp.dets.prov.year


# 8.3) Population, case difference, NPAFP rate and stool adequacy by year (pop.tab) -----

sub.prov.case.ind = prov.case.ind %>%
  select(year, n_npafp, u15pop, prov, npafp_rate)

# only 6 provinces
sub.pstool = pstool %>%
  select(year, per.stool.ad, prov) |>
  filter(!is.na(prov))

sub.prov.join = full_join(sub.prov.case.ind, sub.pstool, by = c("year", "prov")) %>%
  arrange(prov, year)

sub.prov.join = sub.prov.join %>%
  group_by(prov) %>%
  mutate(diff = lag(n_npafp)) %>%
  mutate(diff_per = round(100*(n_npafp - lag(n_npafp))/lag(n_npafp), 1)) %>%
  mutate(across(c(per.stool.ad, diff, diff_per, n_npafp),round, 0)) %>%
  mutate(across(c(npafp_rate), round, 1)) |>
  filter(!is.na(prov))

sub.prov.join

date.analysis =  seq(year(start_date), year(end_date),1)
pop.date.analysis =paste0("u15pop_", date.analysis[1:length(date.analysis)-1])

sub.prov.join.wide = pivot_wider(sub.prov.join, names_from = year, 
                                 values_from = c(per.stool.ad, diff, diff_per, 
                                                 n_npafp, npafp_rate, u15pop))%>%
  select(-all_of(pop.date.analysis))

var.ord = c("prov", paste0("u15pop_", date.analysis[length(date.analysis)]), 
            paste0("n_npafp_", date.analysis), paste0("diff_per_", 
                                                      date.analysis[2:length(date.analysis)]),
            paste0("npafp_rate_", date.analysis), 
            paste0("per.stool.ad_", date.analysis))

sub.prov.join.wide = sub.prov.join.wide[,c(var.ord)] %>%
  replace(is.na(.), 0)

var.ord.case = c("prov", paste0("u15pop_", date.analysis[length(date.analysis)]), 
                 paste0("n_npafp_", date.analysis), paste0("diff_per_", 
                                                           date.analysis[2:length(date.analysis)]))

# NPAFP table ------
col_palette <- c("#FF9999", "white")
col.npafp.rate =  sub.prov.join.wide[,c(paste0("npafp_rate_", date.analysis))] %>%
  mutate(across(everything(),~ replace_na(.x, 0)))%>%
  mutate(across(c(everything()), cut, breaks = c(0,2),
                right = F, label = FALSE))

npafp.rate.colors <- col_palette[as.matrix(col.npafp.rate)]

# Stool adequacy ----
col_palette <- c("#FF9999", "white")
col.stool.ad =  sub.prov.join.wide[,c(paste0("per.stool.ad_", date.analysis))] %>%
  mutate(across(everything(),~ replace_na(.x, 0)))%>%
  mutate(across(c(everything()), cut, breaks = c(0,80),
                right = F, label = FALSE))

stool.ad.colors <- col_palette[as.matrix(col.stool.ad)]

# case vars only -----
sub.prov.join.wide.case = sub.prov.join.wide %>%
  select(all_of(var.ord.case))
# Cases and differences -----

null.col = rep(c(NA),times=ncol(sub.prov.join.wide.case)*nrow(sub.prov.join.wide.case))

col.mat = c(null.col, npafp.rate.colors, stool.ad.colors)

# Make provinces not meeting indicators red
# If stool ad or NPAFP below threshold - color = "#CC0000"
# Subset of prov not meeting indicators any year
inad.prov = sub.prov.join %>%
  filter(npafp_rate<2|per.stool.ad<80)

uni.inad.prov = match(unique(inad.prov$prov), sub.prov.join.wide$prov)

# Color matrix
col.mat.txt = col.mat %>%
  str_replace(., "#FF9999","#CC0000" )
col.mat.txt[uni.inad.prov] = "#CC0000"

# Flextable column formatting calculations
# # NPAFP cases length
npafp.case.length = length(subset(var.ord, grepl("n_n", var.ord) ==T | 
                                    grepl("diff", var.ord)==T))
# NPAFP rate length
npafp.rate.length = length(subset(var.ord, grepl("rate", var.ord) ==T))
# stool adequacy length
stool.ad.length = length(subset(var.ord, grepl("stool", var.ord) ==T))

# Labels for % difference
diff.yr = length(which(grepl("diff", names(sub.prov.join.wide)) ==T))

diff.lab = NULL
for(i in 1:(diff.yr)){
  diff.lab[i] = paste("% difference ", min(date.analysis)+i-1, "-", 
                      min(date.analysis)+i)
}

# Names for flextable columns
names1 = names(sub.prov.join.wide)
names2 = c("Province", paste0("U15 Population - ", max(date.analysis)),
           date.analysis, diff.lab,
           date.analysis, date.analysis)


small_border = fp_border_default(color="black", width = 1)
# pop.tab flextable
pop.tab = flextable(sub.prov.join.wide) %>%
  theme_booktabs() %>%
  bg(j = colnames(sub.prov.join.wide), bg = col.mat) %>%
  color(j = colnames(sub.prov.join.wide), col = col.mat.txt) %>%
  align(align = "center", part = "all") %>%
  set_header_df(mapping = data.frame(keys = names1,
                                     values = names2, 
                                     stringsAsFactors = FALSE),
                key = "keys" ) %>%
  add_header_row(values = c("", "# NP AFP Cases", "NP AFP rate", "% Stool Adequacy"),
                 colwidths = c(2, npafp.case.length, stool.ad.length,
                               stool.ad.length), top = TRUE) %>%
  vline(j = c(2,2+npafp.case.length,2+npafp.case.length+stool.ad.length), 
        border = small_border)%>%
  hline(part = "header") %>%
  bold(bold = TRUE, part = "header") %>%
  align(align = "center", part = "all")

pop.tab

# 9. NPAFP rate/AFP detection province level (npafp.maps) -----------
provnpafp <- prov.extract

provnpafp$cats <- cut(provnpafp$npafp_rate,
                      breaks = c(-1, 0, 1, 2, 3, 1000), right = F,
                      labels = c(
                        "Zero NPAFP cases", "<1",
                        "1-<2", "2-<3", "3+"
                      )
)

prov.cut <- provnpafp %>%
  mutate(cats = as.character(cats)) %>%
  mutate(cats = case_when(
    npafp_rate==0 & u15pop >= 100000 ~ "Silent (u15pop >= 100K)",
    npafp_rate==0 & u15pop < 100000 & u15pop>0~ "No cases (u15pop < 100K)",
    npafp_rate==0 & u15pop == 0 ~ "Missing Pop",
    T ~ cats
  )) %>%
  filter(year >= year(start_date) & year <= year(end_date)) 


prov.cut$cats <- factor(prov.cut$cats,
                        levels = c(
                          "<1" = "<1",
                          "1-<2" = "1-<2",
                          "2-<3" = "2-<3",
                          "3+" = "3+",
                          "Missing Pop" = "Missing Pop",
                          "No cases (u15pop < 100K)" = "No cases (u15pop < 100K)",
                          "Silent (u15pop >= 100K)" = "Silent (u15pop >= 100K)"
                        )
)

prov.cut = ungroup(prov.cut)

# For those with no cases --> separate out districts with u15pop >100K and <100K

prov.pop.case.npafp <- full_join(prov.shape, prov.cut, by = c("GUID" = "adm1guid")) %>%
  filter(year<=year(end_date) & year>=year(start_date))

# Labels for provinces meeting NPAFP rate
# How many provinces meet >2 NPAFP?
prov.2npafp = provnpafp %>%
  group_by(year, adm1guid, prov) %>%
  summarize(meet2 = sum(npafp_rate>=2, na.rm = T)) %>%
  ungroup() %>%
  group_by(year) %>%
  summarize(num.meet2 = sum(meet2, na.rm=T),
            len.year = length(year)) %>%
  mutate(labs = paste0(num.meet2, "/", len.year, " (", 
                       round(100*num.meet2/len.year,0), "%)",
                       " provinces \nwith >= 2 cases of NPAFP \nper 100,000 population"))


# Get coordinates for maps that are plotted
ctcoord = as.data.frame(st_coordinates(ctry.shape))
# Put text at 10% below the minimum X and Y coords for each map
adjy = (range(ctcoord$Y)[1] - range(ctcoord$Y)[2])*.1


npafp.maps <- ggplot() +
  geom_sf(data = ctry.shape, color = "black", fill = NA, size = 1) +
  geom_sf(data = prov.shape, color = "black", fill = "lightgrey", size = .5) +
  geom_sf(data = prov.pop.case.npafp, color = "black", aes(fill = cats)) +
  geom_text(data = prov.2npafp, aes(x=min(ctcoord$X), y = min(ctcoord$Y)+adjy,
                                    label = labs), size=3, check_overlap = TRUE,
            hjust = 0)+
  scale_fill_manual(
    name = "NPAFP rate",
    values = c(
      "No cases (u15pop < 100K)" = "lightgrey",
      "<1" = "#d7191c",
      "1-<2" = "#fdae61",
      "2-<3" = "#a6d96a",
      "3+" = "#1a9641",
      "Missing Pop" = "#2C83C7",
      "Silent (u15pop >= 100K)" = "#5e3c99"
    ), drop = F
  ) +
  ggtitle("NPAFP Rate Annualized - Province") +
  sirfunctions::f.plot.looks("epicurve") +
  facet_wrap(~year, ncol = 4) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )
npafp.maps

# 9.1 NPAFP rate/AFP detection district level (npafp.maps.dist) -----------
distnpafp <- dis.extract

distnpafp$cats <- cut(distnpafp$npafp_rate,
                      breaks = c(-1, 0, 1, 2, 3, 1000), right = F,
                      labels = c(
                        "Zero NPAFP cases", "<1",
                        "1-<2", "2-<3", "3+"
                      )
)

# For those with no cases --> separate out districts with u15pop >100K and <100K

dist.cut <- distnpafp %>%
  mutate(cats = as.character(cats)) %>%
  mutate(cats = case_when(
    npafp_rate==0 & u15pop >= 100000 ~ "Silent (u15pop >= 100K)",
    npafp_rate==0 & u15pop < 100000 & u15pop>0~ "No cases (u15pop < 100K)",
    npafp_rate==0 & u15pop == 0 ~ "Missing Pop",
    T ~ cats
  )) %>%
  filter(year >= year(start_date) & year <= year(end_date)) 


dist.cut$cats <- factor(dist.cut$cats,
                        levels = c(
                          "<1" = "<1",
                          "1-<2" = "1-<2",
                          "2-<3" = "2-<3",
                          "3+" = "3+",
                          "Missing Pop" = "Missing Pop",
                          "No cases (u15pop < 100K)" = "No cases (u15pop < 100K)",
                          "Silent (u15pop >= 100K)" = "Silent (u15pop >= 100K)"
                        )
)

dist.cut = ungroup(dist.cut)

dist.pop.case.npafp <- left_join(dist.shape, dist.cut, by = c("GUID" = "adm2guid"))

# How many districts meet >2 NPAFP?
dist.2npafp = distnpafp %>%
  group_by(year, adm2guid, dist) %>%
  summarize(meet2 = sum(npafp_rate>=2, na.rm = T)) %>%
  ungroup() %>%
  group_by(year) %>%
  summarize(num.meet2 = sum(meet2, na.rm = T),
            len.year = length(year)) %>%
  mutate(labs = paste0(num.meet2, "/", len.year, " (", round(100*num.meet2/len.year,0), "%)",
                       " districts \nwith >= 2 cases of NPAFP \nper 100,000 population"))


# Get coordinates for maps that are plotted
ctcoord = as.data.frame(st_coordinates(ctry.shape))
# Put text at 10% below the minimum X and Y coords for each map
adjy = (range(ctcoord$Y)[1] - range(ctcoord$Y)[2])*.1



npafp.maps.dist <- ggplot() +
  geom_sf(data = ctry.shape, color = "black", fill = NA, size = 1) +
  geom_sf(data = prov.shape, color = "black", fill = "lightgrey", size = .5) +
  #geom_sf(data = ctry.data$dist, color = "black", fill = "lightgrey", size = .5) +
  geom_sf(data = dist.pop.case.npafp |> filter(!is.na(prov)), color = "black", aes(fill = cats)) +
  geom_text(data = dist.2npafp, aes(x=min(ctcoord$X), y = min(ctcoord$Y)+adjy,
                                    label = labs), size = 3, check_overlap = TRUE,
            hjust = 0)+
  scale_fill_manual(
    name = "NPAFP rate",
    values = c(
      "No cases (u15pop < 100K)" = "lightgrey",
      "<1" = "#d7191c",
      "1-<2" = "#fdae61",
      "2-<3" = "#a6d96a",
      "3+" = "#1a9641",
      "Missing Pop" = "#2C83C7",
      "Silent (u15pop >= 100K)" = "#5e3c99"
    ), drop = F
  ) +
  # scale_color_manual(values = sirfunctions::f.color.schemes("para.case"), name = "Case type",
  #                  drop = F) +
  ggtitle("NPAFP Rate Annualized - District") +
  sirfunctions::f.plot.looks("epicurve") +
  facet_wrap(~year, ncol = 4) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )
npafp.maps.dist

# 10. Stool adequacy province (stool.ad.maps) -----------
# Identify all AFP cases by province by year 
allafp = ctry.data$afp.all.2 %>%
  filter(date >= start_date & date <= end_date) %>%
  reframe(group_by(ctry.data$afp.all.2, cdc.classification.all2, 
                   adm1guid, year), freq = n()) %>%
  filter(cdc.classification.all2!="NOT-AFP")

allprov = ctry.data$prov.pop[, c("adm1guid", "year", "prov")] %>%
  filter(year>=year(start_date) & year <= year(end_date))

all.prov.afp = left_join(allprov, allafp) %>%
  group_by(year, adm1guid, prov) %>%
  summarize(allafp = sum(freq, na.rm = T))

stoolad.p = left_join(all.prov.afp, pstool, by = c("prov" = "prov", "year" = "year",
                                                   "adm1guid" = "adm1guid"))

stoolad.p = stoolad.p %>%
  tibble() %>%
  mutate(prop.cat = case_when(
    allafp == 0 ~ "Zero AFP cases",
    allafp!=0 & per.stool.ad < 40 ~ "<40%", 
    allafp!=0 & per.stool.ad >= 40 & per.stool.ad < 60 ~ "40-59%", 
    allafp!=0 & per.stool.ad >= 60 & per.stool.ad < 80 ~  "60-79%", 
    allafp!=0 & per.stool.ad >= 80 ~  "80%+"
  )) %>%
  mutate(prop.cat = factor(prop.cat,
                           levels = c("Zero AFP cases", "<40%", "40-59%", 
                                      "60-79%", "80%+"))) 

stoolad.nums.p = stoolad.p %>%
  group_by(year, adm1guid, prov) %>%
  summarize(meet.stool = sum(per.stool.ad>=80)) %>%
  ungroup() %>%
  group_by(year) %>%
  summarize(num.meet.stool = sum(meet.stool),
            len.year = length(year)) %>%
  mutate(labs = paste0(num.meet.stool, "/", len.year, " (", 
                       round(100*num.meet.stool/len.year,0), "%)",
                       " provinces with >= 80% stool adequacy"))

stool.map.p <- left_join(prov.shape, stoolad.p, by = c("GUID" = "adm1guid"))

# Get coordinates for maps that are plotted
ctcoord = as.data.frame(st_coordinates(ctry.shape))
# Put text at 10% below the minimum X and Y coords for each map
adjy = (range(ctcoord$Y)[1] - range(ctcoord$Y)[2])*.1


stool.ad.maps <- ggplot() +
  geom_sf(data = ctry.shape, color = "black", fill = NA, size = 1) +
  geom_sf(data = prov.shape, color = "black", fill = "lightgrey", size = .5) +
  geom_sf(data = stool.map.p, color = "black", aes(fill = prop.cat)) +
  geom_text(data = stoolad.nums.p, aes(x=min(ctcoord$X), y = min(ctcoord$Y)+adjy,
                                       label = labs), check_overlap = TRUE,
            hjust = 0)+
  scale_fill_manual(
    name = "Stool Adequacy",
    values = c(
      "Zero AFP cases" = "lightgrey",
      "<40%" = "#fdae61",
      "40-59%" = "#ffffbf",
      "60-79%" = "#abd9e9",
      "80%+" = "#2c7bb6",
      "Unable to Assess" = "white"
    ), drop = F
  ) +
  ggtitle("Stool Adequacy - Province") +
  sirfunctions::f.plot.looks("epicurve") +
  facet_wrap(~year, ncol = 4) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )
stool.ad.maps

# 10.1. Stool adequacy district (stool.ad.maps.dist) -----------
# Identify all AFP cases by province by year 
allafp.d = ctry.data$afp.all.2 %>%
  filter(date >= start_date & date <= end_date) %>%
  reframe(group_by(ctry.data$afp.all.2, cdc.classification.all2, 
                   adm2guid, year), freq = n()) %>%
  filter(cdc.classification.all2!="NOT-AFP")

alldist = ctry.data$dist.pop[, c("adm2guid", "year", "prov", "dist")] %>%
  filter(year>=year(start_date) & year <= year(end_date))

all.dist.afp = left_join(alldist, allafp.d) %>%
  group_by(year, adm2guid, prov, dist) %>%
  summarize(allafp = sum(freq, na.rm = T))

stoolad.d = left_join(all.dist.afp, dstool, by = c("prov" = "prov", "year" = "year",
                                                   "adm2guid" = "adm2guid",
                                                   "dist" = "dist"))

stoolad.d = stoolad.d %>%
  tibble() %>%
  mutate(prop.cat = case_when(
    allafp == 0 ~ "Zero AFP cases",
    allafp!=0 & per.stool.ad < 40 ~ "<40%", 
    allafp!=0 & per.stool.ad >= 40 & per.stool.ad < 60 ~ "40-59%", 
    allafp!=0 & per.stool.ad >= 60 & per.stool.ad < 80 ~  "60-79%", 
    allafp!=0 & per.stool.ad >= 80 ~  "80%+"
  )) %>%
  mutate(prop.cat = factor(prop.cat,
                           levels = c("Zero AFP cases", "<40%", "40-59%", "60-79%", "80%+")))

stoolad.nums.d = stoolad.d %>%
  group_by(year, adm2guid, dist) %>%
  summarize(meet.stool = sum(per.stool.ad>=80)) %>%
  ungroup() %>%
  group_by(year) %>%
  summarize(num.meet.stool = sum(meet.stool),
            len.year = length(year)) %>%
  mutate(labs = paste0(num.meet.stool, "/", len.year, " (", 
                       round(100*num.meet.stool/len.year,0), "%)",
                       " districts with >= 80% stool adequacy"))

stool.map.d <- left_join(dist.shape, stoolad.d, by = c("GUID" = "adm2guid"))

stool.ad.maps.dist <- ggplot() +
  geom_sf(data = ctry.shape, color = "black", fill = NA, size = 1) +
  geom_sf(data = prov.shape, color = "black", fill = "lightgrey", size = .5) +
  geom_sf(data = dist.shape, color = "black", fill = "lightgrey", size = .5) +
  geom_sf(data = stool.map.d, color = "black", aes(fill = prop.cat)) +
  geom_text(data = stoolad.nums.d, aes(x=min(ctcoord$X), y = min(ctcoord$Y)+adjy,
                                       label = labs), check_overlap = TRUE,
            hjust = 0)+
  scale_fill_manual(
    name = "Stool Adequacy",
    values = c(
      "Zero AFP cases" = "lightgrey",
      "<40%" = "#fdae61",
      "40-59%" = "#ffffbf",
      "60-79%" = "#abd9e9",
      "80%+" = "#2c7bb6",
      "Unable to Assess" = "white"
    ), drop = F
  ) +
  ggtitle("Stool Adequacy - District") +
  sirfunctions::f.plot.looks("epicurve") +
  facet_wrap(~year, ncol = 4) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )
stool.ad.maps.dist

# 11. Main issues with stool adequacy: list (inad.tab.flex) -----------
# % stool adequacy
cstool$per.stool.ad
# Cases with adequate stool
cstool$num.adj.w.miss
# Cases with inadequate stools
cstool$num.inadequate

stool.sub = cstool[, c("year", "num.adj.w.miss", "num.inadequate", "per.stool.ad", "afp.cases")]

# Late collection (%) among inadequate
inads <- stool.data %>%
  filter(date >= start_date & date <= end_date) %>%
  filter(adequacy.final == "Inadequate")

# Timeliness
late.inads = inads %>%
  mutate(timelystool = case_when(ontostool1 > 13 | 
                                   ontostool1 < 0 | 
                                   is.na(stool1tostool2) == T |
                                   ontostool2 > 14 | 
                                   ontostool2 < 1 | 
                                   stool1tostool2 < 1 ~ "Not Timely",
                                 T ~ "Timely"))


late.stool = count(late.inads,year, timelystool) %>%
  filter(timelystool == "Not Timely")
# variables exclude bad dates as 77 or Unable to Assess
# Missing dates treated as absence of collection of stool as there is no variable
# that specifies stool was not collected

# No stool/one stool/one stool among inadequate
count(inads, stoolmissing, stool1missing, stool2missing)

stool.miss.any = summarize(group_by(inads, year), 
                           stoolmiss = sum(stoolmissing, stool1missing, 
                                           stool2missing, na.rm = T))


# Poor condition among inadequate
count(inads, stool.1.condition, stool.2.condition)

cond.poor = filter(inads, stool.1.condition == "Poor"|
                     stool.2.condition == "Poor")

yrs = as.data.frame(seq(year(start_date), year(end_date),1))
names(yrs) = "year"

cond.poor.num = count(cond.poor, year)
cond.poor.num = left_join(yrs, cond.poor.num, by = c("year"="year")) %>%
  mutate(across(c(n), ~replace_na(.x, 0)))

# ALL AFP
afps.all = ctry.data$afp.all.2 %>%
  filter(date>=start_date & date<=end_date)

# 1 stool within 14 days of onset (+condition)
good.cond.1 = count(afps.all, (ontostool1<=14|ontostool2<=14) & 
                      (stool.1.condition=="Good"|is.na(stool.1.condition)) &
                      (stool.2.condition=="Good"|is.na(stool.2.condition)), year)

colnames(good.cond.1)[1] = "conds"
good.cond.1 = good.cond.1 %>%
  filter(conds == TRUE)
# 2 stools within 21 days of onset (+condition)
good.cond.2 = count(afps.all, stool1missing == 0 &
                      stool2missing == 0 &
                      ontostool2<=21 & 
                      (stool.1.condition=="Good"|is.na(stool.1.condition)) &
                      (stool.2.condition=="Good"|is.na(stool.2.condition)), year)
colnames(good.cond.2)[1] = "conds"
good.cond.2 = good.cond.2 %>%
  filter(conds == TRUE)

# Time to lab
# !!! daysstooltolab is not a variable in afp.all.2, had to recreate it
medi_lab = summarize(group_by(ctry.data$afp.all.2, year),
                     medi = median(ctry.data$afp.all.2$daysstooltolab, na.rm = T))

# Bind together tables
allinadstool = left_join(stool.sub, late.stool, by = "year") %>%
  select(-timelystool) %>%
  rename("timelystool" = "n") %>%
  left_join(stool.miss.any, by = "year") %>%
  left_join(cond.poor.num, by = "year") %>%
  rename("cond.poor.num" = "n") %>%
  left_join(good.cond.1, by = "year") %>%
  select(-conds) %>%
  rename("good.cond.1" = "n") %>%
  left_join(good.cond.2, by = "year") %>%
  select(-conds) %>%
  rename("good.cond.2" = "n")



allinadstool$timelyper = paste0(allinadstool$timelystool, 
                                " (", round(100*allinadstool$timelystool/allinadstool$num.inadequate,0),"%)")
allinadstool$poorper = paste0(allinadstool$cond.poor.num, " (", round(100*allinadstool$cond.poor.num/allinadstool$num.inadequate,0),"%)")
allinadstool$missingper = paste0(allinadstool$stoolmiss, " (", round(100*allinadstool$stoolmiss/allinadstool$num.inadequate,0),"%)")
allinadstool$good.cond.1per = paste0(round(100*allinadstool$good.cond.1/allinadstool$afp.cases,0),"% (",allinadstool$good.cond.1, "/",allinadstool$afp.cases, " cases)")
allinadstool$good.cond.2per = paste0(round(100*allinadstool$good.cond.2/allinadstool$afp.cases,0),"% (",allinadstool$good.cond.2, "/",allinadstool$afp.cases, " cases)")

allinadstool$per.stool.ad = round(allinadstool$per.stool.ad, 1)


#Among inadequate cases
#Among all AFP cases
#Transport to the lab (median days)
#NPENT (%)


inad.tab = as.data.frame(t(allinadstool)) %>%
  row_to_names(row_number = 1) %>%
  rownames_to_column("type") %>%
  filter(type %in% c("num.adj.w.miss", "num.inadequate", "per.stool.ad",
                     "timelyper", "missingper","poorper", "good.cond.1per",
                     "good.cond.2per")) %>%
  mutate(type = case_when(
    type == "num.adj.w.miss" ~ "Cases with adequate stools",
    type == "num.inadequate" ~ "Cases with inadequate stools",
    type == "per.stool.ad" ~ "Stool adequacy*",
    type == "afp.cases" ~ "",
    type == "timelyper" ~ "Late collection (%)",
    type == "missingper" ~ "No Stool/one stool",
    type == "poorper" ~ "Poor condition",
    type == "good.cond.1per" ~ "1 stool within 14 days of onset (+ condition)",
    type == "good.cond.2per" ~ "2 stools within 21 days of onset (+ condition)",
    FALSE ~ type
  ))

inad.tab <- inad.tab[c(3, 1, 2, 4, 6, 5, 7, 8), ] # Reorder the table to be in the correct order

inad.tab$sub = c("","","","Among Inadequate Cases", "Among Inadequate Cases",
                 "Among Inadequate Cases",
                 "Among All Cases", "Among All Cases")

inad.tab.flex.a = as_grouped_data(inad.tab, groups = c("sub"))
inad.tab.flex <- flextable(inad.tab.flex.a) %>%
  theme_booktabs() %>%
  bold(bold = TRUE, part = "header") %>%
  set_header_labels(
    type = "",
    sub = ""
  ) %>%
  add_footer_row(top = F, "*Pending included\n**Stool adequacy defined as per Certification Indicator, i.e., 2 stools collected at least 24h apart AND ≤14d of onset AND received in good condition at a WHO-accredited laboratory (missing condition assumed good)", 
                 colwidths = ncol(inad.tab)) %>%
  autofit()

inad.tab.flex

# 13. 60-day follow-up examinations - Table -----------
# Identify inadequate cases --------------------------------------

stool.data.inad = stool.data %>%
  mutate(stl.adeq.02 = case_when(bad.stool1 == "data entry error" | 
                                   bad.stool1 == "date before onset" | 
                                   bad.stool1 == "date onset missing" ~ 77,
                                 bad.stool2 == "data entry error" | 
                                   bad.stool2 == "date before onset" | 
                                   bad.stool2 == "date onset missing" ~ 77,
                                 ontostool1 <= 13 & ontostool1 >= 0 & ontostool2 <= 14 & 
                                   ontostool2 >= 1 & stool1tostool2 >= 1 & 
                                   is.na(stool1tostool2) == F & 
                                   (stool.1.condition =="Good" | is.na(stool.1.condition)) & 
                                   (stool.2.condition =="Good" | is.na(stool.2.condition)) ~ 1,
                                 ontostool1 > 13 | ontostool1 < 0 | ontostool2 > 14 | 
                                   ontostool2 < 1 | stool1tostool2 < 1 | 
                                   is.na(stool1tostool2) == T| stool.1.condition == "Poor" |
                                   stool.2.condition == "Poor" ~ 0))

cases.need60day <- stool.data.inad |>
  as_tibble() |>
  # filter onset to be >120 days from system date
  filter(date <= as_date(ifelse(end_date>Sys.Date()-120,(Sys.Date() - 120),
                                end_date))) |>
  filter(date>=start_date)|>
  mutate(need60day.v2 = ifelse(adequacy.final2 !="Adequate", 1, 0)) |>
  filter(need60day.v2 == 1 | cdc.classification.all2 == "COMPATIBLE") |>
  filter(age.months<60|is.na(age.months))|> # only children under 5 or missing age
  mutate(
    got60day =
      case_when(
        need60day.v2 == 1 & is.na(followup.date) == F ~ 1,
        need60day.v2 == 1 & is.na(followup.date) == T & is.na(followup.findings) == F ~ 1,
        # If follow up date is missing, but findings are recorded, counts as getting follow up
        need60day.v2 == 1 & is.na(followup.date) == T & is.na(followup.findings) == T ~ 0,
        
        need60day.v2 == 0 ~ 99
      ),
    timeto60day = followup.date - date,
    ontime.60day =
      case_when(
        need60day.v2 == 0 ~ 99, # excluded timely cases
        need60day.v2 == 1 & timeto60day >= 60 & timeto60day <= 90 ~ 1,
        (need60day == 1 & timeto60day < 60 | timeto60day > 90 | is.na(timeto60day) == T) ~ 0
      )
  ) |>
  # note if variables are all missing then this definition needs to be adjusted
  
  mutate(
    pot.compatible = ifelse(
      (followup.findings == "Residual weakness/paralysis" |
         followup.findings == "Died before follow-up" |
         followup.findings == "Lost to follow-up" |
         (is.na(followup.date)&followup.findings!= "No residual weakness/paralysis")) &
        (doses.total < 3 | is.na(doses.total) == T) &
        (classification %in% c("Discarded", "Pending") | is.na(classification) == T), 1, 0
    ),
    pot.compatible = ifelse(is.na(pot.compatible) == T, 0, pot.compatible),
    hot.case.no.review = ifelse(hot.case == 1 &
                                  is.na(followup.date) == T &
                                  (cdc.classification.all2 == "PENDING" | cdc.classification.all2 == "LAB PENDING"), 1, 0)
  ) |>
  mutate(
    missing.fu.date = ifelse(
      need60day.v2 == 1 & is.na(followup.date) == T & is.na(followup.findings) == F,
      1,0
    )
  ) |>
  select(
    epid, year, age.months, hot.case, hot.case.no.review, got60day, ontime.60day, 
    ctry, prov, dist, date, date.notify, date.invest, datestool1, datestool2, 
    stool.1.condition, stool.2.condition,adequacy.03, paralysis.asymmetric, 
    paralysis.rapid.progress, paralysis.onset.fever, pot.compatible, doses.total, 
    timeto60day, followup.date,
    classification,
    cdc.classification.all2,
    missing.fu.date,
    adm1guid, adm2guid
  )

cases.need60day |>
  rename(
    Year = year,
    "age in months" = age.months,
    "Hot Case" = hot.case,
    "Hot Case, no review" = hot.case.no.review,
    "Complete 60 day" = got60day,
    "60-day ontime" = ontime.60day,
    Country = ctry,
    Province = prov,
    District = dist,
    "Adequate stool missing=good" = adequacy.03,
    "total OPV doses" = doses.total,
    "Potentially Compatible" = pot.compatible,
    "Missing followup date but have findings" = missing.fu.date
  ) |>
  write_csv( file.path(excel_output_path, paste0(Sys.Date(), "_", str_to_lower(country), "_60day_followup.csv")))

# Set up table by year
comp.by.year <- cases.need60day |>
  group_by(year) |>
  summarise(
    inadequate = n(),
    got60day = sum(got60day == 1, na.rm = T),
    ontime60day = sum(ontime.60day == 1, na.rm = T),
    compatible = sum(cdc.classification.all2 == "COMPATIBLE"),
    pot.compatible = sum(pot.compatible == 1, na.rm = T),
    missing.fu.date = sum(missing.fu.date == 1, na.rm = T)
  ) |>
  mutate(
    per.got60 = round(got60day / inadequate * 100),
    per.ontime60day = round(ontime60day / inadequate * 100),
    per.comp = round(compatible / inadequate * 100),
    per.pot.comp = round(pot.compatible / inadequate * 100),
    per.got60.2 = paste(got60day, " ", "(", per.got60, "%", ")", sep = ""),
    per.ontime60day.2 = paste(ontime60day, " ", "(", per.ontime60day, "%", ")", sep = ""),
    per.comp.2 = paste(compatible, " ", "(", per.comp, "%", ")", sep = ""),
    per.pot.comp.2 = paste(pot.compatible, " ", "(", per.pot.comp, "%", ")", sep = ""),
    per.missing.fu.date = paste(round(missing.fu.date/inadequate * 100), " ", "(", 
                                missing.fu.date, "%", ")", sep = "")
  ) |>
  select(year, inadequate, per.got60.2, per.ontime60day.2, per.comp.2, per.pot.comp.2, 
         per.missing.fu.date) |>
  mutate(year = as.character(year))


# flex table

tab.60d <- comp.by.year |>
  flextable() |>
  theme_booktabs() |>
  bold(bold = TRUE, part = "header") |>
  set_header_labels(
    year = "Year",
    inadequate = "No. inadequate cases",
    per.got60.2 = "Recorded 60-day follow-up",
    per.ontime60day.2 = "Recorded 60-day ontime",
    per.comp.2 = "Compatible cases",
    per.pot.comp.2 = "Potentially compatible cases",
    per.missing.fu.date = "No. Missing follow up date with findings"
  ) |>
  align(j = 2:7, align = "center", part = "all") |>
  align(j = 1:1, align = "left", part = "all") |>
  fontsize(size = 11, part = "all") |>
  width(j = 1:7, width = 2) #|>

tab.60d



# Timeliness --------------------------------
## 19. National trend in timeliness of AFP case detection -----------
# !!! Modified this to work with raw.data output
ctry.time = f.timely.01(afp.data = ctry.data$afp.all.2,
                        admin.data = ctry.data$ctry.pop,
                        start.date = start_date, 
                        end.date = end_date,
                        spatial.scale = "ctry"
)
tot.time = summarize(group_by(ctry.time, year), tot = sum(medi, na.rm = T))

# Optional code to split into multiple figures when there are a large number of provinces
# NOTE you will need to adjust the output code accordingly to print all the slides
afp.prov.year.lab = count(ctry.data$afp.all.2, prov, year) %>%
  filter(year>=year(start_date) & year<=year(end_date))
afp.prov.year.lab$labs = paste0(afp.prov.year.lab$year,
                                " (N=",afp.prov.year.lab$n,")")

prov.time = f.timely.01(ctry.data$afp.all.2, ctry.data$prov.pop, start_date,
                        end_date, "prov")
prov.time.2 = left_join(prov.time, afp.prov.year.lab, by = c("year", "prov"))

tot.time.p = summarize(group_by(prov.time.2, year), tot = sum(medi, na.rm = T))
# make groups that are 5 provinces each ---
prov5 = as.data.frame(unique(prov.time.2$prov))
# for each 5 provinces, add a grouping variable ---
prov5 = prov5 %>%
  group_by(x = ceiling(row_number()/5)) %>%
  rename("prov" = "unique(prov.time.2$prov)")#%>%


prov.time.2 = left_join(prov.time.2, prov5, by = c("prov"))

## 21. Timeliness across provinces - maps -----------

## Cases notified within 7 days of onset
## Investigated within 2 days of notification
## Collected within 3 days of investigation
## Shipped to lab within 3 days of collection

long.timely <- ctry.data$afp.all.2 %>%
  select(
    epid,
    noti.7d.on,
    inv.2d.noti,
    coll.3d.inv,
    ship.3d.coll,
    year,
    prov, 
    adm1guid
  ) %>%
  pivot_longer(!c(epid, year, prov, adm1guid), names_to = "type", values_to = "value") %>%
  group_by(year, type, prov,adm1guid) %>%
  summarize(prop = sum(value, na.rm = T) / n()) %>%
  ungroup() %>%
  filter(year >= year(start_date) & year <= year(end_date)) %>%
  complete(year, prov, type)

long.timely

for(i in 1:nrow(long.timely)){
  if(is.na(long.timely$adm1guid[i])){
    long.timely$adm1guid[i] = long.timely$adm1guid[which(long.timely$prov==long.timely$prov[i])][1]
  }
}


all.case <- summarize(group_by(ctry.data$afp.all.2, prov, 
                               year,adm1guid), case.num = n()) %>%
  ungroup() %>%
  filter(year >= year(start_date) & year <= year(end_date)) %>%
  complete(year, prov, fill = list(case.num=0))

for(i in 1:nrow(all.case)){
  if(is.na(all.case$adm1guid[i])){
    all.case$adm1guid[i] = all.case$adm1guid[which(all.case$prov==all.case$prov[i])][1]
  }
}


long.timely$prop <- cut(long.timely$prop,
                        breaks = c(-1, 0.2, 0.5, 0.8, 0.9, 1.1, 1.2),
                        right = F, 
                        labels = c("<20%", "20-49%", "50-79%", 
                                   "80-89%", "90-100%", NA)
)
long.timely$prop = fct_na_value_to_level(long.timely$prop, "Missing")# missing date data

time.map <- left_join(prov.shape, long.timely, by = c("GUID" = "adm1guid"))
time.map <- full_join(time.map, all.case, by = c("GUID" = "adm1guid", "year" = "year"))

time.map <- time.map %>%
  mutate(prop = as.character(prop)) %>%
  mutate(prop = ifelse(case.num==0,"No AFP cases",prop)) %>%
  mutate(prop = factor(prop, 
                       levels = c("<20%", "20-49%", "50-79%", 
                                  "80-89%", "90-100%", "No AFP cases",
                                  "Missing")))


# Flag provinces with less than 5 AFP cases reported
low.case.prov <- time.map %>%
  group_by(year, ADM1_NAME) %>%
  filter(case.num <= 5)

# noti.7d.on

mapt1 <- ggplot() +
  geom_sf(data = ctry.shape, color = "black", fill = NA, size = 1) +
  geom_sf(data = prov.shape, color = "black", fill = "lightgrey", size = .5) +
  geom_sf(data = filter(time.map, type == "noti.7d.on"), color = "black", aes(fill = prop)) +
  geom_sf(data = st_centroid(filter(low.case.prov, type == "noti.7d.on")), pch = 4,
          size = 4) +
  scale_fill_manual(
    name = "Proportion",
    values = f.color.schemes("mapval"),
    drop = F
  ) +
  # scale_color_manual(values = sirfunctions::f.color.schemes("para.case"), name = "Case type",
  #                  drop = F) +
  ggtitle("Proportion of cases with notification within 7 days of onset") +
  sirfunctions::f.plot.looks("epicurve") +
  #labs(caption = "Provinces marked by an X have reported 5 or less AFP cases")+
  facet_wrap(~year, ncol = 4) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    plot.caption = element_text(hjust = 0))

mapt1

# inv.2d.noti
mapt2 <- ggplot() +
  geom_sf(data = ctry.shape, color = "black", fill = NA, size = 1) +
  geom_sf(data = prov.shape, color = "black", fill = "lightgrey", size = .5) +
  geom_sf(data = filter(time.map, type == "inv.2d.noti"), color = "black", aes(fill = prop)) +
  geom_sf(data = st_centroid(filter(low.case.prov, type == "inv.2d.noti")), pch = 4,
          size = 4) +
  scale_fill_manual(
    name = "Proportion",
    values = f.color.schemes("mapval"),
    drop = F
  ) +
  # scale_color_manual(values = sirfunctions::f.color.schemes("para.case"), name = "Case type",
  #                  drop = F) +
  ggtitle("Proportion of cases with investigation within 2 days of notification") +
  sirfunctions::f.plot.looks("epicurve") +
  #labs(caption = "Provinces marked by an X have reported 5 or less AFP cases")+
  facet_wrap(~year, ncol = 4) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    plot.caption = element_text(hjust = 0))

# coll.3d.inv
mapt3 <- ggplot() +
  geom_sf(data = ctry.shape, color = "black", fill = NA, size = 1) +
  geom_sf(data = prov.shape, color = "black", fill = "lightgrey", size = .5) +
  geom_sf(data = filter(time.map, type == "coll.3d.inv"), color = "black", aes(fill = prop)) +
  geom_sf(data = st_centroid(filter(low.case.prov, type == "coll.3d.inv")), pch = 4,
          size = 4) +
  scale_fill_manual(
    name = "Proportion",
    values = f.color.schemes("mapval"),
    drop = F
  ) +
  # scale_color_manual(values = sirfunctions::f.color.schemes("para.case"), name = "Case type",
  #                  drop = F) +
  ggtitle("Proportion of cases with collection within 3 days of investigation") +
  sirfunctions::f.plot.looks("epicurve") +
  #labs(caption = "Provinces marked by an X have reported 5 or less AFP cases")+
  facet_wrap(~year, ncol = 4) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    plot.caption = element_text(hjust = 0))

# ship.3d.coll
mapt4 <- ggplot() +
  geom_sf(data = ctry.shape, color = "black", fill = NA, size = 1) +
  geom_sf(data = prov.shape, color = "black", fill = "lightgrey", size = .5) +
  geom_sf(data = filter(time.map, type == "ship.3d.coll"), color = "black", aes(fill = prop)) +
  geom_sf(data = st_centroid(filter(low.case.prov, type == "ship.3d.coll")), pch = 4,
          size = 4) +
  scale_fill_manual(
    name = "Proportion",
    values = f.color.schemes("mapval"),
    drop = F
  ) +
  # scale_color_manual(values = sirfunctions::f.color.schemes("para.case"), name = "Case type",
  #                  drop = F) +
  ggtitle("Proportion of stool shipped to lab within 3 days of collection") +
  sirfunctions::f.plot.looks("epicurve") +
  #labs(caption = "Provinces marked by an X have reported 5 or less AFP cases")+
  facet_wrap(~year, ncol = 4) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    plot.caption = element_text(hjust = 0))

mapt4

mapt_all <- ggarrange(mapt1, mapt2, mapt3, mapt4, ncol = 2, nrow = 2, 
                      common.legend = TRUE, legend="bottom") 

mapt_all = annotate_figure(mapt_all, bottom = text_grob("Provinces marked by an X have reported 5 or less AFP cases",
                                                        hjust = 1))


# ES --------------------------------------------------------------------------
# What is the first date each reported to POLIS? ----
es.data.earlidat <- ctry.data$es %>%
  group_by(site.name) %>%
  summarize(early.dat = min(collect.date)) %>%
  ungroup()

# Create ES dates ----------------
# Usually want to display the last calendar year of data OR last rolling year
# Choose which you want and use to filter
start_date_es = as_date("2023-01-01")
end_date_es = end_date

# NOTE THAT DATE MAY NEED TO BE MANUALLY ADJUSTED DEPENDING ON ANALYSIS DATES
es.data <- ctry.data$es %>%
  filter(between(collect.date, start_date_es, end_date_es)) |>
  left_join(es.data.earlidat, by = c("site.name" = "site.name")) |>
  mutate(nvaccine.2 = NA)




# if ev.detect is yes, then create composite variable of other yes variables

es.data <- es.data %>%
  mutate(vaccine.1.3 = case_when(
    is.na(vaccine.1) == F & vaccine.1 == "Yes" ~ "Sabin 1/3"
  )) %>%
  mutate(vaccine.3.1 = case_when(
    is.na(vaccine.3) == F & vaccine.3 == "Yes" ~ "Sabin 1/3"
  )) %>%
  mutate(vaccine.2 = case_when(
    is.na(vaccine.2) == F & vaccine.2 == "Yes" ~ "Sabin 2"
  )) %>%
  mutate(vdpv.1 = case_when(
    is.na(vdpv.1) == F & vdpv.1 == "Yes" ~ "VDPV1"
  )) %>%
  mutate(vdpv.2 = case_when(
    is.na(vdpv.2) == F & vdpv.2 == "Yes" ~ "VDPV2"
  )) %>%
  mutate(vdpv.3 = case_when(
    is.na(vdpv.3) == F & vdpv.3 == "Yes" ~ "VDPV3"
  )) %>%
  mutate(wild.1 = case_when(
    is.na(wild.1) == F & wild.1 == "Yes" ~ "WPV1"
  )) %>%
  mutate(wild.3 = case_when(
    is.na(wild.3) == F & wild.3 == "Yes" ~ "WPV3"
  )) %>%
  mutate(nvaccine.2 = case_when(
    is.na(nvaccine.2) == F & nvaccine.2 == "Yes" ~ "nOPV2"
  )) %>%
  unite("all_dets", c(
    "vaccine.1.3", "vaccine.3.1", "vaccine.2",
    "vdpv.1", "vdpv.2", "vdpv.3", "wild.1",
    "wild.3", "nvaccine.2"
  ),
  na.rm = TRUE, remove = FALSE, sep = " and "
  ) %>%
  mutate(all_dets = case_when(
    all_dets == "Sabin 1" ~ "Sabin 1 or Sabin 3",
    all_dets == "Sabin 3" ~ "Sabin 1 or Sabin 3",
    all_dets == "Sabin 1 and Sabin 3" ~ "Sabin 1 or Sabin 3",
    TRUE ~ all_dets
  ))

es.data$all_dets = gsub("Sabin 1/3 and Sabin 1/3", "Sabin 1/3", es.data$all_dets) 


es.data.long <- es.data %>%
  select(site.name, ADM1_NAME, collect.date, early.dat, ev.detect, all_dets) %>%
  mutate(ev.detect = as.character(ev.detect)) %>%
  #  filter(collect.date>=start_date & collect.date<=end_date) %>% # Change to only most recent year
  #filter(year(collect.date) == year(end_date)) %>%
  mutate(all_dets = case_when(
    all_dets == "" & ev.detect == "1" ~ "NPEV only",
    all_dets == "" & ev.detect == "0" ~ "No EV isolated",
    TRUE ~ all_dets
  )
  )

count(es.data, all_dets)
count(es.data.long, all_dets)

# SIA ----
# sias
sias = ctry.data$sia %>%
  filter(status == "Done") %>%
  filter(yr.sia>=year(start_date_es) & yr.sia<=year(end_date_es)) %>%
  filter(province %in% es.data.long$ADM1_NAME)

sias$activity.start.date = as.Date(sias$activity.start.date)
sias$activity.end.date = as.Date(sias$activity.end.date)
dim(sias)

count(sias, yr.sia, province, activity.start.date, activity.end.date, vaccine.type)
minsy = count(sias, yr.sia, province, activity.start.date, activity.end.date, vaccine.type) #%>%

colnames(minsy)[colnames(minsy)== "province"] <- "ADM1_NAME"

es.data.long$year = year(es.data.long$collect.date)

## 22.1 ES sites & detection (es.site.det - THINK ABOUT HOW TO WRAP BY PROVINCE) -----------

new.site <- es.data.long %>%
  filter(early.dat >= min(collect.date) & early.dat <= max(collect.date)) %>%
  distinct(site.name, early.dat)


minny = min(es.data.long$collect.date)-7
maxy = max(es.data.long$collect.date)+7

#es.data.long$all_dets = factor(es.data.long$all_dets)

es.site.det <- ggplot() +
  geom_point(
    data = es.data.long |>
      arrange(ADM1_NAME),
    aes(x = collect.date, y = site.name, col = all_dets), pch = 19,
    size = 3
  ) +
  geom_rect(data = minsy, 
            aes(xmin = activity.start.date, xmax = activity.end.date, ymin = 0, ymax = Inf, 
                fill = vaccine.type), alpha =0.5)+
  geom_point(
    data = es.data.long |>
      arrange(ADM1_NAME),
    aes(x = collect.date, y = site.name, col = all_dets), pch = 19,
    size = 3
  ) +
  geom_point(
    data = es.data.long |>
      arrange(ADM1_NAME),
    aes(x = collect.date, y = site.name), fill = NA, pch = 21,
    size = 3
  )+
  #geom_point(
  #  data = subset(es.data.long, early.dat>=start_date & early.dat<=end_date &
  #                  ADM1_NAME == "COPPERBELT"   ), 
  #  aes(x = early.dat, y = site.name), pch = 5,
  #  size = 4
  #) +
  xlab(label = "") +
  ylab(label = "Detection Sites") +
  scale_x_date(limits = c(start_date_es, end_date_es))+
  #scale_y_discrete(limits = factor(0,4))+
  #scale_y_discrete(limits = c("CHIPATA TREATMENT PLANT"))+
  #scale_y_continuous(limits = c(0,1.5))+
  scale_fill_manual(name = "SIAs",
                    values = c(
                      "nOPV2" = "blue", 
                      "bOPV" = "coral1",
                      "mOPV2" = "purple"))+
  scale_color_manual(
    name = "ES detections",
    values = c(
      "No EV isolated" = "#f2f2f2",
      "NPEV only" = "darkgrey",
      "VDPV2" = "darkred",
      "Sabin 1" = brewer_pal(palette = "Set1")(9)[1],
      "Sabin 2" = brewer_pal(palette = "Set1")(9)[8],
      "Sabin 1/Sabin 3" = brewer_pal(palette = "Set1")(9)[2],
      "Sabin 3" = brewer_pal(palette = "Set1")(9)[3],
      "Sabin 1/Sabin 3/VDPV2" = brewer_pal(palette = "Set1")(9)[4],
      "Sabin 1/VDPV2" = brewer_pal(palette = "Set1")(9)[5],
      "Sabin 3/VDPV2" = brewer_pal(palette = "Set1")(9)[6],
      "Sabin 1 or Sabin 3" = brewer_pal(palette = "Set1")(9)[6],
      "Sabin 1/3" = brewer_pal(palette = "Set1")(9)[2],
      "Sabin 1/3 and VDPV2"  = brewer_pal(palette = "Set1")(9)[5]
    )) +
  facet_grid(ADM1_NAME~. ,scales = "free_y" , space = "free",
             switch = "y")+
  theme_bw()

#, strip.position = "left"

es.site.det


## 22.1 ES sites & detection map (es.det.map) -----------
# Need to create an EV detection rate
det.rate <- summarize(group_by(es.data.long, site.name),
                      det.rate = 100 * sum(as.numeric(ev.detect), na.rm = TRUE) / n(),
                      samp.num = n()
)

det.rate$cats <- cut(det.rate$det.rate,
                     breaks = c(0, 50, 80, 101), right = F,
                     labels = c("<50%", "50-79%", "80-100%")
)
det.rate$cats <- as.character(det.rate$cats)

det.rate <- det.rate %>%
  mutate(cats = case_when(
    samp.num < 5 ~ "<5 samples",
    TRUE ~ cats
  ))

site.coord <- reframe(group_by(es.data, site.name), lat = lat, lng = lng)
site.coord <- unique(site.coord)

det.rate$cats <- factor(det.rate$cats,
                        levels = c("<50%", "50-79%", "80-100%", "<5 samples")
)

es.data <- left_join(es.data, det.rate, by = c("site.name" = "site.name"))

det.rate <- left_join(det.rate, site.coord)

# The number of NA NAs here shows how many are not displayed on the map because they do not have coordinates in POLIS ----
count(det.rate, lng, lat)

# ES Map of sites ---------
#randomly put points in their districts

es.det.map <- ggplot() +
  geom_sf(data = ctry.shape, color = "black", fill = NA, size = 1) +
  geom_sf(data = prov.shape, color = "black", fill = NA, size = .5) +
  geom_point(
    data = det.rate, 
    aes(x = as.numeric(lng), y = as.numeric(lat), color = cats)
  ) +
  geom_label_repel(
    data = subset(det.rate, site.name!= "OSHIKANGO TREATMENT PLANT"), aes(
      x = as.numeric(lng), y = as.numeric(lat),
      label = site.name, color = cats,
    ), 
    show.legend = FALSE,
    force = 100
  ) +
  ggtitle(paste0("ES detection rate by site: ", format(start_date_es, "%B %Y"),
                 " - ", format(end_date_es, "%B %Y")))+
  scale_color_manual(
    values = c(
      "<50%" = "#FF0000",
      "50-79%" = "#feb24c",
      "80-100%" = "#0070c0",
      "<5 samples" = "black"
    ),
    name = "EV detection rate", drop = F
  ) +
  sirfunctions::f.plot.looks("02") +
  theme(legend.position = "right")

es.det.map
## 23. ES timeliness of sample transport (DONE) -----------
# This looks like it is just date.shipped.to.ref.lab minus collection.date
# Scatter plot with date on x and transport time on y, colored by site
es.data$timely <- difftime(as.Date(es.data$date.received.in.lab, format = "%d/%m/%Y"), 
                           es.data$collect.date,
                           unit = "days")

per.time = es.data %>% count(timely>3) %>%
  rename(c("timely" = `timely > 3`, "n" = "n"))
# The number that are false are the percentage timely 

per.timely.title = paste0(round(100*filter(per.time, timely == FALSE)["n"] /sum(per.time$n),0),
                          "% of samples were shipped to lab within 3 days of collection - \n",
                          format(start_date_es, "%B %Y"),
                          " - ", format(end_date_es, "%B %Y"))

miss.samp = filter(per.time, is.na(timely))

num.miss.capt = paste0(ifelse(dim(miss.samp)[1]==0,0,
                              (miss.samp["n"])),
                       " (", round(100*as.numeric(ifelse(dim(miss.samp)[1]==0,0,
                                                         (miss.samp["n"])))/sum(per.time$n),0),
                       "%) samples were missing date information")

# Timeliness of ES  ----------------------------
# Excludes those with bad data (e.g. negative timeliness) -------------------------------
es.timely <- ggplot() +
  geom_hline(yintercept = 3, color = "dark gray", linetype = "dashed", lwd = 1) +
  geom_point(
    data = filter(es.data,
                  timely>=0), aes(x = collect.date, y = timely, color = site.name),
    alpha = 0.7, position = position_jitter(height = .2, width = 0.5),
    size = 3
  ) +
  scale_y_continuous(labels = number_format(accuracy = 1), 
                     breaks = c(seq(0, max(pretty(es.data$timely)),6))) +
  labs(
    x = "Date of collection", y = "Transport time to lab (days)",
    color = "Site Name"
  ) +
  labs(title = per.timely.title,
       caption = num.miss.capt)+
  # scale_x_date(date_breaks = "2 months", date_labels = "%b-%y", limits = c(start.date.12m, end.date + months(1))) +
  theme_classic() +
  theme(
    text = element_text(size = 16),
    axis.text = element_text(size = 14),
    plot.caption = element_text(hjust = 0)
  )

es.timely
## 24. ES site details (es.table) -----------
# Big table that needs calculating
# Cols = province, district, site name, earliest sample collected in POLIS,
# n samples collected (earliest to analysis date), % EV detected, % good condition
# % arrived within 3 days, days from collection to lab arrival (median + range),
# WPV/VDPV

# ev.pct = ev percent
# condition percent
# transport percent
# median transport days
# DATES MAY NEED ADJUSTMENT DEPENDING ON ANALYSIS PERIOD


# NOTE THAT RIGHT NOW THIS REMOVES SAMPLES MISSING A CONDITION FROM THE CALCULATIONS----

es.tab1 <- es.data %>%
  #filter(year(collect.date) == year(end_date)) %>%
  group_by(site.name, ADM1_NAME, ADM2_NAME) %>%
  reframe(
    early.dat = format(early.dat, format = "%B %d, %Y"), # earliest report to POLIS
    ev.pct = 100 * sum(as.numeric(ev.detect), na.rm = TRUE) / n(), # percent ev detected
    num.spec = n(), # number of specimens
    condition.pct = 100 * sum(sample.condition == "Good", na.rm = T) / n(), # specimens in good condition
    trans.pct = 100 * sum(as.numeric(timely) <= 3, na.rm = TRUE) / n(), # % timely
    med.trans = paste0(
      median(as.numeric(timely), na.rm = T), " (",
      min(as.numeric(timely), na.rm = T), ", ",
      max(as.numeric(timely), na.rm = T), ")"
    ), # med (range)
    num.wpv.or.vdpv = sum(wpv, na.rm = T) + sum(vdpv, na.rm = T)
  ) %>% # WPV/VDPV
  distinct()

es.tab1

es.tab1 = es.tab1 %>%
  arrange(ADM1_NAME, ADM2_NAME, site.name)
es_site_names <- es.tab1 |>
  select(site.name) |> unique() |> pull()
#uncomment and use as necessary to reduce/filter 
# es_site_names_1 <- es_site_names[1:9]
# es_site_names_2 <- es_site_names[10:19]
# es_site_names_3 <- es_site_names[20:28]

es.table <- es.tab1 %>%
  filter(site.name %in% es_site_names) |>
  flextable(col_keys = c(
    "ADM1_NAME", "ADM2_NAME", "site.name", "early.dat", "num.spec",
    "ev.pct", "condition.pct",
    "trans.pct", "med.trans", "num.wpv.or.vdpv"
  )) %>%
  theme_booktabs() %>%
  add_header_lines(values = paste0(format(start_date_es, "%B %Y"),
                                   " - ", format(end_date_es, "%B %Y"))) %>%
  # hline(part="all", border = gray.border ) %>%
  bold(bold = TRUE, part = "header") %>%
  # hline(part = "header", border = std.border) %>%
  align(j = 4:9, align = "center", part = "all") %>%
  align(j = 1:3, align = "left", part = "all") %>%
  # hline(part = "header", border = std.border) %>%
  # hline_bottom(part = "body", border = std.border ) %>%
  colformat_double(j = 5:8, digits = 0, na_str = "NA") %>%
  width(width = 1) %>%
  width(j = 3, width = 2.5) %>%
  width(j = 1:2, width = 1.5) %>%
  # width(j=10, width = .1) %>%
  #add_footer(province = "Red = indicator not met, * and gray = missing data for >25% of samples, NA = data unavailable; Indicator targets: >=50% for EV (NPEV, vaccine, VDPV, or WPV) detection, =>80% for sample condition and transport time. Sites with <6 months of sample collection are labeled as 'new'.") %>%
  #merge_at(j = 1:9, part = "footer") %>%
  fontsize(size = 11, part = "all") %>%
  set_header_labels(
    ADM1_NAME = "Province",
    ADM2_NAME = "District",
    early.dat = "Earliest date reporting to POLIS",
    site.name = "Site name",
    num.spec = "No. samples collected",
    ev.pct = "% detected EV",
    condition.pct = "% good condition",
    trans.pct = "% arriving within 3 days",
    med.trans = "Median lab transport time (d)",
    num.wpv.or.vdpv = "No. VDPV or WPV"
  )

es.table

# Write out to pptx template ------

tempi <- read_pptx(file.path(here(), "desk_review_template.pptx"))

layout_summary(tempi)
layout_properties(x = tempi, layout = "Two Content", master = "1_Office Theme")
layout_properties(x = tempi, layout = "Title and Content", master = "1_Office Theme")
layout_properties(x = tempi, layout = "4_tile", master = "1_Office Theme")

draft_output <- tempi %>%
  # Title slide ----
add_slide(layout = "Title Slide", master = "1_Office Theme") %>%
  ph_with(
    value = paste(ctry.data$name, "DESK REVIEW"),
    location = ph_location_type("ctrTitle")
  ) %>%
  # Table of Contents  ----
add_slide(layout = "Two Content", master = "1_Office Theme") %>%
  ph_with(
    value = "Table of Contents",
    location = ph_location_type("title")
  ) %>%
  # Analysis notes ----
add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
  ph_with(
    value = "Analysis Notes",
    location = ph_location_type("title")
  ) %>%
  ph_with(
    value = assump,
    location = ph_location_type("body")) %>%
  # Reference map of country  ----
add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
  ph_with(
    value = paste("Reference Map 1", start_date, "-", end_date),
    location = ph_location_type("title")
  ) %>%
  ph_with(dml(ggobj = pop.map), location = ph_location_type("body")) %>%
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
  ph_with(
    value = paste("Reference Map 2", start_date, "-", end_date),
    location = ph_location_type("title")
  ) %>%
  ph_with(dml(ggobj = pop.map.provn), location = ph_location_type("body")) %>%
  # Paralytic polio and compatible cases map ----
add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
  ph_with(
    value = paste("Paralytic Polio and Compatible cases", start_date, "-", end_date),
    location = ph_location_type("title")
  ) %>%
  ph_with(dml(ggobj = afp.case.map), location = ph_location_type("body")) %>%
  # Phylogenetic tree ----
add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
  ph_with(
    value = "Phylogenetic Tree",
    location = ph_location_type("title")
  ) %>%
  # Overview of Sample transport and processing ----
add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
  ph_with(
    value = "Overview of Sample Transport & Processing",
    location = ph_location_type("title")
  ) %>%
  # Section header: AFP surveillance ----
add_slide(layout = "Title Slide", master = "1_Office Theme") %>%
  ph_with(value = "AFP Surveillance", location = ph_location_type("ctrTitle")) %>%
  # Epi-curve ----
add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
  ph_with(
    value = paste("Epicurve", start_date, "-", end_date),
    location = ph_location_type("title")
  ) %>%
  ph_with(dml(ggobj = afp.epi.curve1), location = ph_location_type("body")) %>%
  # POLIS screen shot ----
add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
  ph_with(
    value = "Key message: AFP Surveillance Tracking",
    location = ph_location_type("title")
  ) %>%
  # National surveillance indicators table (MISSING 100K NEEDS CODE REWORK) ----
add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
  ph_with(
    value = "National Surveillance Indicators",
    location = ph_location_type("title")
  ) %>%
  ph_with(surv.ind.tab, location = ph_location_type("body")) %>%
  # AFP detections by province ----
# Tile Chart and Table
add_slide(layout = "Two Content", master = "1_Office Theme") %>%
  ph_with(
    value = "AFP Detections by Province and Year",
    location = ph_location_type("title")
  ) %>%
  ph_with(dml(ggobj = afp.dets.prov.year), location = ph_location_label("Content Placeholder 2")) %>%
  ph_with(pop.tab, location = ph_location_label("Content Placeholder 3")) %>%
  # NPAFP rate/AFP detection by province by year (maps) ----
add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
  ph_with(
    value = "NPAFP rate",
    location = ph_location_type("title")
  ) %>%
  ph_with(dml(ggobj = npafp.maps), location = ph_location_type("body")) %>%
  # NPAFP rate/AFP detection by district by year (maps) ----
add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
  ph_with(
    value = "NPAFP rate District",
    location = ph_location_type("title")
  ) %>%
  ph_with(dml(ggobj = npafp.maps.dist), location = ph_location_type("body")) %>%
  # Stool adequacy by province by year (maps) ----
add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
  ph_with(
    value = "Stool Adequacy",
    location = ph_location_type("title")
  ) %>%
  ph_with(dml(ggobj = stool.ad.maps), location = ph_location_type("body")) %>%
  # Stool adequacy by dist by year (maps) ----
add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
  ph_with(
    value = "Stool Adequacy",
    location = ph_location_type("title")
  ) %>%
  ph_with(dml(ggobj = stool.ad.maps.dist), location = ph_location_type("body")) %>%
  # Main issues with stool adequacy table (NOT DONE) ----
add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
  ph_with(
    value = "Main issues with stool adequacy",
    location = ph_location_type("title")
  ) %>%
  ph_with(inad.tab.flex, location = ph_location_type("body")) %>%
  # Virus isolation in/around SIA rounds (table) (NOT DONE) ----
add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
  ph_with(
    value = "Virus isolation in/around SIA rounds",
    location = ph_location_type("title")
  ) %>%
  # 60 day follow up table by year (table) ----
add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
  ph_with(
    value = "60 day follow up",
    location = ph_location_type("title")
  ) %>%
  ph_with(tab.60d, location = ph_location_type("body")) %>%
  # Possible clusters of potential compatibles (table) (NOT DONE) ----
add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
  ph_with(
    value = "Possible clusters of potential compatibles",
    location = ph_location_type("title")
  ) %>%
  # Contact sampling (table) (NOT DONE) ----
add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
  ph_with(
    value = "Contact sampling",
    location = ph_location_type("title")
  ) %>%
  # Possible clusters of under-immunized NPAFP cases (table) (NOT DONE) ----
add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
  ph_with(
    value = "Possible clusters of under-immunized NPAFP cases",
    location = ph_location_type("title")
  ) %>%
  # Key Points on AFP surveillance ----
add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
  ph_with(
    value = "Key Points on AFP Surveillance",
    location = ph_location_type("title")
  ) %>%
  # Section header: Active surveillance ----
add_slide(layout = "Title Slide", master = "1_Office Theme") %>%
  ph_with(value = "Active Surveillance", location = ph_location_type("ctrTitle")) %>%
  # Annual active surveillance visits by priority level (bar graph) (NOT DONE)----
add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
  ph_with(
    value = "Annual active surveillance visits by priority level",
    location = ph_location_type("title")
  ) %>%
  # Active surveillance visits (table) (NOT DONE)----
add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
  ph_with(
    value = "Active surveillance visits",
    location = ph_location_type("title")
  ) %>%
  # Key points on active surveillance ----
add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
  ph_with(
    value = "Key Points on active Surveillance",
    location = ph_location_type("title")
  ) %>%
  # Section header: AFP Surveillance Timeliness ----
add_slide(layout = "Title Slide", master = "1_Office Theme") %>%
  ph_with(value = "AFP Surveillance Timeliness", location = ph_location_type("ctrTitle")) %>%
  # National trend in timeliness of AFP case detection (bar graph) ----
add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
  ph_with(
    value = "National timeliness",
    location = ph_location_type("title")
  ) %>%
  ph_with(dml(ggobj = timely_nation), location = ph_location_type("body")) %>%
  # Timeliness at provincial level (bar graph) ----
add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
  ph_with(
    value = "Timeliness by province and year",
    location = ph_location_type("title")
  ) %>%
  ph_with(dml(ggobj = timely_prov), location = ph_location_type("body")) %>%
  # Timeliness across provinces (multi map) ----
add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
  ph_with(
    value = "Timeliness by province and year",
    location = ph_location_type("title")
  ) %>%
  ph_with(dml(ggobj = mapt_all), location = ph_location_type("body")) %>%
  # Key points on AFP Surveillance Timeliness ----
add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
  ph_with(
    value = "Key Points on AFP Surveillance Timeliness",
    location = ph_location_type("title")
  ) %>%
  # Section header: Environmental Surveillance ----
add_slide(layout = "Title Slide", master = "1_Office Theme") %>%
  ph_with(value = "Environmental Surveillance", location = ph_location_type("ctrTitle")) %>%
  # ES sites and detection grid ----
add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
  ph_with(
    value = "ES Sites and Detection 1",
    location = ph_location_type("title")
  ) %>%
  ph_with(dml(ggobj = es.site.det), location = ph_location_type("body")) %>%
  #ph_with(dml(ggobj = es.site.det.facet), location = ph_location_type("body")) %>%
  # ES sites and detection map ----
add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
  ph_with(
    value = "ES Sites and Detection 2",
    location = ph_location_type("title")
  ) %>%
  ph_with(dml(ggobj = es.det.map), location = ph_location_type("body")) %>%
  # ES timeliness sample collection to lab ----
add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
  ph_with(
    value = "Timeliness of ES sample transport",
    location = ph_location_type("title")
  ) %>%
  ph_with(dml(ggobj = es.timely), location = ph_location_type("body")) %>%
  # ES site details (table)----
add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
  ph_with(
    value = "ES Site Details",
    location = ph_location_type("title")
  ) %>%
  ph_with(es.table, location = ph_location_type("body")) %>%
  # ES summary ----
add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
  ph_with(
    value = "Key Points on ES Surveillance",
    location = ph_location_type("title")
  ) %>%
  # Section header: Overall observations ----
add_slide(layout = "Title Slide", master = "1_Office Theme") %>%
  ph_with(value = "Overall Observations", location = ph_location_type("ctrTitle")) %>%
  # Summary ----
add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
  ph_with(
    value = "Summary",
    location = ph_location_type("title")
  ) %>%
  # Summary by Province ----
add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
  ph_with(
    value = "Summary by Province",
    location = ph_location_type("title")
  )
# Print output  ----
print(draft_output, file.path(here(), str_to_lower(country), year(Sys.Date()), 
                              paste0("draft_output_", Sys.Date(),"_",country, ".pptx")))

# Additional optional analyses --------
## Zero dose children ----
# View(count(ctry.data$afp.all.2, doses.opv.routine,doses.opv.sia,
#       doses.ipv.routine,
#       doses.ipv.sia,
#       doses.ipv.number))
### Calculate dose number ----

count(ctry.data$afp.all.2, doses.opv.routine,
      doses.opv.sia,
      doses.ipv.routine,
      doses.ipv.sia,
      doses.ipv.number)


ctry.data$afp.all.2 = ctry.data$afp.all.2 %>%
  mutate(across(c(doses.opv.routine,doses.opv.sia,
                  doses.ipv.routine,
                  doses.ipv.sia,
                  doses.ipv.number), as.numeric))
ctry.data$afp.all.2 = ctry.data$afp.all.2 %>%
  mutate(dose.num.calc = rowSums(ctry.data$afp.all.2[, c("doses.opv.routine","doses.opv.sia",
                                                         "doses.ipv.routine",
                                                         "doses.ipv.sia",
                                                         "doses.ipv.number")] * 
                                   (ctry.data$afp.all.2[, c("doses.opv.routine",
                                                            "doses.opv.sia",
                                                            "doses.ipv.routine",
                                                            "doses.ipv.sia",
                                                            "doses.ipv.number")] < 99),
                                 na.rm = T)) %>%
  mutate(dose.num.calc = case_when(
    doses.opv.routine == 99 &
      doses.opv.sia == 99 &
      doses.ipv.routine == 99 &
      doses.ipv.sia == 99 &
      doses.ipv.number == 99 ~ 999,
    is.na(doses.opv.routine) &
      is.na(doses.opv.sia) &
      is.na(doses.ipv.routine) &
      is.na(doses.ipv.sia) &
      is.na(doses.ipv.number) ~ NA,
    T~dose.num.calc
  ))

count(ctry.data$afp.all.2, dose.num.calc)
dose.num.cols = c(
  "0" = "#C00000",
  "1-2" = "#FFC000",
  "3" = "#92D050",
  "4+" = "#548235",
  "Missing" = "#A5A5A5"
)

ctry.data$afp.all.2 = ctry.data$afp.all.2 %>%
  mutate(dose.cat = case_when(
    dose.num.calc == 0 ~ "0",
    dose.num.calc > 0 & dose.num.calc <3 ~ "1-2",
    dose.num.calc == 3 ~ "3",
    dose.num.calc > 3 ~ "4+",
    T ~ as.character(dose.num.calc)
  )) %>%
  mutate(dose.cat = factor(dose.cat, levels = c("Unknown",
                                                "4+",
                                                "3",
                                                "1-2",
                                                "0")))

ctry.data$afp.all.2$dose.cat = forcats::fct_explicit_na(ctry.data$afp.all.2$dose.cat, "Missing")

count(ctry.data$afp.all.2, dose.cat)
### Create zero dose graphs ----
# Cats - 0, 1-2, 3, 4+
dcat.yr.prov = summarize(group_by(ctry.data$afp.all.2|>
                                    filter(date>=start_date &
                                             date<=end_date), dose.cat, year, prov), 
                         freq = n())
# case num by year and province by vaccination status
case.num.dose.g = ggplot()+
  geom_bar(data=dcat.yr.prov, aes(x=year, y = freq, fill = dose.cat),
           stat = "identity", position = "stack")+
  xlab("")+
  ylab("Number of cases")+
  scale_fill_manual("Number of doses - IPV/OPV",values = dose.num.cols, drop = F)+
  facet_grid(.~prov ,scales = "free_x" , space = "free",
             switch = "x")

case.num.dose.g
# case num by year and province by vaccination status
dcat.yr.prov.prop = summarize(group_by(ctry.data$afp.all.2|>
                                         filter(date>=start_date &
                                                  date<=end_date), dose.cat, year, prov), 
                              freq = n()) %>%
  ungroup()%>%
  group_by(year, prov) %>%
  summarize(tot = sum(freq)) %>%
  left_join(., dcat.yr.prov, by = c("year", "prov")) %>%
  mutate(prop = 100*freq/tot)



case.prop.dose.g = ggplot()+
  geom_bar(data=dcat.yr.prov.prop, aes(x=year, y = prop, fill = dose.cat),
           stat = "identity", position = "stack")+
  geom_text(data=dcat.yr.prov.prop,aes(x = year, y = prop, 
                                       label = round(prop,0), group = dose.cat),
            position = position_stack(vjust = 0.5))+
  scale_fill_manual("Number of doses - IPV/OPV",values = dose.num.cols, drop = F)+
  xlab("")+
  ylab("Proportion")+
  facet_grid(.~prov ,scales = "free_x" , space = "free",
             switch = "x")

case.prop.dose.g


# Potential compatibles -----
pot.c.clust = filter(cases.need60day, pot.compatible == 1|classification=="Compatible") %>%
  mutate(class = case_when(
    pot.compatible == 1 ~ "Potentially compatible",
    classification == "Compatible" ~ "Compatible"
  ))



# add 30 days to the first case --> pull out all of the cases that fall within 
# 30 days of that case --> label them as #1 --> then take the subset that is #1 -->
# add 30 days to the max of that --> pull out any that fit etc --> once you 
# don't have anymore that match - pick the minimum date that is NA --> start the
# process over

# All compatible and potentially compatibles that are within 30 days of another case - clust
pot.c.clust = arrange(pot.c.clust, date) # arrange by onset date
pot.c.clust$clust = NA
pot.c.clust$clust[1] = 1

if (nrow(pot.c.clust) > 1) {
  for(i in 2:nrow(pot.c.clust)){
    pot.c.clust$clust[i] = if_else(pot.c.clust$date[i]<=pot.c.clust$date[i-1]+30,
                                   max(pot.c.clust$clust[1:i-1], na.rm = T),
                                   max(pot.c.clust$clust[1:i-1], na.rm = T)+1)
  }
}

# Rolling cluster assignment
x = NULL
y = NULL
for(i in 1:length(unique(pot.c.clust$clust))){
  x = filter(pot.c.clust, clust == i)
  x$geo.clust.prov = vctrs::vec_duplicate_detect(x$prov)
  y = bind_rows(y, x)
}
#If there is a province that is the same, geo.clust.prov will be TRUE
pot.c.clust2 = y

# Write out excel file of compatible and potentially compatible ----
write_xlsx(pot.c.clust2, 
           file.path(here(),str_to_lower(country), year(Sys.Date()), "data",
                     paste0("compatible_pot_compatible_cases_",
                     country, ".xlsx")))

#----- Finishing Up
local_dr_path <- file.path(local_dr_path, "somalia_04232024.R")
github_path <- file.path("C:/Users/XRG9/desktop/gitrepos/sg-desk-reviews")
# saving new changes to the code
upload_dr_to_github(local_dr_path, github_path)
