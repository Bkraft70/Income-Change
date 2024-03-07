# This script pulls and wrangles American Community Survey (ACS) data into 
# a dataset for assessing the change in the populations of three aggregated 
# categories of income-to-poverty level over time, specifically here from 2005 to 2022

# The script here aggregates the income-to-poverty ratio variable into several 
# possible categorical frameworks.
# The ones used in the analysis in the "Montgomery County's Changing Income Trends and
# Repositioning for Shared Prosperity" Research brief are listed here with corresponding dataset code,
# and descriptive name used in the research brief: 

# ACS Aggregate Variable                 Dataset Code        Descriptive Name 
# under 3X poverty level                    u300                Low-Income
# Above 3X and below 5X poverty level       x300to499           Middle-Income
# 5X poverty level and above                x500plus            High-Income

# See research brief for description of and rationale for framework



#### Packages needed
library(dplyr)
library(tidycensus)
library(clipr) # only needed if formatting and manipulating further in Excel

#### Generate list of included counties and equivalents for analysis
# full fips and county + state columns for tidycensus data pull function later
cbsa2fipsxw <- read_csv("T:/Projects/_Economic/R_general/cbsa2fipsxw.csv", 
                        col_types = cols(cbsacode = col_character(), 
                                         csacode = col_character())) %>%
  mutate(state_pad = str_pad(fipsstatecode, 2, "left", pad = "0"),
         county_pad = str_pad(fipscountycode, 3, "left", pad = "0")) %>%
  mutate(countyfips = paste(state_pad, county_pad, sep = ""))

places <- cbsa2fipsxw %>%
  filter(countyfips %in% c("11001", "24021", "24017", "24027", "24031", "24033", "51013", "51059", "51153", "51107", "51510"))

just_fips_name <- places %>%
  select(countycountyequivalent, countyfips) %>%
  rename(name = countycountyequivalent,
         GEOID = countyfips)

#### list of ACS 1-year variables for reference
v22 <- load_variables(2022, "acs1", cache = T)
v05 <- load_variables(2005, "acs", cache = T)

#### The basic analysis below is provided for the list of large counties in the 
# Washington DC region, but can be done at any geographic level for any specific geography
# see Tidycensus (https://walker-data.com/tidycensus/) for geographic specification

#### Income to poverty ratio variables in 1-year ACS
pov_vars <- c("B17002_001",
              "B17002_002",
              "B17002_003",
              "B17002_004",
              "B17002_005",
              "B17002_006",
              "B17002_007",
              "B17002_008",
              "B17002_009",
              "B17002_010",
              "B17002_011",
              "B17002_012",
              "B17002_013")

#### Pull income-to-pov data for 2022 
pov22 <- map2_dfr(
  places$state_pad, places$county_pad, 
  ~get_acs(
    geography = "county",
    variables = pov_vars,
    state = ..1,
    county = ..2,
    summary_var = "B17002_001",
    survey = "acs1",
    year = 2022)
) 

# give descriptive names, aggregate to larger groups, and create "share" (pct_) variables
# script includes extra aggregation categories that were ultimately not used
pov22x <- pov22 %>%
  mutate(var = case_when(
    variable == "B17002_001" ~ "total_22",
    variable == "B17002_002" ~ "under50_22",
    variable == "B17002_003" ~ "x50to74_22",
    variable == "B17002_004" ~ "x75to99_22",
    variable == "B17002_005" ~ "x100to124_22",
    variable == "B17002_006" ~ "x125to149_22",
    variable == "B17002_007" ~ "x150to174_22",
    variable == "B17002_008" ~ "x175to184_22",
    variable == "B17002_009" ~ "x185to199_22",
    variable == "B17002_010" ~ "x200to299_22",
    variable == "B17002_011" ~ "x300to399_22",
    variable == "B17002_012" ~ "x400to499_22",
    variable == "B17002_013" ~ "x500plus_22"
  )) %>%
  pivot_wider(id_cols = GEOID, names_from = var, values_from = estimate) %>%
  mutate(u100_22 = under50_22 + x50to74_22 + x75to99_22,
         x100to199_22 = x100to124_22 + x125to149_22 + x150to174_22 + x175to184_22 + x185to199_22,
         x100to299_22 = x100to199_22 + x200to299_22,
         u300_22 = under50_22 + x50to74_22 + x75to99_22 + x100to199_22 + x200to299_22,
         x200to499_22 = x200to299_22 + x300to399_22 + x400to499_22,
         x300to499_22 = x300to399_22 + x400to499_22,
         x400plus_22 = x400to499_22 + x500plus_22) %>%
  select(GEOID, total_22, u100_22, x100to199_22, x200to299_22, u300_22, x200to499_22, x300to399_22, x400to499_22, x500plus_22, x100to299_22, x300to499_22, x400plus_22) %>%
  mutate(pct_u100_22 = u100_22/total_22,
         pct_x100to199_22 = x100to199_22/total_22,
         pct_x200to299_22 = x200to299_22/total_22,
         pct_u300_22 = u300_22/total_22,
         pct_x300to399_22 = x300to399_22/total_22,
         pct_x400to499_22 = x400to499_22/total_22,
         pct_x500plus_22 = x500plus_22/total_22,
         pct_x100to299_22 = x100to299_22/total_22,
         pct_x200to499_22 = x200to499_22/total_22,
         pct_x300to499_22 = x300to499_22/total_22,
         pct_x400plus_22 = x400plus_22/total_22)



#### Pull income-to-pov data for 2005 
pov05 <- map2_dfr(
  places$state_pad, places$county_pad, 
  ~get_acs(
    geography = "county",
    variables = pov_vars,
    state = ..1,
    county = ..2,
    summary_var = "B17002_001",
    survey = "acs1",
    year = 2005)
) 

pov05x <- pov05 %>%
  mutate(var = case_when(
    variable == "B17002_001" ~ "total_05",
    variable == "B17002_002" ~ "under50_05",
    variable == "B17002_003" ~ "x50to74_05",
    variable == "B17002_004" ~ "x75to99_05",
    variable == "B17002_005" ~ "x100to124_05",
    variable == "B17002_006" ~ "x125to149_05",
    variable == "B17002_007" ~ "x150to174_05",
    variable == "B17002_008" ~ "x175to184_05",
    variable == "B17002_009" ~ "x185to199_05",
    variable == "B17002_010" ~ "x200to299_05",
    variable == "B17002_011" ~ "x300to399_05",
    variable == "B17002_012" ~ "x400to499_05",
    variable == "B17002_013" ~ "x500plus_05"
  )) %>%
  pivot_wider(id_cols = GEOID, names_from = var, values_from = estimate) %>%
  mutate(u100_05 = under50_05 + x50to74_05 + x75to99_05,
         x100to199_05 = x100to124_05 + x125to149_05 + x150to174_05 + x175to184_05 + x185to199_05,
         x100to299_05 = x100to199_05 + x200to299_05,
         u300_05 = under50_05 + x50to74_05 + x75to99_05 + x100to199_05 + x200to299_05,
         x200to499_05 = x200to299_05 + x300to399_05 + x400to499_05,
         x300to499_05 = x300to399_05 + x400to499_05,
         x400plus_05 = x400to499_05 + x500plus_05) %>%
  select(GEOID, total_05, u100_05, x100to199_05, x200to299_05, u300_05, x200to499_05, x300to399_05, x400to499_05, x500plus_05, x100to299_05, x300to499_05, x400plus_05) %>%
  mutate(pct_u100_05 = u100_05/total_05,
         pct_x100to199_05 = x100to199_05/total_05,
         pct_x200to299_05 = x200to299_05/total_05,
         pct_u300_05 = u300_05/total_05,
         pct_x300to399_05 = x300to399_05/total_05,
         pct_x400to499_05 = x400to499_05/total_05,
         pct_x500plus_05 = x500plus_05/total_05,
         pct_x100to299_05 = x100to299_05/total_05,
         pct_x200to499_05 = x200to499_05/total_05,
         pct_x300to499_05 = x300to499_05/total_05,
         pct_x400plus_05 = x400plus_05/total_05)


### change 2005 to 2022 all counties

pov05to22 <- pov05x %>%
  full_join(pov22x, by = "GEOID") %>%
  # raw change
  mutate(chg_total = total_22-total_05,
         chg_u100 = u100_22-u100_05,
         chg_x100to199 = x100to199_22-x100to199_05,
         chg_x200to299 = x200to299_22-x200to299_05,
         chg_u300 = u300_22-u300_05,
         chg_x300to399 = x300to399_22-x300to399_05,
         chg_x400to499 = x400to499_22-x400to499_05,
         chg_x500plus = x500plus_22-x500plus_05,
         chg_x100to299 = x100to299_22-x100to299_05,
         chg_x200to499 = x200to499_22-x200to499_05,
         chg_x300to499 = x300to499_22-x300to499_05,
         chg_x400plus = x400plus_22-x400plus_05) %>%
  # percent change
  mutate(p_chg_u100 = chg_u100/u100_05,
         p_chg_x100to199 = chg_x100to199/x100to199_05,
         p_chg_x200to299 = chg_x200to299/x200to299_05,
         p_chg_u300 = chg_u300/u300_05,
         p_chg_x300to399 = chg_x300to399/x300to399_05,
         p_chg_x400to499 = chg_x400to499/x400to499_05,
         p_chg_x500plus = chg_x500plus/x500plus_05,
         p_chg_x100to299 = chg_x100to299/x100to299_05,
         p_chg_x200to499 = chg_x200to499/x200to499_05,
         p_chg_x300to499 = chg_x300to499/x300to499_05,
         p_chg_x400plus = chg_x400plus/x400plus_05) %>%
  # change in composition
  mutate(ccomp_u100 = pct_u100_22 - pct_u100_05,
         ccomp_x100to199 = pct_x100to199_22 - pct_x100to199_05,
         ccomp_x200to299 = pct_x200to299_22 - pct_x200to299_05,
         ccomp_u300 = pct_u300_22 - pct_u300_05,
         ccomp_x300to399 = pct_x300to399_22 - pct_x300to399_05,
         ccomp_x400to499 = pct_x400to499_22 - pct_x400to499_05,
         ccomp_x500plus = pct_x500plus_22 - pct_x500plus_05,
         ccomp_x100to299 = pct_x100to299_22 - pct_x100to299_05,
         ccomp_x200to499 = pct_x200to499_22 - pct_x200to499_05,
         ccomp_x300to499 = pct_x300to499_22 - pct_x300to499_05,
         ccomp_x400plus = pct_x400plus_22 - pct_x400plus_05) %>%
  full_join(just_fips_name, by = "GEOID") %>%
  select(GEOID, name, everything()) 
  
# copy full regional dataset for Excel
write_clip(pov05to22)


#### Montgomery County 2005 through 2022 year over year for indexed change by income group

pov_vars <- c("B17002_001",
              "B17002_002",
              "B17002_003",
              "B17002_004",
              "B17002_005",
              "B17002_006",
              "B17002_007",
              "B17002_008",
              "B17002_009",
              "B17002_010",
              "B17002_011",
              "B17002_012",
              "B17002_013")

years <- lst(2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2021, 2022)

pov_moco_allx <- map_dfr(
  years,
  ~get_acs(
    geography = "county",
    variables = pov_vars,
    state = "MD",
    county = "Montgomery",
    year = .x,
    summary_var = "B17002_001",
    survey = "acs1"),
  .id = "year"
) %>%
  select(-moe, -summary_moe, -GEOID, -NAME)

pov_moco_all <- pov_moco_allx %>%
  mutate(varname = case_when(
    variable == "B17002_001" ~ "total",
    variable == "B17002_002" ~ "under50",
    variable == "B17002_003" ~ "x50to74",
    variable == "B17002_004" ~ "x75to99",
    variable == "B17002_005" ~ "x100to124",
    variable == "B17002_006" ~ "x125to149",
    variable == "B17002_007" ~ "x150to174",
    variable == "B17002_008" ~ "x175to184",
    variable == "B17002_009" ~ "x185to199",
    variable == "B17002_010" ~ "x200to299",
    variable == "B17002_011" ~ "x300to399",
    variable == "B17002_012" ~ "x400to499",
    variable == "B17002_013" ~ "x500plus"
  )) %>%
  mutate(year = paste0("y", year)) %>%
  pivot_wider(id_cols = year, names_from = varname, values_from = estimate) %>%
  mutate(u100 = under50 + x50to74 + x75to99,
         x100to199 = x100to124 + x125to149 + x150to174 + x175to184 + x185to199,
         u200 = u100 + x100to199,
         x100to299 = x100to199 + x200to299,
         u300 = under50 + x50to74 + x75to99 + x100to199 + x200to299,
         x200to499 = x200to299 + x300to399 + x400to499,
         x300to499 = x300to399 + x400to499,
         x400plus = x400to499 + x500plus) %>%
  mutate(across(under50:x400plus, ~ .x/total, .names = "pct_{.col}")) %>% 
  pivot_longer(cols = total:pct_x400plus, names_to = "varnames") %>%
  pivot_wider(id_cols = varnames, names_from = year, values_from = value) %>%
  mutate(across(starts_with("y"), ~ (.x/y2005)*100, .names = "lev_{.col}"))

write_clip(pov_moco_all)


#### Ranking top 50 counties
# largest 50 counties in 2005
counties_pop05 <- get_acs(geography = "county",
                          variables = "B01001_001",
                          survey = "acs1",
                          year = 2005) %>%
  arrange(desc(estimate)) %>%
  # montgomery co is 40
  slice(1:51) %>%  
  # get rid of Fairfield, CT and add one extra for 50 total
  filter(GEOID != "09001") # CT changed to report for ACS as regions rather than counties, so counties are not comparable over time


county_pop_names <- counties_pop05 %>%
  select(GEOID, NAME) %>%
  rename_all(tolower)

# pull income-to-pov data for top 50 counties
# 2005
cpov05a <- get_acs(
  geography = "county",
  variables = pov_vars,
  survey = "acs1",
  year = 2005
)
cpov05b <- cpov05a %>%
  filter(GEOID %in% county_pop_names$geoid)
# 2022
cpov22a <- get_acs(
  geography = "county",
  variables = pov_vars,
  survey = "acs1",
  year = 2022
)
cpov22b <- cpov22a %>%
  filter(GEOID %in% county_pop_names$geoid) 

# wrangling process identical to above
# create dataframes cpov05 and cpov22

# combine and assign ranks
cpov0522 <- cpov05 %>%
  left_join(cpov22, by = "GEOID") %>%
  mutate(
    # net change
    chg_u100 = u100_22 - u100_05,
    chg_100to199 = x100to199_22 - x100to199_05,
    chg_200to299 = x200to299_22 - x200to299_05,
    chg_u300 = u300_22 - u300_05,
    chg_200to499 = x200to499_22 - x200to499_05,
    chg_300to399 = x300to399_22 - x300to399_05,
    chg_300to499 = x300to499_22 - x300to499_05,
    chg_400to499 = x400to499_22 - x400to499_05,
    chg_500plus = x500plus_22 - x500plus_05,
    # percent change
    pc_u100 = chg_u100/u100_05,
    pc_100to199 = chg_100to199/x100to199_05,
    pc_200to299 = chg_200to299/x200to299_05,
    pc_u300 = chg_u300/u300_05,
    pc_200to499 = chg_200to499/x200to499_05,
    pc_300to399 = chg_300to399/x300to399_05,
    pc_300to499 = chg_300to499/x300to499_05,
    pc_400to499 = chg_400to499/x400to499_05,
    pc_500plus = chg_500plus/x500plus_05,
    # percentage point change (change in share)
    cs_u100 = pct_u100_22 - pct_u100_05,
    cs_100to199 = pct_x100to199_22 - pct_x100to199_05,
    cs_200to299 = pct_x200to299_22 - pct_x200to299_05,
    cs_u300 = pct_u300_22 - pct_u300_05,
    cs_200to499 = pct_x200to499_22 - pct_x200to499_05,
    cs_300to399 = pct_x300to399_22 - pct_x300to399_05,
    cs_300to499 = pct_x300to499_22 - pct_x300to499_05,
    cs_400to499 = pct_x400to499_22 - pct_x400to499_05,
    cs_500plus = pct_x500plus_22 - pct_x500plus_05
  ) %>%
  # rank all counties across all metrics
  mutate(across(chg_u100:cs_500plus, ~ rank(desc(.x)), .names = "r_{.col}")) %>%
  left_join(county_pop_names, by = c("GEOID" = "geoid")) %>%
  select(GEOID, name, everything())

# can divide and export dataset as needed







