################################
## Structure for activities ####
# Further details can be found in the reports.
# All activities are handled in 5 year age groups

# Create helper function to add activities
add_activity <- function(.data, ...) {
  dots <- list(...) |>
    purrr::map_at(
      c("home", "work", "school", "other"),
      ~ ifelse(is.list(.x), .x, list(.x))
    )

  return(dplyr::add_row(.data, !!!dots))
}


# Create tibble to store all activity units:
dk_activity_units <- tibble::tibble(
  activity = character(0), label = character(0),
  home = list(numeric(0)), work = list(numeric(0)),
  school = list(numeric(0)), other = list(numeric(0)),
  risk = numeric(0)
)

# Template for adding new units: dk_activity_units |>                                                                   # nolint start: commented_code_linter
# add_activity(activity = "<programmatic short hand for activity>",
#              label  = "<human readable label for activity>",
#              home = <number in [0, 1]>,
#              work = <number in [0, 1]>,
#              school = <number in [0, 1]>,
#              other = <number in [0, 1]>,
#              risk = <number in [0, 1]>)                                                                               # nolint end: commented_code_linter

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "baseline",
    label = "baseline - full",
    home = 1, work = 1, school = 1, other = 1, risk = 1
  ) |>
  add_activity(
    activity = "fully_open",
    label = "Fully open",
    home = 1, work = 1, school = 1, other = 1, risk = 1
  ) |>
  add_activity(
    activity = "fully_closed",
    label = "Fully closed",
    home = 0, work = 0, school = 0, other = 0, risk = 1
  )

work_force <- 3000000 + 259000 + 74000 # Excluding unemployed, including 259k students + 74k young people over 20
below_10_risk <- 1 # 0.5


## Morten Start ####
################################## #
# Time from 11/3 to 14/4:
## s184334:
## Assumes 55% reduction at home, (Assumes fewer contacts at home with simultaneous reductions in school and work?)
## 55% reduction at work for age groups 0-65,
## 100% reduction in school
## 80% reduction for 0-65 in the "other" category and 90% reduction for the elderly.
dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "lockdown_2020",
    label = "Lockdown on 11/3",
    home = 0.45,
    work = rep(c(0.45, 1), c(13, 3)),
    school = 0,
    other = rep(c(0.2, 0.1), c(14, 2)), risk = 1
  )


################################## #
## Measures in phase 1: from 15/4 ##
# 5800 employees out of 17000 are physically present at gym + VET -- SOURCE?
dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "secondary_education_phase_1_2020",
    label = "Youth education for exams",
    home = 0,
    work = (rep(c(0, 5800 / work_force, 0), c(5, 8, 3))), # Source: BUVM
    school = (rep(c(0, 0.16, 0.04, 0), c(3, 1, 9, 3))), # 80% of the grade level
    other = (rep(c(0, 0.05, 0), c(3, 1, 12))),
    risk = 1
  )


# FM expects 13500 to go to work. Customers are treated as 0.025 in other
dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "liberal_professions_2020",
    label = "Liberal professions",
    home = 0,
    work = (rep(c(13500 / work_force, 0), c(13, 3))), # Source: FM
    school = 0,
    other = 0.025,
    risk = 1
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "practices_2020",
    label = "Practice sector",
    home = 0,
    work = (rep(c(33000 * 0.8 / work_force, 0), c(13, 3))),
    school = 0,
    other = 0.025,
    risk = 1
  )


# Courts: 5000 people + 7900 in selected higher education according to UFM. (7900 added 2020-05-12)
dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "courts_2020",
    label = "Courts and higher education",
    home = 0,
    work = (rep(c((5000 + 7900) / work_force, 0), c(13, 3))),
    school = 0,
    other = 0,
    risk = 1
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "private_companies_phase_1_2020",
    label = "Private job market after Easter",
    home = 0,
    work = (rep(c(0.12, 0), c(13, 3))), # Traffic Google 60% at work OK with + 0.15 Source: FM
    school = 0,
    other = 0,
    risk = 1
  )


# All children up to 11 years old. + 0.05 in other for 10-14 years old
# 71341 teachers at work (out of 102502)
# Assumes it adds + 0.05 home contacts
dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "daycares_and_schools_phase_1_2020",
    label = "Daycares and school 0-5 grade",
    home = 0.05,
    work = (rep(c(0, 71341 / work_force, 0), c(5, 8, 3))),
    school = (c(1, 1, 0.4, 0, rep(0.6, 9), 0, 0, 0)),
    other = (rep(c(0, 0.05, 0), c(2, 1, 13))),
    risk = 1 * below_10_risk
  )


################################## #
## Measures being explored in phase 2 ##
# Normally 450000 now 150000 customers per day.
# 60000 (Source: FM)
dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "retail_2020",
    label = "Retail: Shopping centers and department stores",
    home = 0,
    work = (rep(c(60000 / work_force, 0), c(13, 3))),
    school = 0,
    other = 0.05,
    risk = 1
  )


# Professional sports (3000), libraries (only loans) (BIB7: 3500 employees in public libraries),
# public sector AT etc. 5000 Total AO.
# Zoo 2000 employees and 4500000 annual visits ~ 3 hours per Dane per year ~ 0.5 min/day = 0.002 in other
# (This was used on 6/5, but in the recalculation, it was only running)
# Updated 2020-05-12 to only access by cars so 340 employees (KM) and none in other:
dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "misc_phase_2_2020",
    label = "Pro sports + libraries + 5000 public employees",
    home = 0,
    work = (rep(c((3000 + 3500 + 5000 + 340) / work_force, 0), c(13, 3))),
    school = 0,
    other = 0,
    risk = 1
  )


# Schools
# 9-10th is about 1.5 grades, of which 0.5 grade is at boarding school
# teacher at work, must be the rest: 102502-71341 = 31161
# Home is expected to increase by + 0.05
dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "schools_6_10_grade_2020",
    label = "6-10th grade and club offers",
    home = 0.05,
    work = (rep(c(0, 31161 / work_force, 0), c(5, 8, 3))), # OBS
    school = (c(0, 0, 0.6, 0.2, rep(0.212, 9), 0, 0, 0)),
    other = (rep(c(0, 0.10, 0.03, 0), c(2, 1, 1, 12))),
    risk = 1
  )

# 1/4 of the teachers compared to 6-10th grade
dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "schools_9_10_grade_2020",
    label = "9-10th grade and club offers",
    home = 0.05,
    work = (rep(c(0, 31161 / 4 / work_force, 0), c(5, 8, 3))),
    school = (c(0, 0, 0, 0.2, rep(0.05, 9), 0, 0, 0)),
    other = (rep(c(0, 0.03, 0), c(3, 1, 12))),
    risk = 1
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "efterskoler_2020",
    label = "Boarding schools - school part",
    home = 0,
    work = (rep(c(5000 / work_force, 0), c(13, 3))),
    school = (c(0, 0, 0, 0.1, rep(0.028, 9), 0, 0, 0)),
    other = (rep(c(0, 0.03, 0), c(3, 1, 12))),
    risk = 1
  )

# Restaurant and cafe
# Ref. HORESTA publication "Danskernes udespisevaner i 2019"
# Number * <share cafe + restaurant> for [12-19, 20-29, 30-39, 40-49, 50-64, 65-]
c(109, 136, 101, 98, 62, 33) * (c(0.27, 0.26, 0.26, 0.31, 0.4, 0.55) + 0.1)

# UPDATE 20200512: + 0.1 corresponds to pubs + cocktail bars + some fast food.
# Assumed for the young, not mentioned in the HORESTA publication.
tmp <- c(10, 20, 30, 40, 49, 49, 36, 36, 40, 40, 31, 31, 31, 21, 21, 21)

# Average 1/2 visit per week ~ 30min per week ~ 5min per day corresponding to 2% of the time in 'other' * 2 due
# to higher number of contacts / time
tmp2 <- tmp / mean(tmp) * 0.04 * 0.7 # 0.7 due to 70% activity
# At ~70% activity
# 40,000 are expected to go to work Source: EM
dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "restaurants_and_cafes_2020",
    label = "Restaurants and cafes",
    home = 0,
    work = (rep(c(0, 40000 / work_force, 0), c(3, 10, 3))),
    school = 0,
    other = (tmp2),
    risk = 1
  )


## Outdoor serving is 10% of full turnover (Source: FM)
dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "restaurants_and_cafes_outdoor_2020",
    label = "Restaurants and cafes - outdoor serving",
    home = 0,
    work = (rep(c(0, 40000 / 7 / work_force, 0), c(3, 10, 3))),
    school = 0,
    other = (tmp2 / 7),
    risk = 1
  )


## For the private job market Source: FM
# 2020-05-17: Adjusted to 0.05 Source: FM
dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "private_companies_phase_2_2020",
    label = "Private job market after second opening",
    home = 0,
    work = (rep(c(0.05, 0), c(13, 3))),
    school = 0,
    other = 0,
    risk = 1
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "misc_education_2020",
    label = "STU + VET + FGU + boarding school",
    home = 0,
    work = 0,
    school = (rep(c(0, 0.15, 0.04, 0), c(3, 1, 9, 3))),
    other = (rep(c(0, 0.05, 0), c(3, 1, 12))),
    risk = 1
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "exams_2020",
    label = "Higher education with physical exams",
    home = 0,
    work = (rep(c(0, 13400 / 765000, 0), c(4, 2, 10))), # 765000 people in 20-29 years old
    school = 0,
    other = 0,
    risk = 1
  )


# 650,000 people starting outdoor activities. We assume 15 min per day corresponding to 5% of 'other'
dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "sports_2020",
    label = "Sports - outdoor",
    home = 0, work = 0, school = 0, other = 0.05,
    risk = 1
  )


###################################### #
## Phase 3 ##
# See note on the SSI's website for documentation where details are not mentioned
# Summer activities are handled separately
# 35.8 million visitors per year ~ 3 minutes on average per day ~ 0.01 in 'other,' but as cinema, theater,
# and zoo have higher density 0.02 is used
dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "culture_2020",
    label = "Museums, theater, cinema, film and TV, zoo",
    home = 0,
    work = (rep(c(0, (6660 + 6400 + 2000 + 2500 + 960) / work_force, 0), c(3, 10, 3))),
    school = 0,
    other = 0.02,
    risk = 1
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "public_sector_2020",
    label = "Public sector with case backlogs",
    home = 0,
    work = (rep(c(0, (33000) / work_force, 0), c(3, 10, 3))), # Source: FM
    school = 0,
    other = 0,
    risk = 1
  ) # About 90,000 citizens for meetings, but average time consumption is very low

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "public_research_2020",
    label = "Public research requiring presence",
    home = 0,
    work = (rep(c(0, (14500 + 300) / work_force, 0), c(3, 10, 3))), # Source: UFM, KEFM, KUM, VIVE
    school = 0,
    other = 0,
    risk = 1
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "adult_education_2020",
    label = "Adult education for the unemployed",
    home = 0,
    work = (rep(c(0, (21800 + 1500 + 3720 + 117) / work_force, 0), c(3, 10, 3))),
    school = 0,
    other = 0,
    risk = 1
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "hojskoler_2020",
    label = "Folk high schools, etc.",
    home = 0,
    work = (rep(c(0, (1500) / work_force, 0), c(3, 10, 3))) + # Employees
      (rep(c(0, (5000) / 765000, 0), c(4, 2, 10))) +          # Youth on long courses
      (rep(c(0, (1000) / 640000, 0), c(13, 2, 1))),           # Elderly (65-75 years old) on short courses

    school = 0,
    other = 0.005,
    risk = 1
  )


# Equivalent to 1.37 minutes per Dane per day ~ 0.005 in 'other' with an estimated fourfold risk per time: 0.02
dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "indoor_sports_2020",
    label = "Indoor sports and community life",
    home = 0,
    work = (rep(c(0, (2000 + 7000 + 3500) / work_force, 0), c(3, 10, 3))),
    school = 0,
    other = 0.02,
    risk = 1
  )


################################# #
## Phase 3 +: Extended Phase 3 ##
dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "dr_tv2_2020",
    label = "DR and TV2 fully open",
    home = 0,
    work = (rep(c(0, (4695) / work_force, 0), c(3, 10, 3))),
    school = 0,
    other = 0,
    risk = 1
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "public_sector_f3p_2020",
    # Split in three parts to allow partial closing later on
    label = "Public employees in phase 3 +",
    home = 0.05 - 0.02, # Normalization of society
    work = (rep(c(0, (190000) / work_force - 0.015 - 0.02, 0), c(3, 10, 3))), # Source: FM
    school = 0,
    other = 0.05 - 0.02,
    risk = 1
  ) # Combination of 5% more work, which involves transportation and citizen contact

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "public_sector_f3p_2020B_closing_sep_2020",
    label = "Public employees in phase 3 + (1.5% sent home in Sep 2020)",
    home = 0,
    work = (rep(c(0, 0.015, 0), c(3, 10, 3))), # Source: FM
    school = 0,
    other = 0,
    risk = 1
  ) # Combination of 5% more work, which involves transportation and citizen contact

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "public_sector_f3p_2020C_closing_oct_2020",
    label = "Public employees in phase 3 + (2% reduction in Oct 2020)",
    home = 0.02,
    work = (rep(c(0, 0.02, 0), c(3, 10, 3))), # Source: FM
    school = 0,
    other = 0.02,
    risk = 1
  ) # Combination of 5% more work, which involves transportation and citizen contact


################################# #
## Phase 4:  ##
dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "education_2020",
    label = "All other education including 1st and 2nd grade",
    home = 0.1,
    work = (rep(c(0, (320000) / work_force, 0), c(3, 10, 3))),
    school = (rep(c(0, 0.39, 0.08, 0), c(3, 1, 9, 3))), # Note: estimated two age groups 15-19 years old
    other = 0.1,
    risk = 1
  )

# Source: Danish Swimming Pool Technical Association.
# Each Dane uses 4 minutes/day ~ 0.015 pp in 'other.' There is an estimated fourfold risk, so 0.06
dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "fitness_2020",
    label = "Fitness + swimming pools + play areas",
    home = 0,
    work = (rep(c(0, (1900) / work_force, 0), c(3, 10, 3))),
    school = 0,
    other = 0.06,
    risk = 1
  )

## Phase 6: ???##
dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "behavior_phase_6_2020",
    label = "Behavioral changes related to October 26 and Northern Jutland",
    home = 0.02,
    work = (rep(c(0.02, 0), c(13, 3))), # Reduction in contacts by -1.5% (- 1-2% agreed with Lasse)
    school = 0,
    other = 0.02,
    risk = 1
  )
## Morten End ####


dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "health_sector",
    label = "Health",
    home = 0,
    work = (rep(c(443000 / work_force, 1), c(13, 3))), # Full work contacts from the old care scenario
    school = 0,
    other = 0.025,
    risk = 1
  )


# Courts, etc. See Excel
dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "courts",
    label = "Courts, Police, Fire, and Prison",
    home = 0,
    work = (rep(c(30000 / work_force, 0), c(13, 3))),
    school = 0,
    other = 0,
    risk = 1
  )


# Churches
dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "churches_lockdown",
    label = "Churches - only the national church",
    home = 0,
    work = (rep(c(5000 / work_force, 0), c(13, 3))),
    school = 0,
    other = (0.5 * rep(c(0.005, 0.02), c(12, 4))),
    risk = 0.5
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "churches_open",
    label = "Churches - only the national church",
    home = 0,
    work = (rep(c(15100 / work_force, 0), c(13, 3))),
    school = 0,
    other = (rep(c(0.005, 0.02), c(12, 4))),
    risk = 1
  )


##################################### #
## School and Daycare ####

# Add 2021-01-05: RAS309, DB07 Division of DayCareSchool.0-5
# Note: BUVM specifies 72000 as educational staff; we have used 8891[123]0, which gives 37265
# LAEC: Moved up: below_10_risk <- 0.5
dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "daycares_jan_2021",
    label = "Daycares",
    home = 0.025,
    work = (rep(c(0, (87000) / work_force, 0), c(5, 8, 3))),
    school = (0.68 * c(1, 0.2, 0, 0, rep(0.3, 9), 0, 0, 0)),
    other = (rep(c(0, 0.0, 0), c(2, 1, 13))),
    risk = below_10_risk
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "daycares_feb_2021",
    label = "Daycares",
    home = 0.025,
    work = (rep(c(0, (87000) / work_force, 0), c(5, 8, 3))),
    school = (0.78 * c(1, 0.2, 0, 0, rep(0.3, 9), 0, 0, 0)),
    other = (rep(c(0, 0.0, 0), c(2, 1, 13))),
    risk = below_10_risk
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "daycares_open",
    label = "Daycares - normal from March 1",
    home = 0.025,
    work = (rep(c(0, (87000) / work_force, 0), c(5, 8, 3))),
    school = (1 * c(1, 0.2, 0, 0, rep(0.3, 9), 0, 0, 0)),
    other = (rep(c(0, 0.0, 0), c(2, 1, 13))),
    risk = below_10_risk
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "daycares_summer",
    label = "Daycares - summer vacation",
    home = 0.025,
    work = (rep(c(0, (0) / work_force, 0), c(5, 8, 3))),
    school = (0.25 * c(1, 0.2, 0, 0, rep(0.3, 9), 0, 0, 0)),
    other = (rep(c(0, 0.0, 0), c(2, 1, 13))),
    risk = below_10_risk
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "schools_0_4_grade_critical",
    label = "School 0-4th grade emergency care January 2021",
    home = 0.02 * 0.164,
    work = (rep(c(0, ((11000 + 39130) * 0.25) / work_force, 0), c(5, 8, 3))), # pedagogical + share of teachers
    school = (0.164 * c(0, 0.8, 0.2, 0, rep(0.3, 9), 0, 0, 0)),
    other = (0.164 * rep(c(0, 0.05, 0), c(2, 1, 13))),
    risk = below_10_risk * 0.5
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "schools_0_4_grade_open_R1",
    label = "School 0-4 grade full",
    home = 0.02,
    work = (rep(c(0, (11000 + 39130) / work_force, 0), c(5, 8, 3))),
    school = (c(0, 0.8, 0.2, 0, rep(0.3, 9), 0, 0, 0)),
    other = (rep(c(0, 0.04, 0.01, 0), c(2, 1, 1, 12))),
    risk = below_10_risk
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "schools_0_4_grade_summer_R1",
    label = "School 0-4 grade summer",
    home = 0.02,
    work = (rep(c(0, (0 + 0) / work_force, 0), c(5, 8, 3))),
    school = (0.25 * c(0, 0.8, 0.2, 0, rep(0.3, 9), 0, 0, 0)),
    other = (rep(c(0, 0.04, 0.01, 0), c(2, 1, 1, 12))),
    risk = below_10_risk
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "schools_0_4_grade_open_R0_75",
    label = "School 0-4 grade full",
    home = 0.02,
    work = (rep(c(0, (11000 + 39130) / work_force, 0), c(5, 8, 3))),
    school = (c(0, 0.8, 0.2, 0, rep(0.3, 9), 0, 0, 0)),
    other = (rep(c(0, 0.04, 0.01, 0), c(2, 1, 1, 12))), # Corrected: c(0, 0.05, 0, ...) => c(0, 0.04, 0.01, ...)
    risk = below_10_risk * 0.75
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "schools_0_4_grade_open_R0_5",
    label = "School 0-4 grade full",
    home = 0.02,
    work = (rep(c(0, (11000 + 39130) / work_force, 0), c(5, 8, 3))),
    school = (c(0, 0.8, 0.2, 0, rep(0.3, 9), 0, 0, 0)),
    other = (rep(c(0, 0.04, 0.01, 0), c(2, 1, 1, 12))), # Corrected: c(0, 0.05, 0, ...) => c(0, 0.04, 0.01, ...)
    risk = below_10_risk * 0.5
  )


# Schools
# 9-10 is approximately 1.5 grades, of which 0.5 grade is on boarding school
# Teacher at work must be the rest: 102502-71341 = 31161
# Home + 0.05 + 0.05 (Last term from 5th grade)
dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "schools_5_8_grade_critical_jan_2021",
    label = "5-8 grade and club offers - emergency care January 2021",
    home = 0.005,
    work = (rep(c(0, (2300 + 33600) * 0.04 / work_force, 0), c(5, 8, 3))), # OBS
    school = (0.04 * c(0, 0, 0.8, 0.2, rep(0.212, 9), 0, 0, 0)),
    other = (rep(0.04 * c(0, 0.1, 0.03, 0), c(2, 1, 1, 12))),
    risk = 0.5
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "schools_5_8_grade_critical_mar_2021",
    label = "5-8 grade and club offers - emergency care March 2021",
    home = 0.005,
    work = (rep(c(0, (2300 + 33600) * 0.062 / work_force, 0), c(5, 8, 3))), # OBS
    school = (0.062 * c(0, 0, 0.8, 0.2, rep(0.212, 9), 0, 0, 0)),
    other = (rep(0.062 * c(0, 0.1, 0.03, 0), c(2, 1, 1, 12))),
    risk = 0.5
  )


# Temporarily make R use "_" as decimal separator
options(OutDec = "_")
for (risk_test_school in c(0.5, 0.75, 1)) {

  # 90% of teachers and 75% for children
  dk_activity_units <- dk_activity_units |>
    add_activity(
      activity = paste0("schools_5_8_grade_20pp_R", risk_test_school),
      label = "5-8 grade without club offers - one day per week",
      home = (rep(c(0.015, 0.005), c(12, 4))),
      work = (rep(c(0, (2300 * 0.062 + 33600 * (0.2 + 0.062)) / work_force, 0), c(5, 8, 3))), # OBS
      school = (0.2 * c(0, 0, 0.8, 0.2, rep(0.212, 9), 0, 0, 0)),
      other = (0.2 * rep(c(0, 0.1, 0.03, 0), c(2, 1, 1, 12))),
      risk = risk_test_school
    )

  dk_activity_units <- dk_activity_units |>
    add_activity(
      activity = paste0("schools_5_8_grade_bidaily_R", risk_test_school),
      label = "5-8 grade and club offers - every other day",
      home = (rep(c(0.055, 0.02), c(12, 4))), # LAEC c(0.055, 0.005)
      work = (rep(c(0, (2300 * 0.062 + 33600 * (0.5 + 0.062)) / work_force, 0), c(5, 8, 3))), # OBS
      school = (0.75 * c(0, 0, 0.8, 0.2, rep(0.212, 9), 0, 0, 0)),
      other = (0.5 * rep(c(0, 0.1, 0.03, 0), c(2, 1, 1, 12))),
      risk = risk_test_school
    )

  dk_activity_units <- dk_activity_units |>
    add_activity(
      activity = paste0("schools_5_8_grade_biweekly_R", risk_test_school),
      label = "5th-8th grade without club offers - every other week",
      home = (rep(c(0.03, 0.01), c(12, 4))), # LAEC c(0.03, 0.005)
      work = (rep(c(0, (2300 * 0.062 + 33600 * (0.5 + 0.062)) / work_force, 0), c(5, 8, 3))), # OBS
      school = (0.5 * c(0, 0, 0.8, 0.2, rep(0.212, 9), 0, 0, 0)),
      other = (0.5 * rep(c(0, 0.1, 0.03, 0), c(2, 1, 1, 12))),
      risk = risk_test_school
    )

  dk_activity_units <- dk_activity_units |>
    add_activity(
      activity = paste0("schools_5_8_grade_open_R", risk_test_school),
      label = "5th-8th grade with club offers - full attendance",
      home = (rep(c(0.055, 0.03), c(12, 4))), # LAEC c(0.055, 0.005)
      work = (rep(c(0, (2300 + 33600) / work_force, 0), c(5, 8, 3))), # OBS
      school = (c(0, 0, 0.8, 0.2, rep(0.212, 9), 0, 0, 0)),
      other = (rep(c(0, 0.1, 0.03, 0), c(2, 1, 1, 12))),
      risk = risk_test_school
    )

  dk_activity_units <- dk_activity_units |>
    add_activity(
      activity = paste0("schools_5_8_grade_summer_R", risk_test_school),
      label = "5th-8th grade with club offers - summer",
      home = (rep(c(0.055, 0.03), c(12, 4))), # LAEC c(0.055, 0.005)
      work = (rep(c(0, (0 + 0) / work_force, 0), c(5, 8, 3))), # OBS
      school = (0.25 * c(0, 0, 0.8, 0.2, rep(0.212, 9), 0, 0, 0)),
      other = (rep(c(0, 0.1, 0.03, 0), c(2, 1, 1, 12))),
      risk = risk_test_school
    )

  dk_activity_units <- dk_activity_units |>
    add_activity(
      activity = "schools_9_10_grade_critical_jan_2021",
      label = "9th-10th grade without club offerings - from January",
      home = 0.04 * 0.05,
      work = (rep(0.04 * c(0, 9400 / work_force, 0), c(5, 8, 3))),
      school = (0.04 * c(0, 0, 0, 0.2, rep(0.05, 9), 0, 0, 0)),
      other = (0.04 * rep(c(0, 0.03, 0), c(3, 1, 12))),
      risk = 1
    ) # FAST

  dk_activity_units <- dk_activity_units |>
    add_activity(
      activity = "schools_9_10_grade_critical_mar_2021",
      label = "9th-10th grade without club offerings - from March",
      home = 0.062 * 0.05,
      work = (rep(0.062 * c(0, 9400 / work_force, 0), c(5, 8, 3))),
      school = (0.062 * c(0, 0, 0, 0.2, rep(0.05, 9), 0, 0, 0)),
      other = (0.062 * rep(c(0, 0.03, 0), c(3, 1, 12))),
      risk = 1
    ) # FAST

  dk_activity_units <- dk_activity_units |>
    add_activity(
      activity = paste0("schools_9_10_grade_50pp_R", risk_test_school),
      label = "9th-10th grade without club offerings - half",
      home = (rep(c(0.5 * 0.05, 0.25 * 0.05), c(12, 4))), # LAEC c(0.5 * 0.05, 0.062 * 0.05)
      work = (rep(c(0, (0.5 + 0.062) * 9400 / work_force, 0), c(5, 8, 3))),
      school = (0.5 * c(0, 0, 0, 0.25, rep(0.05, 9), 0, 0, 0)), # 1, 25 grades were 0.2
      other = (0.5 * rep(c(0, 0.03, 0), c(3, 1, 12))),
      risk = risk_test_school
    )

  dk_activity_units <- dk_activity_units |>
    add_activity(
      activity = paste0("schools_9_10_grade_open_R", risk_test_school),
      label = "9th-10th grade with club offerings",
      home = (rep(c(0.05, 0.5 * 0.05), c(12, 4))), # LAEC c(0.05, 0.062 * 0.05)
      work = (rep(c(0, 9400 / work_force, 0), c(5, 8, 3))),
      school = (c(0, 0, 0, 0.25, rep(0.05, 9), 0, 0, 0)), # 1, 25 grades were 0.2
      other = (rep(c(0, 0.03, 0), c(3, 1, 12))),
      risk = risk_test_school
    )

  dk_activity_units <- dk_activity_units |>
    add_activity(
      activity = paste0("schools_9_10_grade_summer_R", risk_test_school),
      label = "9th-10th grade with club offerings - summer",
      home = (rep(c(0.05, 0.5 * 0.05), c(12, 4))),
      work = (rep(c(0, 0 / work_force, 0), c(5, 8, 3))),
      school = (0.25 * c(0, 0, 0, 0.25, rep(0.05, 9), 0, 0, 0)), # 1,25 grades were 0.2
      other = (rep(c(0, 0.03, 0), c(3, 1, 12))),
      risk = risk_test_school
    )

  dk_activity_units <- dk_activity_units |>
    add_activity(
      activity = paste0("efterskoler_R", risk_test_school),
      label = "Boarding Schools - school part",
      home = 0,
      work = (rep(c((8634) / work_force, 0), c(13, 3))),
      school = (c(0, 0, 0, 0.1, rep(0.028, 9), 0, 0, 0)),
      other = (rep(c(0, 0.03, 0), c(3, 1, 12))),
      risk = risk_test_school
    )

  dk_activity_units <- dk_activity_units |>
    add_activity(
      activity = paste0("efterskoler_summer_R", risk_test_school),
      label = "Boarding Schools - school part",
      home = 0,
      work = (rep(c((0) / work_force, 0), c(13, 3))),
      school = (0.25 * c(0, 0, 0, 0.1, rep(0.028, 9), 0, 0, 0)),
      other = (rep(c(0, 0.03, 0), c(3, 1, 12))),
      risk = risk_test_school
    )


  # Total Young Education: 168k + 94k = 264k students, of which 74k are over 20 years old.
  # Distributed with 1/3 in graduation classes
  # 15-19: 190k
  # 20-24: 35k
  # 25-29: 17k
  # 30+: 22k
  dk_activity_units <- dk_activity_units |>
    add_activity(
      activity = "high_school_non_senior_year_critical",
      label = "Upper secondary education - non-graduating",
      home = 0.025 * 0.1,
      work = (rep(0.025 * c(0, 19000 / work_force, 0), c(3, 10, 3)) +
                rep(c(0, 22, 11, 14, 0) * 1000 / work_force, c(4, 1, 1, 1, 9))), # Work + 20+ year old students
      school = (rep(0.025 * c(0, 0.34, 0.08, 0), # Estimated two grades 15-19 years old - adjusted to 1,7 grades
                    c(3, 1, 9, 3))),
      other = 0.025 * 0.07,
      risk = 1
    ) # FAST

  dk_activity_units <- dk_activity_units |>
    add_activity(
      activity = paste0("high_school_non_senior_year_open_R", risk_test_school),
      label = "Upper secondary education - non-graduating",
      home = (rep(c(0.1, 0.25 * 0.1), c(12, 4))),
      work = (rep(c(0, 19000 / work_force, 0), c(3, 10, 3)) +
                rep(c(0, 22, 11, 14, 0) * 1000 / work_force, c(4, 1, 1, 1, 9))), # Work + 20+ year old students
      school = (rep(c(0, 0.34, 0.08, 0), # Estimated two grades 15-19 years old - adjusted to 1,7 grades
                    c(3, 1, 9, 3))),
      other = 0.07,
      risk = risk_test_school
    )

  dk_activity_units <- dk_activity_units |>
    add_activity(
      activity = paste0("high_school_non_senior_year_summer_R", risk_test_school),
      label = "Upper secondary education - non-graduating - summer",
      home = (rep(c(0.1, 0.25 * 0.1), c(12, 4))),
      work = (rep(c(0, 0 / work_force, 0), c(3, 10, 3)) +
                rep(0.25 * c(0, 22, 11, 14, 0) * 1000 / work_force, c(4, 1, 1, 1, 9))), # Work + 20+ year old students
      school = (0.25 * rep(c(0, 0.34, 0.08, 0), # Estimated two grades 15-19 years old - adjusted to 1,7 grades
                           c(3, 1, 9, 3))),
      other = 0.07,
      risk = risk_test_school
    )

  # OBS: New
  dk_activity_units <- dk_activity_units |>
    add_activity(
      activity = paste0("high_school_non_senior_year_20pp_R", risk_test_school),
      label = "Upper secondary education - non-graduating",
      home = (rep(0.2 * c(0.1, 0.25 * 0.1), c(12, 4))),
      work = 0.2 * (rep(c(0, 19000 / work_force, 0), c(3, 10, 3)) +
                      rep(c(0, 22, 11, 14, 0) * 1000 / work_force, c(4, 1, 1, 1, 9))), # Work + 20+ year old students
      school = 0.2 * (rep(c(0, 0.34, 0.08, 0), # Estimated two grades 15-19 years old - adjusted to 1,7 grades
                          c(3, 1, 9, 3))),
      other = 0.2 * 0.07,
      risk = risk_test_school
    )

  dk_activity_units <- dk_activity_units |>
    add_activity(
      activity = paste0("high_school_non_senior_year_50pp_R", risk_test_school),
      label = "Upper secondary education - non-graduating",
      home = (rep(0.5 * c(0.1, 0.25 * 0.1), c(12, 4))),
      work = 0.5 * (rep(c(0, 19000 / work_force, 0), c(3, 10, 3)) +
                      rep(c(0, 22, 11, 14, 0) * 1000 / work_force, c(4, 1, 1, 1, 9))), # Work + 20+ year old students
      school = 0.5 * (rep(c(0, 0.34, 0.08, 0), # Estimated two grades 15-19 years old - adjusted to 1,7 grades
                          c(3, 1, 9, 3))),
      other = 0.5 * 0.07,
      risk = risk_test_school
    )

  dk_activity_units <- dk_activity_units |>
    add_activity(
      activity = "high_school_senior_year_critical",
      label = "Upper secondary education + AMU + VEU - graduating",
      home = 0.025 * 0.1,
      work = (0.025 * (rep(c(0, 11600 / work_force, 0), c(3, 10, 3)) +
                         1 / 3 * rep(c(0, 35, 17, 14, 8, 0) * 1000 / work_force, c(4, 1, 1, 1, 1, 8))) +
                rep(c(0, (3700 + 320) / work_force, 0), c(6, 7, 3))), # Work + 20+ year old students
      school = (0.025 * rep(c(0, 0.1, 0.03, 0), # 1/2 grade, as some are 20+ years old,
                            c(3, 1, 9, 3))),    # also reduced among children to adults
      other = 0.025 * 0.03,
      risk = 1
    ) # FAST

  dk_activity_units <- dk_activity_units |>
    add_activity(
      activity = paste0("high_school_senior_year_50pp_R", risk_test_school),
      label = "Upper secondary education + AMU + VEU + FGU - graduating - half",
      home = (rep(c(0.5 * 0.1, 0.25 * 0.1), c(12, 4))), # LAEC c(0.5 * 0.1, 0.025 * 0.1)
      work = (rep(0.75 * c(0, 11600 / work_force, 0), c(3, 10, 3)) +    # Work + 20+-year old students
                0.5 * 1 / 3 * rep(c(0, 35, 17, 14, 8, 0) * 1000 / work_force, c(4, 1, 1, 1, 1, 8)) +
                rep(c(0, 0.5 * (3000 + 13100 + 43400 + 750) / work_force, 0), c(6, 7, 3))),
      school = (0.5 * rep(c(0, 0.1, 0.03, 0), # 1/2 grade, as some are 20+-year old,
                          c(3, 1, 9, 3))),    # also reduced among children to adults
      other = 0.5 * 0.03,
      risk = risk_test_school
    )

  dk_activity_units <- dk_activity_units |>
    add_activity(
      activity = paste0("high_school_senior_year_open_R", risk_test_school),
      label = "Upper secondary education + AMU + VEU + FGU - graduating",
      home = (rep(c(0.1, 0.25 * 0.1), c(12, 4))),
      work = (rep(c(0, 11600 / work_force, 0), c(3, 10, 3)) +
                1 / 3 * rep(c(0, 35, 17, 14, 8, 0) * 1000 / work_force, c(4, 1, 1, 1, 1, 8)) +
                rep(c(0, (3000 + 13100 + 43400 + 750) / work_force, 0), c(6, 7, 3))), # Work + 20+-year old students
      school = (rep(c(0, 0.1, 0.03, 0), # 1/2 grade, as some are 20+-year old,
                    c(3, 1, 9, 3))),    # also reduced among children to adults
      other = 0.03,
      risk = risk_test_school
    )

  dk_activity_units <- dk_activity_units |>
    add_activity(
      activity = paste0("high_school_senior_year_summer_R", risk_test_school),
      label = "Upper secondary education + AMU + VEU + FGU - graduating - summer",
      home = (rep(c(0.1, 0.25 * 0.1), c(12, 4))),
      work = (rep(c(0, 0 / work_force, 0), c(3, 10, 3)) +   # Work + 20+-year old students
                0.25 * 1 / 3 * rep(c(0, 35, 17, 14, 8, 0) * 1000 / work_force, c(4, 1, 1, 1, 1, 8)) +
                0.25 * rep(c(0, (3000 + 13100 + 43400 + 750) / work_force, 0), c(6, 7, 3))),
      school = (rep(0.25 * c(0, 0.1, 0.03, 0), # 1/2 grade, as some are 20+-year old,
                    c(3, 1, 9, 3))),           # also reduced among children to adults
      other = 0.03,
      risk = risk_test_school
    )

} # EndOf: risk_test_school
options(OutDec = ".") # Reset the decimal separator

# The above loop introduces duplicated rows. Remove those here (when risk is always 1)
dk_activity_units <- dk_activity_units |> dplyr::distinct()


dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "higher_education_critical_jan_2021",
    label = "Higher Education - Closed January 2021",
    home = 0.0,
    work = (rep(c(0, (259000 * 0.025 + 42000 * 0.11) / work_force, 0), c(3, 10, 3))),
    school = 0,
    other = 0.003,
    risk = 0.5
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "higher_education_open",
    label = "Higher Education - Full Opening",
    home = 0.0,
    work = (rep(c(0, (259000 + 42000) / work_force, 0), c(3, 10, 3))),
    school = 0,
    other = 0.03,
    risk = 1
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "higher_education_open_R0_5",
    label = "Higher Education - Full Opening",
    home = 0.0,
    work = (rep(c(0, (259000 + 42000) / work_force, 0), c(3, 10, 3))),
    school = 0,
    other = 0.03,
    risk = 0.5
  ) # No room for everyone, distance, and testing of those who are not vaccinated.

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "higher_education_summer",
    label = "Higher Education - Summer",
    home = 0.0,
    work = (rep(0.25 * c(0, (259000 + 42000) / work_force, 0), c(3, 10, 3))),
    school = 0,
    other = 0.03,
    risk = 1
  )


# Higher Education - Final Year - Closed from March (Risk 0.5)
dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "higher_education_seniors_critical_mar_2021_R0_5",
    label = "Higher Education - Final Year - Closed from March",
    home = 0.0,
    work = (rep(c(0, (0.06 * 55100 + 0.11 * 8900) / work_force, 0), c(3, 10, 3))),
    school = 0,
    other = 0.03 * 0.21 * 0.1,
    risk = 0.5
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "higher_education_seniors_20pp_R0_5",
    label = "Higher Education - Final Year - 20% Attendance",
    home = 0.0,
    work = (rep(c(0, (0.2 * 55100 + 0.25 * 8900) / work_force, 0), c(3, 10, 3))),
    school = 0,
    other = 0.03 * 0.21 * 0.5,
    risk = 0.5
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "higher_education_seniors_50pp_R0_5",
    label = "Higher Education - Final Year - 50% Attendance",
    home = 0.0,
    work = (rep(c(0, (0.5 * 55100 + 8900) / work_force, 0), c(3, 10, 3))),
    school = 0,
    other = 0.03 * 0.21 * 0.5,
    risk = 0.5
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "higher_education_seniors_open_R0_5",
    label = "Higher Education - Final Year - 100% Attendance",
    home = 0.0,
    work = (rep(c(0, (55100 + 8900) / work_force, 0), c(3, 10, 3))),
    school = 0,
    other = 0.03 * 0.21,
    risk = 0.5
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "higher_education_non_seniors_critical_mar_2021_R0_5",
    label = "Higher Education - Non-Seniors - Closed from March",
    home = 0.0,
    work = (rep(c(0, (0.06 * 204600 + 0.11 * 33400) / work_force, 0), c(3, 10, 3))),
    school = 0,
    other = 0.03 * 0.79 * 0.1,
    risk = 0.5
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "higher_education_non_seniors_20pp_R0_5",
    label = "Higher Education - Non-Seniors - 20% Attendance",
    home = 0.0,
    work = (rep(c(0, (0.2 * 204600 + 0.25 * 33400) / work_force, 0), c(3, 10, 3))),
    school = 0,
    other = 0.03 * 0.79 * 0.5,
    risk = 0.5
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "higher_education_non_seniors_50pp_R0_5",
    label = "Higher Education - Non-Seniors - 50% Attendance",
    home = 0.0,
    work = (rep(c(0, 0.5 * (204600 + 33400) / work_force, 0), c(3, 10, 3))),
    school = 0,
    other = 0.03 * 0.79 * 0.5,
    risk = 0.5
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "higher_education_non_seniors_open_R0_5",
    label = "Higher Education - Non-Seniors - 100% Attendance",
    home = 0.0,
    work = (rep(c(0, (204600 + 33400) / work_force, 0), c(3, 10, 3))),
    school = 0,
    other = 0.03 * 0.79,
    risk = 0.5
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "hojskoler",
    label = "High Schools and Similar Institutions",
    home = 0,
    work = (rep(c(0, (1000) / work_force, 0), c(3, 10, 3))) + # Employees
      (rep(c(0, (6100) / work_force, 0), c(4, 2, 10))) +      # students on long stays
      (rep(c(0, (1000) / work_force, 0), c(13, 2, 1))),       # older adults (65-75 years old) on short stays
    school = 0,
    other = 0.005,
    risk = risk_test_school
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "hojskoler_long_stays",
    label = "High Schools - Long Stays",
    home = 0,
    work = (rep(c(0, (325 + 400) / work_force, 0), c(3, 10, 3))) +
      (rep(c(0, (6100) / work_force, 0), c(4, 2, 10))), # Employees + students on long stays
    school = 0,
    other = 0.004, # 80% of all high schools
    risk = risk_test_school
  )


# Residual Public Sector - Lockdown
dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "remaining_public_sector_lockdown",
    label = "Remaining Public Sector Employees - Working During Lockdown in January 2021",
    home = 0.0,
    work = (rep(c(0, (7000) / work_force, 0), c(3, 10, 3))),
    school = 0,
    other = 0.002, # Corresponds to the population share
    risk = 1
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "remaining_public_sector_recommendation",
    label = "Remaining Public Sector Employees - Recommended to Work from Home",
    home = 0.0,
    work = (rep(c(0, (74000 * 0.02 + 7000) / work_force, 0), c(3, 10, 3))), #
    school = 0,
    other = 0.002, # Corresponds to the population share
    risk = 1
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "remaining_public_sector_phase_5",
    label = "Remaining Public Sector Employees - Phase 5 (May 21)",
    home = 0.0,
    work = (rep(c(0, (15000) / work_force, 0), c(3, 10, 3))),
    school = 0,
    other = 0.004, # Corresponds to the population share
    risk = 1
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "remaining_public_sector_phase_6",
    label = "Remaining Public Sector Employees - Phase 6 (June 14)",
    home = 0.0,
    work = (rep(c(0, (37000) / work_force, 0), c(3, 10, 3))),
    school = 0,
    other = 0.01, # Corresponds to the population share
    risk = 1
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "remaining_public_sector_open",
    label = "Remaining Public Sector Employees - Not Directly Mentioned",
    home = 0.0,
    work = (rep(c(0, (135000) / work_force, 0), c(3, 10, 3))),
    school = 0,
    other = 0.02,
    risk = 1
  )


################################## #
## Private sector ####

# FM expects that 13,500 people go to work. Customers are handled as 0.025 in 'other'.
dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "liberal_professions",
    label = "Liberal Professions",
    home = 0,
    work = (rep(c(16400 / work_force, 0), c(13, 3))), # Source: FM
    school = 0,
    other = 0.025, # Kept here as it includes the increased risk
    risk = 1
  ) # Check risk = 0.61) # 0.61 from Marten's version of Viggo's note

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "grocery_stores",
    label = "Grocery Stores",
    home = 0,
    work = (rep(c(110000 / work_force, 0), c(13, 3))),
    school = 0,
    other = 0.05,
    risk = 1
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "shopping_malls_no_loitering",
    label = "Shopping Malls - No Loitering",
    home = 0,
    work = (rep(c(60000 / work_force, 0), c(13, 3))),
    school = 0,
    other = 0.05,
    risk = 0.5
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "shopping_malls_lockdown",
    label = "Shopping Malls - Lockdown 2021",
    home = 0,
    work = (rep(c(27000 / work_force, 0), c(13, 3))),
    school = 0,
    other = 0.005,
    risk = 0.5
  )

# At full capacity, only 75% of customers are expected to return - in the short term.
dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "retail_stores_over_5000_sqm_open",
    label = "Retail Stores - Over 5000 sqm - Open",
    home = 0,
    work = (rep(c(15000 / work_force, 0), c(13, 3))), # Note: Many employees
    school = 0,
    other = 0.05 * 35 / 235, # 60 million  -> 36
    risk = 0.5
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "retail_stores_over_1000_sqm_under_5000_sqm_open",
    label = "Retail Stores - 1000 to 5000 sqm - Open",
    home = 0,
    work = (rep(c(15000 / work_force, 0), c(13, 3))),
    school = 0,
    other = 0.05 * 35 / 235, # 10 million  -> 35
    risk = 0.5
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "retail_stores_under_1000_sqm_open",
    label = "Retail Stores - Under 1000 sqm - Open",
    home = 0,
    work = (rep(c(45000 / work_force, 0), c(13, 3))),
    school = 0,
    other = 0.05 * 105 / 235, # 135 million  -> 105
    risk = 0.5
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "retail_stores_over_5000_sqm_lockdown_jan_2021",
    label = "Retail Stores - Over 5000 sqm - Lockdown",
    home = 0,
    work = (rep(c(7500 / work_force, 0), c(13, 3))),
    school = 0,
    other = 0.005 * 36 / 235, # 60 million  -> 36
    risk = 0.5
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "retail_stores_over_5000_sqm_lockdown_mar_2021",
    label = "Retail Stores - Over 5000 sqm - Lockdown with appointments Mar 2021",
    home = 0,
    work = (rep(c(7500 / work_force, 0), c(13, 3))),
    school = 0,
    other = 0.005 * 12 / 235, # 12 million corresponding to 25% of normal
    risk = 0.5
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "retail_stores_over_1000_sqm_under_5000_sqm_lockdown",
    label = "Retail Stores - 1000 to 5000 sqm - Lockdown 2021",
    home = 0,
    work = (rep(c(7500 / work_force, 0), c(13, 3))),
    school = 0,
    other = 0.005 * 35 / 235, # 10 million  -> 35
    risk = 0.5
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "retail_stores_under_1000_sqm_lockdown",
    label = "Retail Stores - Under 1000 sqm - Lockdown 2021",
    home = 0,
    work = (rep(c(22500 / work_force, 0), c(13, 3))),
    school = 0,
    other = 0.005 * 105 / 235, # 165 million  -> 105
    risk = 0.5
  )


# Restaurant and cafe
# Ref. HORESTA publication "Danes' eating habits outside the home in 2019"
# Number * <proportion of cafe + restaurant> for [12-19, 20-29, 30-39, 40-49, 50-64, 65-]
c(109, 136, 101, 98, 62, 33) * (c(0.27, 0.26, 0.26, 0.31, 0.4, 0.55) + 0.1)

# UPDATE 20200512:  + 0.1 corresponds to pubs + cocktail bars + some fast food.
# Assumed for the young, not covered in HORESTA's publication.
tmp <- c(10, 20, 30, 40, 49, 49, 36, 36, 40, 40, 31, 31, 31, 21, 21, 21)

# Average 1 / 2 visits per week ~ 30min per week ~ 5min per day equivalent to 2% of time in 'other'  * 2 due
# to higher number of contacts / time
tmp2 <- tmp / mean(tmp) * 0.04
#  Source: EM
dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "restaurants_and_cafes_lockdown",
    label = "Restaurants and Cafes - Takeaway etc. during lockdown January 2021",
    home = 0,
    work = (rep(c(0, 33000 / work_force, 0), c(3, 10, 3))),
    school = 0,
    other = (tmp2) * 0.25, # Factor 0.25 for half activity and half risk due to takeaway,
    risk = 1
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "restaurants_and_cafes_semi_lockdown",
    label = "Restaurants and Cafes - 75% activity aka May",
    home = 0,
    work = (rep(c(0, 74000 * 0.9 / work_force, 0), c(3, 10, 3))), # Check: 90% at work source EM  qualitative 23 / 4
    school = 0,
    other = (tmp2) * 0.75, # Check: 75% activity source EM qualitative 23 / 4
    risk = 1
  ) # Check # As bars are not open yet

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "restaurants_and_cafes_open",
    label = "Restaurants and Cafes - Fully Open including Bars",
    home = 0,
    work = (rep(c(0, 74000 / work_force, 0), c(3, 10, 3))),
    school = 0,
    other = (tmp2),
    risk = 2
  ) # Check # As it also includes bars

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "conferences",
    label = "Conferences and Fairs",
    home = 0,
    work = (rep(c(4000 / work_force, 0), c(13, 3))),
    school = 0,
    other = 0.01,
    risk = 1
  )

# Corresponds to 1,37 min per Dane per day ~ 0.005 in 'other' with estimated fourfold risk per time: 0.02
dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "indoor_sports_and_association",
    label = "Indoor Sports and Associations and Leisure Activities",
    home = 0,
    work = (rep(c(0, (57600) / work_force, 0), c(3, 10, 3))),
    school = 0,
    other = 0.005,
    risk = 2
  ) # Including fitness etc.

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "indoor_sports_and_association_lockdown",
    label = "Indoor Sports and Associations and Leisure Activities - Lockdown Jan 2021",
    home = 0,
    work = (rep(c(0, (2200) / work_force, 0), c(3, 10, 3))),
    school = 0,
    other = 0.005 / 20,
    risk = 1
  )


# 650,000 people starting outdoor activities. We assume 15 minutes per day, equivalent to 5% of 'other'
## All ages (Phase 2 scenario 7)
# Active: DIF, DGI + DFS + Scouts
#  January: 93774 + 0 + 0 = 93774                                                                                       # nolint start: commented_code_linter
#  March: 119231 + 400 + 64500 = 184131 (Update 307k)
#  April: 202920 + 400 + 80750 = 284070 (Update 485k)
#  May: 312567 + 400 + 97000 = 409967 (Update 693k)
#  Full: 657612 + 800 + 97000 = 754612  # 8 / 8                                                                         # nolint end: commented_code_linter
dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "outdoor_sports_over_21_lockdown",
    label = "Sports - outdoor January", home = 0, work = 0, school = 0, other = 0.05 * (1 / 8 - 1 / 32),
    risk = 0.25
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "outdoor_sports_all_lockdown",
    label = "Sports - outdoor January", home = 0, work = 0, school = 0, other = 0.05 * 1 / 8,
    risk = 0.25
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "outdoor_sports_all_mar_2021",
    label = "Sports - outdoor March", home = 0, work = 0, school = 0, other = 0.05 * 3 / 8,
    risk = 0.25
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "outdoor_sports_all_apr_2021",
    label = "Sports - outdoor April", home = 0, work = 0, school = 0, other = 0.05 * 5 / 8,
    risk = 0.25
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "outdoor_sports_all_may_2021",
    label = "Sports - outdoor May", home = 0, work = 0, school = 0, other = 0.05 * 7 / 8,
    risk = 0.25
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "outdoor_sports_all_open",
    label = "Sports - outdoor all open", home = 0, work = 0, school = 0, other = 0.05 * 8 / 8,
    risk = 0.25
  )


# Max 50
# April 602374
# May 839647
dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "outdoor_sports_all_max_50_people_apr_2021",
    label = "Sports - outdoor April - max 50 pers", home = 0, work = 0, school = 0, other = 0.05 * 0.8,
    risk = 0.25
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "outdoor_sports_all_max_50_people_may_2021",
    label = "Sports - outdoor May - max 50 pers", home = 0, work = 0, school = 0, other = 0.05 * 1.11,
    risk = 0.25
  )


# Check
# 35.8 million visitors per year ~ 3min on average per day ~ 0.01 in other,
# but since cinema, theater has higher density use 0.02
dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "culture",
    label = "Museums, theater, cinema, film and TV",
    home = 0,
    work = (rep(c(0, (21000) / work_force, 0), c(3, 10, 3))),
    school = 0,
    other = 0.02,
    risk = 1
  )

# 1300 + various from others than zoo + additional zoo
dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "culture_outdoor_open",
    label = "Culture + Zoo etc. + amusements",
    home = 0,
    work = (rep(c(0, (6600) / work_force, 0), c(3, 10, 3))),
    school = 0,
    other = 0.001,
    risk = 0.25
  ) # Check

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "culture_outdoor_phase_2",
    label = "Culture + Zoo etc. + amusements - phase2 scenario 9 ",
    home = 0,
    work = (rep(c(0, (4400) / work_force, 0), c(3, 10, 3))),
    school = 0,
    other = 0.001,
    risk = 0.25
  ) # Check

## Residual private ####
private_residual_other_add <- 0.1 # LAEC: New to match January
dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "remaining_private_companies_lockdown",
    label = "Private employees not directly mentioned and working during lockdown in January 2021",
    home = 0.0,
    # LAEC:work = (rep(c(0, (1069000) / work_force, 0), c(3, 10, 3))),  #
    work = (rep(c((1069000) / work_force, 0), c(13, 3))), #
    school = 0,
    other = 0.05 + private_residual_other_add,
    risk = 1
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "remaining_private_companies_phase_1",
    label = paste0("Private employees not directly mentioned and working",
                   "during lockdown after the opening of 0-4th grade (phase 1)"),
    home = 0.0,
    # LAEC work = (rep(c(0, (1069000) / work_force, 0), c(3, 10, 3))),  #
    work = (rep(c((1069000) / work_force, 0), c(13, 3))), #
    school = 0,
    other = 0.05 + private_residual_other_add,
    risk = 1
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "remaining_private_companies_phase_2",
    label = "Private employees not directly mentioned and working during lockdown after March 1st (phase 2)",
    home = 0.0,
    # LAEC work = (rep(c(0, (1262000) / work_force, 0), c(3, 10, 3))),  #
    work = (rep(c((1262000) / work_force, 0), c(13, 3))), #
    school = 0,
    other = 0.05 + private_residual_other_add,
    risk = 1
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "remaining_private_companies_phase_4",
    label = "Private employees not directly mentioned and working during lockdown after May 1st",
    home = 0.0,
    # LAEC work = (rep(c(0, (1332000) / work_force, 0), c(3, 10, 3))),  #
    work = (rep(c((1332000) / work_force, 0), c(13, 3))), #
    school = 0,
    other = 0.06 + private_residual_other_add,
    risk = 1
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "remaining_private_companies_phase_5",
    label = "Private employees not directly mentioned and working during lockdown after May 21st",
    home = 0.0,
    # LAEC work = (rep(c(0, (1489000) / work_force, 0), c(3, 10, 3))),  #
    work = (rep(c((1489000) / work_force, 0), c(13, 3))), #
    school = 0,
    other = 0.06 + private_residual_other_add,
    risk = 1
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "remaining_private_companies_phase_6",
    label = "Private employees not directly mentioned and working during lockdown after June 14th",
    home = 0.0,
    # LAEC work = (rep(c(0, (1594000) / work_force, 0), c(3, 10, 3))),  #
    work = (rep(c((1594000) / work_force, 0), c(13, 3))), #
    school = 0,
    other = 0.07 + private_residual_other_add,
    risk = 1
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "remaining_private_companies_summer",
    label = "Private employees not directly mentioned and working during the summer vacation",
    home = 0.0,
    work = (rep(c((1094000) / work_force, 0), c(13, 3))), #
    school = 0,
    other = 0.05 + private_residual_other_add,
    risk = 1
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "remaining_private_companies_open",
    label = "Private employees not directly mentioned",
    home = 0.0,
    work = (rep(c((1750000) / work_force, 0), c(13, 3))), #
    school = 0,
    other = 0.07 + private_residual_other_add,
    risk = 1
  )



################################## #
dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "home_22_5pp",
    label = "Home + 22.5%", home = 0.225, work = 0, school = 0, other = 0, risk = 1
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "home_25pp",
    label = "Home + 25%", home = 0.25, work = 0, school = 0, other = 0, risk = 1
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "home_27_5pp",
    label = "Home + 27.5%", home = 0.275, work = 0, school = 0, other = 0, risk = 1
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "home_35pp",
    label = "Home + 35%", home = 0.35, work = 0, school = 0, other = 0, risk = 1
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "home_05pp",
    label = "Home + 5%", home = 0.05, work = 0, school = 0, other = 0, risk = 1
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "home_10pp",
    label = "Home + 10%", home = 0.10, work = 0, school = 0, other = 0, risk = 1
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "home_15pp",
    label = "Home + 15%", home = 0.15, work = 0, school = 0, other = 0, risk = 1
  )

dk_activity_units <- dk_activity_units |>
  add_activity(
    activity = "other_10pp",
    label = "Other + 10%", home = 0, work = 0, school = 0, other = 0.10, risk = 1
  ) # Mentality change around Feb 1, 2022

###################################### #

# Source: Danish Swimming Pool Technical Association.
# Each Dane uses 4 minutes/day ~ 0.015 pp in other. There is assumed four times the risk, so 0.06

# Convert to data.frame and then into nested lists
df_to_list <- function(df, name_col = NULL) {
  out <- split(dplyr::select(df, !tidyselect::any_of(name_col)), seq_len(nrow(df)))
  if (!is.null(name_col)) names(out) <- dplyr::pull(df, {{ name_col }})
  return(out)
}

dk_activity_units <- dk_activity_units |>
  as.data.frame() |>
  df_to_list(name_col = "activity") |>
  purrr::map(~ as.list(.) |> purrr::map(unlist))


usethis::use_data(dk_activity_units, overwrite = TRUE)
