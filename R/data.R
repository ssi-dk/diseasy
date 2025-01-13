#' Contact basis
#'
#' This data set contains contact matrices and population data for 176 geographical regions.
#' The contact matrices uses data provided by the `contactdata` package while the population
#' data is provided by the US Census Bureau.
#'
#' The contact matrices are provided for each of the four arenas: "Work", "Home", "School", "Other" in
#' 5-year age groups.
#'
#' The `diseasy` SEIR models are configured to use a specific transformation of the contact matrices.
#' To make the definition of "contact" matrix more clear, lets start with the definitions from this paper:
#' https://www.medrxiv.org/content/10.1101/2020.02.16.20023754v2.full.pdf
#'
#' m_ij the raw contact matrix elements from age group i to age group j\cr
#' c_ij the reciprocal contact matrix elements\cr
#' w_i  the proportion of population that fall into age group i\cr
#'
#' The `diseasy` SEIR models are configured to use a symmetric, weighted set of
#' contacts matrices where the elements are 0.5 * (c_ij * w_j + c_ji * w_i)
#'
#' Since the `contactdata` package gives the number of contacts directly (in their
#' framework, denoted as X_ij), we transform to the above elements through these intermediaries: \cr
#' m_ij = X_ij / w_j\cr
#' c_ij = m_ij * w_i = X_ij * w_i / w_j\cr
#'
#'
#' The population data includes the proportion of the population in these 5-year age groups as well as
#' information of 1-year age-groups to allow transformation of the contact matrices into other age cuts.
#'
#' This product uses the Census Bureau Data API but is not endorsed or certified by the Census Bureau.
#'
#' @name contact_basis
#' @docType data
#' @author Rasmus Skytte Randl\\u00F8v \email{rske@ssi.dk}
#' @author Lasse Engbo Christiansen \email{lsec@ssi.dk}
#' @source
#'  U.S. Census Bureau. (n.d.). _International Database: World Population Estimates and Projections_.
#'  U.S. Department of Commerce. Retrieved
#'  `r format(attr(contact_basis, "creation_date"), "%B %d, %Y")`,
#'  from https://data.census.gov/
#' @source
#'  Gruson H (2023). "contactdata: Social Contact Matrices for 152
#'  Countries." R package version 1.0.0,
#'  <https://CRAN.R-project.org/package=contactdata>.
#' @source
#'   Prem K, Cook AR, Jit M (2017). "Projecting social contact
#'   matrices in 152 countries using contact surveys and demographic
#'   data." _PLoS Computational Biology_, *13*(9), e1005697.
#'   doi:10.1371/journal.pcbi.1005697
#'   <https://doi.org/10.1371/journal.pcbi.1005697>.
#' @source
#'   Prem K, van Zandvoort K, Klepac P, Eggo RM, Davies NG, Group
#'   CCW, Cook AR, Jit M (2021). "Projecting contact matrices in 177
#'   geographical regions: An update and comparison with empirical
#'   data for the COVID-19 era." _PLoS Computational Biology_,
#'   *17*(7), e1009098. doi:10.1371/journal.pcbi.1009098
#'   <https://doi.org/10.1371/journal.pcbi.1009098>.
#' @keywords data
NULL


#' dk_activity_units
#'
#' This data set contains the "units" of activity that was developed for the Danish COVID-19 modelling efforts.
#' Each "activity unit" specifies the amount of activity for the four arenas: Home, Work, School, Other.
#' By combining these units of activity, a complete, granular picture of the societal activity is formed
#' (see dk_reference_scenario for details).
#'
#' These units are developed to cover the proposed and realized scenarios of societal restrictions during the COVID-19
#' pandemic in Denmark. If other scenarios are to be tested, new "activity units" will (likely) have to be developed.
#' We supply the script that generates these units with the package (see data-raw/dk-activity-units.R for details).
#'
#' @name dk_activity_units
#' @docType data
#' @author Lasse Engbo Christiansen \email{lsec@ssi.dk}
#' @author Carsten Kirkeby \email{ckir@sund.ku.dk}
#' @author Frederik Plesner Lyngse \email{frederik.lyngse@sund.ku.dk}
#' @author Adam Mielke \email{admi@dtu.dk}
#' @author Rasmus Skytte Randl\\u00F8v \email{rske@dtu.dk}
#' @keywords data
NULL


#' dk_reference_scenario
#'
#' This data set contains the combination of "activity units" (see dk_activity_units for details) that form a
#' description of the societal activity in Denmark through out the COVID-19 pandemic.
#'
#' The data set consists of a `tibble` with four columns:
#'  - `date`
#'  - `opening`
#'  - `closing`
#'  - `social_distance_work`
#'
#' `date` indicates the date of the activity change
#' `opening` indicates the "activity unit" that is added to the scenario on the given date
#' `closing` indicates the "activity unit" that is removed from the scenario on the given date
#' `social_distance_work` indicates the reduction in transmission risk in the "Work" arena due to social distancing
#'
#' @name dk_reference_scenario
#' @docType data
#' @author Lasse Engbo Christiansen \email{lsec@ssi.dk}
#' @keywords data
NULL


#' diseasy_immunity_optimiser_results
#'
#' This data set contains a summery of the tested optimiser for `?DiseasyImmunity$approximate_compartmental()` method.
#'
#' The data set consists of a `tibble` with columns:
#'  - `target`: The name of the target function being approximated.
#'  - `variation`: Indicating the variation on the target function. One of:
#'    - "Base": using a time scale of 20.
#'    - "Non-zero asymptote": asymptotically approaching 0.2 instead of 0.
#'    - "Twice the time scale": using a time scale of 40.
#'    - `method`: The method of parametrising the approximation.
#'      One of: "free_delta", "free_gamma" or "all_free".
#'    - `strategy`: The strategy employed for optimisation.
#'      One of: "naive", "recursive" or "combination".
#'  - `penalty`: Were unit penalties (`monotonous` and `individual_level`) added to the objective function?
#'  - `M`: The number of compartments.
#'  - `value`: The smallest determined value of the objective function for the optimiser.
#'  - `execution_time`: The time spent by the optimiser.
#'  - `optim_method`: A human-readable label of the optimiser configuration.
#'  - `target_label`: A short-hand combination of target and variation.
#' @name diseasy_immunity_optimiser_results
#' @seealso [DiseasyImmunity]
#' @docType data
#' @author Rasmus Skytte Randl\\u00F8v \email{rske@ssi.dk}
#' @keywords data
NULL


#' seir_example_data
#'
#' This data set contains a the simulation output from a SEIR model to use when testing and developing models.
#'
#' The underlying model is a SEIR model (`?DiseasyModelOdeSeir`) with 2 exposed compartments, 1 infected compartment
#' and 1 recovered compartment. The model uses an exposed period of 2.1 days and a infectious period of 4.5 days.
#' The model uses Denmark for its population and activity scenario with the "baseline" activity unit as the level of
#' activity. The overall infection risk is set to be 0.025 and initially, 0.2 % of the population is newly exposed
#' (i.e. placed in the E1 compartments).
#'
#' The model outputs included in the dataset is in the form of a tibble with the following columns:
#'   - `date`: The time for the output.
#'   - `age_group`: The age group for the output.
#'   - `n_infected`: The direct output of the SEIR model ( = (I1 * L) / 4.5 days).
#'   - `n_positive_simple`: A realisation of the number of test-positives with a 65% test probability.
#'   - `n_positive_simple`: A realisation of the number of test-positives with a overall 65% test probability but with
#'     a reduced test-probability in the weekend.
#'   - `n_admission`: A realisation of the number of admissions to hospital delayed relative to incidence and
#'     accounting for a age dependent risk of admission.
#'
#' @name seir_example_data
#' @docType data
#' @author Rasmus Skytte Randl\\u00F8v \email{rske@dtu.dk}
#' @keywords data
NULL
