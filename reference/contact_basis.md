# Contact basis

This data set contains contact matrices and population data for 176
geographical regions. The contact matrices uses data provided by the
`contactdata` package while the population data is provided by the US
Census Bureau.

## Source

U.S. Census Bureau. (n.d.). *International Database: World Population
Estimates and Projections*. U.S. Department of Commerce. Retrieved
October 03, 2024, from https://data.census.gov/

Gruson H (2023). "contactdata: Social Contact Matrices for 152
Countries." R package version 1.0.0,
<https://CRAN.R-project.org/package=contactdata>.

Prem K, Cook AR, Jit M (2017). "Projecting social contact matrices in
152 countries using contact surveys and demographic data." *PLoS
Computational Biology*, *13*(9), e1005697.
doi:10.1371/journal.pcbi.1005697
<https://doi.org/10.1371/journal.pcbi.1005697>.

Prem K, van Zandvoort K, Klepac P, Eggo RM, Davies NG, Group CCW, Cook
AR, Jit M (2021). "Projecting contact matrices in 177 geographical
regions: An update and comparison with empirical data for the COVID-19
era." *PLoS Computational Biology*, *17*(7), e1009098.
doi:10.1371/journal.pcbi.1009098
<https://doi.org/10.1371/journal.pcbi.1009098>.

## Details

The contact matrices are provided for each of the four arenas: "Work",
"Home", "School", "Other" in 5-year age groups.

The `diseasy` SEIR models are configured to use a specific
transformation of the contact matrices. To make the definition of
"contact" matrix more clear, lets start with the definitions from this
paper:
https://www.medrxiv.org/content/10.1101/2020.02.16.20023754v2.full.pdf

m_ij the raw contact matrix elements from age group i to age group j  
c_ij the reciprocal contact matrix elements  
w_i the proportion of population that fall into age group i  

The `diseasy` SEIR models are configured to use a symmetric, weighted
set of contacts matrices where the elements are 0.5 \* (c_ij \* w_j +
c_ji \* w_i)

Since the `contactdata` package gives the number of contacts directly
(in their framework, denoted as X_ij), we transform to the above
elements through these intermediaries:  
m_ij = X_ij / w_j  
c_ij = m_ij \* w_i = X_ij \* w_i / w_j  

The population data includes the proportion of the population in these
5-year age groups as well as information of 1-year age-groups to allow
transformation of the contact matrices into other age cuts.

This product uses the Census Bureau Data API but is not endorsed or
certified by the Census Bureau.

## Author

Rasmus Skytte Randl\u00F8v <rske@ssi.dk>

Lasse Engbo Christiansen <lsec@ssi.dk>
