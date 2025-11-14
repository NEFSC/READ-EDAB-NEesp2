#' @title Highly Migratory Species (HMS) Names and Species Codes
#' @description A list of HMS species in the North Atlantic with corresponding species code numbers and categories.
#' @format A data frame with 42 rows and 4 variables:
#' \describe{
#'   \item{\code{SP_CODE}}{double Species code numbers to create the rec_hms indicator in ecodata}
#'   \item{\code{COMMON_NAME}}{character Common name of the species}
#'   \item{\code{SP_CATEGORY}}{character HMS Category (Large Coastal, Small Coastal, Prohibited, Pelagic, Scombridae, Billfishes)}
#'   \item{\code{COMMON_POP}}{character Other identifier for some shark species} 
#'}
"hms_key"
#'
#'
#' @title Species codes for bottom trawl survey
#' @description A list of species codes (SVSPP) and their corresponding common names as found in a bottom trawl survey data pull.
#' @format A data frame with 50 rows and 2 variables:
#' \describe{
#'   \item{\code{SVSPP}}{integer A standard code which represents a species caught in a trawl or dredge. Refer to the SVDBS.SVSPECIES_LIST}
#'   \item{\code{Species}}{character Common name of the species} 
#'}
"species.codes"
#'
#'
#' @title Strata EPU Key
#' @description A list of bottom trawl survey strata and their corresponding EPUs on the Northeast U.S. Continental Shelf.
#' @format A data frame with 1553 rows and 2 variables:
#' \describe{
#'   \item{\code{STRATUM}}{integer Bottom trawl survey strata number}
#'   \item{\code{EPU}}{character Ecological Production Unit (Mid-Atlantic Bight - MAB, Georges Bank - GB, Gulf of Maine - GOM, Scotian Shelf -SS)} 
#'}
"strata_epu_key"
#'
#'
#'#' @title Length-weight parameters from Wigley et al. (2003)
#' @description Length-weight relationships for 74 fish species collected during NEFSC research vessel bottom trawl surveys, 1992-99 as published in Wigley et al. (2003)
#' @format A data frame with 235 rows and 14 variables:
#' \describe{
#'   \item{\code{SpeciesName}}{character Species common name}
#'   \item{\code{LW_SVSPP}}{integer A standard code which represents a species caught in a trawl or dredge. Refer to the SVDBS.SVSPECIES_LIST}
#'   \item{\code{Season}}{character Season sampled (Winter, Autumn, Spring or a combination)}
#'   \item{\code{Gender}}{character Gender of the fish sampled (Female, Male, or Combined)}
#'   \item{\code{N}}{integer Number of fish sampled}
#'   \item{\code{ln_a}}{character Length-weight parameter estimate}
#'   \item{\code{SEa}}{double Standard error of ln a}
#'   \item{\code{b}}{double Length-weight parameter estimate}
#'   \item{\code{SEb}}{double Standard error of b}
#'   \item{\code{SE_estimate}}{double Standard error of the weight estimate}
#'   \item{\code{r2}}{double Regression correlation coefficient}
#'   \item{\code{lna1}}{double Length-weight parameter estimate}
#'   \item{\code{lna}}{double Inverse of ln_a and lna1}
#'   \item{\code{SEASON}}{character Parsed season data into Winter, Spring, or Fall} 
#'}
"LWparams"
#'
#'
#' @title Survdat data pull 
#' @description A data pull of bottom trawl survey data from 1983-2024 using the survdat package.
#' @format A data frame with 16527 rows and 28 variables:
#' \describe{
#'   \item{\code{CRUISE6}}{integer Code uniquely identifying cruise. The first four digits indicate the year and the last two digit uniquely identify the cruise within the year.}
#'   \item{\code{STATION}}{integer Unique sequential order in which stations have been completed.}
#'   \item{\code{STRATUM}}{integer A predefined area where a net dredge, or other piece of gear was deployed. Code consists of 2 parts: Stratum group code number (2 bytes) and stratum number (3 bytes).}
#'   \item{\code{SVSPP}}{integer A standard code which represents a species caught in a trawl or dredge. Refer to the SVDBS.SVSPECIES_LIST}
#'   \item{\code{CATCHSEX}}{integer Code used to identify species that are sexed at the catch level.}
#'   \item{\code{LENGTH}}{integer Measured length of species in centimeters (cm).}
#'   \item{\code{TOW}}{integer Sequential number representing order in which station was selected within a stratum.}
#'   \item{\code{SVVESSEL}}{character Standard two character code for a survey vessel.}
#'   \item{\code{YEAR}}{integer Year in which cruise was conducted.}
#'   \item{\code{SEASON}}{character Season of the year in which cruise was conducted.}
#'   \item{\code{LAT}}{double Beginning latitude of tow in decimal degrees.}
#'   \item{\code{LON}}{double Beginning longitude of tow in decimal degrees.}
#'   \item{\code{EST_TOWDATE}}{character Date and time represented by Eastern Standard Time (EST) for the start of a tow or deployment.}
#'   \item{\code{DEPTH}}{integer A four digit number recording the average depth, to the nearest meter, during a survey gear deployment.}
#'   \item{\code{SURFTEMP}}{double Surface temperature of water (degrees Celsius).}
#'   \item{\code{SURFSALIN}}{double Salinity at water surface in practical salinity units (PSU).}
#'   \item{\code{BOTTEMP}}{double Bottom temperature (degrees Celsius).}
#'   \item{\code{BOTSALIN}}{double Bottom salinity in Practical Salinity Units (PSU).}
#'   \item{\code{ABUNDANCE}}{integer Expanded number of individuals of a species caught at a given station.}
#'   \item{\code{BIOMASS}}{double Expanded catch weight of a species caught at a given station.}
#'   \item{\code{NUMLEN}}{integer Expanded number of specimens at a given length.}
#'   \item{\code{INDID}}{integer A unique identifier for each fish sampled.}
#'   \item{\code{INDWT}}{double Individual weight (KG) of species being sampled.}
#'   \item{\code{SEX}}{integer Code indicating sex of fish or invertebrate species.}
#'   \item{\code{MATURITY}}{character Stage of maturation of the fish being sampled.}
#'   \item{\code{AGE}}{integer Age of specimen in years.}
#'   \item{\code{STOM_VOLUME}}{double Volume of the stomach contents of the fish sampled, measured to the nearest tenth of a cubic centimeter (cc).}
#'   \item{\code{STOM_WGT}}{double Stomach weight of an individual fish in grams.} 
#'}
"survdat_subset"
#'
#'
#' @title Species lookup table from {survdat}
#' @description A species lookup table generated from survdat::get_species()
#' @format A data frame with 2047 rows and 21 variables:
#' \describe{
#'   \item{\code{SCINAME}}{character Scientific name of specimen.}
#'   \item{\code{COMNAME}}{character Accepted common name of a fish or invertebrate species.}
#'   \item{\code{SVSPP}}{integer A standard code which represents a species caught in a trawl or dredge.}
#'   \item{\code{SVABBR}}{character Abbreviation of common name of specimen.}
#'   \item{\code{PYSPP}}{double Prey species name.}
#'   \item{\code{PYABBR}}{character Prey abbreviation.}
#'   \item{\code{NODCCODE}}{character National Oceanographic Data Center (NODC) taxononic code for species.}
#'   \item{\code{NODCTYPE}}{logical National Oceanographic Data Center (NODC) Type name code: S-scientific name, C-Common name}
#'   \item{\code{NODCLEVL}}{character National Oceanographic Data Center (NODC) taxanomic level: P-Phylum, C-Class, O-Order, F-Family, G-Genus, S-Species}
#'   \item{\code{NODCCTRL}}{character National Oceanographic Data Center (NODC).}
#'   \item{\code{NODCAUTH}}{character NODC code author (person who described species).}
#'   \item{\code{AUTHOR}}{character Author of document.}
#'   \item{\code{PICTURE}}{character Citation for picture of species.}
#'   \item{\code{COMMNT}}{character Comment related to species in SVDBS.SVSPECIES_LIST.}
#'   \item{\code{O_SVSCNM}}{character Former scientific name used by Survey branch.}
#'   \item{\code{O_SVCONM}}{character Former common name used by Survey branch.}
#'   \item{\code{O_SVSPP}}{integer Former species code.}
#'   \item{\code{DOC}}{character Date when the current record was updated or changed. Date format (MM/DD/YY HH:MI:SS)}
#'   \item{\code{UOC}}{character The Oracle username of the individual who changed the current record.}
#'   \item{\code{DOE}}{character Date when record was inserted into Oracle. Date format (MM/DD/YY HH:MI:SS).}
#'   \item{\code{UOE}}{character The Oracle username of the individual who entered the new record.} 
#'}
"species_data"