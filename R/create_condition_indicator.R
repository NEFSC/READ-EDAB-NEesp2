
#' Create species condition indicator
#'
#' This function calculates species condition from a survdat data pull. 
#' Methods derived from Laurel Smith (https://github.com/Laurels1/Condition/blob/master/R/RelConditionEPU.R)
#' 
#' @param data data table. NEFSC survey data generated by 'survdat::get_survdat_data(channel, getBio = T, getLengths = T)'
#' @param LWparams L-W parameters from Wigley et al. 2003 from NEesp2::LWparams
#' @param species.codes data table. Species codes and names from NEesp2::species.codes 
#' @importFrom magrittr %>%
#' @return Returns R object `condition`, a data frame of species condition by EPU with scale.
#' @export

species_condition <- function(data, 
                              LWparams = NEesp2::LWparams, 
                              species.codes = NEesp2::species.codes) {
  
  #Parsing survey data to EPU based on STRATUM instead of EPU.shp files in survdat and filter out NA
  survey.data <- data %>% 
    dplyr::left_join(NEesp2::strata_epu_key)
    # dplyr::mutate(EPU = dplyr::case_when(STRATUM %in% c(1010:1080, 1100:1120, 1600:1750, 3010:3450, 3470, 3500, 3510) ~ 'MAB',
    #                                      STRATUM %in% c(1090, 1130:1210, 1230, 1250, 3460, 3480, 3490, 3520:3550) ~ 'GB',
    #                                      STRATUM %in% c(1220, 1240, 1260:1290, 1360:1400, 3560:3830)~ 'GOM',
    #                                      STRATUM %in% c(1300:1352, 1401:1599, 3840:3990)~ 'SS'))
  
  #Change sex = NA to sex = 0
  fall <- survey.data %>% 
    dplyr::filter(SEASON == 'FALL') %>% 
    dplyr::mutate(sex = dplyr::if_else(is.na(SEX), '0', SEX))
  
  
  #Standardize syntax of Condition L-W data for merge with survey data:
  # LWparams <- readr::read_csv(here::here("data-raw/tech_memo_parameters_table_format.csv"))
  # LWparams1 <- dplyr::mutate(LWparams,
  #                            lna1 = substr(ln_a, 2, nchar(ln_a)),
  #                            lna = as.numeric(lna1)*-1)
  
  #Parse data by season
  # LWpar <- LWparams %>% 
  #   dplyr::mutate(SEASON = dplyr::case_when(Season == 'Autumn' | 
  #                                             Season == 'Win/Aut' | 
  #                                             Season == 'Spr/Aut' |
  #                                             Season == 'Win/Spr/Aut' ~ "FALL",
  #                                           Season == 'Win/Spr' |
  #                                             Season == 'Spring' ~ 'SPRING',
  #                                           Season == 'Winter' ~ 'WINTER',
  #                                           TRUE ~ NA))
                    
                    
                    # dplyr::if_else(Season == 'Autumn', as.character('FALL'),
                                          # dplyr::if_else(Season == 'Win/Aut', as.character('FALL'),
                                                         # dplyr::if_else(Season == 'Spr/Aut', as.character('FALL'),
                                                                        # dplyr::if_else(Season == 'Win/Spr/Aut', as.character('FALL'),        
                                                                                       # dplyr::if_else(Season == 'Win/Spr', as.character('SPRING'),
                                                                                                      # dplyr::if_else(Season == 'Spring', as.character('SPRING'), 
                                                                                                                     # dplyr::if_else(Season == 'Winter', as.character('WINTER'),'NA'))))))))
  LWfall <- LWparams %>% dplyr::filter(SEASON == 'FALL')
  
  #By Species: Parse Combined gender L-Ws by sex if no sex-specific parameters available. Otherwise assign SEX codes:
  LWfall_orig <- LWfall
  LWfall <- LWfall[-c(1:nrow(LWfall)),]
  speciesList <- unique(LWfall_orig$SpeciesName)
  numSpecies <- length(speciesList)
  for (spp in 1:numSpecies) {
    sppTibble <- dplyr::filter(LWfall_orig,SpeciesName == speciesList[spp])
    if (nrow(sppTibble) == 1) {
      LWfall <- rbind(LWfall,sppTibble)
      newRow <- sppTibble[1,]
      newRow$Gender <- "Male"
      LWfall <- rbind(LWfall,newRow)
      newRow <- sppTibble[1,]
      newRow$Gender <- "Female"
      LWfall <- rbind(LWfall,newRow)
    } else if (nrow(sppTibble) == 3) {
      LWfall <- rbind(LWfall,sppTibble)
    }
  }
  
  #Add SEX for Combined gender back into Wigley at all data (loses 4 Gender==Unsexed):
  LWpar_sexed <- LWfall %>% 
    dplyr::mutate(sex = dplyr::if_else(Gender == 'Combined', as.character(0),
                                       dplyr::if_else(Gender == 'Unsexed', as.character(0),
                                                      dplyr::if_else(Gender == 'Male', as.character(1),
                                                                     dplyr::if_else(Gender == 'Female', as.character(2),'NA')))))
  
  #Duplicate Combined for sex=0 and sex=4 (Trans) for BSB:
  LWpar_BSB <- LWpar_sexed %>% 
    dplyr::filter(LW_SVSPP == 141, Gender == 'Combined') %>%
    dplyr::slice(1) %>%
    dplyr::mutate(sex = as.character(4))
  
  LWpar_sex <- dplyr::bind_rows(LWpar_sexed, LWpar_BSB)
  
  LWpar_spp <- LWpar_sex %>% 
    dplyr::mutate(SVSPP = as.numeric(LW_SVSPP))
  
  #Join survdat data with LW data
  mergedata <- dplyr::left_join(fall, LWpar_spp, by= c('SEASON', 'SVSPP', 'sex'))
  
  #filters out values without losing rows with NAs:
  mergewt <- dplyr::filter(mergedata, is.na(INDWT) | INDWT<900)
  mergewtno0 <- dplyr::filter(mergewt, is.na(INDWT) | INDWT>0.004)
  mergelenno0 <- dplyr::filter(mergewtno0, is.na(LENGTH) | LENGTH>0)
  mergelen <- dplyr::filter(mergelenno0, !is.na(LENGTH))
  mergeindwt <- dplyr::filter(mergelen, !is.na(INDWT))
  mergeLW <- dplyr::filter(mergeindwt, !is.na(lna))
  
  ###########################################
  ### Calculate species condition ###
  
  condcalc <- dplyr::mutate(mergeLW, 
                            predwt = (exp(lna))*LENGTH^b,
                            RelCond = INDWT/predwt)
  
  cond.epu <- dplyr::filter(condcalc, is.na(RelCond) | RelCond<300)
  
  #calculate single standard deviation and mean of relative condition for each species and sex:
  condstdev <- dplyr::group_by(cond.epu, SVSPP, SEX) %>% 
    dplyr:: summarize(mean = mean(RelCond), sd = sd(RelCond))
  
  #Remove relative conditions that are outside of 1 standard deviation
  condsd <- dplyr::left_join(cond.epu, condstdev, by=c('SVSPP', 'SEX'))
  dplyr::ungroup(condsd)
  cond.sd <- dplyr::filter(condsd, RelCond < (mean+(2*sd)) & RelCond > (mean-(2*sd)))
  
  #Only including condition that is within 1 standard deviation of mean for each species:
  cond.epu <- cond.sd %>% dplyr::filter(is.na(sex) | sex != 4)
  cond.epu <- cond.epu %>% dplyr::mutate(sexMF = sex)
  
  #Read in df of SVSPP codes + Species names and join by Species
  # species.codes <- utils::read.csv(here::here("data-raw/bottomtrawl_species_codes_names.csv"))
  cond.epu <- dplyr::left_join(cond.epu, species.codes, by= c('SVSPP'))
  
  #Summarize annually by EPU 
  annualcondEPU <- cond.epu %>% 
    dplyr::group_by(Species,EPU, YEAR) %>% 
    dplyr::summarize(MeanCond = mean(RelCond), nCond = dplyr::n())
  
  condN <- dplyr::filter(annualcondEPU, nCond>=3) %>% 
    dplyr::ungroup()
  
  condNSppEPU <- condN %>% 
    dplyr::add_count(Species, EPU) %>% 
    dplyr::filter(n >= 20)
  
  #Rename and select columns
  cond_ecodata <- condNSppEPU %>% dplyr::rename(Time = YEAR, Var = Species)
  rel_condition <- cond_ecodata %>% dplyr::select(Var, EPU, Time, MeanCond, nCond)
  
  #Add variance and sd
  condition <- rel_condition %>%
    dplyr::rename(Species = Var) %>%
    dplyr::mutate(sd = sd(MeanCond, na.rm = TRUE),
                  variance = var(MeanCond, na.rm = TRUE))
  
  return(condition)
}
