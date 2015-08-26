age_years <- function(earlier, later)
{
  lt <- data.frame(earlier, later)
  age <- as.numeric(format(lt[,2],format="%Y")) - as.numeric(format(lt[,1],format="%Y"))
  
  dayOnLaterYear <- ifelse(format(lt[,1],format="%m-%d")!="02-29",
                           as.Date(paste(format(lt[,2],format="%Y"),"-",format(lt[,1],format="%m-%d"),sep="")),
                           ifelse(as.numeric(format(later,format="%Y")) %% 400 == 0 | as.numeric(format(later,format="%Y")) %% 100 != 0 & as.numeric(format(later,format="%Y")) %% 4 == 0,
                                  as.Date(paste(format(lt[,2],format="%Y"),"-",format(lt[,1],format="%m-%d"),sep="")),
                                  as.Date(paste(format(lt[,2],format="%Y"),"-","02-28",sep=""))))  
  age[which(dayOnLaterYear > lt$later)] <- age[which(dayOnLaterYear > lt$later)] - 1
  age
}


#' An Import Function
#'
#' This function allows you to import data from eHARS into R
#' @param eharsdir The directory in which the eHARS person view file can be found
#' @keywords ehars import
#' @export

importehars <- function(eharsdir){
  person <- data.frame(fread(eharsdir, header = T, sep = ','))
  # fread gives warnings; it works but read.csv is an alternative
  keep <- c('cur_state_cd',
            'cur_zip_cd',
            'rbi_state_cd',
            'rbi_zip_cd',
            'rad_state_cd',
            'rad_zip_cd',
            'rsh_state_cd',
            'rsh_zip_cd',
            'rsa_state_cd',
            'rsa_zip_cd',
            'rhe_state_cd',
            'rhe_zip_cd',
            'birth_defects',
            'birth_type',
            'birth_wt',
            'breastfed',
            'delivery_method',
            'm_hiv_dt',
            'maternal_dob',
            'maternal_stateno',
            'month_preg_pnc',
            'neonatal_status',
            'neonatal_status_weeks',
            'num_pnc_visits',
            'other_art_labor',
            'other_art_neonatal',
            'other_art_neonatal_dt',
            'other_art_preg',
            'zido_labor',
            'zido_neonatal',
            'zido_neonatal_dt',
            'zido_preg',
            'zido_prior_preg',
            'zido_week',
            'aids_age_mos',
            'aids_age_yrs',
            'aids_categ',
            'aids_cdc',
            'aids_dx_dt',
            'cd4_recent_cnt_dt',
            'cd4_recent_cnt_pct_dt',
            'cd4_recent_cnt_value',
            'cd4_recent_pct_dt',
            'cd4_recent_pct_value',
            'cur_age',
            'death_age_mos',
            'death_age_yrs',
            'dx_status',
            'expo_categ',
            'hiv_aids_dx_dt',
            'hiv_age_mos',
            'hiv_age_yrs',
            'hiv_categ',
            'hiv_cdc',
            'hiv_dx_dt',
            'race',
            'trans_categ',
            'transx_categ',
            'vl_recent_dt',
            'vl_recent_value',
            'death_state_cd',
            'dod',
            'hiv_death',
            'birth_country_cd',
            'birth_sex',
            'current_gender',
            'current_sex',
            'dob',
            'ethnicity1',
            'race1',
            'race2',
            'race3',
            'race4',
            'race5',
            'vital_status',
            'hf_state_cd',
            'hf_zip_cd',
            'bf_state_cd',
            'bf_zip_cd',
            'pf_state_cd',
            'pf_zip_cd',
            'stateno',
            'rsd_state_cd',
            'rsd_zip_cd',
            'rsx_state_cd',
            'rsx_zip_cd')
  ehars <- person[keep]
  
  dt_names <- c(names(ehars[grepl("_dt", names(ehars))]), "dob", "dod")
  for (i in 1:length(names(ehars))) {
    if (names(ehars)[i] %in% dt_names) {
        ehars[[i]] <- ifelse(grepl("\\.\\.\\.\\.", ehars[[i]]),  paste(substr(ehars[[i]],1,4),'0615',sep=''), 
                              ifelse(grepl("\\.\\.", ehars[[i]]),  paste(substr(ehars[[i]],1,6),'15',sep=''), ehars[[i]]))
    }
  }  
  ehars
}

createehars <- function(){
  start_date <- as.Date("1950/01/01")
  end_date <- Sys.Date()
  dts <- seq.Date(start_date, end_date, by="day")
  
  race_cat <- c("(6)Not Hispanic, White", "(4)Not Hispanic, Black",
                    "(1)Hispanic, All races", "(3)Not Hispanic, Asian",
                    "(8)Not Hispanic, Multi-race", "(2)Not Hispanic, Amer Indian/Alaska Nat",
                    "(9)Unknown", "(5)Not Hispanic, Nat Hawaiian/Pac Isl", "")  
  
  birth_sex_cat <- c("(M)Male", "(F)Female", "")
  
  trans_categ_cat <-  c("(01)Adult male sexual cntct male (MSM)", "(10)Adult no risk factor reported (NRR)",
                        "(03)Adult MSM & IDU", "(09)Adult no identified risk factor (NIR)",
                        "(04)Adult rcvd clotting factor", "(06)Adult rcvd transfusion/transplant",    
                        "(02)Adult injection drug use (IDU)", "(13)Child rcvd transfusion/transplant",    
                        "(05)Adult heterosexual contact", "(11)Child rcvd clotting factor",           
                        "(12)Perinatal exposure", "(19)Child no identified risk factor (NIR)",
                        "(08)Adult other confirmed risk", "(07)Perinatal exposure w/HIV age 13+ yrs",
                        "(20)Child no risk factor reported (NRR)", "(18)Child other confirmed risk")
  
  age_cat_cat <- c(NA, "(05)30-34 years", "(08)45+ years", "(04)25-29 years", "(06)35-39 years", "(07)40-44 years",
                   "(03)20-25 years", "(02)15-19 years", "(01)<15 years") 
  
  
  for (yr_hiv in 1979:2013) {
    n <- sample(300:1500, 1)
    dts_dob <- dts[year(dts) < yr_hiv]
    dob <- sample(dts_dob, n, replace=TRUE)
    dts_hiv <- dts[year(dts) == yr_hiv]
    hiv_aids_dx_dt <- sample(dts_hiv, n, replace=TRUE)
    dts_aids <- dts[year(dts) > yr_hiv]
    aids_dx_dt <- sample(dts_aids, n, replace=TRUE)
    dts_death <- dts[year(dts) > yr_hiv]    
    dod <- sample(dts_death, n, replace=TRUE)
    race <- sample(race_cat, n, replace=TRUE)
    birth_sex <- sample(birth_sex_cat, n, replace=TRUE)
    trans_categ <- sample(trans_categ_cat, n, replace =TRUE)
    
    data <- data.frame(1:n, yr_hiv, dob, hiv_aids_dx_dt, aids_dx_dt, dod, race, birth_sex, trans_categ, 
                       stringsAsFactors=FALSE)
    
    if(yr_hiv == 1979){ ehars <- data.frame(data)
    } else {ehars <- data.frame(rbind(ehars, data))}
    
  }
  ehars
}
  
formatehars <- function(ehars){
  ehars$count <- 1

  ## Format variables
  
  if (class(ehars$dob) != "Date"){
    ehars$dob <- as.Date(as.character(ehars$dob), "%Y%m%d")
  }
  
  ehars$age_hiv <- age_years(ehars$dob, Sys.Date())
  
  ehars$age_cat <- ifelse(ehars$age_hiv < 15, "(01)<15 years", 
                        ifelse(ehars$age_hiv < 20, "(02)15-19 years",
                               ifelse(ehars$age_hiv < 25, "(03)20-25 years",
                                      ifelse(ehars$age_hiv < 30, "(04)25-29 years",
                                             ifelse(ehars$age_hiv < 35, "(05)30-34 years",
                                                    ifelse(ehars$age_hiv < 40, "(06)35-39 years", 
                                                           ifelse(ehars$age_hiv < 45, "(07)40-44 years",
                                                                  ifelse(ehars$age_hiv >= 45, "(08)45+ years", NA))))))))
  ehars$yr_birth <- year(ehars$dob)
  ehars$yr_death <- as.numeric(substr(as.character(ehars$dod),1,4))
  ehars$yr_hiv <- as.numeric(substr(as.character(ehars$hiv_aids_dx_dt),1,4))
  ehars$yr_aids <- as.numeric(substr(as.character(ehars$aids_dx_dt),1,4))

  ## Drop Variables
  keep <- names(ehars) %in% c("count","yr_birth","yr_hiv","yr_aids","yr_death","race","trans_categ", "age_cat", "birth_sex")
  ehars <- ehars[keep]
  ehars
}

eharsdata <- function(savedir, eharsdir = NULL) {
  if(is.null(eharsdir)) {
    ehars <- createehars() 
  } else {
    ehars <- importehars(eharsdir) }  
  ehars <- formatehars(ehars)
  save(ehars,file=savedir)
}


# 2010 Census Summary Downloaded from:
# http://factfinder.census.gov/bkmk/table/1.0/en/DEC/10_SF1/P1/0100000US.86000

#zip5_pop <- read.csv("./zip5_pop.csv", skip=1)
#zip5_pop <- zip5_pop[c("Id2","Total")]
#zip3_pop <- aggregate(as.numeric(zip5_pop$Total), by=list(substr(zip5_pop$Id2, 1, 3)), FUN=sum)
#save(zip3_pop, file="./zip3_pop.Rda")

# http://aidsvu.org/data-methods/data-methods-zip-code-census-tract/
# To protect the confidentiality of persons living with an HIV/AIDS diagnosis, AIDSVu does not display rates and case counts when the numerator (number of persons living with an HIV/AIDS diagnosis) is less than 5 and/or the denominator (number of people in the ZIP code in that population group) is less than 500. ZIP codes appear in a shade of gray when one or both of these conditions are met (see footnote below map scale). Because rates are not displayed when the numerator is less than 5, the "unstable rate" indicator mentioned above will only be displayed when the numerator is 5 or greater, but less than 12.


# Get variable categories from eHARS
# v <- c()
# for(i in 1:length(person)){
#   add <- unique(person[i])
#   if(nrow(add) < 100) {
#     if(is.null(v)){
#       v <- add
#     } else {
#       v <- c(v, add)
#     }}
# }
# save(v, file="./cats.Rda")


test <- c('cur_state_cd',
  'cur_zip_cd',
  'rbi_state_cd',
  'rbi_zip_cd',
  'rad_state_cd',
  'rad_zip_cd',
  'rsh_state_cd',
  'rsh_zip_cd',
  'rsa_state_cd',
  'rsa_zip_cd',
  'rhe_state_cd',
  'rhe_zip_cd',
  'birth_defects',
  'birth_type',
  'birth_wt',
  'breastfed',
  'delivery_method',
  'm_hiv_dt',
  'maternal_dob',
  'maternal_stateno',
  'month_preg_pnc',
  'neonatal_status',
  'neonatal_status_weeks',
  'num_pnc_visits',
  'other_art_labor',
  'other_art_neonatal',
  'other_art_neonatal_dt',
  'other_art_preg',
  'zido_labor',
  'zido_neonatal',
  'zido_neonatal_dt',
  'zido_preg',
  'zido_prior_preg',
  'zido_week',
  'aids_age_mos',
  'aids_age_yrs',
  'aids_categ',
  'aids_cdc',
  'aids_dx_dt',
  'cd4_recent_cnt_dt',
  'cd4_recent_cnt_pct_dt',
  'cd4_recent_cnt_value',
  'cd4_recent_pct_dt',
  'cd4_recent_pct_value',
  'cur_age',
  'death_age_mos',
  'death_age_yrs',
  'dx_status',
  'expo_categ',
  'hiv_aids_dx_dt',
  'hiv_age_mos',
  'hiv_age_yrs',
  'hiv_categ',
  'hiv_cdc',
  'hiv_dx_dt',
  'race',
  'trans_categ',
  'transx_categ',
  'vl_recent_dt',
  'vl_recent_value',
  'death_zip_cd',
  'death_state_cd',
  'dod',
  'hiv_death',
  'birth_country_cd',
  'birth_sex',
  'current_gender',
  'current_sex',
  'dob',
  'ethnicity1',
  'race1',
  'race2',
  'race3',
  'race4',
  'race5',
  'vital_status',
  'hf_state_cd',
  'hf_zip_cd',
  'bf_state_cd',
  'bf_zip_cd',
  'pf_state_cd',
  'pf_zip_cd',
  'stateno',
  'rsd_state_cd',
  'rsd_zip_cd',
  'rsx_state_cd',
  'rsx_zip_cd')
