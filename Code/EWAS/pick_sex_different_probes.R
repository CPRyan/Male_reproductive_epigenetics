
here::here()


## General Packages - messages hidden
library(tidyverse) # loads ggplot, tidyr, readr, dplyr, purrr, forcats, tibble, stringr
library(haven) # Used to load different datatypes, like .DTA or .dat
library(readr) # Also used to read different data types
library(sjlabelled) # as_factor, as_character
library(here)
library(knitr)
library(kableExtra)


## Bioconductor packages - messages hidden
library(limma)
library(Biobase)



# Load DNAm Data

## Select men with data
men_w_eage <-read_csv(here::here("Data/Other", "men_w_eage.csv")) %>% 
  mutate(uncchdid = as_character(uncchdid))

## Get Mvals
mvals <-data.table::fread(file = here::here("Data/DNAm", "mvals_cebu_final_Xincl.csv")) 

mvals <-mvals %>% column_to_rownames("V1")


# Loads the function for beta2m and m2beta
source(here::here("Code/EWAS", "EWAS_functions.R"))

# Originally used to calculate 5 percent variable. But loads as names_var_probes below. 
# source(here("Code/functions", "b-val_filter_5percent.R"))

############ 
# Control Vars
mf_clockdata <-read_csv(here::here("Data/DNAmAge_new", "QC_Norm_LisaMcEwan_meth_DatMini3.output.csv")) %>% 
  mutate(uncchdid = as_character(uncchdid)) %>% 
  mutate(Sex = if_else(Female == "1", "Female", "Male")) %>% 
  filter(Female == 1 & predictedGender == "female" | Female == 0 & predictedGender == "male") %>% 
  select(uncchdid, Female, Sex, predictedGender, Age)
  

# make_matrix.
# Note that because I was starting with more in mvals than I had in phenos, I needed to filter out replicates etc. first before using sex_matrix_drop. 
ids <-substr(names(mvals), 0,5)
ids <-ids[!duplicated(ids)]
mvals <-mvals[, which(colnames(mvals) %in% ids)]

sex_matrix_drop <-make_matrix(mvals, mf_clockdata)

# Fix columns.
sex_shuffle <-reshuffle_pheno_rows(Matrix = sex_matrix_drop, phenos = mf_clockdata)

# might be ok, but I gotta check if I need the order of columns to change. 
#################################
# Check - e.g. 
mvals["cg00000289", "23217"]
r_matrix_drop["cg00000289", "23217"]

check_shuffle(sex_matrix_drop, sex_shuffle)
# Must be true to proceed.


##################################
# Create the design matrix;
names(sex_shuffle)

################
# P value for cutoff
pval_cutoff <- 0.05
pval_all <-1
################
                                                                                 

################
# romantic
################
design <- model.matrix(~ Sex + Age, data = sex_shuffle);

# Use limma_fit function
lmfit_results <-limma_fit(generic_matrix = sex_matrix_drop, generic_design = design)

# Check the summary table
sexdiff_summary <-limma_summary(lmfit_results, pvally = pval_cutoff)
sexdiff_summary

# Check the toptable. 
sexdiff_toptable <-limma_toptable(lmfit_results, coeffy = "SexMale", pvally = pval_all)


sex_probes <-sexdiff_toptable %>% 
  filter(adj.P.Val < 0.05) %>% 
  select(probe)

write_csv(sex_probes, here::here("Data/DNAm", "sex_probes.csv"))
