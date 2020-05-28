here <-here::here()

source(here::here("/Code/survey_measures", "01c_repro.R"))


# Pick the variables of interest. 
names(repro)


#################################

# Pick those with few missing values. 

#################################


colSums(is.na(repro))

# Cut allele1_num because I'm missing 8 guys
r <-repro %>% 
  select(uncchdid, romantic, sexinter, presrela, evermar, numbsex, new_numbpreg) %>% 
  na.omit() 

############ 
# Other info incorporate
control_vars <-read_csv(here::here("Data/Other", "control_vars.csv")) %>% mutate(uncchdid = as_character(uncchdid))
names(control_vars)

r <-left_join(control_vars, r, by = "uncchdid") %>% na.omit()
############ 

#################################
# Function for make matrix and fix columns
source(here::here("/Code/EWAS", "EWAS_functions.R"))

# make_matrix.
r_matrix_drop <-make_matrix(mvals, r)

# Fix columns.
r_shuffle <-reshuffle_pheno_rows(Matrix = r_matrix_drop, phenos = r)

# might be ok, but I gotta check if I need the order of columns to change. 
#################################
# Check - e.g. 
mvals["cg00000289", "23217"]
r_matrix_drop["cg00000289", "23217"]

check_shuffle(r_matrix_drop, r_shuffle)
# Must be true to proceed.


#################################
# Make sure EWAS_functions.R is loaded
source(here::here("/Code/EWAS", "EWAS_functions.R"))

# Create the design matrix;
names(r_shuffle)

################
# P value for cutoff
pval_cutoff <- 0.1
pval_all <-1
################



################
# romantic
################
design <- model.matrix(~ romantic + age_blood05 + smoke + drink +  icpc1 + icpc2 + icpc3 + bmi + SEAsum_83_05, data = r_shuffle);

# Use limma_fit function
lmfit_results <-limma_fit(generic_matrix = r_matrix_drop, generic_design = design)

# Check the summary table
romantic_summary <-limma_summary(lmfit_results, pvally = pval_cutoff)
romantic_summary

# Check the toptable. 
romantic_toptable <-limma_toptable(lmfit_results, coeffy = "romantic", pvally = pval_all)


################
# sexual intercourse
################
design <- model.matrix(~ sexinter + age_blood05 + smoke + drink +  icpc1 + icpc2 + icpc3 + bmi + SEAsum_83_05, data = r_shuffle);

# Use limma_fit function
lmfit_results <-limma_fit(generic_matrix = r_matrix_drop, generic_design = design)

# Check the summary table
sexinter_summary <-limma_summary(lmfit_results, pvally = pval_cutoff)
sexinter_summary

# Check the toptable. 
sexinter_toptable <-limma_toptable(lmfit_results, coeffy = "sexinter", pvally = pval_all)

################
# presrela
################
design <- model.matrix(~ presrela + age_blood05 + smoke + drink +  icpc1 + icpc2 + icpc3 + bmi + SEAsum_83_05, data = r_shuffle);

# Use limma_fit function
lmfit_results <-limma_fit(generic_matrix = r_matrix_drop, generic_design = design)

# Check the summary table
presrela_summary <-limma_summary(lmfit_results, pvally = pval_cutoff)
presrela_summary

# Check the toptable. 
presrela_toptable <-limma_toptable(lmfit_results, coeffy = "presrela", pvally = pval_all)


################
# numbsex
################
design <- model.matrix(~ log(numbsex+1) + age_blood05 + smoke + drink +  icpc1 + icpc2 + icpc3 + bmi + SEAsum_83_05, data = r_shuffle);

# Use limma_fit function
lmfit_results <-limma_fit(generic_matrix = r_matrix_drop, generic_design = design)

# Check the summary table
numbsex_summary <-limma_summary(lmfit_results, pvally = pval_cutoff)
numbsex_summary

# Check the toptable. 
numbsex_toptable <-limma_toptable(lmfit_results, coeffy = "log(numbsex + 1)", pvally = pval_all)


################
# numbpreg
################
design <- model.matrix(~ new_numbpreg + age_blood05 + smoke + drink +  icpc1 + icpc2 + icpc3 + bmi + SEAsum_83_05, data = r_shuffle);

# Use limma_fit function
lmfit_results <-limma_fit(generic_matrix = r_matrix_drop, generic_design = design)

# Check the summary table
new_numbpreg_summary <-limma_summary(lmfit_results, pvally = pval_cutoff)
new_numbpreg_summary

# Check the toptable. 
new_numbpreg_toptable <-limma_toptable(lmfit_results, coeffy = "new_numbpreg", pvally = pval_all)






