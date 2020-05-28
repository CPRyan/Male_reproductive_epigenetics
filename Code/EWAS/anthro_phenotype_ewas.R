here <-here::here()

source(here::here("/Code/survey_measures", "01a_anthro.R"))
source(here::here("/Code/survey_measures", "01h_anthro_1998_05.R"))

anthro<-anthro_time

# Pick the variables of interest. 
names(anthro)


#################################

# Pick those with few missing values. 

#################################
colSums(is.na(anthro))

############ 
# Other info incorporate
control_vars <-read_csv(here::here("Data/Other", "control_vars.csv")) %>% mutate(uncchdid = as_character(uncchdid)) %>% select(-bmi)
names(control_vars)

a <-left_join(control_vars, anthro, by = "uncchdid") %>% na.omit()
############

#################################
# Function for make matrix and fix columns
source(here::here("/Code/EWAS", "EWAS_functions.R"))

# make_matrix.
a_matrix_drop <-make_matrix(mvals, a)

# Fix columns.
a_shuffle <-reshuffle_pheno_rows(Matrix = a_matrix_drop, phenos = a)

# might be ok, but I gotta check if I need the order of columns to change. 
#################################
# Check - e.g. 
mvals["cg00000289", "23217"]
r_matrix_drop["cg00000289", "23217"]

check_shuffle(a_matrix_drop, a_shuffle)
# Must be true to proceed.


##################################################################
# Make sure EWAS_functions.R is loaded
source(here::here("/Code/EWAS", "EWAS_functions.R"))

# Create the design matrix;
names(a_shuffle)

################

################
# P value for cutoff
pval_cutoff <- 0.1
pval_all <-1
################



################
# Height
################
design <- model.matrix(~ height + age_blood05 + smoke + drink + icpc1 + icpc2 + icpc3 + SEAsum_83_05, data = a_shuffle);

# Use limma_fit function
lmfit_results <-limma_fit(generic_matrix = a_matrix_drop, generic_design = design)

# Check the summary table
height_summary <-limma_summary(lmfit_results, pvally = pval_cutoff)
height_summary

# Check the toptable. 
height_toptable <-limma_toptable(lmfit_results, coeffy = "height", pvally = pval_all)

################
# BMI
################
design <- model.matrix(~ bmi + age_blood05 + smoke + drink + icpc1 + icpc2 + icpc3 +  SEAsum_83_05, data = a_shuffle);

# Use limma_fit function
lmfit_results <-limma_fit(generic_matrix = a_matrix_drop, generic_design = design)

# Check the summary table
bmi_summary <-limma_summary(lmfit_results, pvally = pval_cutoff)
bmi_summary

# Check the toptable. 
bmi_toptable <-limma_toptable(lmfit_results, coeffy = "bmi", pvally = pval_all)



################
# arm muscle area
################
design <- model.matrix(~ arm_musc_area + age_blood05 + smoke + drink + icpc1 + icpc2 + icpc3 + SEAsum_83_05, data = a_shuffle);

# Use limma_fit function
lmfit_results <-limma_fit(generic_matrix = a_matrix_drop, generic_design = design)

# Check the summary table
arm_musc_area_summary <-limma_summary(lmfit_results, pvally = pval_cutoff)
arm_musc_area_summary

# Check the toptable. 
arm_musc_area_toptable <-limma_toptable(lmfit_results, coeffy = "arm_musc_area", pvally = pval_all)



################
# fatfree_mass
################
design <- model.matrix(~ fatfree_mass + age_blood05 + smoke + drink + icpc1 + icpc2 + icpc3 + SEAsum_83_05, data = a_shuffle);

# Use limma_fit function
lmfit_results <-limma_fit(generic_matrix = a_matrix_drop, generic_design = design)

# Check the summary table
fatfree_mass_summary <-limma_summary(lmfit_results, pvally = pval_cutoff)
fatfree_mass_summary

# Check the toptable. 
fatfree_mass_toptable <-limma_toptable(lmfit_results, coeffy = "fatfree_mass", pvally = pval_all)


################
# bfperc
################
design <- model.matrix(~ bfperc + age_blood05 + smoke + drink + icpc1 + icpc2 + icpc3 +  SEAsum_83_05, data = a_shuffle);

# Use limma_fit function
lmfit_results <-limma_fit(generic_matrix = a_matrix_drop, generic_design = design)

# Check the summary table
bfperc_summary <-limma_summary(lmfit_results, pvally = pval_cutoff)
bfperc_summary

# Check the toptable. 
bfperc_toptable <-limma_toptable(lmfit_results, coeffy = "bfperc", pvally = pval_all)

################
# Height Accretion
################
design <- model.matrix(~ height_accretion + age_blood05 + smoke + drink + icpc1 + icpc2 + icpc3 + SEAsum_83_05 +time_bt_98_05, data = a_shuffle);

# Use limma_fit function
lmfit_results <-limma_fit(generic_matrix = a_matrix_drop, generic_design = design)

# Check the summary table
height_acc_summary <-limma_summary(lmfit_results, pvally = pval_cutoff)
height_acc_summary

# Check the toptable. 
height_acc_toptable <-limma_toptable(lmfit_results, coeffy = "height_accretion", pvally = pval_all)


################
# Height
################
design <- model.matrix(~ muscle_accretion + age_blood05 + smoke + drink + icpc1 + icpc2 + icpc3 + SEAsum_83_05 +time_bt_98_05, data = a_shuffle);

# Use limma_fit function
lmfit_results <-limma_fit(generic_matrix = a_matrix_drop, generic_design = design)

# Check the summary table
muscle_acc_summary <-limma_summary(lmfit_results, pvally = pval_cutoff)
muscle_acc_summary

# Check the toptable. 
muscle_acc_toptable <-limma_toptable(lmfit_results, coeffy = "muscle_accretion", pvally = pval_all)

################
# Height
################
design <- model.matrix(~ arm_accretion + age_blood05 + smoke + drink + icpc1 + icpc2 + icpc3 + SEAsum_83_05 +time_bt_98_05, data = a_shuffle);

# Use limma_fit function
lmfit_results <-limma_fit(generic_matrix = a_matrix_drop, generic_design = design)

# Check the summary table
arm_acc_summary <-limma_summary(lmfit_results, pvally = pval_cutoff)
arm_acc_summary

# Check the toptable. 
arm_acc_toptable <-limma_toptable(lmfit_results, coeffy = "arm_accretion", pvally = pval_all)

