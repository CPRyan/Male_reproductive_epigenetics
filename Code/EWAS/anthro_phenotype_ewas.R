here <-here::here()

source(here::here("/Code/survey_measures", "01a_anthro.R"))


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
pvally <- 0.1

################



################
# Height
################
design <- model.matrix(~ height + age_blood05 + smoke + drink + icpc1 + icpc2 + icpc3 + SEAsum_83_05, data = a_shuffle);

# Use limma_fit function
lmfit_results <-limma_fit(generic_matrix = a_matrix_drop, generic_design = design)

# Check the summary table
height_summary <-limma_summary(lmfit_results, pvally = pvally)
height_summary

# Check the toptable. 
height_toptable <-limma_toptable(lmfit_results, coeffy = "height", pvally = pvally)

################
# BMI
################
design <- model.matrix(~ bmi + age_blood05 + smoke + drink + icpc1 + icpc2 + icpc3 +  SEAsum_83_05, data = a_shuffle);

# Use limma_fit function
lmfit_results <-limma_fit(generic_matrix = a_matrix_drop, generic_design = design)

# Check the summary table
bmi_summary <-limma_summary(lmfit_results, pvally = pvally)
bmi_summary

# Check the toptable. 
bmi_toptable <-limma_toptable(lmfit_results, coeffy = "bmi", pvally = pvally)



################
# arm muscle area
################
design <- model.matrix(~ arm_musc_area + age_blood05 + smoke + drink + icpc1 + icpc2 + icpc3 + SEAsum_83_05, data = a_shuffle);

# Use limma_fit function
lmfit_results <-limma_fit(generic_matrix = a_matrix_drop, generic_design = design)

# Check the summary table
arm_musc_area_summary <-limma_summary(lmfit_results, pvally = pvally)
arm_musc_area_summary

# Check the toptable. 
arm_musc_area_toptable <-limma_toptable(lmfit_results, coeffy = "arm_musc_area", pvally = pvally)



################
# skinfold_sum
################
design <- model.matrix(~ skinfold_sum + age_blood05 + smoke + drink + icpc1 + icpc2 + icpc3 + SEAsum_83_05, data = a_shuffle);

# Use limma_fit function
lmfit_results <-limma_fit(generic_matrix = a_matrix_drop, generic_design = design)

# Check the summary table
skinfold_sum_summary <-limma_summary(lmfit_results, pvally = pvally)
skinfold_sum_summary

# Check the toptable. 
skinfold_sum_toptable <-limma_toptable(lmfit_results, coeffy = "skinfold_sum", pvally = pvally)


################
# whr
################
design <- model.matrix(~ whr + age_blood05 + smoke + drink + icpc1 + icpc2 + icpc3 +  SEAsum_83_05, data = a_shuffle);

# Use limma_fit function
lmfit_results <-limma_fit(generic_matrix = a_matrix_drop, generic_design = design)

# Check the summary table
whr_summary <-limma_summary(lmfit_results, pvally = pvally)
whr_summary

# Check the toptable. 
whr_toptable <-limma_toptable(lmfit_results, coeffy = "whr", pvally = pvally)
