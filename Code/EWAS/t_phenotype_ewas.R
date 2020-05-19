here <-here::here()

source(here::here("/Code/survey_measures", "01e_testost.R"))


# Pick the variables of interest. 
names(testost)


#################################


#################################
# Testosterone

colSums(is.na(testost %>% select(-contains("09"))))

colSums(is.na(testost %>% 
  select(uncchdid, date_blood, age_blood05, time_blood, salivacollectiondate05, birthday, amt05, wakeup_time, amtime05, pmt05, pmtime05, icpc1:icpc10,
         allele1_num, anyoffspring05, hholdkids05, fath05c, maristat05, icpcfat05,
         rightgrip_mean, leftgrip_mean)))

# Cut allele1_num because I'm missing 8 guys
t <-testost %>% 
  select(uncchdid, age_blood05, salivacollectiondate05, age_saliva, birthday, age_saliva, amt05, wakeup_time, amtime05, pmt05, pmtime05, icpc1:icpc10, log_blood_t,
         anyoffspring05, fath05c, hholdkids05, maristat05, icpcfat05,
         rightgrip_mean, leftgrip_mean) %>% 
  na.omit() 


#################################
# Other info incorporate
control_vars <-read_csv(here::here("Data/Other", "control_vars.csv")) %>% mutate(uncchdid = as_character(uncchdid))
names(control_vars)

t <-left_join(control_vars %>% select(uncchdid, smoke, drink, bmi, SEAsum_83_05), t, by = "uncchdid") %>% na.omit()
# The rest of the variables (genetic scores) are already in the data.


#################################
# Function for make matrix and fix columns
source(here::here("/Code/EWAS", "EWAS_functions.R"))

# make_matrix.
t_matrix_drop <-make_matrix(mvals, t)

# Fix columns.
t_shuffle <-reshuffle_pheno_rows(Matrix = t_matrix_drop, phenos = t)

# might be ok, but I gotta check if I need the order of columns to change. 
#################################
# Check - e.g. 
mvals["cg00000289", "23217"]
t_matrix_drop["cg00000289", "23217"]

check_shuffle(t_matrix_drop, t_shuffle)
# Must be true to proceed.


#################################
# Make sure EWAS_functions.R is loaded
source(here::here("/Code/EWAS", "EWAS_functions.R"))

# Create the design matrix;
names(t_shuffle)


################
# pvalue
################
pvally <- 0.1
################
# AMT
################
design <- model.matrix(~ amt05 + age_saliva + smoke + drink +  amtime05 + icpc1 + icpc2 + icpc3 + bmi + SEAsum_83_05, data = t_shuffle);

# Use limma_fit function
lmfit_results <-limma_fit(generic_matrix = t_matrix_drop, generic_design = design)

# Check the summary table
amt_summary <-limma_summary(lmfit_results, pvally = pvally)
amt_summary

# Check the toptable. 
amt_toptable <-limma_toptable(lmfit_results, coeffy = "amt05", pvally = pvally)

#################
# PMT
#################
design <- model.matrix(~ pmt05 + age_saliva + smoke + drink +  pmtime05 + icpc1 + icpc2 + icpc3 + bmi + SEAsum_83_05, data = t_shuffle);

# Use limma_fit function
lmfit_results <-limma_fit(generic_matrix = t_matrix_drop, generic_design = design)

# Check the table. 
pmt_summary <-limma_summary(lmfit_results, pvally = pvally)
pmt_summary

# Check the toptable. 
pmt_toptable <-limma_toptable(lmfit_results, coef = "pmt05", pvally = pvally)


#################
# Father in 05?
#################
design <- model.matrix(~ fath05c + age_blood05 + smoke + drink + icpc1 + icpc2 + icpc3+ bmi + SEAsum_83_05, data = t_shuffle);

# Use limma_fit function
lmfit_results <-limma_fit(generic_matrix = t_matrix_drop, generic_design = design)

# Check the table. 
father_summary <-limma_summary(lmfit_results, pvally = pvally)
father_summary

# Check the toptable. 
father_toptable <-limma_toptable(lmfit_results, coef = "fath05c", pvally = pvally)

#################
# Household kids
#################
design <- model.matrix(~ hholdkids05 + smoke + drink + age_blood05 + icpc1 + icpc2 + icpc3 + bmi + SEAsum_83_05, data = t_shuffle);

# Use limma_fit function
lmfit_results <-limma_fit(generic_matrix = t_matrix_drop, generic_design = design)

# Check the table. 
hhkids_summary <-limma_summary(lmfit_results, pvally = pvally)
hhkids_summary

# Check the toptable. 
hhkids_toptable <-limma_toptable(lmfit_results, coef = "hholdkids05", pvally = pvally)

#################
# icpcfat05
#################
design <- model.matrix(~ icpcfat05 + age_blood05 + smoke + drink + icpc1 + icpc2 + icpc3 + bmi + SEAsum_83_05, data = t_shuffle);

# Use limma_fit function
lmfit_results <-limma_fit(generic_matrix = t_matrix_drop, generic_design = design)

# Check the table. 
icpcfat_summary <-limma_summary(lmfit_results, pvally = pvally)
icpcfat_summary

# Check the toptable. 
icpcfat_toptable <-limma_toptable(lmfit_results, coef = "icpcfat05", pvally = pvally)

#################
# Grip
#################
design <- model.matrix(~ rightgrip_mean + age_blood05 + smoke + drink + icpc1 + icpc2 + icpc3 + bmi + SEAsum_83_05, data = t_shuffle);

# Use limma_fit function
lmfit_results <-limma_fit(generic_matrix = t_matrix_drop, generic_design = design)

# Check the table. 
rgrip_summary <-limma_summary(lmfit_results, pvally = pvally)
rgrip_summary

# Check the toptable. 
rgrip_toptable <-limma_toptable(lmfit_results, coef = "rightgrip_mean", pvally = pvally)


#################
# Blood testosterone?
#################
design <- model.matrix(~ log_blood_t + age_blood05 + smoke + drink + icpc1 + icpc2 + icpc3 + bmi + SEAsum_83_05, data = t_shuffle);

# Use limma_fit function
lmfit_results <-limma_fit(generic_matrix = t_matrix_drop, generic_design = design)

# Check the table. 
blood_t_summary <-limma_summary(lmfit_results, pvally = pvally)
blood_t_summary

# Check the toptable. 
blood_t_toptable <-limma_toptable(lmfit_results, coef = "log_blood_t", pvally = pvally)
