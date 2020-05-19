
##########################################################
#############################
# Converting bt. mvals and bvals
#############################
##########################################################


# From library(lumi)
# Beta to Mvals
beta2m <-function (beta) 
{
  m <- log2(beta/(1 - beta))
  return(m)
}


# Mvals to Beta
m2beta <-function (m) 
{
  beta <- 2^m/(2^m + 1)
  return(beta)
}






##########################################################
#############################
# Create Matrix Function
#############################
##########################################################

# This function doesn't deal with X##### values, or differentiate between multiple replicates. It just removes more than 5 characters


make_matrix <- function(mvalues, phenos) {
  # get uncchdids
  ids <-names(mvals)
  # Remove the x's: 
  # This doesn't disambiguate between replicates if present!!
  ids <-substr(ids, 0,5) 
  # Remove replicates, make dataframe, and characterize
  ids <-as.data.frame(ids[!duplicated(ids)])
  names(ids) <-"uncchdid"
  #prepare phenos
  phenos$uncchdid <-as.character(phenos$uncchdid)
  ids$uncchdid <-as.character(ids$uncchdid)
  # Make new object with phenotypic data and ids
  focals <-dplyr::left_join(x = ids, y = phenos, by ="uncchdid")
  # Make rownames from "uncchdid"
  rownames(focals) <-focals[,"uncchdid"]
  # Remove any rows where phenotypic data is missing.
  focals2 <-focals[!rowSums(is.na(focals)), ] 
  # Are there individuals in focals (ids, phenos) that aren't in focals 2
  # i.e. are there individuals missing data for a given row?
  no.data <-setdiff(focals$uncchdid, focals2$uncchdid)
  # This is super important chuck that basically fixes an issue if there ARE NO missing ICs
  # Without it, it retuns an empty dataframe. With it, it does what it has to. 
  ifelse(rlang::is_empty(no.data),
     mvals.drop <-mvals, {
       # If so, let's remove them from mvals.
       # The grep finds them no matter what, regardless of the X in front or the _Rn after.
      
       matches <-unique( grep(paste(no.data, collapse="|"),
                         names(mvals), value = TRUE))
       mvals.drop <-mvals[, -which(names(mvals) %in% c(matches))]
     }
     )
  # Number of rows to create the matrix.
  rowz <-nrow(mvals.drop)
  # Number of columns to create the matrix.
  colz <-ncol(mvals.drop)
  # Apply all the columns of mvals.drop, (I don't have probe here - don't worry)
  # using the number of rows "rowz" and number of columns, 'colz', by column 
  # (basically recreate mvals.drop as a matrix)
  Matrix <-matrix(sapply(c(mvals.drop), as.numeric), nrow= rowz, ncol=colz , byrow=FALSE)
  rownames(Matrix) <-rownames(mvals)
  colnames(Matrix) <-focals2$uncchdid
  Matrix
}

#############################
#############################

# Add on function for reshaping phenotypic data to match matrix rows.
reshuffle_pheno_rows <- function(Matrix, phenos) {
  x <-colnames(Matrix)
  phenos %>% 
    slice(match(x, uncchdid))
}

check_shuffle <- function(Matrix, shuffled_phenos) {
  if( !identical(colnames(Matrix), shuffled_phenos$uncchdid))  
    warning('Do NOT PROCEED! Your matrix columns and phenotypic data rows are NOT aligned')
  else("Proceed :) Matrix columns and phenotypic data ARE ALIGNED")
}


##########################################################
#############################
# Run the limma regressions, smooth, and make a table.

#############################
##########################################################
library(limma)

# Fit limma object, smooth, and extract 
limma_fit <- function(generic_matrix, generic_design) {
  lmFit_object <-lmFit(generic_matrix, generic_design, na.action = na.omit)
  lmFit_object <- eBayes(lmFit_object)
  lmFit_object5 <-lmFit_object[ c(names_var_probes$probe), ]
  lmFit_object5
}

# e.g. 
# lmfit_results <-limma_fit(generic_matrix = r_matrix_drop, generic_design = design)


# Pull limma summary and make topTable
limma_toptable <- function(limma_fit_object, coeffy, numby = Inf, adjusty = "BH", pvally = 0.05) {
  limma_toptable <-topTable(limma_fit_object, coef = coeffy, number = numby, adjust.method = adjusty, p.value = pvally)
  rownames_to_column(limma_toptable, var = "probe")
}


# Pull limma summary and make table 
limma_summary <- function(limma_fit_object, adjusty = "BH", pvally = 0.05) {
  limma_results <-decideTests(limma_fit_object, adjust.method = adjusty, p.value = pvally)
  summary(limma_results) %>%
    kable("html") %>%
    kable_styling()
}




# Check the table manually
# newnumbpreg_table <-summary(lmfit_results) %>%
#   kable("html") %>%
#   kable_styling()
# 
# newnumbpreg_table


##########################################################
#############################
# not in function

"%nin%" <-function(x, y) {
  return( !x %in% y ) 
}

#############################
##########################################################

##########################################################
#############################
# Extract annotation

#############################
##########################################################

merge_annot <- function(limma_toptable, annot, pval = 0.05) {
  if("probe" %nin% colnames(limma_toptable)) {
    limma_toptable %>% 
      rownames_to_column(var = "probe") %>% 
      filter(adj.P.Val < pval) %>% 
      left_join(., annot, by = "probe")
  } else {
    limma_toptable %>% 
      filter(adj.P.Val < pval) %>% 
      left_join(., annot, by = "probe")
  }
}



#############################
# Look at the relationships between DNAm and phenotypic variable of interest

#############################
peep_dnam_pheno <- function(top_annotation_file, pheno_file) {
  left_join(mvals[top_annotation_file$probe, ] %>% 
              rownames_to_column("probe") %>% 
              gather(key = "uncchdid", value = "value", 2:100),
            pheno_file, 
            by = "uncchdid"
  )
}
