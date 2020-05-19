# Make matrix in which all mvals have phenotypic data, and vice versa.
# This makes sure the dimensionality of the mvalues matrix matches that of the phenotypic data - no missing values.

# Input: mvalues and phenotypic data called "phenos"
  # Note that all the uncchdid and CpG probe names must be 'as.character'

require(tidyverse)
################

# Create names and remove x's and replicate values at end. 
ids <-names(mvals)

# Remove the x's
ids <-substr(ids, 2,6)

# Remove any duplicated IDS (replicates?)
ids[duplicated(ids)]

# Code them as.character
ids <-as.character(ids)

# Make as a dataframe (I believe necessary for merge)
ids <-as.data.frame(ids)

names(ids)<-"uncchdid"

ids$uncchdid <-as.character(ids$uncchdid)

str(ids)
####################

# Make phenos$uncchdid as character:
phenos$uncchdid <-as.character(phenos$uncchdid)
ids$uncchdid <-as.character(ids$uncchdid)

# Left join to discard those IDs from mvals not in phenos
focals <-dplyr::left_join(x = ids, y = phenos, by ="uncchdid")
dim(focals) # 496   6

# Rownames of focals are now the IDs
rownames(focals) <-focals[,1]

focals2 <-focals[!rowSums(is.na(focals)), ] 

# Check new dimensions
dim(focals2) # 493 6

# Check the difference between focals (might have missing data) and focals2 (will not hav missing data)
length(setdiff(focals$uncchdid, focals2$uncchdid)) 

no.data <-setdiff(focals$uncchdid, focals2$uncchdid)

# No data for the following individuals. 
no.data

# "43310" "23323" are always missing. 

###############

# Create the matrix:

# Find any of the names in mvals that are missing (from no.data)
# The grep finds them no matter what, regardless of the X in front or the _Rn after.
matches <-unique( grep(paste(no.data, collapse="|"),
                       names(mvals), value = TRUE))

# And extract the unc names in mvals that are in 'matches' (i.e. no data)
mvals.drop <-mvals[, -which(names(mvals) %in% c(matches))]

dim(mvals.drop) #    
######################

# Number of rows to create the matrix.
rowz <-nrow(mvals.drop)

# Number of columns to create the matrix.
colz <-ncol(mvals.drop[,-1])

# Apply all the columns of mvals.drop, except the first (probe), 
# using the number of rows "rowz" and number of columns, 'colz', by column 
# (basically recreate mvals.drop as a matrix)
matrix <-matrix(sapply(c(mvals.drop[,-1]), as.numeric), nrow= rowz, ncol=colz , byrow=FALSE)

# Dimension of the matrix
dim(matrix)
# Check the first few rows/columns
matrix[1:4,1:4]

# Name the rownames based on the 'probe' labels from the mvals. data
rownames(matrix) <-mvals[,1]

# The column names for matrix are: focals2$uncchdid
