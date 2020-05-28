
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








#############################
# CPR_manhattan
# a package based on qqman package. 
# Needs calibrate for 'textxy' function
# Basically allows me to plot the gene names and change the colors. Genius. 
#############################

# CPR_manhattan

CPR_manhattan <-function(x, chr = "CHR", bp = "BP", p = "P", snp = "SNP", col = c("gray10", 
                                                                                  "gray60"), chrlabs = NULL, suggestiveline = -log10(1e-05), 
                         genomewideline = -log10(5e-08), highlight = NULL, logp = TRUE, 
                         annotatePval = NULL, annotateTop = TRUE, gene = "GENE", ...) 
{
  CHR = BP = P = index = NULL
  if (!(chr %in% names(x))) 
    stop(paste("Column", chr, "not found!"))
  if (!(bp %in% names(x))) 
    stop(paste("Column", bp, "not found!"))
  if (!(p %in% names(x))) 
    stop(paste("Column", p, "not found!"))
  if (!(snp %in% names(x))) 
    warning(paste("No SNP column found. OK unless you're trying to highlight."))
  if (!is.numeric(x[[chr]])) 
    stop(paste(chr, "column should be numeric. Do you have 'X', 'Y', 'MT', etc? If so change to numbers and try again."))
  if (!is.numeric(x[[bp]])) 
    stop(paste(bp, "column should be numeric."))
  if (!is.numeric(x[[p]])) 
    stop(paste(p, "column should be numeric."))
  d = data.frame(CHR = x[[chr]], BP = x[[bp]], P = x[[p]], GENE = x[[gene]])
  if (!is.null(x[[snp]])) 
    d = transform(d, SNP = x[[snp]])
  d <- subset(d, (is.numeric(CHR) & is.numeric(BP) & is.numeric(P)))
  d <- d[order(d$CHR, d$BP), ]
  if (logp) {
    d$logp <- -log10(d$P)
  }
  else {
    d$logp <- d$P
  }
  d$pos = NA
  d$index = NA
  ind = 0
  for (i in unique(d$CHR)) {
    ind = ind + 1
    d[d$CHR == i, ]$index = ind
  }
  nchr = length(unique(d$CHR))
  if (nchr == 1) {
    d$pos = d$BP
    ticks = floor(length(d$pos))/2 + 1
    xlabel = paste("Chromosome", unique(d$CHR), "position")
    labs = ticks
  }
  else {
    lastbase = 0
    ticks = NULL
    for (i in unique(d$index)) {
      if (i == 1) {
        d[d$index == i, ]$pos = d[d$index == i, ]$BP
      }
      else {
        lastbase = lastbase + tail(subset(d, index == 
                                            i - 1)$BP, 1)
        d[d$index == i, ]$pos = d[d$index == i, ]$BP + 
          lastbase
      }
      ticks = c(ticks, (min(d[d$index == i, ]$pos) + max(d[d$index == 
                                                             i, ]$pos))/2 + 1)
    }
    xlabel = "Chromosome"
    labs <- unique(d$CHR)
  }
  xmax = ceiling(max(d$pos) * 1.03)
  xmin = floor(max(d$pos) * -0.03)
  def_args <- list(xaxt = "n", bty = "n", xaxs = "i", yaxs = "i", 
                   las = 1, pch = 20, xlim = c(xmin, xmax), ylim = c(0, 
                                                                     ceiling(max(d$logp))), xlab = xlabel, ylab = expression(-log[10](italic(p))))
  dotargs <- list(...)
  do.call("plot", c(NA, dotargs, def_args[!names(def_args) %in% 
                                            names(dotargs)]))
  if (!is.null(chrlabs)) {
    if (is.character(chrlabs)) {
      if (length(chrlabs) == length(labs)) {
        labs <- chrlabs
      }
      else {
        warning("You're trying to specify chromosome labels but the number of labels != number of chromosomes.")
      }
    }
    else {
      warning("If you're trying to specify chromosome labels, chrlabs must be a character vector")
    }
  }
  if (nchr == 1) {
    axis(1, ...)
  }
  else {
    axis(1, at = ticks, labels = labs, ...)
  }
  col = rep(col, max(d$CHR))
  if (nchr == 1) {
    with(d, points(pos, logp, pch = 20, col = col[1], ...))
  }
  else {
    icol = 1
    for (i in unique(d$index)) {
      with(d[d$index == unique(d$index)[i], ], points(pos, 
                                                      logp, col = col[icol], pch = 20, ...))
      icol = icol + 1
    }
  }
  if (suggestiveline) 
    abline(h = suggestiveline, col = "blue")
  if (genomewideline) 
    abline(h = genomewideline, col = "red")
  if (!is.null(highlight)) {
    d.highlight = d[which(d$SNP %in% highlight), ]
    with(d.highlight, points(pos, logp, col = "blue", pch = 20, 
                             ...))
  }
  if (!is.null(annotatePval)) {
    topHits = subset(d, P <= annotatePval)
    par(xpd = TRUE)
    if (annotateTop == FALSE) {
      with(subset(d, P <= annotatePval), textxy(pos, -log10(P), 
                                                offset = 0.625, labs = topHits$GENE, cex = 0.45), 
           ...)
    }
    else {
      topHits <- topHits[order(topHits$P), ]
      topGENEs <- NULL
      for (i in unique(topHits$CHR)) {
        chrGENEs <- topHits[topHits$CHR == i, ]
        topGENEs <- rbind(topGENEs, chrGENEs[1, ])
      }
      textxy(topGENEs$pos, -log10(topGENEs$P), offset = 0.625, 
             labs = topGENEs$GENE, cex = 0.5, ...)
    }
  }
  par(xpd = FALSE)
}
