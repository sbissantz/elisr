## code to prepare `trust` dataset goes here
# path <- "~/Downloads/ZA5272_v1-0-0.sav"

dta_ls <- foreign::read.spss(path, use.value.labels = TRUE, use.missings = -9)

# Read Metadata

spsdta <- memisc::spss.system.file(path)
pat <- "pt[1-20]"
var_pos <- grep(pat, names(dta_ls))
dta <- spsdta[var_pos]

# Show codebook to see missing values and coding

f <- function(var) {
  memisc::codebook(var)
}
lapply(dta, f)

# Check min and max to assure that missing values (-9) are NA

g <- function(var){
  c(min(var), max(var))
}
lapply(dta, g)

# Coerce to integers

h <- function(col){
  as.integer(col)
}
dta_ls <- lapply(dta, h)
res <- as.data.frame(dta_ls)
colnames(res) <- c(
'healserv',
'fccourt',
'bundtag',
'munadmin',
'judsyst',
'tv',
'newsppr',
'uni',
'fedgovt',
'police',
'polpati',
'eucomisn',
'eupalmnt'
)

# Reorder the 0815 dta to boost anonymity

#set.seed(***)
rows <- nrow(res)
new_order <- sample(1:rows, rows, replace = FALSE)
trust <- res[new_order,]

# New rownames

no_rownms <- length(rownames(trust))
new_rownms <- as.character(seq(1,no_rownms))
rownames(trust) <- new_rownms

usethis::use_data(trust, overwrite = TRUE)
