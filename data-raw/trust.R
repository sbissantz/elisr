#
#
# Code: trust
#
#
dta_ls <- foreign::read.spss(path, use.value.labels = TRUE, use.missings = TRUE)

#
# Read Metadata
#

spsdta <- memisc::spss.system.file(path)
pat <- "pt[1-20]"
var_pos <- grep(pat, names(dta_ls))
dta <- spsdta[var_pos]

#
# Codebook: missing values & coding
#

show_cb <- function(var) {
  memisc::codebook(var)
}
lapply(dta, show_cb)

#
# Check: min & max -- assure missing values (-9) are NA
#

lapply(dta, range)

#
# Coerce: integers
#

make_int <- function(col) {
  as.integer(col)
}

#
# Rename: variables
#

dta_ls <- lapply(dta, make_int)
res <- as.data.frame(dta_ls)
colnames(res) <- c("healserv", "fccourt", "bundtag", "munadmin", "judsyst",
                   "tv", "newsppr", "uni", "fedgovt", "police", "polpati",
                   "eucomisn", "eupalmnt")

#
# Reorder: 0815-Data -- boost anonymity
#

#set.seed(***)
rows <- nrow(res)
new_order <- sample(1:rows, rows, replace = FALSE)
trust <- res[new_order, ]

#
# New rownames
#

no_rownms <- length(rownames(trust))
new_rownms <- as.character(seq(1, no_rownms))
rownames(trust) <- new_rownms

#
# Send
#

usethis::use_data(trust, overwrite = TRUE)
