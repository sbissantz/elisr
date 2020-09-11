rm(list=ls())
# Bottle neck?
system.time(muscat(df=mtcars))
scldf <- muscat(df=mtcars)

system.time(muscat(df=mtcars,overlap = TRUE, overlap_with = "nucleus",
                   pre_scldf=scldf))

(scldf <- muscat(df=mtcars, rit_min=.6, negative_too = FALSE))

(scldf <- muscat(df=mtcars, rit_min=.6, negative_too = FALSE,
                 use="everything"))
(scldf <- muscat(df=mtcars, rit_min=.6, negative_too = FALSE,
                 use="all.obs"))
(scldf <- muscat(df=mtcars, rit_min=.6, negative_too = FALSE,
                 use="complete.obs"))
(scldf <- muscat(df=mtcars, rit_min=.6, negative_too = FALSE,
                 use="pairwise.complete.obs"))
(scldf <- muscat(df=mtcars, rit_min=.6, negative_too = FALSE,
                 use="na.or.complete"))

muscat(df=mtcars, rit_min=.3, negative.too = TRUE, full_scl_val = 7)

muscat(mtcars, overlap = TRUE, overlap_with = "nucleus")

muscat(mtcars, overlap = TRUE, overlap_with = "full.scale")

muscat(df=mtcars, rit_min=.6, negative.too = TRUE, full_scl_val = 7, checkup = FALSE)

# Error-Tests -------------------------------------------------------------

muscat(df=mtcars,
       rit_min=.6,
       negative.too = TRUE)

muscat(df=df,
       rit_min=0.3,
       negative.too = TRUE,
       full_scl_val = 7)

muscat(mtcars, overlap = TRUE)

muscat(df=df,
       rit_min=0.3,
       negative.too = TRUE,
       full_scl_val = 7)

names(mtcars) <- NULL
muscat(mtcars, overlap = TRUE)

# Mattloff ----------------------------------------------------------------

#    df.len <- length(df)
#    # Nice trick from Norman Mattloff to use recycling for naming
#    attr(df,"names") <- paste("V",seq(df.len), sep="")
#    }
