#' @title German General Social Survey's Trust Items
#'
#' @description A subset of the German General Social Survey (ALLBUS) 2018
#'   containing its trust items (F018).
#'
#' @format Participants were asked about their trust in public institutions and
#'   organizations. `1` means they have absolutely no trust at all, whereas `7`
#'   represents a great deal of trust. The data frame has 3477 rows and 13
#'   variables:
#'   \describe{
#'   \item{healserv}{Health service}
#'   \item{fccourt}{German constitutional court}
#'   \item{bundtag}{German Parliament}
#'   \item{munadmin}{Municipal administration}
#'   \item{judsyst}{Judicial system}
#'   \item{tv}{Television}
#'   \item{newsppr}{Newspapers}
#'   \item{uni}{Universities and other institutes of higher education}
#'   \item{fedgovt}{German government}
#'   \item{police}{Police}
#'   \item{polpati}{Political parties}
#'   \item{eucomisn}{European Commission}
#'   \item{eupalmnt}{European Parliament}
#'   }
#'
#' @details Please note that in comparison to the original data set participant
#'   order has been randomly rearranged to further ensure anonymity. This might
#'   lead to differences when trying to reproduce the results of the analysis
#'   with the original data set (mentioned below).
#'
#' @references GESIS - Leibniz-Institut für Sozialwissenschaften (2019). German
#' General Social Survey - ALLBUS 2018. GESIS Datenarchiv, Köln. ZA5272
#' Datenfile Version 1.0.0, https://doi.org/10.4232/1.13325.
#'
#' @examples
#' # Use trust with disjoint
#' disjoint(trust, mrit_min = .4)
"trust"
