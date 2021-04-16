#' @importFrom stringi stri_trans_general stri_trans_tolower stri_trim
#'
dot_paste <- function(x, y) paste(x, y, sep = " \u00b7 ")

remove_stopwords <- function(x) {
  # (de, del, des), (d'en), (de la, de las, de les, de los), (d'), (do, da, do, das),
  # (de l', de s'), (de sa, de ses), (can), (ca l', ca la, ca n', ca na), (y, i)
  sw1 <- c(paste0("(de[ls]?|d en|de l[aeo]s?|d|d[oa]s?|",
                  "de [ls]|de s[ae]s?|can|ca [ln][a]?|[yie])"))

  # (el, es, ses, en), (la, lo, na), (las, les, los), (l', s'), (els), (sa), (o, a, os, as)
  sw2 <- c("(s?e[lsn]|[ln][ao]|l[aeo]s|[ls]|els|sa|[oa]s?)")

  pat1 <- paste0("(?<=^| |-)", sw1, "(?= [a-z])")
  pat2 <- paste0("(?<= |-)", sw1, "(?= [a-z]|$)")
  pat3 <- paste0("(?<=^| |-)", sw2, "(?= [a-z])")
  pat4 <- paste0("(?<= |-)", sw2, "(?= [a-z]|$)")

  x <- gsub("[']", " ", x)
  x <- gsub(pat1, "", x, ignore.case = TRUE, perl = TRUE)
  x <- gsub(pat2, "", x, ignore.case = TRUE, perl = TRUE)
  x <- gsub(pat3, "", x, ignore.case = TRUE, perl = TRUE)
  x <- gsub(pat4, "", x, ignore.case = TRUE, perl = TRUE)
  x <- remove_extra_spaces(x)
  x
}

strip_place <- function(x) {
  x <- tolower(x)
  x <- gsub("\\(.*\\)", "", x)
  x <- remove_vowel_diacritics(x)
  x <- gsub("\u00b4", "'", x)
  x <- remove_stopwords(x)
  x <- gsub("\\<sto?.? ", "santo ", x)
  x <- gsub("\\<sta.? ", "santa ", x)
  x <- gsub(".* ?/(?! ?.* -)", "", x, perl = TRUE)
  x <- gsub(".* [-\u2014]", "", x)
  x <- gsub("-", " ", x)
  x <- gsub("[[:punct:]]", "", x)
  x <- remove_extra_spaces(x)
  x
}

rename_place <- function(x) {

  sw1 <- c(paste0("(de[ls]?|d en|de l[aeo]s?|d|d[oa]s?|",
                  "de [ls]|de s[ae]s?|can|ca [ln][a]?|[yie])"))
  sw2 <- c("(s?e[lsn]|[ln][ao]|l[aeo]s|[ls]|els|sa|[oa]s?)")

  pat1 <- paste0("(.*) \\((", sw1, "|", sw2, ")\\)$")
  pat2 <- paste0("(.*), (", sw1, "|", sw2, ")$")

  x <- gsub(pat1, "\\2 \\1", x, ignore.case = TRUE)
  x <- gsub(pat2, "\\2 \\1", x, ignore.case = TRUE)
  x <- gsub("\u00b4", "'", x)
  x <- gsub(" \\(", "; ", x)
  x <- gsub("[,)]", "", x)
  x <- remove_extra_spaces(x)
  x
}

strip_name <- function(x) {
  x <- tolower(x)
  x <- gsub("\\d*", "", x)
  x <- gsub("\\.?\u00aa+", "aria", x)
  x <- gsub("\\<[a-z]{1}\\.", "", x)
  x <- gsub("\\s{2,}", " ", x)
  x <- stri_trim(x)
  x
}

wash_name <- function(x) {
  x <- gsub("\\d+", "", x)  # Rm numbers
  x <- gsub("[[:cntrl:]]+.*", "", x)  # Rm control chars
  x <- gsub("(')( +)(?=[A-z])", "\\1", x, perl = TRUE)  # Binds rhs words with '
  x <- gsub("(?<=[A-Z])\u00b4(?=[A-Z])", "'", x, perl = TRUE)  # ´->' (btw capis)
  x <- gsub("(?<=[A-z])( *)(-)( *)(?=[A-z])", "\\2", x, perl = TRUE)  # Binds to hyphen
  x <- gsub("(?<![A-z])(-)(?![A-z])", "", x, perl = TRUE)  # Rm other hyphens
  x <- gsub("\\<m\\.?\u00aa+", "Mar\u00eda", x, ignore.case = TRUE)
  x <- gsub("(\u00ba|\u00aa)+", "\\.", x)
  x <- gsub("\\.\\s+", ". ", x)
  x <- gsub("(?!['.-])[[:punct:]]|\u00b4|\u00b7", "", x, perl = TRUE)
  x
}

dry_name <- function(x) {
  stopwords1 <- c(paste0("( de )|( du )|( d')|( del )|( di )|( do )|( da )|",
                         "( dos )|( das )"))
  stopwords2 <- c(paste0("( de l')|( de la )|( de los )|( de las )|( della )|",
                         "( dalla )|( dell')|( von )|( van )"))
  stopwords3 <- c("( y )|( i )|( e )")
  transformation1 <- c("\\L\\1\\2\\3\\4\\5\\6\\7\\8\\9")
  transformation2 <- c("\\L\\1\\2\\3\\4\\5\\6\\7\\8\\9")
  transformation3 <- c("\\L\\1\\2\\3")

  x <- stri_trans_totitle(x, opts_brkiter = stri_opts_brkiter(locale = "en"))
  x <- gsub("('|-|\u2014)([a-z])", "\\1\\U\\2", x, perl = TRUE)
  x <- gsub((stopwords1), transformation1, x, ignore.case = TRUE, perl = TRUE)
  x <- gsub((stopwords2), transformation2, x, ignore.case = TRUE, perl = TRUE)
  x <- gsub((stopwords3), transformation3, x, ignore.case = TRUE, perl = TRUE)
  x <- gsub("(\\<De La )", "De la ", x)
  x <- gsub("(\\<De Los )", "De los ", x)
  x <- gsub("(\\<De Las )", "De las ", x)
  x <- gsub("\\s{2,}", " ", x)
  x <- stri_trim(x)
  x
}

check_values <- function(x, division) {
  if (division == "municipio" && !all(x %in% unique(municipios$c.muni))) {
    warning(paste("Some of the values in `x` aren't proper `municipio` codes.",
                  "Type ?codigator for more information."), immediate. = TRUE)
  } else if (division == "provincia" && !all(x %in% c(1:52))) {
    warning(paste("Some of the values in `x` aren't proper `provincia` codes.",
                  "Type ?codigator for more information."), immediate. = TRUE)
  } else if (division == "ccaa" && !all(x %in% c(1:19))) {
    warning(paste("Some of the values in `x` aren't proper `comunidad ",
                  "aut\u00f3noma` codes. Type ?codigator for more information."),
            immediate. = TRUE)
  }
}

retrieve_params <- function(division) {
  switch(division,
         "municipio" = {
           col_names <- c("a", "municipio")
           ref <- municipios
           m <- ref$pobl
           n <- ref$division
           dist <- 1
         },
         "provincia" = {
           col_names <- c("a", "provincia")
           ref <- provincias[-4]
           m <- ref$prov
           n <- ref$division
           dist <- 2
         },
         "ccaa" = {
           col_names <- c("a", "ccaa")
           ref <- provincias[-5]
           ref$division <- gsub("^.*; ", "", ref$division)
           m <- ref$ccaa
           n <- ref$division
           dist <- 3
         }
  )
  output <- list(col_names, ref, m, n, dist)
  output
}

pop <- function(x) {
  pat1 <- "(?(?=^{){(.*)}.*|(.*)(;.*){2})"
  pat2 <- "{.*} "
  pat3 <- "(^.*; )(.*)(;.*$)"
  pat4 <- "^(.*; ){2}"

  paste0("<h3>", gsub(pat1, "\\1", x[[4]], perl = TRUE), "</h3>",
         "<b>Municipio: </b>", gsub(pat2, "", x[[4]], perl = TRUE),
         " [", x[[3]], "]<br>",
         "<b>Provincia: </b>", gsub(pat3, "\\2", x[[5]], perl = TRUE),
         " [", x[[2]], "]<br>",
         "<b>C. A.: </b>", gsub(pat4, "", x[[5]]),
         " [", x[[1]], "]")
}
