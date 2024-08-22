lycday <- function(date) {
  dplyr::if_else(lubridate::year(date) %% 4 == 0,
                 lubridate::yday(date),
                 lubridate::yday(date) +
                   vapply(X = date, FUN = function(d) {
                     sum(c(366, 365, 365)[seq_len(lubridate::year(d) %% 4)])
                   },
                   FUN.VALUE = numeric(1))
  )
}


append_harmonics <- function(df, n_harmonics) {
  df %>%
    dplyr::mutate(data.frame(sapply(X = paste0("harmonic_s", seq_len(n_harmonics)),
                                    FUN = function(name) {
                                      sin(lycday*
                                            2*pi*
                                            as.numeric(gsub("harmonic_s","",name))/annual_period_in_days
                                      )
                                    },
                                    simplify = FALSE, USE.NAMES = TRUE)),
                  data.frame(sapply(X = paste0("harmonic_c", seq_len(n_harmonics)),
                                    FUN = function(name) {
                                      cos(lycday*
                                            2*pi*
                                            as.numeric(gsub("harmonic_c","",name))/annual_period_in_days
                                      )
                                    },
                                    simplify = FALSE, USE.NAMES = TRUE)))
}

