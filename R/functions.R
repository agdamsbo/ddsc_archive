#' Read the database variables
#'
#' @return tibble
read_database_variables <- function() {
  REDCapR::redcap_variables(
    redcap_uri = "https://redcap.au.dk/api/",
    token = keyring::key_get("REDCAP_API_DDSC")
  )$data
}

#' Focuse dapproach to reading the databse
#'
#' @return tibble
read_focused_database <- function() {
  REDCapR::redcap_read(
    redcap_uri = "https://redcap.au.dk/api/",
    token = keyring::key_get("REDCAP_API_DDSC"),
    fields = c(
      "forloebid",
      "ageforloeb_patiente",
      "sex_patiente",
      "forloebdato_patiente",
      "forloebtid_patiente",
      "ankomstdato_basisske",
      "ankomsttid_basisske",
      "mrsfoer_basisske",
      "symptdebexactdato_basisske",
      "symptdebexacttid_basisske",
      "modtagetsomtrombolysekandidat_basisske",
      "vaskdiag_basisske",
      "diagnosecerebraltinfarkt_basisske",
      "nihss_basisske",
      "id_tromboly",
      "ankomsttidtrombol_tromboly",
      "id_trombekt",
      "mdmrs3_tremdrop",
      "ankomstdatotrombek_trombekt"
    )
  )$data
}


#' Filter the raw dataset to only have relevant infarkts
#'
#' @param data Raw data set
#'
#' @return tibble
filter_raw_dataset <- function(data) {
  data |> dplyr::filter(diagnosecerebraltinfarkt_basisske == "TRUE")
}


#' Modify the dataset
#'
#' @param data filtered data base output
#'
#' @return tibble
modify_filtered_dataset <- function(data) {
  data |>
    filter_raw_dataset() |>
    dplyr::mutate(
      mrsfoer_basisske = mrsfoer_basisske - 2,
      mdmrs3_tremdrop = mdmrs3_tremdrop - 1,
      correct_arrival_tid = dplyr::if_else(hms::as_hms(forloebtid_patiente) == lubridate::hms("00:00:00"), ankomsttid_basisske, forloebtid_patiente),
      dplyr::across(tidyselect::contains("tid"), hms::as_hms),
      dplyr::across(tidyselect::contains("dat[eo]"), lubridate::ymd)
    ) |>
    dplyr::transmute(
      onset = lubridate::ymd_hms(stringr::str_c(symptdebexactdato_basisske,
        symptdebexacttid_basisske,
        sep = " "
      )),
      arrival = lubridate::ymd_hms(stringr::str_c(forloebdato_patiente,
        correct_arrival_tid,
        sep = " "
      )),
      sex = sex_patiente,
      age = ageforloeb_patiente,
      nihss = nihss_basisske,
      ivt = !is.na(id_tromboly),
      evt = !is.na(id_trombekt),
      mrs_0 = factor(ifelse(!mrsfoer_basisske %in% 0:5, NA, mrsfoer_basisske)),
      mrs_3 = factor(ifelse(!mdmrs3_tremdrop %in% 0:5, NA, mdmrs3_tremdrop)),
      delay = as.numeric(difftime(arrival, onset, units = "mins")),
      year = as.numeric(substr(as.character(arrival), 1, 4))
    ) |>
    dplyr::filter(delay < 1440, delay > 0)
}
