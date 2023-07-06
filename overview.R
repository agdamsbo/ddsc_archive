require(REDCapR)
require(keyring)

## Run the following to save the API key
keyring::key_set("REDCAP_API_DDSC")


## Get available variable names
ds <- REDCapR::redcap_variables(
  redcap_uri = "https://redcap.au.dk/api/",
  token = keyring::key_get("REDCAP_API_DDSC")
)

## Download specified variables; be patient!
ds <- REDCapR::redcap_read(
      redcap_uri = "https://redcap.au.dk/api/",
      token = keyring::key_get("REDCAP_API_DDSC"),
      fields = c("forloebid","modafdankomstdato_basisske","nihss_basisske")
    )$data


# Visualising the number of patients with available NIHSS (vascular diagnosis approximation)
# distributed by week of admittance
x <- as.Date(ds$modafdankomstdato_basisske)
hist(x[x > as.Date("2013-07-01") & !is.na(ds$nihss_basisske)],breaks = "weeks")
