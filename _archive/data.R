################################################################################
## Data files
################################################################################

## Specifying to ensure order
## Could be loaded with 'list.files(pattern = ".xlsx")' as well.
files <- c("2018-09-07_ddsc_dataexport.xlsx", 
           "2023-06-30_ddsc_dataexport_aarhus.xlsx",
           "2023-06-30_ddsc_dataexport-godstrup.xlsx")


## Libraries
library(readxl)
library(tidyverse)

################################################################################
## Loading
################################################################################

sheetnames_list <-
  sapply(files, readxl::excel_sheets) |> sapply(tolower)

ds_list <- lapply(seq_along(sheetnames_list), function(i) {
  ds <- sapply(seq_along(sheetnames_list[[i]]), function(j) {
    sh <- readxl::read_excel(files[i], sheet = j)
    colnames(sh) <- tolower(colnames(sh))
    sh
  })
  names(ds) <- sheetnames_list[[i]]
  ds
})

################################################################################
## Preparing merge
################################################################################


## The first sheet in each spreadsheet files is "brugere", which is omitted
sheetnames <- unique(do.call(c, sheetnames_list))[-1]

sheetnames_clean <-
  gsub("[ .-]", "", sheetnames) # cleaned sheetnames for suffixes
sheetnames_chars <- gsub("3", "tre", gsub("7", "syv", sheetnames_clean))

## Determining sheets to join by matching names for each element in list
## Luckily naming of instruments/forms is kept
matches_list <- lapply(seq_along(sheetnames), function(i) {
  do.call(c, lapply(seq_along(ds_list), function(j) {
    match(sheetnames[i], names(ds_list[[j]]))
  }))
})

## Merging data from each
ds_list_merge <- lapply(seq_along(sheetnames), function(i) {
  ls <- sapply(seq_along(ds_list), function(j) {
    ds_list[[j]][matches_list[[i]][j]]
  })
  
  ls <- ls[!is.na(matches_list[[i]])]
  
  # names(ls)
  
  if (length(ls) > 1) {
    ds <- Reduce(f = full_join, ls)
  } else {
    ds <- Reduce(tibble, ls)
  }
  
  ## Logical index of "forloebid"
  nm_id <- !colnames(ds) == "forloebid"
  
  ## No naming change for "forloebid"
  colnames(ds)[nm_id] <-
    paste0(colnames(ds)[nm_id], "_", substr(sheetnames_chars[i], 1, 8))
  
  ds
})

################################################################################
## Data dictionary creation
################################################################################

require(stRoke)
source("ds2dd.R")

dd <- do.call(rbind, lapply(seq_along(ds_list_merge), function(i) {
  ds2dd(ds_list_merge[[i]],
        record.id = "forloebid",
        form.name = sheetnames_chars[i])
}))

## Check duplicated
## Proceed if duplications are only "forloebid"
if (all(dd$field_name[duplicated(dd$field_name)] == "forloebid")) {
  dd <- dd[!duplicated(dd$field_name), ]
} else {
  dd$field_name[duplicated(dd$field_name)]
  write.csv(dd, "dd_ddsc.csv")
  stop("Please have a look at field name duplications!")
}

################################################################################
## Data upload - YOU WISH!! -> handle duplicated data!
################################################################################

# keyring::key_set("REDCAP_API_DDSC")

## Project has been moved into production, so 'redcap_metadata_write()' is no longer possible.
# REDCapR::redcap_metadata_write(ds = dd,
#                                redcap_uri = "https://redcap.au.dk/api/",
#                                token = keyring::key_get("REDCAP_API_DDSC"))

# lapply(seq_along(ds_list_merge), function(i) {
#   REDCapR::redcap_write(
#     ds = ds_list_merge[[i]] |> head(1000),
#     redcap_uri = "https://redcap.au.dk/api/",
#     token = keyring::key_get("REDCAP_API_DDSC")
#   )
# })

## Resorting to manual upload after failed auto upload
# REDCapR::redcap_write(
#   ds = ds_list_merge[[3]],
#   redcap_uri = "https://redcap.au.dk/api/",
#   token = keyring::key_get("REDCAP_API_DDSC")
# )

################################################################################
## Solutions to data duplication and data upload
################################################################################

## Sheet 1 - Patienter
## 
## Keeping both "cpr" and "patientid"
## Apparently from merging some not-completely-identical entries

s1 <- lapply(split(ds_list_merge[[1]],ds_list_merge[[1]]$forloebid),function(i){
  if (nrow(i)>1) {
    i[2,1] <- i[1,1]
    i[2,]
    }
  }) |> do.call(rbind,args=_)

# s1 |> REDCapR::redcap_write(
#     redcap_uri = "https://redcap.au.dk/api/",
#     token = keyring::key_get("REDCAP_API_DDSC")
#   )

#===========
## DONE!! ##
#===========

## Sheet 2:10
## 
## Taking newest according to "sidstopdateret"
## The first entry contains basic data including cpr.
## lapply-function to keep the newest entry data by writing over the first entry for each forloebid.
## 
## Supposedly from patients having newer data after admittance to Gødstrup

s210 <- lapply(2:10,function(i){
  # Limiting the load by only considering duplicated
  ds <- ds_list_merge[[i]][ds_list_merge[[i]]$forloebid %in% ds_list_merge[[i]]$forloebid[duplicated(ds_list_merge[[i]]$forloebid)],]
  
  indx1 <- grep("^sidstopdateret_",names(ds))
  
  nr <- ncol(ds) # ncols for nested lapply()
  
  keep <- lapply(split(ds,ds$forloebid),function(j){
    
    if (nrow(j)>1) {
      indx2 <- which.max(unlist(j[indx1]))
      # non_na <- c(!is.na(j[indx2,]))
      # j[1,non_na] <- j[indx2,non_na] # Writing newest data to the first entry, without NAs
      
      ## Only using non_nas does not respect that data may have been deleted.
      
      ## Col 1:3 are cpr, id and forloebid, which are kept for 2:10
      
      j[1,4:nr] <- j[indx2,4:nr]
      j[1,]
    } else j
    
  }) |> do.call(rbind,args=_)
  
  unique_forl <- ds_list_merge[[i]][!ds_list_merge[[i]]$forloebid %in% ds$forloebid,]
  
  rbind(unique_forl,keep)
  
  })

## Upload missings

# lapply(seq_along(s210), function(i) {
#   REDCapR::redcap_write(
#     ds = s210[[i]],
#     redcap_uri = "https://redcap.au.dk/api/",
#     token = keyring::key_get("REDCAP_API_DDSC"),
#     overwrite_with_blanks = TRUE # Overwrtie blanks to ensure newest data is uploaded
#   )
# })

## Check that no entries are repeated
# dups2 <- lapply(seq_along(s210),function(i){
#   s210[[i]][s210[[i]]$forloebid %in% s210[[i]]$forloebid[duplicated(s210[[i]]$forloebid)],]})
# 
# Check!

#===========
## DONE!! ##
#===========



## Sheet 11
## 
## Resorting to making it a repeated instrument.
## Introduce redcap_repeat_instance

# s11_oplyst <- ds_list_merge[[11]][!ds_list_merge[[11]]$uoplyst_ichblodt,]

## To save everybodys time, only entries with data on BP/puls is kept
s11_data <- ds_list_merge[[11]][apply(!is.na(ds_list_merge[[11]][,c("systolisk_ichblodt","diastolisk_ichblodt","puls_ichblodt")]),1,any),]

s11 <- do.call(rbind,lapply(split(s11_data,s11_data$forloebid),function(i){
  i |> mutate(redcap_repeat_instrument=sheetnames_chars[11],
              redcap_repeat_instance=seq_len(nrow(i)))
}))

# REDCapR::redcap_write(
#   ds = s11 |> head(100),
#   redcap_uri = "https://redcap.au.dk/api/",
#   token = keyring::key_get("REDCAP_API_DDSC"),overwrite_with_blanks = TRUE
# )

#===========
## DONE!! ##
#===========

## Exporting user data to upload as file in the database

users <- do.call(rbind,lapply(1:3,function(i){
  ds_list[[1]][[1]]}))

write.csv(users,"ddsc_users.csv")

## Uploaded til REDCap
