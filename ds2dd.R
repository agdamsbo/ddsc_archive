ds2dd <- function(ds,
         record.id = "record_id",
         form.name = "basis",
         field.type = "text",
         field.label = NULL,
         include.column.names = FALSE) {
  dd <- data.frame(matrix(ncol = length(stRoke::metadata_names), nrow = ncol(ds)))
  colnames(dd) <- stRoke::metadata_names
  
  if (is.character(record.id) & !record.id %in% colnames(ds)) {
    stop("Provided record.id is not a variable name in provided data set.")
  }
  
  # renaming to lower case and substitute spaces with underscore
  field.name <- gsub(" ", "_", tolower(colnames(ds)))
  
  # handles both character and integer
  colsel <-
    colnames(ds) == colnames(ds[record.id])
  
  if (summary(colsel)[3] != 1) {
    stop("Provided record.id has to be or refer to a uniquely named column.")
  }
  
  dd[, "field_name"] <-
    c(field.name[colsel], field.name[!colsel])
  
  if (length(form.name) > 1 & length(form.name) != ncol(ds)) {
    stop(
      "Provided form.name should be of length 1 (value is reused) or equal 
        length as number of variables in data set."
    )
  }
  dd[, "form_name"] <- form.name
  
  if (length(field.type) > 1 & length(field.type) != ncol(ds)) {
    stop(
      "Provided field.type should be of length 1 (value is reused) or equal 
        length as number of variables in data set."
    )
  }
  
  dd[, "field_type"] <- field.type
  
  if (is.null(field.label)) {
    dd[, "field_label"] <- dd[, "field_name"]
  } else
    dd[, "field_label"] <- field.label
  
  if (include.column.names){
    list("DataDictionary"=dd,"Column names"=field.name)
  } else dd
}
