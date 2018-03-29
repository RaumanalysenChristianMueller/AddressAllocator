# AddressAllocator
# Author: Raumanalysen - Christian Mueller
# raumanalysen@mailbox.org

# This script unifies address-ID-fields by removing special characters and unifying different writing styles


unifyAddressIDField <- function(data, addressIDField){
  
  # get address-ID-entries
  entries <- as.character(data[,addressIDField])
  
  # remove special characters and unify writing styles in a cascading substitution
  out <- gsub(
          x = gsub(
            x = gsub(
              x = gsub(
                x = gsub(
                  x = gsub(
                    x = gsub(
                      x = gsub(
                        x = gsub(
                          x = gsub(
                            x = gsub(
                              x = gsub(
                                x = gsub(
                                  x = gsub(
                                    x = gsub(
                                      x = gsub(
                                        x = gsub(
                                          x = gsub(
                                            x = gsub(x = entries, pattern = "?", replacement = ""),
                                          pattern = ",", replacement = ""),
                                        pattern = "str�e", replacement = "str"),
                                      pattern = "Str�e", replacement = "str"),
                                    pattern = "Str.", replacement = "str"),
                                  pattern = "strasse", replacement = "str"),
                                pattern = "Strasse", replacement = "str"),
                              pattern = "stra�e", replacement = "str"),
                            pattern = "Stra�e", replacement = "str"),
                          pattern = "�", replacement = "ae"),
                        pattern = "�", replacement = "ue"),
                      pattern = "�", replacement = "oe"),
                    pattern = "�", replacement = "Ae"),
                  pattern = "�", replacement = "Oe"),
                pattern = "�", replacement = "Ue"),
              pattern = "�", replacement = "ss"),
            pattern = "-", replacement = ""),
          pattern = " ", replacement = ""),
        pattern = "NA", replacement = "")
  
  # update address-ID-field
  data[,addressIDField] <- out
  
  # return substituted dataset
  return(data)
  
}