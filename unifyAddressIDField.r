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
                                        pattern = "strße", replacement = "str"),
                                      pattern = "Strße", replacement = "str"),
                                    pattern = "Str.", replacement = "str"),
                                  pattern = "strasse", replacement = "str"),
                                pattern = "Strasse", replacement = "str"),
                              pattern = "straße", replacement = "str"),
                            pattern = "Straße", replacement = "str"),
                          pattern = "ä", replacement = "ae"),
                        pattern = "ü", replacement = "ue"),
                      pattern = "ö", replacement = "oe"),
                    pattern = "Ä", replacement = "Ae"),
                  pattern = "Ö", replacement = "Oe"),
                pattern = "Ü", replacement = "Ue"),
              pattern = "ß", replacement = "ss"),
            pattern = "-", replacement = ""),
          pattern = " ", replacement = ""),
        pattern = "NA", replacement = "")
  
  # update address-ID-field
  data[,addressIDField] <- out
  
  # return substituted dataset
  return(data)
  
}