# read Ed Stats Census Common Core Of Data (ccd) from here:
# http://nces.ed.gov/ccd/pubagency.asp
# read directly from url as explained here:
# http://stackoverflow.com/questions/3053833/using-r-to-download-zipped-data-file-extract-and-import-data
library(data.table)
library(dplyr)
urls <- c("http://nces.ed.gov/ccd/data/zip/agn14pre_txt.zip", 
          "http://nces.ed.gov/ccd/data/zip/ag131a_supp_txt.zip",
          "http://nces.ed.gov/ccd/Data/zip/ag121a_supp_txt.zip", 
          "http://nces.ed.gov/ccd/Data/zip/ag111a_txt.zip", 
          "http://nces.ed.gov/ccd/Data/zip/ag102a_txt.zip", 
          "http://nces.ed.gov/ccd/data/zip/ag092a_txt.zip")
urls <- paste("curl", urls, sep = " ")
urls <- paste(urls, "| funzip", sep = " ")
dtset <- lapply(urls, function(x) {
  dt <- fread(x)
  out <- try(filter(dt, LSTATE == 'NC'))
  if('try-error' %in% class(out)) return(dt)
  out
})

# 2014 data is useless as of this writing, and 2009 
# has a different naming convention that I could work
# with in a pinch, but not worth it right now. so:
# dtset[[c(1, 6)]] <- NULL


# First pass
durhamco <- lapply(dtset, function(x) filter(x, CONUM == 37063)) 
durhamco <- lapply(durhamco, function(x) {
  colnames(x) <- tolower(colnames(x))
  keeps <- c('survyear', 'name', 'sch', 'speced', 'ell')
  teachers <- colnames(x)[grep('tch$', colnames(x))]
  othstaff <- colnames(x)[grep('(gui|spe|sup|adm)$', colnames(x))]
  students <- c('pk', 'kg', paste('g0', c(1:9), sep = ''), 'g10', 'g11', 'g12', 'ug', 'member')
  keeps <- c(keeps, teachers, 'aides', othstaff, students)
  drps <- c("aaidcorsup", 
            "iaaidcorsup", 
            "aleaadm", 
            "ialeaadm", 
            "aschadm", 
            "iaschadm")
  keeps <- setdiff(keeps, drps)
  
  # Second pass
  dplyr::select_(x, .dots = keeps) %>%
    filter(name == "DURHAM PUBLIC SCHOOLS")
})
durhamco <- do.call(rbind, durhamco) %>%
  mutate(name = NULL)
durhamco[durhamco < 0] <- 0

# Now count people you care about: students, teachers, staff:
durhamco <- dplyr::select(durhamco, survyear, sch, 
                          tottch, aides, totgui, libspe, libsup, 
                          leaadm, leasup, schadm, schsup, stusup, othsup, 
                          member, speced, ell)
save(durhamco, file = 'data/durham_people.RData')
