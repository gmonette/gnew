#' Create excel file from Zoom chat file
#' 
#' Excel file allows sorting by poster so all submissions 
#' are contiguous and in time order
#' 
#' @param file chat file
#' @param outfile default 'file' with '.txt' suffix replaced with .xlsx
#' @export
chat2xl <- function(file, outfile = NULL, overwrite = FALSE) {
  library(spida2)
  library(openxlsx)
  if(is.null(outfile)) outfile <- sub('.txt$', '.xlsx', file)
  chat <- scan(file, what = 'a', sep = '\n')
  out <- list()
  index <- 0
  for(n in 1:length(chat)) {
    new <- chat[n]
    if(grepl('\\t', new)) index <- index + 1
    out[index] <- paste0(out[index],'\n',new)
  }
  out %>% 
    lapply(function(x) sub('^NULL\\\n','',x)) %>% 
    lapply(function(x) sub('\\t ', '>#$<', x)) %>%
    lapply(function(x) sub(' : ', '>#$<', x)) %>% 
    lapply(strsplit, '>#$<', fixed = TRUE) %>%
    lapply(unlist) %>% 
    do.call(rbind, .) %>% 
    as.data.frame -> z
  names(z) <- c('time','who','what')
  write.xlsx(z, file = outfile, overwrite = overwrite, asTable = T)
  invisible(z)
}
