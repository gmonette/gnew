#' Create excel file from Zoom chat file
#' 
#' Reads a Zoom "chat.txt" file (recorded portion of meeting)
#' or a "meeting_saved_chat.txt" file (entire meeting)
#' and writes a '.xlsx' Excel file in which lines are sorted by
#' participant and by time.
#' 
#' When a participant submits a chat message in multiple lines, the subsequent
#' lines may not be identified with the name of the participant, or they
#' may be separated by other chat submissions from other participants.
#' \code{chat2xl} resolves these problems and creates a file in which all
#' submissions are labelled with the identifier of the participant and
#' and ordered by participant and by time. When viewed, the excel file can be reordered
#' by time if desired.
#' 
#' @param file chat file, usually "chat.txt" or "meeting_saved_chat.txt"
#' @param outfile default 'file' with '.txt' suffix replaced with .xlsx
#' @param overwrite will an existing ".xlsx" file be overwritten, default FALSE
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
  z <- sortdf(z, ~ who / time)
  write.xlsx(z, file = outfile, overwrite = overwrite, asTable = T)
  invisible(z)
}
