#' ---
#' header-includes:
#' - \usepackage{amsmath}
#' - \usepackage{longtable}
#' - \usepackage{geometry}
#' - \geometry{papersize={8in,4.2in},left=1in,right=1in,top=.2in,bottom=.45in} # minimum for page no. at bottom
#' # - \geometry{papersize={8in,4.2in},left=.3in,right=.3in,top=.2in,bottom=.45in} # minimum for page no. at bottom
#' - \newcommand{\var}{\mathrm{Var}}
#' - \raggedright
#' # - \usepackage{siunitx}
#' # - \newcolumntype{d}{S[table-format=3.2]}
#' - \usepackage{hyperref}
#' - \hypersetup{colorlinks=true,linkcolor=blue,filecolor=magenta,urlcolor=cyan}
#' output: pdf_document
#' ---
#+ results='asis'
if(F){}
library(spida2)
  library(gnew)
  library(kableExtra)
  a <- Titanic
  list_array(a, 4)  
  list_array(a, 4)  %>% pr
  list_array(a, 3:4)  
  list_array(a, 3:4)  %>% pr
  kbl_array(tab(a),2:3)
  kbl_array(tab(a, pct =1 ),2:3)

  kbl_array  

kk <-  function(a, MARGIN, ...) {
    library(kableExtra)
    larr <- list_array(a, MARGIN)
    mat <- do.call(rbind, larr)
    narr <- sapply(larr, nrow)
    nend <- cumsum(narr)
    nr <- narr[1]
    nstart <- nend - nr + 1
    
    ret <- kableExtra::kbl(mat,...)
    for( i in seq_along(narr)){
      ret <- kableExtra::group_rows(ret, names(larr)[i], start_row = nstart[i], end_row = nend[i]) 
    }
    ret <- kableExtra::kable_styling(ret)
    ret
}

kk(Titanic, c(2,3)) 


Titanic %>% 
  as.data.frame %>% 
  {.[,c(4,3,2,1,5)]} %>% 
  kbl(longtable= T, caption = 'This is the caption') %>%
  #  collapse_rows(1:4,  row_group_label_position = 'stack', valign = 'top') %>% 
  collapse_rows(1:4) %>% 
  kable_styling(full_width=F, latex_options = c('repeat_header'))
# kable_styling(full_width=F)
Titanic %>% 
  as.data.frame %>% 
  {.[,c(4,3,2,1,5)]} %>% 
  kbl(longtable= T, caption = 'This is the caption') %>%
  #  collapse_rows(1:4,  row_group_label_position = 'stack', valign = 'top') %>% 
  collapse_rows(1:4) %>% 
  kable_styling(full_width=F)

# kable_styling(full_width=F)

