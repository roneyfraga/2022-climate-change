
source('utils.R')
library(pipeR)
library(rlist)
library(rscopus)

# existe API
rscopus::have_api_key()

rio::import('rawfiles/M_adaptation.rds') |>
    tibble::as_tibble() ->
    M

# ----------
## testar busca no Scopus
M[10, ] |> dplyr::pull(TI) 

Query <- "TITLE ('ASSESSMENT OF SMALLHOLDER FARMERS ADAPTIVE CAPACITY TO CLIMATE CHANGE : USE OF A MIXED WEIGHTING SCHEME')  AND  (LIMIT-TO ( DOCTYPE , 'ar')  OR  LIMIT-TO ( DOCTYPE ,  're' ))"

scopus_search(query = Query, 
              view = "COMPLETE", 
              count = 200) ->
    x

rscopusAffiliation(x)
rscopusAutInsArt(x)
rscopusAuthors(x)
# ----------

## busca real

p1 <- "( TITLE ( '"
p2 <- "' )  OR  DOI ("
p3 <- ") )"

M |> 
    dplyr::select(TI, DI) |>
    dplyr::mutate(query = paste0(p1, TI, p2, DI, p3)) |>
    dplyr::pull(query) ->
    query

my_scopus_search <- function(x) { 
    rscopus::scopus_search(query = x, view = "COMPLETE", count = 200) 
}

query[1:12] |>
    purrr::map(purrr::safely(my_scopus_search)) |>
    purrr::map(purrr::pluck, 'result') |>
    purrr::map(rscopusAffiliation) |>
    purrr::map(tibble::as_tibble) ->
    afiliacao



