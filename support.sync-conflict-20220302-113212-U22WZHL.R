
## rscopus

library(rscopus)

have_api_key()

Query <- paste0("AF-ID(60006514) AND SUBJAREA(", 
                subject_areas(), 
                ") AND PUBYEAR = 2018 AND ACCESSTYPE(OA)")


rio::import('rawfiles/M_adaptation.rds') |>
    tibble::as_tibble() ->
    M


rscopusAutInsArt('10.18472/SustDeb.v11n3.2020.33838')

rscopusAffiliation('10.18472/SustDeb.v11n3.2020.33838')

rscopusAuthors('10.18472/SustDeb.v11n3.2020.33838')
