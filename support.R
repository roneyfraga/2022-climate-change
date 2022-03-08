
#######################
#######################

rio::import('rawfiles/M_adaptation.rds') |>
    tibble::as_tibble() ->
    M

load('rawfiles/afiliacao.RData')

afiliacao |> head()

# retirar NULL e padronizar nomes das variáveis
afiliacao |>
    purrr::compact() |> 
    purrr::map(janitor::clean_names) ->
    aff

aff |>
    purrr::map(~ .x |> dplyr::pull(affiliation_country)) ->
    res

# combinações de países no mesmo artigo
lapply(res, function(x) {expand.grid.unique(x, x, include.equals = F)}) ->
    temp

temp |>
    dplyr::bind_rows() |>
    tibble::as_tibble() ->
    ide

c(ide$V1, ide$V2) |> 
    unique() |>
    sort() ->
    idv

ide |>
    dplyr::filter(!is.na(V1)) |>
    dplyr::filter(!is.na(V2)) ->
    ide2

# criar uma rede, com pesos, normalizando A -> e B -> A com adição nos pesos
graph.data.frame(ide2, directed = FALSE, vertices = idv) |> 
    {\(x) graph.adjacency(get.adjacency(x), weighted = TRUE)}() |>
    {\(x) simplify(x, remove.multiple = T, edge.attr.comb = list(weight = 'sum'))}() |>
    tidygraph::as_tbl_graph() -> 
    net

# ---------
## encontrar quantidade de artigos com ao menos um autor de cada país
M |>
    dplyr::pull(C1) |> 
    {\(x) length(grep('Brazil', x, ignore.case = T))}() 

M |>
    dplyr::pull(C1) -> 
    endereco

idv |>
    purrr::map_int(function(x) length(grep(x, endereco, ignore.case = T))) |>
    {\(x) tibble::tibble('name' = idv, 'autores' = x)}() ->
    idv2

# ---------

net |>
    tidygraph::activate(nodes) |>
    dplyr::left_join(idv2) |>
    dplyr::arrange(desc(autores)) -> 
    net2

eb <- igraph::cluster_louvain(as.undirected(net2))

# exportar para o vosviewer
V(net2)$id <- V(net2)$name
igraph::write_graph(net2, file = 'networks/net2.net', format = c("pajek"))
writePajek(eb$membership, 'networks/net2_cluster.clu')
writePajek(V(net2)$autores, 'networks/net2.vec')


