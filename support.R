
#######################
# rede de colaboração entre países

rio::import('rawfiles/M_adaptation.rds') |>
    tibble::as_tibble() ->
    M

# gerar variável id, presente em afiliacao
M$id <- as.character(1:nrow(M))

load('rawfiles/afiliacao.RData')

afiliacao |> head()

# ------------
# adicionar afiliação no arquivo M, como lista de coluna num tibble()
afiliacao |> 
    dplyr::bind_rows(.id = 'id') |>
    dplyr::group_by(id) |>
    tidyr::nest() |>
    dplyr::rename(affiliation = data) |> 
    dplyr::ungroup() |>
    dplyr::right_join(M) ->
    M2

M2 |> 
    dplyr::select(SR, id, affiliation)  |>
    tidyr::unnest_wider(affiliation) |>
    tidyr::unnest_longer(affiliation_country) |>
    dplyr::select(SR, id, affiliation_country) |>
    dplyr::distinct(.keep_all = TRUE) |>
    dplyr::group_by(SR, id) |>
    dplyr::summarise(qtde_paises = n()) |>
    dplyr::ungroup() ->
    artigo_qtde_paises

# ------------
### extrair informações de uma coluna nested 

# retirar NULL, padronizar nomes, obter combinações de países no mesmo artigo, gerar lista de edges 
afiliacao |>
    purrr::compact() |> 
    purrr::map(~ .x |> janitor::clean_names()) |>
    purrr::map(~ .x |> dplyr::pull(affiliation_country)) |>
    purrr::map(function(x) {expand.grid.unique(x, x, include.equals = F)}) |>
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
igraph::graph.data.frame(ide2, directed = FALSE, vertices = idv) |> 
    {\(x) igraph::graph.adjacency(igraph::get.adjacency(x), weighted = TRUE)}() |>
    {\(x) igraph::simplify(x, remove.multiple = T, edge.attr.comb = list(weight = 'sum'))}() |>
    tidygraph::as_tbl_graph() -> 
    net

# ---------
## encontrar quantidade de artigos com ao menos um autor de cada país

M2 |> 
    dplyr::select(SR, id, affiliation)  |>
    tidyr::unnest_wider(affiliation) |>
    tidyr::unnest_longer(affiliation_country) |>
    dplyr::select(SR, id, affiliation_country) |>
    dplyr::distinct(.keep_all = TRUE) |>
    dplyr::group_by(affiliation_country) |>
    dplyr::summarise(qtde_artigos = n()) |>
    dplyr::rename(name = affiliation_country) |> 
    dplyr::ungroup() ->
    paises_artigos

net |>
    tidygraph::activate(nodes) |>
    dplyr::left_join(paises_artigos) |>
    dplyr::arrange(desc(qtde_artigos)) -> 
    net2

# ---------
## exportar para vosviewer

eb <- igraph::cluster_louvain(as.undirected(net2))

# exportar para o vosviewer
igraph::V(net2)$id <- igraph::V(net2)$name
igraph::write_graph(net2, file = 'networks/net2.net', format = c("pajek"))
writePajek(eb$membership, 'networks/net2_cluster.clu')
writePajek(V(net2)$autores, 'networks/net2.vec')


