# 00b_piloto_bcn.R
# Piloto: explora los datos abiertos de la BCN (datos.bcn.cl) para obtener
# la historia de militancia de los parlamentarios con fechas.
# Genera piloto_bcn_salida.txt.

paquetes <- c("httr", "jsonlite")
faltan <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan) > 0) install.packages(faltan)
library(httr)
library(jsonlite)

endpoint <- "https://datos.bcn.cl/sparql"

consultar <- function(q) {
  r <- GET(endpoint,
           query = list(query = q, format = "application/sparql-results+json"),
           user_agent("R (investigacion academica)"), timeout(120))
  cat("   estado HTTP:", status_code(r), "\n")
  if (status_code(r) != 200) { cat(substr(content(r, "text", encoding = "UTF-8"), 1, 500), "\n"); return(NULL) }
  j <- fromJSON(content(r, "text", encoding = "UTF-8"))
  b <- j$results$bindings
  if (length(b) == 0) return(data.frame())
  as.data.frame(lapply(b, function(col) col$value))
}

sink("piloto_bcn_salida.txt", split = TRUE)
options(width = 200)

cat("\n1. Recursos cuyo nombre contiene 'Kast Rist'\n")
p1 <- consultar('
SELECT DISTINCT ?s ?p ?o WHERE {
  ?s ?p ?o .
  ?s <http://xmlns.com/foaf/0.1/name> ?n .
  FILTER(CONTAINS(LCASE(STR(?n)), "kast rist"))
} LIMIT 200')
print(p1)

cat("\n2. Propiedades de los objetos ligados a esa persona (dos saltos)\n")
p2 <- consultar('
SELECT DISTINCT ?p1 ?o1 ?p2 ?o2 WHERE {
  ?s <http://xmlns.com/foaf/0.1/name> ?n .
  FILTER(CONTAINS(LCASE(STR(?n)), "kast rist"))
  ?s ?p1 ?o1 .
  ?o1 ?p2 ?o2 .
  FILTER(isIRI(?o1))
} LIMIT 400')
print(p2)

cat("\n3. Predicados que contienen militancia o partido en todo el grafo\n")
p3 <- consultar('
SELECT DISTINCT ?p (COUNT(*) AS ?n) WHERE {
  ?s ?p ?o .
  FILTER(CONTAINS(LCASE(STR(?p)), "milit") || CONTAINS(LCASE(STR(?p)), "party") || CONTAINS(LCASE(STR(?p)), "partido"))
} GROUP BY ?p LIMIT 100')
print(p3)

sink()
cat("\nListo. Sube el archivo", normalizePath("piloto_bcn_salida.txt"), "\n")
