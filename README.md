# ¿Quién se va y qué cambia? Salidas de partido en la derecha chilena y el voto en la Cámara de Diputadas y Diputados, 2014–2026

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.24026991.svg)](https://doi.org/10.5281/zenodo.24026991)

Código y datos del **Documento de trabajo N.° 5** de Lucas Bottinelli Baltra. El estudio pregunta quiénes renuncian a un partido de derecha durante su mandato en la Cámara de Diputadas y Diputados de Chile y qué cambia en su voto. Reconstruye el universo de salidas entre 2014 y 2026 con fecha de anuncio público y fuente verificable, estima la posición de cada diputado en cada trimestre a partir de las votaciones nominales de Sala, y combina un modelo de riesgo de salida, un estudio de eventos con placebos y pruebas de proceso caso a caso.

**Autor:** Lucas Bottinelli Baltra · [lucasbb1.github.io](https://lucasbb1.github.io) · ORCID [0009-0000-4865-7261](https://orcid.org/0009-0000-4865-7261)

El documento está en `documento/` (Word y PDF) y en Zenodo: https://doi.org/10.5281/zenodo.24026531

## Resultados principales

- Entre 2014 y 2026 hubo 26 salidas de partidos de derecha durante el mandato en la Cámara (25 personas); 12 fueron de la derecha convencional hacia la derecha radical.
- Quienes salen hacia la derecha radical votaban, en el año previo, a la derecha de la mediana de su partido (percentil medio de 0,78 o 0,69 según la especificación), sin diferencias entre períodos de gobierno y de oposición de la derecha.
- Esa distancia aparece en las votaciones sobre orden y seguridad y sobre economía, no en las de género, sexualidad y familia.
- La demanda electoral de derecha radical en el distrito, la votación personal y el límite a la reelección no predicen las salidas.
- El voto no se aleja del partido de origen antes de la salida, sino después. En el primer año se ubican 0,4 desviaciones estándar más a la derecha y su acuerdo con la mayoría del partido de origen cae 11 puntos.
- El registro oficial de militancias anota las salidas con una mediana de 72 días de atraso respecto del anuncio público, y omite la renuncia de Miguel Mellado a RN en 2025.
- En el Senado (8 salidas) no hubo renuncias hacia la derecha radical; el senador que RN suspendió en 2025 por apoyar a Kast era el más a la derecha de su bancada.

## Scripts

Correr en orden desde `Cambio-Partido-Derecha.Rproj`, o completo con `R/00_ejecutar_todo.R` (después de los scripts 01, 01b y 01c y de descargar los archivos presidenciales).

| Script | Qué hace |
|---|---|
| `00_funciones.R` | Paquetes, rutas, estimador IRT probit dinámico por EM y errores estándar agrupados |
| `00_piloto_opendata.R` | Piloto que verifica las operaciones del servicio de votaciones de la Cámara |
| `01_descargar_votaciones.R` | Descarga las votaciones de Sala de 2014 a 2026 y las deja en formato largo |
| `01b_descargar_proyectos.R` | Descarga la ficha (título y materias) de cada proyecto votado en la Sala de la Cámara |
| `01c_descargar_senado.R` | Descarga las votaciones de Sala del Senado de 2014 a 2026 (web-back.senado.cl) |
| `02_puntos_ideales.R` | Posición de cada diputado por trimestre (ω² = 0,1; con `omega2_usado <- 1` y `sufijo_salida <- "_omega1"`, la especificación menos suavizada) |
| `02b_bootstrap.R` | Bootstrap paramétrico de las posiciones (40 réplicas) |
| `03_panel.R` | Militancia corregida por trimestre, distancia al partido y desfase entre anuncio y registro |
| `04_riesgo_salida.R` | Modelos de riesgo de salida |
| `04b_incertidumbre.R` | Propaga la incertidumbre de las posiciones al modelo de riesgo y a la prueba de rangos |
| `05_estudio_eventos.R` | Estudio de eventos de la posición y del acuerdo con el partido de origen, con placebos |
| `06_distritos.R` | Demanda de derecha radical por distrito (primera vuelta presidencial) y votación personal |
| `07_casos.R` | Casos negativos emparejados y candidaturas siguientes |
| `08_pruebas_proceso.R` | Matriz de evidencia, pruebas de aro y pistola humeante, prueba de rangos |
| `04c_jackknife.R` | Reestima el modelo de riesgo y la prueba de rangos excluyendo cada salida |
| `11_temas.R` | Clasifica las votaciones por área temática con un diccionario aplicado al título del proyecto |
| `12_posiciones_tema.R` | Percentil previo a la salida por área temática (IRT estático por período y área) |
| `13_senado.R` | Posiciones de los senadores, militancia por fecha y percentil previo de las salidas del Senado |
| `09_figuras.R` | Figuras del documento |
| `10_cifras.R` | Cifras citadas en el documento (`output/cifras.txt`) |

Paquetes: `dplyr`, `tidyr`, `readr`, `purrr`, `stringr`, `ggplot2`, `survival`, `scales`, `readxl`, `stringi`, `httr`, `xml2`. El estimador de posiciones está programado en R base y no requiere paquetes adicionales.

## Datos

- `data/fuentes/`: base de salidas verificadas con sus fuentes, detonantes, registro de militancias de la Cámara y resultados del SERVEL (ver `data/fuentes/LEEME.md`).
- `data/processed/`: votaciones y votos procesados, posiciones por trimestre, paneles y trayectorias.
- `data/raw/`: archivos originales que no se versionan (ver `data/raw/LEEME.md`).

## Cómo citar

Bottinelli Baltra, L. (2026). *¿Quién se va y qué cambia? Salidas de partido en la derecha chilena y el voto en la Cámara de Diputadas y Diputados, 2014–2026* (Documento de trabajo N.° 5). https://doi.org/10.5281/zenodo.24026531

Código y datos: Bottinelli Baltra, L. (2026). *Cambio-Partido-Derecha* (Versión 1.0.0) [Software]. Zenodo. https://doi.org/10.5281/zenodo.24026991

## Licencias

Código: MIT (`LICENSE`). Datos procesados, tablas, figuras y texto: CC BY 4.0 (`LICENSE-DATOS.md`).
