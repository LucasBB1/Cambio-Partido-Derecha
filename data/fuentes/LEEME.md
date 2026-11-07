# data/fuentes/

| Archivo | Contenido | Fuente |
|---|---|---|
| `salidas_partido.csv` | Las 26 salidas de partidos de derecha durante el mandato en la Cámara (2014–2026): fecha del anuncio público y su precisión, destino, bloque de destino y su fundamento, motivo declarado (principal y secundario), resumen y fuentes | Prensa y reseñas parlamentarias de la BCN (dirección electrónica de cada fuente en las columnas `fuente_fecha` y `fuente_motivo`) |
| `salidas_senado.csv` | Las 8 salidas de partidos de derecha durante el mandato en el Senado (2014–2026), con el identificador del senador en el servicio del Senado (`parlid`), tipo (renuncia o suspensión), destino, motivo declarado y fuentes | Prensa y reseñas parlamentarias de la BCN |
| `senadores_periodos.csv`, `cambios_militancia_senado.csv` | Composición del Senado por período (2010–2026) y cambios de militancia documentados | Documento de trabajo N.° 4 (BCN y prensa) |
| `detonantes.csv` | Detonante de cada salida (sanción, conflicto, candidatura, investigación o ninguno), su fecha cuando se conoce y su fuente | Prensa y reseñas parlamentarias de la BCN |
| `militancias_diputados_2010_2030.csv` | Militancias de cada diputado de los períodos 2010–2014 a 2026–2030, con fecha de inicio y de término | Cámara de Diputadas y Diputados, datos abiertos (https://opendata.camara.cl/), servicio `retornarDiputadosXPeriodo`. Es el mismo archivo del Documento de trabajo N.° 4 |
| `correcciones_militancia_camara.csv` | Historial de militancia 2010–2018 de seis diputados cuyo registro fija los cambios en los límites del período | Reseñas parlamentarias de la BCN (Documento de trabajo N.° 4) |
| `partidos_homologacion.csv` | Correspondencia entre nombres de partido y siglas | Elaboración propia (Documento de trabajo N.° 4) |
| `candidatos_servel.rds`, `candidatos_diputados_servel.rds` | Resultados por candidato de las elecciones de diputados y senadores de 2009 a 2025, con sigla y condición de militante o independiente | SERVEL, procesados en el Documento de trabajo N.° 4 |
| `electos_diputados_servel.csv` | Diputados electos de 2009 a 2025 con distrito, sigla, condición y votos | SERVEL, procesados en el Documento de trabajo N.° 4 |

**Criterios de la base de salidas.** Se incluyen las renuncias voluntarias a un partido de derecha de diputados en ejercicio. Se excluyen las renuncias anteriores a asumir el cargo, los cambios de bancada sin cambio de militancia y los cambios forzados por la disolución de un partido. La fecha es la del anuncio público; la fecha en que el registro de la Cámara anota el cambio se calcula en el script 03 y se compara en `output/tablas/03_desfase_registro.csv`.
