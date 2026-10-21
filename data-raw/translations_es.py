# -*- coding: utf-8 -*-
# Spanish UI translations (neutral Latin American Spanish, "usted").
# Keys are the English strings; see data-raw/translations.py.

ES = {
"Next": "Siguiente", "Back": "Atrás", "Done": "Finalizar", "Tour": "Tour",
"Introduction": "Introducción", "Data Upload": "Carga de datos", "Data Inspection": "Inspección de datos",
"Zero Handling": "Tratamiento de ceros", "Compositional Analysis": "Análisis composicional",
"Balances (SBP)": "Balances (SBP)", "Statistical Modeling": "Modelado estadístico",
"Reproducibility": "Reproducibilidad",
"Welcome to CoDa Stereo": "Bienvenido a CoDa Stereo",
"This tool implements the Aitchison geometry for stereological and histomorphometric data.":
  "Esta herramienta implementa la geometría de Aitchison para datos estereológicos e histomorfométricos.",
"Follow the sidebar from top to bottom to complete the PPDAC cycle.":
  "Siga el menú lateral de arriba hacia abajo para completar el ciclo PPDAC.",
"The interface is available in English, Portuguese and Spanish (EN / PT / ES at the top). Changing the language reloads the app: save your session first. Figures, the report and the R script stay in English, for publication.":
  "La interfaz está disponible en inglés, portugués y español (EN / PT / ES arriba). Cambiar el idioma recarga la aplicación: guarde su sesión antes. Las figuras, el informe y el script de R permanecen en inglés, para publicación.",
"Start guided tour": "Iniciar tour guiado",
"At least 2 parts are required for compositional analysis.": "El análisis composicional requiere al menos 2 partes.",
"'%s' has mean = %.2f, which is %.0f× the median part mean (%.2f). Is it really a compositional part?":
  "'%s' tiene media = %.2f, que es %.0f× la mediana de las medias de las partes (%.2f). ¿Es realmente una parte de la composición?",
"'%s' contains negative values, which are undefined in the simplex.":
  "'%s' contiene valores negativos, que no están definidos en el símplex.",
"Row sums have high variability (CV = %.1f%%). Parts may not form a closed composition, or a non-compositional column may be included.":
  "Las sumas de las filas varían mucho (CV = %.1f%%). Las partes podrían no formar una composición cerrada, o se incluyó una columna no composicional.",
"%d parts need exactly %d balances.": "%d partes requieren exactamente %d balances.",
"Balance %s needs at least one +1 and one -1 part.": "El balance %s necesita al menos una parte +1 y una -1.",
"Exactly one balance (the first split) must involve all parts.":
  "Exactamente un balance (la primera división) debe incluir todas las partes.",
"Balances %s and %s are not nested: each balance must split one group of its parent.":
  "Los balances %s y %s no están anidados: cada balance debe dividir uno de los grupos del balance anterior.",
"Figure export settings": "Opciones de exportación de figuras",
"Format:": "Formato:", "Width (cm):": "Ancho (cm):", "Height (cm):": "Alto (cm):", "DPI:": "DPI:",
"Font size (pt):": "Tamaño de fuente (pt):", "Plot settings": "Opciones de los gráficos",
"Colour palette:": "Paleta de colores:",
"Interactive plots (hover shows the sample)": "Gráficos interactivos (al pasar el mouse se ve la muestra)",
"Ternary Plot": "Diagrama ternario", "Ternary mode:": "Modo del ternario:",
"Select 3 parts": "Seleccionar 3 partes", "Amalgamate into 3 groups": "Amalgamar en 3 grupos",
"Group 1:": "Grupo 1:", "Group 2:": "Grupo 2:", "Group 3:": "Grupo 3:",
"Group regions:": "Regiones de los grupos:", "Convex hull": "Envolvente convexa",
"95% confidence (mean)": "Confianza del 95% (media)", "95% predictive": "Predicción del 95%",
"None": "Ninguno", "Show group centres": "Mostrar centros de los grupos",
"Download figure": "Descargar figura",
"Compositional PCA (clr-Biplot)": "PCA composicional (biplot clr)",
"X axis:": "Eje X:", "Y axis:": "Eje Y:", "Label samples": "Etiquetar muestras",
"How to read this biplot": "Cómo leer este biplot",
"<strong>Rays</strong> (arrows) are the parts, as clr variables. Ray length is proportional to the clr standard deviation: longer rays are parts whose share varies more relative to the geometric mean.":
  "Los <strong>rayos</strong> (flechas) son las partes, como variables clr. La longitud del rayo es proporcional a la desviación estándar clr: los rayos más largos son partes cuya proporción varía más respecto de la media geométrica.",
"<strong>Links</strong> are the (imaginary) segments joining two ray tips. Link length is proportional to the standard deviation of the log-ratio of those two parts. A short link means the two parts stay nearly proportional across samples.":
  "Los <strong>enlaces</strong> (links) son los segmentos (imaginarios) que unen las puntas de dos rayos. Su longitud es proporcional a la desviación estándar del log-cociente de esas dos partes. Un enlace corto indica que las dos partes se mantienen casi proporcionales entre muestras.",
"The cosine of the angle between two links approximates the correlation between the two log-ratios; links at right angles indicate uncorrelated log-ratios.":
  "El coseno del ángulo entre dos enlaces aproxima la correlación entre los dos log-cocientes; enlaces en ángulo recto indican log-cocientes no correlacionados.",
"Do <em>not</em> read the angle between two rays as a correlation between the parts themselves.":
  "<em>No</em> interprete el ángulo entre dos rayos como una correlación entre las partes mismas.",
"<strong>Points</strong> are samples; points lying toward a ray tend to have relatively more of that part. Polygons are the convex hulls of each group.":
  "Los <strong>puntos</strong> son muestras; los puntos en la dirección de un rayo tienden a tener relativamente más de esa parte. Los polígonos son las envolventes convexas de cada grupo.",
"All of this is a planar approximation: rely on it only when the two axes explain a large share of the total variance (shown in the axis titles and the scree plot).":
  "Todo esto es una aproximación en el plano: confíe en ella solo cuando los dos ejes explican gran parte de la varianza total (indicada en los títulos de los ejes y en el scree plot).",
"Scree plot and ray lengths": "Scree plot y longitud de los rayos",
"Variation matrix": "Matriz de variación",
"Variance of the log-ratio of each pair of parts. Values near zero mean the two parts stay proportional; high values mark the trade-offs that drive compositional change. These are the squared link lengths of the biplot, in full dimension.":
  "Varianza del log-cociente de cada par de partes. Valores cercanos a cero indican que las dos partes se mantienen proporcionales; valores altos marcan los compromisos (trade-offs) que impulsan el cambio composicional. Son las longitudes al cuadrado de los enlaces del biplot, en la dimensión completa.",
"Download matrix (CSV)": "Descargar matriz (CSV)",
"Compositional center (geometric mean) by group": "Centro composicional (media geométrica) por grupo",
"Stacked bars showing the closed geometric mean composition per group. Unlike arithmetic means, the compositional center respects the simplex geometry.":
  "Barras apiladas con la composición media geométrica (cerrada) de cada grupo. A diferencia de la media aritmética, el centro composicional respeta la geometría del símplex.",
"Total variance by group": "Varianza total por grupo",
"Total metric variance (mvar) of the composition per group. Higher values indicate greater internal dispersion in the simplex.":
  "Varianza métrica total (mvar) de la composición en cada grupo. Valores mayores indican mayor dispersión interna en el símplex.",
"Download imputed data": "Descargar datos imputados", "Download imputed data (CSV)": "Descargar datos imputados (CSV)",
"Colour / group by:": "Colorear / agrupar por:", "Select 3 Parts:": "Seleccione 3 partes:", "Parts:": "Partes:",
"Shape by:": "Forma según:", "Add log-total (T-space biplot):": "Agregar log-total (biplot en el espacio T):",
"No": "No", "Sum of parts (Ts)": "Suma de las partes (Ts)", "Product total (Tp)": "Total producto (Tp)",
"Choose two different axes.": "Elija dos ejes diferentes.",
"Sequential binary partition (SBP)": "Partición binaria secuencial (SBP)",
"The SBP is built automatically by Ward clustering of the parts on clr distances. Edit any cell to define your own balances: +1 = numerator, -1 = denominator, 0 = not in this balance.":
  "La SBP se construye automáticamente por agrupamiento de Ward de las partes con distancias clr. Edite cualquier celda para definir sus propios balances: +1 = numerador, -1 = denominador, 0 = fuera de este balance.",
"Reset to automatic SBP": "Volver a la SBP automática", "Download SBP (CSV)": "Descargar SBP (CSV)",
"CoDa dendrogram": "Dendrograma CoDa",
"Each horizontal axis is one balance. The vertical bar above it is the balance variance, and where the bar meets the axis is the balance mean. Coloured boxes show the balance distribution per group.":
  "Cada eje horizontal es un balance. La barra vertical sobre él es la varianza del balance, y el punto donde la barra toca el eje es la media del balance. Las cajas de colores muestran la distribución del balance en cada grupo.",
"Balances by group": "Balances por grupo",
"Each panel is one balance (numerator / denominator parts). Positive values mean the numerator parts dominate. Test each balance with a univariate permutational ANOVA; Z is the standardised effect size. With several balances, interpret P-values with the multiple comparisons in mind.":
  "Cada panel es un balance (partes del numerador / denominador). Valores positivos indican que dominan las partes del numerador. Pruebe cada balance con un ANOVA permutacional univariado; Z es el tamaño del efecto estandarizado. Con varios balances, interprete los valores de P teniendo en cuenta las comparaciones múltiples.",
"Test balances (999 permutations)": "Probar balances (999 permutaciones)", "Download tests (CSV)": "Descargar pruebas (CSV)",
" Valid SBP.": " SBP válida.", "Invalid SBP:": "SBP no válida:",
"Fix the SBP to draw the dendrogram.": "Corrija la SBP para dibujar el dendrograma.",
"Testing balances...": "Probando balances...",
"Data Input": "Entrada de datos", "Choose CSV or Excel File": "Elija un archivo CSV o Excel",
"Browse...": "Examinar...", "No file selected": "Ningún archivo seleccionado",
"First row is header": "La primera fila es el encabezado",
"CSV separator (ignored for Excel)": "Separador del CSV (se ignora en Excel)",
"Comma": "Coma", "Semicolon": "Punto y coma", "Tab": "Tabulación",
"Or try it out:": "O pruébelo:", "Example dataset:": "Conjunto de datos de ejemplo:",
"Simulated liver (2 groups)": "Hígado simulado (2 grupos)",
"Spleen, point counts per field (case study 1)": "Bazo, conteo de puntos por campo (estudio de caso 1)",
"Kidney, point counts per field (case study 2)": "Riñón, conteo de puntos por campo (estudio de caso 2)",
"Leukocyte differential counts (case study 3)": "Recuento diferencial de leucocitos (estudio de caso 3)",
"Erythrocyte nuclear abnormalities (case study 4)": "Anomalías nucleares en eritrocitos (estudio de caso 4)",
"Load example": "Cargar ejemplo", "Data Preview": "Vista previa de los datos",
"Specimens (nested designs)": "Especímenes (diseños anidados)",
"When rows are fields of view (or sections) nested within specimens, analyse one composition per specimen: fields of the same animal are not independent replicates. Point counts are summed; areas or proportions are averaged.":
  "Cuando las filas son campos de visión (o cortes) anidados en especímenes, analice una composición por espécimen: los campos de un mismo animal no son réplicas independientes. Los conteos de puntos se suman; las áreas o proporciones se promedian.",
"Aggregate rows by specimen": "Agregar filas por espécimen", "Combine parts by:": "Combinar partes por:",
"Sum (counts)": "Suma (conteos)", "Mean (areas, proportions)": "Media (áreas, proporciones)",
"Compositional parts": "Partes de la composición",
"<strong>Select which numeric columns are the compositional parts.</strong> Columns whose name looks like an ID are deselected by default; non-compositional covariates (body weight, biochemical assays on a different scale, etc.) should also be unchecked.":
  "<strong>Seleccione qué columnas numéricas son las partes de la composición.</strong> Las columnas cuyo nombre parece un ID aparecen desmarcadas; las covariables no composicionales (peso corporal, determinaciones bioquímicas en otra escala, etc.) también deben desmarcarse.",
"Pre-flight check": "Verificación previa",
"Automatic diagnostics for the selected parts:": "Diagnóstico automático de las partes seleccionadas:",
"Pattern of zeros and missing values": "Patrón de ceros y valores faltantes",
"Each column is a sample, each row a compositional part. Cells are coloured according to whether the value was <span style='color:#7570B3; font-weight:bold;'>observed</span>, a <span style='color:#D95F02; font-weight:bold;'>structural zero</span> or <span style='color:#1B9E77; font-weight:bold;'>missing (NA)</span>.":
  "Cada columna es una muestra y cada fila una parte de la composición. Las celdas se colorean según el valor sea <span style='color:#7570B3; font-weight:bold;'>observado</span>, un <span style='color:#D95F02; font-weight:bold;'>cero estructural</span> o <span style='color:#1B9E77; font-weight:bold;'>faltante (NA)</span>.",
"Missing values (naniar::vis_miss)": "Valores faltantes (naniar::vis_miss)", "Counts per part": "Conteos por parte",
"Excel sheet:": "Hoja de Excel:", "Unsupported file type:": "Tipo de archivo no admitido:",
"Specimen column:": "Columna del espécimen:",
"The specimen column cannot be a compositional part.": "La columna del espécimen no puede ser una parte de la composición.",
" %d rows aggregated into %d specimens (%s).": " %d filas agregadas en %d especímenes (%s).",
" Columns not constant within specimens were dropped: %s.": " Se eliminaron las columnas que varían dentro de los especímenes: %s.",
"Compositional parts (numeric columns to analyse):": "Partes de la composición (columnas numéricas a analizar):",
" All checks passed.": " Todas las verificaciones pasaron.",
"%d parts selected. Row sums look consistent.": "%d partes seleccionadas. Las sumas de las filas parecen consistentes.",
" Issues detected:": " Problemas detectados:", " Consider unchecking: %s": " Considere desmarcar: %s",
"R script": "Script de R",
"An R script that reproduces the steps you ran in the app (data, aggregation, zero replacement, balances and the permutational MANOVA), for the supplementary material of a paper.":
  "Un script de R que reproduce los pasos ejecutados en la aplicación (datos, agregación, reemplazo de ceros, balances y el MANOVA permutacional), para el material suplementario de un artículo.",
"Download R script": "Descargar script de R", "Report and session": "Informe y sesión", "Report": "Informe",
"A self-contained HTML report with the settings, figures, tables, model output and the R script.":
  "Un informe HTML autocontenido con la configuración, figuras, tablas, salidas del modelo y el script de R.",
"Download HTML report": "Descargar informe HTML", "Session": "Sesión",
"Save the data and every setting to a file and load it later to resume the analysis. Results are recomputed on loading (the same seed gives the same results).":
  "Guarde los datos y toda la configuración en un archivo y cárguelo después para retomar el análisis. Los resultados se recalculan al cargar (la misma semilla da los mismos resultados).",
"Save session": "Guardar sesión", "Load session (.json)": "Cargar sesión (.json)",
"Load data to generate the script.": "Cargue datos para generar el script.",
"Building report...": "Generando informe...",
"Session loaded: recomputing results...": "Sesión cargada: recalculando resultados...",
"Zero-replacement method": "Método de reemplazo de ceros",
"Compositional data may carry different kinds of zeros:": "Los datos composicionales pueden tener distintos tipos de ceros:",
"<strong>CZM</strong>: Bayesian-multiplicative replacement for <em>count zeros</em> (multinomial sampling). Use with point counts.":
  "<strong>CZM</strong>: reemplazo bayesiano-multiplicativo para <em>ceros de conteo</em> (muestreo multinomial). Úselo con conteo de puntos.",
"<strong>multRepl</strong>: multiplicative replacement for <em>rounded zeros</em> (below detection limit). Use with percentages.":
  "<strong>multRepl</strong>: reemplazo multiplicativo para <em>ceros por redondeo</em> (bajo el límite de detección). Úselo con porcentajes.",
"<strong>lrEM</strong>: log-ratio EM algorithm, also for <em>rounded zeros</em>. More efficient for moderate to large n.":
  "<strong>lrEM</strong>: algoritmo EM en log-cocientes, también para <em>ceros por redondeo</em>. Más eficiente para n moderado a grande.",
"Choose method:": "Elija el método:", "CZM (count zeros)": "CZM (ceros de conteo)",
"multRepl (rounded zeros)": "multRepl (ceros por redondeo)", "lrEM (rounded zeros)": "lrEM (ceros por redondeo)",
"Missing values (NA) handling": "Tratamiento de valores faltantes (NA)",
"If selected parts contain <code>NA</code> values, choose how to deal with them <em>before</em> zero imputation.":
  "Si las partes seleccionadas contienen valores <code>NA</code>, elija cómo tratarlos <em>antes</em> de la imputación de ceros.",
"Drop rows containing NAs (listwise deletion)": "Eliminar filas con NA (eliminación por lista)",
"Replace NAs with column geometric mean": "Reemplazar NA por la media geométrica de la columna",
"Run Imputation": "Ejecutar imputación", "no NAs detected": "no se detectaron NA",
"Replacing zeros (%s)...": "Reemplazando ceros (%s)...", "Zero method:": "Método para ceros:",
"NA handling:": "Tratamiento de NA:", "Rows analysed: %d of %d": "Filas analizadas: %d de %d",
"Parts analysed:": "Partes analizadas:",
"First five imputed rows (closed composition):": "Primeras cinco filas imputadas (composición cerrada):",
"%d row(s) (%d NA cell(s)) dropped via listwise deletion": "%d fila(s) (%d celda(s) NA) eliminada(s) por eliminación por lista",
"%d NA cell(s) partially imputed; %d residual row(s) dropped": "%d celda(s) NA imputada(s) parcialmente; %d fila(s) restante(s) eliminada(s)",
"%d NA cell(s) imputed via column geometric mean": "%d celda(s) NA imputada(s) con la media geométrica de la columna",
"No zeros detected. Using raw composition.": "No se detectaron ceros. Se usa la composición observada.",
"Permutational MANOVA (RRPP)": "MANOVA permutacional (RRPP)",
"Build a factorial model on the ilr coordinates. Select one or more factors. With two or more factors, choose between an additive model (main effects only) or a full factorial (with all interactions).":
  "Construya un modelo factorial sobre las coordenadas ilr. Seleccione uno o más factores. Con dos o más factores, elija entre un modelo aditivo (solo efectos principales) o un factorial completo (con todas las interacciones).",
"Sums of squares:": "Sumas de cuadrados:", "Type I (sequential)": "Tipo I (secuencial)",
"Type II": "Tipo II", "Type III": "Tipo III", "Permutations:": "Permutaciones:",
"199 (quick look)": "199 (vistazo rápido)", "999 (default)": "999 (predeterminado)", "9999 (final analysis)": "9999 (análisis final)",
"Total (size):": "Total (tamaño):", "Not used (composition only)": "No usar (solo la composición)",
"Joint response (composition + total)": "Respuesta conjunta (composición + total)",
"Covariate (does composition change with size?)": "Covariable (¿cambia la composición con el tamaño?)",
"Total:": "Total:", "Sum of parts (Ts; recommended)": "Suma de las partes (Ts; recomendado)",
"Product of parts^(1/sqrt(D)) (Tp; same as log of each part)": "Producto de las partes^(1/sqrt(D)) (Tp; equivale al log de cada parte)",
"Uses the log-total of each specimen (Pawlowsky-Glahn, Egozcue & Lovell 2015, Statistical Modelling 15:175-190). As a joint response it tests composition and size together; as a covariate it asks whether the composition shifts with size. Use it only when parts are on an absolute scale (e.g., absolute volumes, cells per microlitre). Point counts with a fixed number of grid points or cells have a total fixed by design.":
  "Usa el log-total de cada espécimen (Pawlowsky-Glahn, Egozcue & Lovell 2015, Statistical Modelling 15:175-190). Como respuesta conjunta, prueba composición y tamaño a la vez; como covariable, evalúa si la composición cambia con el tamaño. Úselo solo cuando las partes están en escala absoluta (por ejemplo, volúmenes absolutos, células por microlitro). Los conteos con un número fijo de puntos de grilla o de células tienen un total fijado por el diseño.",
"Run Model": "Ajustar modelo", "Download ANOVA table (CSV)": "Descargar tabla del ANOVA (CSV)",
"Total (size) alone": "Total (tamaño) por separado",
"Univariate permutational ANOVA of the log-total with the same factors, to separate the size effect from the compositional one.":
  "ANOVA permutacional univariado del log-total con los mismos factores, para separar el efecto del tamaño del efecto composicional.",
"Pairwise comparisons": "Comparaciones por pares",
"Permutational pairwise tests between group means (Euclidean distances in ilr space, plus the log-total when included) and between group dispersions (compositional disparity). Z is the standardised effect size.":
  "Pruebas permutacionales por pares entre medias de grupos (distancias euclidianas en el espacio ilr, más el log-total cuando se incluye) y entre dispersiones de grupos (disparidad composicional). Z es el tamaño del efecto estandarizado.",
"Run Pairwise": "Ejecutar comparaciones", "Download pairwise (CSV)": "Descargar comparaciones (CSV)",
"Differences in means": "Diferencias de medias", "Differences in dispersion": "Diferencias de dispersión",
"Model diagnostics": "Diagnóstico del modelo", "Residual plots from the fitted RRPP model.": "Gráficos de residuos del modelo RRPP ajustado.",
" Totals are almost constant (CV = %.1f%%): they are probably fixed by the sampling design and carry no biological information. Do not include the total.":
  " Los totales son casi constantes (CV = %.1f%%): probablemente están fijados por el diseño de muestreo y no aportan información biológica. No incluya el total.",
"Factor(s) for the model:": "Factor(es) del modelo:", "Model design:": "Diseño del modelo:",
"Additive (main effects only)": "Aditivo (solo efectos principales)", "Factorial (with interactions)": "Factorial (con interacciones)",
"Numeric covariate(s) (ANCOVA):": "Covariable(s) numérica(s) (ANCOVA):",
"Block (permutations restricted within blocks):": "Bloque (permutaciones restringidas dentro de los bloques):",
"Formula:": "Fórmula:",
"(y = ilr coordinates of the imputed composition + log-total [%s])": "(y = coordenadas ilr de la composición imputada + log-total [%s])",
"(y = ilr coordinates; log-total [%s] as covariate)": "(y = coordenadas ilr; log-total [%s] como covariable)",
"(y = ilr coordinates of the imputed composition)": "(y = coordenadas ilr de la composición imputada)",
"\nBlock: %s": "\nBloque: %s",
"Grouping factor for pairwise tests:": "Factor de agrupamiento para las comparaciones:",
"Running permutations...": "Ejecutando permutaciones...", "Model failed:": "El modelo falló:",
" Running %s permutations in the background...": " Ejecutando %s permutaciones en segundo plano...",
"Fitted formula:": "Fórmula ajustada:", "Running pairwise tests...": "Ejecutando comparaciones por pares...",
"<b>Welcome to CoDa Stereo!</b><br><br> This app implements the Aitchison geometry for stereological and histomorphometric data.<br><br> Follow the sidebar <b>top to bottom</b> to complete the PPDAC cycle. Click <b>Next</b> to continue.":
  "<b>¡Bienvenido a CoDa Stereo!</b><br><br> Esta aplicación implementa la geometría de Aitchison para datos estereológicos e histomorfométricos.<br><br> Siga el menú lateral <b>de arriba hacia abajo</b> para completar el ciclo PPDAC. Haga clic en <b>Siguiente</b> para continuar.",
"<b>1 &mdash; Data Upload</b><br><br> Upload a <b>CSV or Excel file</b>, or load one of the <b>example datasets</b> (the four case studies of the companion paper).<br><br> If rows are fields of view, <b>aggregate them by specimen</b>.<br><br> After uploading, the <b>Compositional parts</b> selector lets you choose which numeric columns are the parts of the composition. ID columns are excluded automatically.<br><br> A <b>Pre-flight check</b> alerts you if a selected column has an incompatible scale (e.g., Glycogen measured separately).":
  "<b>1 &mdash; Carga de datos</b><br><br> Suba un <b>archivo CSV o Excel</b>, o cargue uno de los <b>conjuntos de ejemplo</b> (los cuatro estudios de caso del artículo asociado).<br><br> Si las filas son campos de visión, <b>agréguelas por espécimen</b>.<br><br> Después de cargar, el selector de <b>Partes de la composición</b> permite elegir qué columnas numéricas son las partes. Las columnas de ID se excluyen automáticamente.<br><br> La <b>Verificación previa</b> le avisa si alguna columna seleccionada está en una escala incompatible (por ejemplo, glucógeno medido por separado).",
"<b>Parts selector &mdash; what to include</b><br><br> Select <em>only</em> columns that form a <b>closed composition</b> (volume fractions, area percentages, or cell counts summing to a constant).<br><br> Non-compositional covariates (body weight, biochemical assays on a different scale) must be unchecked.":
  "<b>Selector de partes &mdash; qué incluir</b><br><br> Seleccione <em>solo</em> columnas que forman una <b>composición cerrada</b> (fracciones de volumen, porcentajes de área o conteos de células que suman una constante).<br><br> Las covariables no composicionales (peso corporal, determinaciones bioquímicas en otra escala) deben desmarcarse.",
"<b>2 &mdash; Data Inspection</b><br><br> Before any transformation, visualise:<br> <ul> <li><b>Structural zeros</b> &mdash; rare tissue compartments</li> <li><b>Missing values (NA)</b></li> </ul> The three-state heatmap distinguishes observed values, zeros, and NAs &mdash; a critical distinction for choosing the right imputation method.":
  "<b>2 &mdash; Inspección de datos</b><br><br> Antes de cualquier transformación, visualice:<br> <ul> <li><b>Ceros estructurales</b> &mdash; compartimentos tisulares raros</li> <li><b>Valores faltantes (NA)</b></li> </ul> El mapa de calor de tres estados distingue valores observados, ceros y NA &mdash; una distinción clave para elegir el método de imputación.",
"<b>3 &mdash; Zero and NA handling</b><br><br> Choose the imputation method before running:<br> <ul> <li><b>CZM</b> &mdash; count zeros (point-counting stereology)</li> <li><b>multRepl</b> &mdash; rounded zeros (percentages)</li> <li><b>lrEM</b> &mdash; rounded zeros, larger samples</li> </ul> If the data has NAs, choose <em>drop rows</em> or <em>geometric-mean imputation</em> first.":
  "<b>3 &mdash; Tratamiento de ceros y NA</b><br><br> Elija el método de imputación antes de ejecutar:<br> <ul> <li><b>CZM</b> &mdash; ceros de conteo (estereología por conteo de puntos)</li> <li><b>multRepl</b> &mdash; ceros por redondeo (porcentajes)</li> <li><b>lrEM</b> &mdash; ceros por redondeo, muestras más grandes</li> </ul> Si hay NA, elija primero <em>eliminar filas</em> o <em>imputación por media geométrica</em>.",
"<b>4 &mdash; Compositional Analysis</b><br><br> <b>Ternary plot</b>: visualise 3 parts in the simplex. Use <em>Amalgamate</em> if D &gt; 3. Convex hulls or 95% regions delimit each group.<br><br> <b>clr-PCA Biplot</b>: ray length ~ clr standard deviation; the distance between two ray tips ~ standard deviation of their log-ratio. Open <em>How to read this biplot</em> for details.<br><br> <b>Variation matrix</b> and <b>compositional descriptives</b>: log-ratio variances, geometric-mean composition and total variance per group.<br><br> Use <b>Colour palette</b> to switch between high-contrast colour schemes, including colour-blind-safe options.":
  "<b>4 &mdash; Análisis composicional</b><br><br> <b>Diagrama ternario</b>: visualice 3 partes en el símplex. Use <em>Amalgamar</em> si D &gt; 3. Las envolventes convexas o regiones del 95% delimitan cada grupo.<br><br> <b>Biplot clr-PCA</b>: longitud del rayo ~ desviación estándar clr; la distancia entre las puntas de dos rayos ~ desviación estándar de su log-cociente. Abra <em>Cómo leer este biplot</em> para más detalles.<br><br> <b>Matriz de variación</b> y <b>descriptivos composicionales</b>: varianzas de los log-cocientes, composición media geométrica y varianza total por grupo.<br><br> Use la <b>Paleta de colores</b> para alternar entre esquemas de alto contraste, incluidas opciones aptas para daltonismo.",
"<b>5 &mdash; Balances (SBP)</b><br><br> A <b>sequential binary partition</b> splits the parts into nested groups; each split defines one <b>balance</b> (an ilr coordinate). <br><br> The SBP is built automatically from the data, and you can edit any cell (+1 / -1 / 0).<br><br> The <b>CoDa dendrogram</b> shows each balance's variance (bar height), mean, and per-group distribution; each balance can be tested between groups.":
  "<b>5 &mdash; Balances (SBP)</b><br><br> Una <b>partición binaria secuencial</b> divide las partes en grupos anidados; cada división define un <b>balance</b> (una coordenada ilr). <br><br> La SBP se construye automáticamente a partir de los datos, y usted puede editar cualquier celda (+1 / -1 / 0).<br><br> El <b>dendrograma CoDa</b> muestra la varianza (altura de la barra), la media y la distribución por grupo de cada balance; cada balance puede probarse entre grupos.",
"<b>6 &mdash; Statistical Modeling</b><br><br> Fits a permutational MANOVA on <b>ilr coordinates</b> via RRPP. <br><br> Select one or more factors; toggle additive or factorial design. The formula is previewed before fitting.<br><br> Covariates, a block term and the <b>total (size)</b> can be added when parts are on an absolute scale.<br><br> After fitting: run <b>pairwise tests</b> of means and dispersions and inspect <b>residual diagnostics</b>.":
  "<b>6 &mdash; Modelado estadístico</b><br><br> Ajusta un MANOVA permutacional sobre las <b>coordenadas ilr</b> mediante RRPP. <br><br> Seleccione uno o más factores; elija diseño aditivo o factorial. La fórmula se muestra antes del ajuste.<br><br> Se pueden agregar covariables, un término de bloque y el <b>total (tamaño)</b> cuando las partes están en escala absoluta.<br><br> Después del ajuste: ejecute <b>comparaciones por pares</b> de medias y dispersiones y revise el <b>diagnóstico de residuos</b>.",
"<b>7 &mdash; Reproducibility</b><br><br> Download an <b>R script</b> that reproduces your analysis, a self-contained <b>HTML report</b>, or <b>save the session</b> to resume it later.":
  "<b>7 &mdash; Reproducibilidad</b><br><br> Descargue un <b>script de R</b> que reproduce su análisis, un <b>informe HTML</b> autocontenido, o <b>guarde la sesión</b> para retomarla después.",
"<b>You're ready!</b><br><br> That covers the full PPDAC cycle.<br><br> Start by loading an example (<em>Load example</em> in the Data Upload tab) or uploading your own file.<br><br> Click this button anytime to restart the tour.":
  "<b>¡Todo listo!</b><br><br> Eso cubre el ciclo PPDAC completo.<br><br> Comience cargando un ejemplo (<em>Cargar ejemplo</em> en la pestaña Carga de datos) o subiendo su propio archivo.<br><br> Haga clic en este botón en cualquier momento para reiniciar el tour.",
"Okabe-Ito (colour-blind safe)": "Okabe-Ito (apta para daltonismo)", "Dark2 (ColorBrewer)": "Dark2 (ColorBrewer)",
"Viridis": "Viridis", "sum": "suma", "mean": "media",
"This file is not a CoDa Stereo session.": "Este archivo no es una sesión de CoDa Stereo.",
"The session has no data.": "La sesión no contiene datos.",
"The session's parts are not numeric columns of its data.": "Las partes de la sesión no son columnas numéricas de sus datos.",
}
