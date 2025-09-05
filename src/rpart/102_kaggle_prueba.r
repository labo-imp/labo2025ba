# Script prueba de submit a las dos competencias Kaggle 

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("yaml")

#------------------------------------------------------------------------------

getandincrement <- function( nom_archivo )
{
  contador <- read_yaml(nom_archivo)
  valor <- contador$contador
  contador$contador <- contador$contador + 1L
  write_yaml( contador, file=nom_archivo )
  return( valor )
}
#------------------------------------------------------------------------------

generarmodelo <- function( param )
{
  # cargo el dataset pequeno
  dataset <- fread( paste0("~/datasets/dataset_pequeno.csv" ) )

  dtrain <- dataset[foto_mes == 202107] # defino donde voy a entrenar
  dapply <- dataset[foto_mes == 202109] # defino donde voy a aplicar el modelo

  # genero el modelo,  aqui se construye el arbol
  # quiero predecir clase_ternaria a partir de el resto de las variables
  modelo <- rpart(
      formula = "clase_ternaria ~ .",
      data = dtrain, # los datos donde voy a entrenar
      xval = 0,
      control = param,
  )

  # aplico el modelo a los datos nuevos
  prediccion <- predict(
      object = modelo,
      newdata = dapply,
      type = "prob"
  )

  # genero la prediccion
  tb_prediccion <- as.data.table(list( 
    "numero_de_cliente" = dapply$numero_de_cliente,
    "prob"=prediccion[, "BAJA+2"] 
  ))

  # solo le envio estimulo a los registros
  #  con probabilidad de BAJA+2 mayor  a  1/40
  tb_prediccion[, Predicted := as.numeric(prob > 1 / 40)]

  # archivo de salida
  contador <- getandincrement("contador.yml")
  archivo_submit <- paste0( "K102_",
     sprintf("%.3d", contador),
     ".csv"
  )

  # solo los campos para Kaggle
  fwrite(tb_prediccion[, list(numero_de_cliente, Predicted)],
         file = archivo_submit,
         sep = ","
  )

  # preparo todo para el submit
  comentario <- paste0( "'",
      "cp=-1",
      " minsplit=", param$minsplit,
      " minbucket=", param$minbucket,
      " maxdepth=", param$maxdepth,
      "'"
  )

  comando <- paste0( "~/install/proc_kaggle_submit_sr.sh ",
      "TRUE ",
      archivo_submit, " ",
      comentario
  )

  ganancia <- system( comando, intern=TRUE )
  cat( paste0( ganancia, "\t", archivo_submit, "\n"),
      file="tb_ganancias.txt",
      append=TRUE
  )

  return(  as.numeric(ganancia) )
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# aqui empieza el programa

# creo la carpeta donde voy a trabajar
dir.create("~/buckets/b1/exp/KA2000", showWarnings = FALSE)
setwd("~/buckets/b1/exp/KA2000")

# creo el contador
if( !file.exists( "contador.yml" ) )
{
  contador <- list( "contador" = 1L )
  write_yaml( contador, file="contador.yml" )
}

# genero al azar maxdepth, minsplit y minbucket
set.seed( Sys.time() )

# modelo
param <- list()
param$cp <- -1
param$maxdepth <- sample( 4:10, 1 )
param$minsplit <- sample( 50:500, 1 )
param$minbucket <- sample( 1:(param$minsplit/2), 1 )
gan <- generarmodelo( param )

quit( save="no" )
