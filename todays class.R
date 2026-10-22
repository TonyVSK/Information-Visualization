#1. Importar dados/sensor_bruto/sensor_bruto.csv com read_csv2() e o locale() correto.

sensor <- readr::read_csv2(
  "inserir diretorio",
  locale = readr::locale(encoding = "latin1", decimal_mark = ","),
  show_col_types = FALSE
)
### COMANDOS PARA VERMOS A TABELA INTEIRA E IMPRIMIR 10 LINHAS
dplyr::glimpse(sensor)
# head(sensor, 10)
# View(sensor)

### Com isso, obtemos data hora, sensor, temperatura, umidade

#2 E 3. Renomear as três colunas de nome problemático para carimbo, temperatura_c e umidade_pct. Tratar o valor-sentinela com na_if(temperatura_c, 999).
sensor <- sensor |>
  dplyr::rename(
    carimbo = `data hora`,
    temperatura_c = `Temperatura (C)`,
    umidade_pct = `Umidade (%)`
  ) |>
  dplyr::mutate(
    temperatura_c = dplyr::na_if(temperatura_c, 999)
  )

names(sensor)
sum(sensor$temperatura_c == 999, na.rm = TRUE)
sum(is.na(sensor$temperatura_c))


#4. Converter o carimbo com dmy_hm(carimbo, tz = "America/Sao_Paulo").
## Tive que refazer tudo porque o código acima fez substituir carimbo por NA. o que está acima mantive para validação individual das etapas 1 a 3
sensor <- readr::read_csv2(
  "diretorio",
  locale = readr::locale(encoding = "latin1", decimal_mark = ","),
  show_col_types = FALSE
) |>
  dplyr::rename(
    carimbo = `data hora`,
    temperatura_c = `Temperatura (C)`,
    umidade_pct = `Umidade (%)`
  ) |>
  dplyr::mutate(
    temperatura_c = dplyr::na_if(temperatura_c, 999)
  )
## aqui sim começaria a etapa 4, mas tive que reescrever o código acima
sensor <- sensor |>
  dplyr::mutate(
    carimbo = lubridate::dmy_hm(carimbo, tz = "America/Sao_Paulo")
  )

#5. Agregar por sensor e por mês. O mês se obtém com mutate(mes = floor_date(carimbo, "month")), que arredonda cada instante para o primeiro dia do seu mês. Depois:
## Tabela está no arquivo HTML
sensor_mensal <- sensor |>
  dplyr::mutate(## criei a coluna mês, leitura inicia no primeiro dia arredondando para 1
    mes = lubridate::floor_date(carimbo, "month")
  ) |>
  dplyr::group_by(sensor, mes) |> #linhas por combinação de sensor e mes
  dplyr::summarise( # para cada grupo de sensor e mes, teremos os parâmetros abaixo
    temp_media = mean(temperatura_c, na.rm = TRUE),
    temp_max = max(temperatura_c, na.rm = TRUE),
    n_leituras = dplyr::n(),
    n_faltantes = sum(is.na(temperatura_c)),
    .groups = "drop"
  )

sensor_mensal






readr::write_csv(
  sensor_mensal,
  "diretorio%para%salvar%arquivo"
)

