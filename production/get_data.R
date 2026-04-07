# install.packages('formr', repos = c('https://rforms.r-universe.dev', 'https://cloud.r-project.org'))
pacman::p_load(formr, dplyr, httr, purrr)

formr_connect("kevin.carrasco@ug.uchile.cl", "k959371343") #use API to connect to formr
cuotas <- formr_raw_results("inicio")
meritocracia <- formr_raw_results("meritocracy")
vignetas <- formr_raw_results("vignetas")
cohesion <- formr_raw_results("cohesion")

etapa2 <- reduce(
  list(
    select(cuotas, -modified, -ended, -expired), 
    select(meritocracia, -session_id, -study_id, -iteration, start_1=created, modified_1=modified, ended_1=ended, expired_1=expired),
    select(vignetas, -session_id, -study_id, -iteration, start_2=created, modified_2=modified, ended_2=ended, expired_2=expired),
    select(cohesion, -session_id, -study_id, -iteration, start_3=created, ended_3=ended, expired_3=expired)
  ),
  left_join,
  by = "session"
)


etapa2 <- etapa2 %>% filter(!pid %in% c("[ticket]", "ticket", "test", "test2", "test3", "test4", "test5",
                                        "test6", "test7", "test8", "test9", "test10", "test11", "test12", 
                                        "test13", "test14", "test15", "test17dec24", "test16", "test18",
                                        "test19", "test20", "test21", "test22", "test23", "test24", "test25"))

participantes <- etapa2[complete.cases(etapa2[, c("session", "pid")]), ]
iniciadas <- etapa2[complete.cases(etapa2[, c("session", "start_01")]), ]
table(iniciadas$start_01)
completas <- etapa2[complete.cases(etapa2[, c("session", "cohesion_12")]), ]
tiempo <- select(completas, created, modified)

# tiempo <- dplyr::select(etapa2,start_vig,end_vig)

# tiempo$created <- as.POSIXct(tiempo$created, format = "%Y-%m-%d %H:%M:%S")
# tiempo$modified <- as.POSIXct(tiempo$modified, format = "%Y-%m-%d %H:%M:%S")

# Paso 2: Calcular la diferencia de tiempo (en segundos, minutos, horas, etc.)
tiempo$time_diff <- as.numeric(difftime(tiempo$modified, tiempo$created, units = "mins"))  # En minutos

# Paso 3: Calcular el promedio del tiempo de diferencia
mean(tiempo$time_diff, na.rm = TRUE)
summary(tiempo$time_diff)



hombres <- sum(completas$screen_sex==1)
mujeres <- sum(completas$screen_sex==2)

educ <- completas %>% mutate(educ_cuotas= case_when(screen_education == 1 | screen_education == 2 ~ "edu1",
                                                    screen_education == 3 | screen_education == 4 ~ "edu2",
                                                    screen_education == 5 | screen_education == 6 ~ "edu3",
                                                    screen_education >= 7  ~ "edu4"))
sjmisc::frq(educ$educ_cuotas)
age <- completas %>% mutate(age_cuotas= case_when(screen_age >= 18 & screen_age<25 ~ "age_18",
                                                  screen_age >= 25 & screen_age<35 ~ "age_25",
                                                  screen_age >= 35 & screen_age<45 ~ "age_35",
                                                  screen_age >= 45 & screen_age<55 ~ "age_45",
                                                  screen_age >= 55 & screen_age<65 ~ "age_55",
                                                  screen_age >= 65 ~"age_65"))
sjmisc::frq(age$age_cuotas)

sjmisc::frq(completas$carac_a10)


ingreso <- completas %>% mutate(nse= case_when(carac_a10 == "Menos de $280.000 mensuales liquidos" | carac_a10 == "De $280.001 a $380.000 mensuales liquidos" | carac_a10 == "De $380.001 a $470.000 mensuales liquidos" ~ "DE",
                                               carac_a10 == "De $470.001 a $610.000 mensuales liquidos" | carac_a10 == "De $610.001 a $730.000 mensuales liquidos" ~ "C3",
                                               carac_a10 == "De $730.001 a $890.000 mensuales liquidos" | carac_a10 == "De $890.001 a $1.100.000 mensuales liquidos" ~ "C2",
                                               carac_a10 == "De $1.100.001 a $2.700.000 mensuales liquidos" | carac_a10 == "De $2.700.001 a $4.100.000 mensuales liquidos" | carac_a10 == "Mas de $4.100.001 mensuales liquidos" ~ "ABC1ab"))

ingreso$nse <- factor(ingreso$nse, levels= c("DE", "C3", "C2", "ABC1ab"))
sjmisc::frq(ingreso$nse)

sjmisc::frq(completas$des_06)

data <- completas
dim(data)
# save(data, file="input/data/original/data-230125.RData")
# save(etapa2, file="input/data/original/etapa5-23012025.RData")

save(etapa2, file=here::here("input/data/original/survey-edumer/study3_rawdata.RData"))


id <- completas %>% select(pid, pid2)
writexl::write_xlsx(id, "id.xlsx")

## Cuotas hombres
formr_connect("kevin.carrasco@ug.uchile.cl", "k959371343")
n_vps <- formr_raw_results("inicio")
n_vps <- n_vps[complete.cases(n_vps[,c("session","screen_sex")]),]
cohesion <- formr_raw_results("cohesion")

completas <- merge(n_vps, cohesion, by = "session", all = FALSE)
completas <- completas[complete.cases(completas[, c("session", "cohesion_12")]), ]
completas <- completas[!completas$pid %in% c("[ticket]", "ticket", "test", "test2", "test3", "test4", 
                                             "test5", "test6", "test7", "test8", "test9", "test10", 
                                             "test11", "test12", "test13", "test14", "test15", 
                                             "test17dec24", "test16", "test18", "test19", "test20", 
                                             "test21", "test22", "test23", "test24", "test25"), ]
nrow(completas[completas$screen_sex == 1,])

## Cuotas mujeres
formr_connect("kevin.carrasco@ug.uchile.cl", "k959371343")
n_vps <- formr_raw_results("inicio")
n_vps <- n_vps[complete.cases(n_vps[,c("session","screen_sex")]),]
cohesion <- formr_raw_results("cohesion")

completas <- merge(n_vps, cohesion, by = "session", all = FALSE)
completas <- completas[complete.cases(completas[, c("session", "cohesion_12")]), ]
completas <- completas[!completas$pid %in% c("[ticket]", "ticket", "test", "test2", "test3", "test4", 
                                             "test5", "test6", "test7", "test8", "test9", "test10", 
                                             "test11", "test12", "test13", "test14", "test15", 
                                             "test17dec24", "test16", "test18", "test19","test20", 
                                             "test21", "test22", "test23", "test24", "test25"), ]
nrow(completas[completas$screen_sex == 2,])

# Cuotas edad

formr_connect("kevin.carrasco@ug.uchile.cl", "k959371343")
n_vps <- formr_raw_results("inicio")
n_vps <- n_vps[complete.cases(n_vps[,c("session","screen_sex")]),]
cohesion <- formr_raw_results("cohesion")
completas <- merge(n_vps, cohesion, by = "session", all = FALSE)
completas <- completas[complete.cases(completas[, c("session", "cohesion_12")]), ]
completas <- completas[!completas$pid %in% c("[ticket]", "ticket", "test", "test2", "test3", "test4", 
                                             "test5", "test6", "test7", "test8", "test9", "test10", 
                                             "test11", "test12", "test13", "test14", "test15", 
                                             "test17dec24", "test16", "test18", "test19","test20", 
                                             "test21", "test22", "test23", "test24", "test25"), ]
nrow(completas[completas$screen_age >= 18 & completas$screen_age < 25,])

formr_connect("kevin.carrasco@ug.uchile.cl", "k959371343")
n_vps <- formr_raw_results("inicio")
n_vps <- n_vps[complete.cases(n_vps[,c("session","screen_sex")]),]
cohesion <- formr_raw_results("cohesion")
completas <- merge(n_vps, cohesion, by = "session", all = FALSE)
completas <- completas[complete.cases(completas[, c("session", "cohesion_12")]), ]
completas <- completas[!completas$pid %in% c("[ticket]", "ticket", "test", "test2", "test3", "test4", 
                                             "test5", "test6", "test7", "test8", "test9", "test10", 
                                             "test11", "test12", "test13", "test14", "test15", 
                                             "test17dec24", "test16", "test18", "test19","test20", 
                                             "test21", "test22", "test23", "test24", "test25"), ]
nrow(completas[completas$screen_age >= 25 & completas$screen_age < 35,])

formr_connect("kevin.carrasco@ug.uchile.cl", "k959371343")
n_vps <- formr_raw_results("inicio")
n_vps <- n_vps[complete.cases(n_vps[,c("session","screen_sex")]),]
cohesion <- formr_raw_results("cohesion")
completas <- merge(n_vps, cohesion, by = "session", all = FALSE)
completas <- completas[complete.cases(completas[, c("session", "cohesion_12")]), ]
completas <- completas[!completas$pid %in% c("[ticket]", "ticket", "test", "test2", "test3", "test4", 
                                             "test5", "test6", "test7", "test8", "test9", "test10", 
                                             "test11", "test12", "test13", "test14", "test15", 
                                             "test17dec24", "test16", "test18", "test19","test20", 
                                             "test21", "test22", "test23", "test24", "test25"), ]
nrow(completas[completas$screen_age >= 35 & completas$screen_age < 45,])

formr_connect("kevin.carrasco@ug.uchile.cl", "k959371343")
n_vps <- formr_raw_results("inicio")
n_vps <- n_vps[complete.cases(n_vps[,c("session","screen_sex")]),]
cohesion <- formr_raw_results("cohesion")
completas <- merge(n_vps, cohesion, by = "session", all = FALSE)
completas <- completas[complete.cases(completas[, c("session", "cohesion_12")]), ]
completas <- completas[!completas$pid %in% c("[ticket]", "ticket", "test", "test2", "test3", "test4", 
                                             "test5", "test6", "test7", "test8", "test9", "test10", 
                                             "test11", "test12", "test13", "test14", "test15", 
                                             "test17dec24", "test16", "test18", "test19","test20", 
                                             "test21", "test22", "test23", "test24", "test25"), ]
nrow(completas[completas$screen_age >= 45 & completas$screen_age < 55,])

formr_connect("kevin.carrasco@ug.uchile.cl", "k959371343")
n_vps <- formr_raw_results("inicio")
n_vps <- n_vps[complete.cases(n_vps[,c("session","screen_sex")]),]
cohesion <- formr_raw_results("cohesion")
completas <- merge(n_vps, cohesion, by = "session", all = FALSE)
completas <- completas[complete.cases(completas[, c("session", "cohesion_12")]), ]
completas <- completas[!completas$pid %in% c("[ticket]", "ticket", "test", "test2", "test3", "test4", 
                                             "test5", "test6", "test7", "test8", "test9", "test10", 
                                             "test11", "test12", "test13", "test14", "test15", 
                                             "test17dec24", "test16", "test18", "test19","test20", 
                                             "test21", "test22", "test23", "test24", "test25"), ]
nrow(completas[completas$screen_age >= 55 & completas$screen_age < 65,])

formr_connect("kevin.carrasco@ug.uchile.cl", "k959371343")
n_vps <- formr_raw_results("inicio")
n_vps <- n_vps[complete.cases(n_vps[,c("session","screen_sex")]),]
cohesion <- formr_raw_results("cohesion")
completas <- merge(n_vps, cohesion, by = "session", all = FALSE)
completas <- completas[complete.cases(completas[, c("session", "cohesion_12")]), ]
completas <- completas[!completas$pid %in% c("[ticket]", "ticket", "test", "test2", "test3", "test4", 
                                             "test5", "test6", "test7", "test8", "test9", "test10", 
                                             "test11", "test12", "test13", "test14", "test15", 
                                             "test17dec24", "test16", "test18", "test19","test20", 
                                             "test21", "test22", "test23", "test24", "test25"), ]
nrow(completas[completas$screen_age >= 65,])
