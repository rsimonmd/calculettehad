## code to prepare `bdd` dataset goes here

library(dplyr, warn.conflicts = FALSE)

dta_raw <- readxl::read_excel("data-raw/ghpc_2022.xlsx", sheet = 3)
dta_libs_mps <- readxl::read_excel("data-raw/lib_mps.xlsx")

dta_select <-
    dta_raw |> 
    select(ghpc = GHPC,
           inat = `Association inattendue`,
           mpp = MPP,
           mpa = MPA,
           ik = IK, 
           t1 = `IPT T1`,
           t2 = `IPT T2`,
           t3 = `IPT T3`,
           t4 = `IPT T4`,
    )

dta_pivot <-
    dta_select |> 
    tidyr::pivot_longer(
        cols = c(
            t1,
            t2,
            t3,
            t4
        ),
        names_to = "tranche",
        values_to = "pond"
    )

dta_mutateik <-
    dta_pivot |> 
    mutate(
        ik1 = stringr::str_extract(
            string = ik, 
            pattern = "^\\d+"
        ),
        ik2 = stringr::str_extract(
            string = ik, 
            pattern = "\\d+$"
        )
    ) |> 
    select(-ik)

dta_pivotik <-
    dta_mutateik |> 
    tidyr::pivot_longer(
        cols = c(ik1, ik2),
        names_to = "ik_val",
        values_to = "ik"
    ) |> 
    select(-ik_val) |> 
    distinct()

# Pour tester le non equi join

dta_ght <-
    data.frame(
        lower_pond =
            seq(
                from = .57,
                to = 6.57,
                by = .2
            ),
        upper_pond = 
            c(
                seq(
                from = .77,
                to = 6.57,
                by = .2
            ),
            99
            ),
        ght = 1:31
    )

dta_join <-
    dta_pivotik |> 
    left_join(
        y = dta_ght,
        by = join_by(pond >= lower_pond, pond < upper_pond)
    ) |> 
    mutate(
        tranche = case_when(
            tranche == "t1" ~ "J1-J4",
            tranche == "t2" ~ "J5-J9",
            tranche == "t3" ~ "J10-J30",
            tranche == "t4" ~ "J31-sortie",
            TRUE ~ "erreur"
        )
    )

# Ajout des libellés pour select

dta_libs <- 
    dta_join |> 
    left_join(
        y = dta_libs_mps,
        by = c("mpp" = "code")
    ) |> 
    rename(libmpp = libmp) |> 
    left_join(
        y = dta_libs_mps,
        by = c("mpa" = "code")
    ) |> 
    rename(libmpa = libmp) |> 
    mutate(
        libmpp = paste0(mpp, " : ", libmpp),
        libmpa = paste0(mpa, " : ", libmpa)
    )
    

bdd <-
    dta_libs |> 
    select(
        mpp, libmpp, mpa, libmpa, ik, tranche, ght, ghpc
    )

usethis::use_data(bdd, overwrite = TRUE)
