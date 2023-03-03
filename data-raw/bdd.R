## code to prepare `bdd` dataset goes here

library(dplyr, warn.conflicts = FALSE)

dta_raw <- readxl::read_excel("data-raw/ghpc_2022.xlsx", sheet = 3)

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

dta_final <-
    dta_pivotik |> 
    left_join(
        y = dta_ght,
        by = join_by(pond >= lower_pond, pond < upper_pond)
    )


# dta_final <-
#     dta_pivotik |> 
#     mutate(
#         ght = case_when(
#             pond < .57 ~ "00",
#             pond < .77 ~ "",
#             pond < .97 ~ "",
#             pond < 1.17 ~ "",
#             pond <  ~ "",
#             pond <  ~ "",
#             pond <  ~ "",
#             pond <  ~ "",
#             pond <  ~ "",
#             pond <  ~ "",
#             pond <  ~ "",
#             pond <  ~ "",
#             pond <  ~ "",
#             pond <  ~ "",
#             pond <  ~ "",
#             pond <  ~ "",
#             pond <  ~ "",
#             pond <  ~ "",
#             pond <  ~ "",
#             pond <  ~ "",
#             pond <  ~ "",
#             pond <  ~ "",
#             pond <  ~ "",
#             pond <  ~ "",
#             pond <  ~ "",
#             pond <  ~ "",
#             pond <  ~ "",
#             pond <  ~ "",
#             pond <  ~ "",
#             pond <  ~ "",
#             pond <  ~ "",
#             pond <  ~ "",
#             pond <  ~ ""
#         )
#     )

bdd <-
    dta_final |> 
    select(
        mpp, mpa, ik, tranche, ght, ghpc, inat
    )

usethis::use_data(bdd, overwrite = TRUE)
