macierz_testowa <- matrix(
  c(
    2, 3, 4, 7, 8, 9,
    4, 5, 6, 5, 6, 7,
    6, 7, 8, 3, 4, 5,
    7, 8, 9, 2, 3, 4
  ),
  nrow = 4,
  byrow = TRUE
)
attr(macierz_testowa, "nazwy_kryteriow") <- c("Jakosc", "Ryzyko")
rownames(macierz_testowa) <- paste0("A", seq_len(nrow(macierz_testowa)))
typy_testowe <- c("max", "min")

test_that("wagi obiektywne sa poprawnie normalizowane", {
  w_critic <- oblicz_wagi_critic(macierz_testowa, typy_testowe)
  w_entropii <- oblicz_wagi_entropii(macierz_testowa, typy_testowe)

  expect_equal(sum(w_critic), 1, tolerance = 1e-8)
  expect_equal(sum(w_entropii), 1, tolerance = 1e-8)
  expect_named(w_critic, c("Jakosc", "Ryzyko"))
  expect_true(all(w_critic >= 0))
  expect_true(all(w_entropii >= 0))
})

test_that("metody MCDA zwracaja kompletne rankingi", {
  wynik_vikor <- rozmyty_vikor(macierz_testowa, typy_testowe, metoda_wag = "critic")
  wynik_topsis <- rozmyty_topsis(macierz_testowa, typy_testowe, metoda_wag = "critic")
  wynik_waspas <- rozmyty_waspas(macierz_testowa, typy_testowe, metoda_wag = "entropia")

  expect_s3_class(wynik_vikor, "rozmyty_vikor_wynik")
  expect_s3_class(wynik_topsis, "rozmyty_topsis_wynik")
  expect_s3_class(wynik_waspas, "rozmyty_waspas_wynik")

  expect_setequal(wynik_vikor$wyniki$Ranking, seq_len(nrow(macierz_testowa)))
  expect_setequal(wynik_topsis$wyniki$Ranking, seq_len(nrow(macierz_testowa)))
  expect_setequal(wynik_waspas$wyniki$Ranking, seq_len(nrow(macierz_testowa)))
  expect_true(all(is.finite(wynik_vikor$wyniki$Def_Q)))
})

test_that("meta-ranking i alias angielski zachowuja zgodnosc", {
  meta <- fuzzy_meta_ranking(macierz_testowa, typy_testowe, metoda_wag = "critic")
  alias <- fuzzy_vikor(macierz_testowa, typy_testowe, weight_method = "critic")

  expect_equal(nrow(meta$comparison), nrow(macierz_testowa))
  expect_equal(sum(meta$weights), 1, tolerance = 1e-8)
  expect_s3_class(alias, "fuzzy_vikor_res")
  expect_equal(alias$results$Ranking, rozmyty_vikor(macierz_testowa, typy_testowe)$wyniki$Ranking)
})

test_that("tabele APA sa generowane dla wynikow MCDA", {
  wynik_vikor <- rozmyty_vikor(macierz_testowa, typy_testowe, metoda_wag = "critic")
  wynik_topsis <- rozmyty_topsis(macierz_testowa, typy_testowe, metoda_wag = "critic")
  wynik_waspas <- rozmyty_waspas(macierz_testowa, typy_testowe, metoda_wag = "critic")
  meta <- fuzzy_meta_ranking(macierz_testowa, typy_testowe, metoda_wag = "critic")
  alias <- fuzzy_vikor(macierz_testowa, typy_testowe, weight_method = "critic")

  expect_s3_class(tabela_apa(wynik_vikor), "flextable")
  expect_s3_class(tabela_apa(wynik_topsis), "flextable")
  expect_s3_class(tabela_apa(wynik_waspas), "flextable")
  expect_s3_class(tabela_apa(meta), "flextable")
  expect_s3_class(tabela_apa(alias), "flextable")
})
