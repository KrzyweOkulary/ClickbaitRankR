#' Fuzzy TOPSIS
#'
#' @description Angielski alias zgodności dla `rozmyty_topsis()` z wagami obiektywnymi.
#'
#' @param decision_mat Macierz decyzyjna TFN.
#' @param criteria_types Wektor `"max"` / `"min"` dla kryteriów.
#' @param weights Opcjonalny wektor wcześniej obliczonych wag obiektywnych.
#' @param weight_method Metoda obiektywnego ważenia: `"critic"`, `"entropia"` albo `"entropy"`.
#'
#' @return Ramka danych z wynikiem TOPSIS.
#' @export
fuzzy_topsis <- function(decision_mat, criteria_types, weights = NULL,
                         weight_method = c("critic", "entropia", "entropy")) {
  wynik <- rozmyty_topsis(
    macierz_decyzyjna = decision_mat,
    typy_kryteriow = criteria_types,
    metoda_wag = .mapuj_metode_wag(weight_method),
    wagi = weights
  )

  data.frame(
    Alternative = wynik$wyniki$Alternatywa,
    Score = wynik$wyniki$Wynik,
    Ranking = wynik$wyniki$Ranking,
    row.names = NULL
  )
}

#' Fuzzy VIKOR
#'
#' @description Angielski alias zgodności dla `rozmyty_vikor()` z wagami obiektywnymi.
#'
#' @inheritParams fuzzy_topsis
#' @param v Waga strategii użyteczności grupowej.
#'
#' @return Obiekt klasy `fuzzy_vikor_res`.
#' @export
fuzzy_vikor <- function(decision_mat, criteria_types, v = 0.5, weights = NULL,
                        weight_method = c("critic", "entropia", "entropy")) {
  wynik <- rozmyty_vikor(
    macierz_decyzyjna = decision_mat,
    typy_kryteriow = criteria_types,
    v = v,
    metoda_wag = .mapuj_metode_wag(weight_method),
    wagi = weights
  )

  out <- list(
    results = data.frame(
      Alternative = wynik$wyniki$Alternatywa,
      Def_S = wynik$wyniki$Def_S,
      Def_R = wynik$wyniki$Def_R,
      Def_Q = wynik$wyniki$Def_Q,
      Ranking = wynik$wyniki$Ranking,
      row.names = NULL
    ),
    details = wynik$detale,
    params = wynik$parametry
  )
  class(out) <- "fuzzy_vikor_res"
  out
}

#' Fuzzy WASPAS
#'
#' @description Angielski alias zgodności dla `rozmyty_waspas()` z wagami obiektywnymi.
#'
#' @inheritParams fuzzy_topsis
#' @param lambda Udział komponentu WSM w wyniku końcowym.
#'
#' @return Ramka danych z wynikiem WASPAS.
#' @export
fuzzy_waspas <- function(decision_mat, criteria_types, lambda = 0.5, weights = NULL,
                         weight_method = c("critic", "entropia", "entropy")) {
  wynik <- rozmyty_waspas(
    macierz_decyzyjna = decision_mat,
    typy_kryteriow = criteria_types,
    lambda = lambda,
    metoda_wag = .mapuj_metode_wag(weight_method),
    wagi = weights
  )

  data.frame(
    Alternative = wynik$wyniki$Alternatywa,
    WSM = wynik$wyniki$WSM,
    WPM = wynik$wyniki$WPM,
    W_Index = wynik$wyniki$Wynik,
    Ranking = wynik$wyniki$Ranking,
    row.names = NULL
  )
}
