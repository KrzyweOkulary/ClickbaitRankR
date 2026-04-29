#' Rozmyta metoda TOPSIS
#'
#' @description
#' Implementuje rozmytą metodę TOPSIS dla macierzy TFN. Wagi kryteriów są
#' wyznaczane obiektywnie metodą CRITIC albo entropii Shannona.
#'
#' @param macierz_decyzyjna Macierz decyzyjna `(m x 3n)` w układzie TFN.
#' @param typy_kryteriow Wektor `"max"` / `"min"` określający kierunek preferencji kryteriów.
#' @param metoda_wag Metoda obiektywnego ważenia: `"critic"` albo `"entropia"`.
#' @param wagi Opcjonalny wektor wcześniej obliczonych wag obiektywnych.
#'
#' @return Obiekt klasy `rozmyty_topsis_wynik` z rankingiem i parametrami obliczeń.
#' @export
rozmyty_topsis <- function(macierz_decyzyjna, typy_kryteriow,
                           metoda_wag = c("critic", "entropia"),
                           wagi = NULL) {
  .sprawdz_macierz_tfn(macierz_decyzyjna)

  n_kolumn <- ncol(macierz_decyzyjna)
  n_kryteriow <- n_kolumn / 3
  typy_rozmyte <- .rozszerz_typy_kryteriow(typy_kryteriow, n_kryteriow)
  finalne_wagi <- .pobierz_finalne_wagi(macierz_decyzyjna, typy_kryteriow, metoda_wag, wagi)

  macierz_norm <- matrix(nrow = nrow(macierz_decyzyjna), ncol = n_kolumn)
  mianowniki <- sqrt(apply(macierz_decyzyjna^2, 2, sum))
  mianowniki[mianowniki == 0] <- 1e-9

  for (i in seq(1, n_kolumn, 3)) {
    macierz_norm[, i] <- macierz_decyzyjna[, i] / mianowniki[i + 2]
    macierz_norm[, i + 1] <- macierz_decyzyjna[, i + 1] / mianowniki[i + 1]
    macierz_norm[, i + 2] <- macierz_decyzyjna[, i + 2] / mianowniki[i]
  }

  macierz_wazona <- macierz_norm %*% diag(finalne_wagi$rozmyte)

  ideal_poz <- ifelse(
    typy_rozmyte == "max",
    apply(macierz_wazona, 2, max),
    apply(macierz_wazona, 2, min)
  )
  ideal_neg <- ifelse(
    typy_rozmyte == "min",
    apply(macierz_wazona, 2, max),
    apply(macierz_wazona, 2, min)
  )

  roznice_poz <- (macierz_wazona - matrix(ideal_poz, nrow = nrow(macierz_decyzyjna), ncol = n_kolumn, byrow = TRUE))^2
  roznice_neg <- (macierz_wazona - matrix(ideal_neg, nrow = nrow(macierz_decyzyjna), ncol = n_kolumn, byrow = TRUE))^2

  d_poz <- matrix(0, nrow(macierz_decyzyjna), 3)
  d_neg <- matrix(0, nrow(macierz_decyzyjna), 3)

  d_poz[, 1] <- sqrt(rowSums(roznice_poz[, seq(1, n_kolumn, 3), drop = FALSE]))
  d_poz[, 2] <- sqrt(rowSums(roznice_poz[, seq(2, n_kolumn, 3), drop = FALSE]))
  d_poz[, 3] <- sqrt(rowSums(roznice_poz[, seq(3, n_kolumn, 3), drop = FALSE]))

  d_neg[, 1] <- sqrt(rowSums(roznice_neg[, seq(1, n_kolumn, 3), drop = FALSE]))
  d_neg[, 2] <- sqrt(rowSums(roznice_neg[, seq(2, n_kolumn, 3), drop = FALSE]))
  d_neg[, 3] <- sqrt(rowSums(roznice_neg[, seq(3, n_kolumn, 3), drop = FALSE]))

  mianownik <- d_neg + d_poz
  mianownik[mianownik == 0] <- 1e-9

  cc_rozmyte <- matrix(0, nrow(macierz_decyzyjna), 3)
  cc_rozmyte[, 1] <- d_neg[, 1] / mianownik[, 3]
  cc_rozmyte[, 2] <- d_neg[, 2] / mianownik[, 2]
  cc_rozmyte[, 3] <- d_neg[, 3] / mianownik[, 1]

  wynik_def <- .defuzyfikuj_tfn(cc_rozmyte[, 1], cc_rozmyte[, 2], cc_rozmyte[, 3])

  ramka_wynikow <- data.frame(
    Alternatywa = .nazwy_alternatyw(macierz_decyzyjna),
    D_plus = rowMeans(d_poz),
    D_minus = rowMeans(d_neg),
    Wynik = wynik_def,
    Ranking = rank(-wynik_def, ties.method = "first"),
    row.names = NULL
  )

  wynik <- list(
    wyniki = ramka_wynikow,
    detale = list(D_plus_rozmyte = d_poz, D_minus_rozmyte = d_neg, CC_rozmyte = cc_rozmyte),
    parametry = list(
      metoda = "TOPSIS",
      metoda_wag = finalne_wagi$metoda,
      wagi = finalne_wagi$ostre
    )
  )
  class(wynik) <- "rozmyty_topsis_wynik"
  wynik
}
