#' Rozmyta metoda WASPAS
#'
#' @description
#' Implementuje rozmytą metodę WASPAS, łącząc wariant addytywny WSM
#' oraz multiplikatywny WPM. Wagi kryteriów są wyznaczane obiektywnie
#' metodą CRITIC albo entropii Shannona.
#'
#' @param macierz_decyzyjna Macierz decyzyjna `(m x 3n)` w układzie TFN.
#' @param typy_kryteriow Wektor `"max"` / `"min"` określający kierunek preferencji kryteriów.
#' @param lambda Udział komponentu WSM w wyniku końcowym, z przedziału `[0, 1]`.
#' @param metoda_wag Metoda obiektywnego ważenia: `"critic"` albo `"entropia"`.
#' @param wagi Opcjonalny wektor wcześniej obliczonych wag obiektywnych.
#'
#' @return Obiekt klasy `rozmyty_waspas_wynik` z rankingiem.
#' @export
rozmyty_waspas <- function(macierz_decyzyjna, typy_kryteriow, lambda = 0.5,
                           metoda_wag = c("critic", "entropia"),
                           wagi = NULL) {
  .sprawdz_macierz_tfn(macierz_decyzyjna)
  if (!is.numeric(lambda) || length(lambda) != 1 || lambda < 0 || lambda > 1) {
    stop("Parametr 'lambda' musi byc pojedyncza liczba z przedzialu [0, 1].")
  }

  n_kolumn <- ncol(macierz_decyzyjna)
  n_kryteriow <- n_kolumn / 3
  typy_rozmyte <- .rozszerz_typy_kryteriow(typy_kryteriow, n_kryteriow)
  finalne_wagi <- .pobierz_finalne_wagi(macierz_decyzyjna, typy_kryteriow, metoda_wag, wagi)

  baza_norm <- ifelse(
    typy_rozmyte == "max",
    apply(macierz_decyzyjna, 2, max),
    apply(macierz_decyzyjna, 2, min)
  )
  baza_norm[baza_norm == 0] <- 1e-9

  macierz_norm <- matrix(0, nrow(macierz_decyzyjna), n_kolumn)
  for (j in seq(1, n_kolumn, 3)) {
    if (typy_rozmyte[j] == "max") {
      macierz_norm[, j] <- macierz_decyzyjna[, j] / baza_norm[j + 2]
      macierz_norm[, j + 1] <- macierz_decyzyjna[, j + 1] / baza_norm[j + 2]
      macierz_norm[, j + 2] <- macierz_decyzyjna[, j + 2] / baza_norm[j + 2]
    } else {
      macierz_norm[, j] <- baza_norm[j] / pmax(macierz_decyzyjna[, j + 2], 1e-9)
      macierz_norm[, j + 1] <- baza_norm[j] / pmax(macierz_decyzyjna[, j + 1], 1e-9)
      macierz_norm[, j + 2] <- baza_norm[j] / pmax(macierz_decyzyjna[, j], 1e-9)
    }
  }

  suma_wazona <- macierz_norm %*% diag(finalne_wagi$rozmyte)
  WSM_rozmyte <- matrix(0, nrow(macierz_decyzyjna), 3)
  WSM_rozmyte[, 1] <- rowSums(suma_wazona[, seq(1, n_kolumn, 3), drop = FALSE])
  WSM_rozmyte[, 2] <- rowSums(suma_wazona[, seq(2, n_kolumn, 3), drop = FALSE])
  WSM_rozmyte[, 3] <- rowSums(suma_wazona[, seq(3, n_kolumn, 3), drop = FALSE])

  iloczyn_wazony <- matrix(0, nrow(macierz_decyzyjna), n_kolumn)
  for (j in seq(1, n_kolumn, 3)) {
    iloczyn_wazony[, j] <- macierz_norm[, j] ^ finalne_wagi$rozmyte[j + 2]
    iloczyn_wazony[, j + 1] <- macierz_norm[, j + 1] ^ finalne_wagi$rozmyte[j + 1]
    iloczyn_wazony[, j + 2] <- macierz_norm[, j + 2] ^ finalne_wagi$rozmyte[j]
  }

  WPM_rozmyte <- matrix(0, nrow(macierz_decyzyjna), 3)
  WPM_rozmyte[, 1] <- apply(iloczyn_wazony[, seq(1, n_kolumn, 3), drop = FALSE], 1, prod)
  WPM_rozmyte[, 2] <- apply(iloczyn_wazony[, seq(2, n_kolumn, 3), drop = FALSE], 1, prod)
  WPM_rozmyte[, 3] <- apply(iloczyn_wazony[, seq(3, n_kolumn, 3), drop = FALSE], 1, prod)

  def_wsm <- .defuzyfikuj_tfn(WSM_rozmyte[, 1], WSM_rozmyte[, 2], WSM_rozmyte[, 3])
  def_wpm <- .defuzyfikuj_tfn(WPM_rozmyte[, 1], WPM_rozmyte[, 2], WPM_rozmyte[, 3])
  wynik_q <- lambda * def_wsm + (1 - lambda) * def_wpm

  ramka_wynikow <- data.frame(
    Alternatywa = .nazwy_alternatyw(macierz_decyzyjna),
    WSM = def_wsm,
    WPM = def_wpm,
    Wynik = wynik_q,
    Ranking = rank(-wynik_q, ties.method = "first"),
    row.names = NULL
  )

  wynik <- list(
    wyniki = ramka_wynikow,
    detale = list(WSM_rozmyte = WSM_rozmyte, WPM_rozmyte = WPM_rozmyte),
    parametry = list(
      metoda = "WASPAS",
      lambda = lambda,
      metoda_wag = finalne_wagi$metoda,
      wagi = finalne_wagi$ostre
    )
  )
  class(wynik) <- "rozmyty_waspas_wynik"
  wynik
}
