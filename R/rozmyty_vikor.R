#' Rozmyta metoda VIKOR
#'
#' @description
#' Implementuje rozmytą metodę VIKOR jako główną metodę kompromisowego rankingu
#' źródeł lub artykułów clickbaitowych. Wagi kryteriów są wyznaczane obiektywnie
#' metodą CRITIC albo entropii Shannona.
#'
#' @param macierz_decyzyjna Macierz decyzyjna `(m x 3n)` w układzie TFN.
#' @param typy_kryteriow Wektor `"max"` / `"min"` określający kierunek preferencji kryteriów.
#' @param v Waga strategii użyteczności grupowej, z przedziału `[0, 1]`.
#' @param metoda_wag Metoda obiektywnego ważenia: `"critic"` albo `"entropia"`.
#' @param wagi Opcjonalny wektor wcześniej obliczonych wag obiektywnych.
#'
#' @return Obiekt klasy `rozmyty_vikor_wynik` z indeksami `S`, `R`, `Q` i rankingiem.
#' @export
rozmyty_vikor <- function(macierz_decyzyjna, typy_kryteriow, v = 0.5,
                          metoda_wag = c("critic", "entropia"),
                          wagi = NULL) {
  .sprawdz_macierz_tfn(macierz_decyzyjna)
  if (!is.numeric(v) || length(v) != 1 || v < 0 || v > 1) {
    stop("Parametr 'v' musi byc pojedyncza liczba z przedzialu [0, 1].")
  }

  n_kolumn <- ncol(macierz_decyzyjna)
  n_kryteriow <- n_kolumn / 3
  typy_rozmyte <- .rozszerz_typy_kryteriow(typy_kryteriow, n_kryteriow)
  finalne_wagi <- .pobierz_finalne_wagi(macierz_decyzyjna, typy_kryteriow, metoda_wag, wagi)

  ideal_poz <- ifelse(
    typy_rozmyte == "max",
    apply(macierz_decyzyjna, 2, max),
    apply(macierz_decyzyjna, 2, min)
  )
  ideal_neg <- ifelse(
    typy_rozmyte == "min",
    apply(macierz_decyzyjna, 2, max),
    apply(macierz_decyzyjna, 2, min)
  )

  macierz_d <- matrix(0, nrow = nrow(macierz_decyzyjna), ncol = n_kolumn)

  for (i in seq(1, n_kolumn, 3)) {
    if (typy_rozmyte[i] == "max") {
      mianownik <- ideal_poz[i + 2] - ideal_neg[i]
      if (mianownik == 0) mianownik <- 1e-9
      macierz_d[, i] <- (ideal_poz[i] - macierz_decyzyjna[, i + 2]) / mianownik
      macierz_d[, i + 1] <- (ideal_poz[i + 1] - macierz_decyzyjna[, i + 1]) / mianownik
      macierz_d[, i + 2] <- (ideal_poz[i + 2] - macierz_decyzyjna[, i]) / mianownik
    } else {
      mianownik <- ideal_neg[i + 2] - ideal_poz[i]
      if (mianownik == 0) mianownik <- 1e-9
      macierz_d[, i] <- (macierz_decyzyjna[, i] - ideal_poz[i + 2]) / mianownik
      macierz_d[, i + 1] <- (macierz_decyzyjna[, i + 1] - ideal_poz[i + 1]) / mianownik
      macierz_d[, i + 2] <- (macierz_decyzyjna[, i + 2] - ideal_poz[i]) / mianownik
    }
  }

  macierz_wazona_d <- macierz_d %*% diag(finalne_wagi$rozmyte)

  S_rozmyte <- matrix(0, nrow(macierz_decyzyjna), 3)
  R_rozmyte <- matrix(0, nrow(macierz_decyzyjna), 3)

  S_rozmyte[, 1] <- rowSums(macierz_wazona_d[, seq(1, n_kolumn, 3), drop = FALSE])
  S_rozmyte[, 2] <- rowSums(macierz_wazona_d[, seq(2, n_kolumn, 3), drop = FALSE])
  S_rozmyte[, 3] <- rowSums(macierz_wazona_d[, seq(3, n_kolumn, 3), drop = FALSE])

  R_rozmyte[, 1] <- apply(macierz_wazona_d[, seq(1, n_kolumn, 3), drop = FALSE], 1, max)
  R_rozmyte[, 2] <- apply(macierz_wazona_d[, seq(2, n_kolumn, 3), drop = FALSE], 1, max)
  R_rozmyte[, 3] <- apply(macierz_wazona_d[, seq(3, n_kolumn, 3), drop = FALSE], 1, max)

  s_star <- min(S_rozmyte[, 1])
  s_minus <- max(S_rozmyte[, 3])
  r_star <- min(R_rozmyte[, 1])
  r_minus <- max(R_rozmyte[, 3])

  mianownik_s <- s_minus - s_star
  mianownik_r <- r_minus - r_star
  if (mianownik_s == 0) mianownik_s <- 1
  if (mianownik_r == 0) mianownik_r <- 1

  czlon_s <- (S_rozmyte - s_star) / mianownik_s
  czlon_r <- (R_rozmyte - r_star) / mianownik_r
  Q_rozmyte <- v * czlon_s + (1 - v) * czlon_r

  def_S <- .defuzyfikuj_tfn(S_rozmyte[, 1], S_rozmyte[, 2], S_rozmyte[, 3])
  def_R <- .defuzyfikuj_tfn(R_rozmyte[, 1], R_rozmyte[, 2], R_rozmyte[, 3])
  def_Q <- .defuzyfikuj_tfn(Q_rozmyte[, 1], Q_rozmyte[, 2], Q_rozmyte[, 3])

  ramka_wynikow <- data.frame(
    Alternatywa = .nazwy_alternatyw(macierz_decyzyjna),
    Def_S = def_S,
    Def_R = def_R,
    Def_Q = def_Q,
    Ranking = rank(def_Q, ties.method = "first"),
    row.names = NULL
  )

  wynik <- list(
    wyniki = ramka_wynikow,
    detale = list(S_rozmyte = S_rozmyte, R_rozmyte = R_rozmyte, Q_rozmyte = Q_rozmyte),
    parametry = list(
      metoda = "VIKOR",
      v = v,
      metoda_wag = finalne_wagi$metoda,
      wagi = finalne_wagi$ostre
    )
  )

  class(wynik) <- "rozmyty_vikor_wynik"
  wynik
}
