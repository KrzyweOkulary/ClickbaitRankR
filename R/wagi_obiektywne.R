#' Obliczanie wag metodą entropii Shannona
#'
#' @description
#' Wyznacza obiektywne wagi kryteriów na podstawie zróżnicowania informacji w danych.
#' Kryteria są najpierw defuzyfikowane wzorem `(l + 4m + u) / 6`, a następnie
#' normalizowane z uwzględnieniem kierunku preferencji.
#'
#' @param macierz_decyzyjna Rozmyta macierz decyzyjna w układzie TFN: `(l, m, u)` dla każdego kryterium.
#' @param typy_kryteriow Wektor znakowy z wartościami `"max"` lub `"min"`.
#'   Jeśli pominięty, wszystkie kryteria są traktowane jako zyskowe.
#'
#' @return Nazwany wektor wag ostrych sumujących się do 1.
#' @export
oblicz_wagi_entropii <- function(macierz_decyzyjna, typy_kryteriow = NULL) {
  n_kryteriow <- .liczba_kryteriow(macierz_decyzyjna)
  if (is.null(typy_kryteriow)) {
    typy_kryteriow <- rep("max", n_kryteriow)
  }
  typy_kryteriow <- .sprawdz_typy_kryteriow(typy_kryteriow, n_kryteriow)

  macierz_ostra <- .defuzyfikuj_macierz_tfn(macierz_decyzyjna)
  macierz_norm <- .normalizuj_macierz_obiektywna(macierz_ostra, typy_kryteriow)

  if (nrow(macierz_norm) <= 1) {
    return(.nazwij_wagi(rep(1 / n_kryteriow, n_kryteriow), macierz_decyzyjna))
  }

  sumy_kolumn <- colSums(macierz_norm)
  P <- matrix(0, nrow = nrow(macierz_norm), ncol = ncol(macierz_norm))
  for (j in seq_len(ncol(macierz_norm))) {
    if (sumy_kolumn[j] > 0) {
      P[, j] <- macierz_norm[, j] / sumy_kolumn[j]
    }
  }

  stala <- 1 / log(nrow(P))
  entropia <- numeric(ncol(P))

  for (j in seq_len(ncol(P))) {
    p <- P[, j]
    p <- p[p > 0]
    if (length(p) == 0) {
      entropia[j] <- 1
    } else {
      entropia[j] <- -stala * sum(p * log(p))
    }
  }

  dywersyfikacja <- pmax(0, 1 - entropia)
  if (sum(dywersyfikacja) == 0) {
    wagi <- rep(1 / n_kryteriow, n_kryteriow)
  } else {
    wagi <- dywersyfikacja / sum(dywersyfikacja)
  }

  .nazwij_wagi(wagi, macierz_decyzyjna)
}

#' Obliczanie wag metodą CRITIC
#'
#' @description
#' Wyznacza obiektywne wagi metodą CRITIC, łącząc zmienność kryterium
#' z jego konfliktem informacyjnym wobec pozostałych kryteriów.
#' Macierz TFN jest defuzyfikowana wzorem `(l + 4m + u) / 6`.
#'
#' @param macierz_decyzyjna Rozmyta macierz decyzyjna w układzie TFN: `(l, m, u)` dla każdego kryterium.
#' @param typy_kryteriow Wektor znakowy z wartościami `"max"` lub `"min"`.
#'   Jeśli pominięty, wszystkie kryteria są traktowane jako zyskowe.
#'
#' @return Nazwany wektor wag ostrych sumujących się do 1.
#' @export
oblicz_wagi_critic <- function(macierz_decyzyjna, typy_kryteriow = NULL) {
  n_kryteriow <- .liczba_kryteriow(macierz_decyzyjna)
  if (is.null(typy_kryteriow)) {
    typy_kryteriow <- rep("max", n_kryteriow)
  }
  typy_kryteriow <- .sprawdz_typy_kryteriow(typy_kryteriow, n_kryteriow)

  if (n_kryteriow == 1) {
    return(.nazwij_wagi(1, macierz_decyzyjna))
  }

  macierz_ostra <- .defuzyfikuj_macierz_tfn(macierz_decyzyjna)
  macierz_norm <- .normalizuj_macierz_obiektywna(macierz_ostra, typy_kryteriow)

  odchylenia <- apply(macierz_norm, 2, stats::sd)
  odchylenia[!is.finite(odchylenia)] <- 0

  korelacje <- suppressWarnings(stats::cor(macierz_norm, use = "pairwise.complete.obs"))
  korelacje[!is.finite(korelacje)] <- 0
  diag(korelacje) <- 1

  konflikt <- colSums(1 - korelacje)
  informacja <- odchylenia * konflikt

  if (sum(informacja) == 0) {
    wagi <- rep(1 / n_kryteriow, n_kryteriow)
  } else {
    wagi <- informacja / sum(informacja)
  }

  .nazwij_wagi(wagi, macierz_decyzyjna)
}

#' @title Nazwanie wektora wag
#' @keywords internal
.nazwij_wagi <- function(wagi, macierz_decyzyjna) {
  nazwy <- attr(macierz_decyzyjna, "nazwy_kryteriow")
  if (!is.null(nazwy) && length(nazwy) == length(wagi)) {
    names(wagi) <- nazwy
  }
  wagi
}

#' @title Ustalenie wag dla metod MCDA
#' @description Pobiera wagi obiektywne albo normalizuje wcześniej obliczony wektor wag.
#' @keywords internal
.pobierz_finalne_wagi <- function(macierz_decyzyjna, typy_kryteriow,
                                  metoda_wag = c("critic", "entropia"),
                                  wagi = NULL) {
  n_kryteriow <- .liczba_kryteriow(macierz_decyzyjna)
  typy_kryteriow <- .sprawdz_typy_kryteriow(typy_kryteriow, n_kryteriow)

  if (!is.null(wagi)) {
    if (length(wagi) == n_kryteriow) {
      wagi_ostre <- .normalizuj_wagi(wagi)
      return(list(
        ostre = .nazwij_wagi(wagi_ostre, macierz_decyzyjna),
        rozmyte = rep(wagi_ostre, each = 3),
        metoda = "podane"
      ))
    }
    if (length(wagi) == ncol(macierz_decyzyjna)) {
      wagi_rozmyte <- .normalizuj_wagi(wagi)
      wagi_macierz <- matrix(wagi_rozmyte, ncol = 3, byrow = TRUE)
      wagi_ostre <- .normalizuj_wagi(.defuzyfikuj_tfn(
        wagi_macierz[, 1],
        wagi_macierz[, 2],
        wagi_macierz[, 3]
      ))
      return(list(
        ostre = .nazwij_wagi(wagi_ostre, macierz_decyzyjna),
        rozmyte = wagi_rozmyte,
        metoda = "podane"
      ))
    }
    stop("Dlugosc 'wagi' musi odpowiadac liczbie kryteriow albo liczbie kolumn macierzy TFN.")
  }

  metoda_wag <- match.arg(metoda_wag)
  wagi_ostre <- switch(
    metoda_wag,
    critic = oblicz_wagi_critic(macierz_decyzyjna, typy_kryteriow),
    entropia = oblicz_wagi_entropii(macierz_decyzyjna, typy_kryteriow)
  )

  list(
    ostre = wagi_ostre,
    rozmyte = rep(as.numeric(wagi_ostre), each = 3),
    metoda = metoda_wag
  )
}

#' @title Mapowanie angielskich nazw metod wag
#' @keywords internal
.mapuj_metode_wag <- function(metoda_wag) {
  metoda_wag <- match.arg(metoda_wag, c("critic", "entropia", "entropy"))
  if (metoda_wag == "entropy") "entropia" else metoda_wag
}
