#' @title Walidacja rozmytej macierzy decyzyjnej
#' @description Sprawdza podstawową strukturę macierzy TFN.
#' @keywords internal
.sprawdz_macierz_tfn <- function(macierz_decyzyjna) {
  if (!is.matrix(macierz_decyzyjna)) {
    stop("Argument 'macierz_decyzyjna' musi byc macierza.")
  }
  if (!is.numeric(macierz_decyzyjna)) {
    stop("Macierz decyzyjna musi zawierac wartosci numeryczne.")
  }
  if (ncol(macierz_decyzyjna) %% 3 != 0) {
    stop("Liczba kolumn macierzy decyzyjnej musi byc wielokrotnoscia 3: (l, m, u) dla kazdego kryterium.")
  }
  if (any(!is.finite(macierz_decyzyjna), na.rm = FALSE)) {
    stop("Macierz decyzyjna nie moze zawierac wartosci NA, NaN ani Inf.")
  }
  invisible(TRUE)
}

#' @title Liczba kryteriów w macierzy TFN
#' @keywords internal
.liczba_kryteriow <- function(macierz_decyzyjna) {
  .sprawdz_macierz_tfn(macierz_decyzyjna)
  ncol(macierz_decyzyjna) / 3
}

#' @title Defuzyfikacja trójkątnej liczby rozmytej
#' @description Stosuje ważony środek TFN: (l + 4m + u) / 6.
#' @keywords internal
.defuzyfikuj_tfn <- function(l, m, u) {
  (l + 4 * m + u) / 6
}

#' @title Defuzyfikacja macierzy TFN
#' @description Zamienia macierz (l, m, u) na macierz ostrą metodą (l + 4m + u) / 6.
#' @keywords internal
.defuzyfikuj_macierz_tfn <- function(macierz_decyzyjna) {
  .sprawdz_macierz_tfn(macierz_decyzyjna)
  n_kryteriow <- ncol(macierz_decyzyjna) / 3
  macierz_ostra <- matrix(0, nrow = nrow(macierz_decyzyjna), ncol = n_kryteriow)

  for (k in seq_len(n_kryteriow)) {
    j <- 3 * k - 2
    macierz_ostra[, k] <- .defuzyfikuj_tfn(
      macierz_decyzyjna[, j],
      macierz_decyzyjna[, j + 1],
      macierz_decyzyjna[, j + 2]
    )
  }

  nazwy_kryteriow <- attr(macierz_decyzyjna, "nazwy_kryteriow")
  if (!is.null(nazwy_kryteriow) && length(nazwy_kryteriow) == n_kryteriow) {
    colnames(macierz_ostra) <- nazwy_kryteriow
  }
  rownames(macierz_ostra) <- rownames(macierz_decyzyjna)
  macierz_ostra
}

#' @title Walidacja typów kryteriów
#' @keywords internal
.sprawdz_typy_kryteriow <- function(typy_kryteriow, n_kryteriow) {
  if (length(typy_kryteriow) != n_kryteriow) {
    stop("Dlugosc 'typy_kryteriow' musi odpowiadac liczbie kryteriow.")
  }
  if (!all(typy_kryteriow %in% c("max", "min"))) {
    stop("Argument 'typy_kryteriow' moze zawierac tylko wartosci 'max' albo 'min'.")
  }
  typy_kryteriow
}

#' @title Rozszerzenie typów kryteriów do kolumn TFN
#' @keywords internal
.rozszerz_typy_kryteriow <- function(typy_kryteriow, n_kryteriow) {
  rep(.sprawdz_typy_kryteriow(typy_kryteriow, n_kryteriow), each = 3)
}

#' @title Nazwy alternatyw z macierzy decyzyjnej
#' @keywords internal
.nazwy_alternatyw <- function(macierz_decyzyjna) {
  nazwy <- rownames(macierz_decyzyjna)
  if (is.null(nazwy)) {
    nazwy <- as.character(seq_len(nrow(macierz_decyzyjna)))
  }
  nazwy
}

#' @title Bezpieczna normalizacja do przedziału [0, 1]
#' @keywords internal
.normalizuj_min_max <- function(x, kierunek = "max") {
  zakres <- range(x, na.rm = TRUE)
  if (!is.finite(zakres[1]) || !is.finite(zakres[2]) || zakres[1] == zakres[2]) {
    return(rep(1, length(x)))
  }

  if (kierunek == "max") {
    (x - zakres[1]) / (zakres[2] - zakres[1])
  } else {
    (zakres[2] - x) / (zakres[2] - zakres[1])
  }
}

#' @title Macierz zorientowana dla obiektywnych metod wag
#' @description Przekształca kryteria kosztowe i zyskowe tak, aby większa wartość oznaczała korzystniejszą ocenę.
#' @keywords internal
.normalizuj_macierz_obiektywna <- function(macierz_ostra, typy_kryteriow) {
  wynik <- matrix(0, nrow = nrow(macierz_ostra), ncol = ncol(macierz_ostra))
  for (j in seq_len(ncol(macierz_ostra))) {
    wynik[, j] <- .normalizuj_min_max(macierz_ostra[, j], typy_kryteriow[j])
  }
  colnames(wynik) <- colnames(macierz_ostra)
  rownames(wynik) <- rownames(macierz_ostra)
  wynik
}

#' @title Stabilna normalizacja wektora wag
#' @keywords internal
.normalizuj_wagi <- function(wagi) {
  if (!is.numeric(wagi) || any(!is.finite(wagi))) {
    stop("Wagi musza byc skonczonym wektorem numerycznym.")
  }
  if (any(wagi < 0)) {
    stop("Wagi nie moga byc ujemne.")
  }
  suma <- sum(wagi)
  if (suma <= 0) {
    stop("Suma wag musi byc dodatnia.")
  }
  wagi / suma
}
