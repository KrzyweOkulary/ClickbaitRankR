#' Domyślne nazwy serwisów informacyjnych
#'
#' @description
#' Zwraca wektor z nazwami 20 badanych portali i serwisów informacyjnych.
#'
#' @return Wektor tekstowy o długości 20.
#' @export
pobierz_nazwy_serwisow <- function() {
  c(
    "Radio ZET", "TVN24", "Interia.pl", "wPolityce.pl", "InfoSecurity24", 
    "Defence24", "Rzeczpospolita", "Onet.pl", "Gazeta.pl", "WP.pl", 
    "Newsweek", "Polityka", "Krytyka Polityczna", "Do Rzeczy", "Dziennik Gazeta Prawna",
    "Niezalezna.pl", "Klub Jagiellonski", "OKO.press", "Polsat News", "xyz.pl"
  )
}

#' Domyślne kierunki preferencji kryteriów
#'
#' @return Wektor tekstowy o długości 9.
#' @export
pobierz_typy_kryteriow <- function() {
  c("max", "min", "max", "max", "min", "min", "max", "min", "min")
}