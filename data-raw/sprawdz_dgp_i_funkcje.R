# Test integracyjny DGP i funkcji pakietu ClickbaitRankR
# Uruchom z katalogu glownego pakietu:
# Rscript data-raw/sprawdz_dgp_i_funkcje.R

sprawdz <- function(warunek, komunikat) {
  if (!isTRUE(warunek)) {
    stop("BLAD: ", komunikat, call. = FALSE)
  }
  message("[OK] ", komunikat)
}

sprawdz_zbior_rang <- function(rangi, n, komunikat) {
  sprawdz(setequal(rangi, seq_len(n)), komunikat)
}

if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(".", quiet = TRUE)
} else {
  library(ClickbaitRankR)
}

srodowisko_dgp <- new.env(parent = globalenv())
source("data-raw/generuj_dane_mcda.R", local = srodowisko_dgp)

parametry_testu <- modifyList(
  srodowisko_dgp$parametry_dgp,
  list(
    seed = 20260430,
    liczba_alternatyw = 10,
    liczba_ekspertow = 6,
    udzial_zrodel_sensacyjnych = 0.40,
    prawdopodobienstwo_braku = 0.04,
    prawdopodobienstwo_kodu_bledu = 0.01
  )
)

dane_dgp <- srodowisko_dgp$generuj_dane_clickbait(parametry_testu)

sprawdz(is.data.frame(dane_dgp), "DGP zwraca ramke danych")
sprawdz(
  nrow(dane_dgp) == parametry_testu$liczba_alternatyw * parametry_testu$liczba_ekspertow,
  "liczba obserwacji zgadza sie z parametrami DGP"
)
sprawdz(
  length(unique(dane_dgp$Alternatywa)) == parametry_testu$liczba_alternatyw,
  "liczba alternatyw zgadza sie z parametrami DGP"
)
sprawdz(
  length(unique(dane_dgp$EkspertID)) == parametry_testu$liczba_ekspertow,
  "liczba ekspertow zgadza sie z parametrami DGP"
)

skladnia_clickbait <- "
  Wiarygodnosc =~ autor_transparentnosc + autor_ekspertyza + autor_reputacja;
  Koszt =~ koszt_subskrypcji;
  Dostepnosc =~ dostep_do_tresci_bez_zalogowania;
  Jakosc_zrodel =~ multimedia_dowodowe + cytowania_ekspertow;
  Opoznienie_zrodel =~ zrodla_zewnetrzne;
  Sensacyjnosc =~ przymiotniki_emocjonalne + jezyk_hiperbola + clickbait_luka_informacyjna + clickbait_nacechowanie_wartosciujaco + clickbait_wrogi_jezyk;
  Zbalansowanie =~ zbalansowanie_stron + neutralnosc_naglowka;
  Manipulacja_faktami =~ fakt_wyrwanie_z_kontekstue + fakt_selektywnosc + fakt_anegdotyczny;
  Manipulacja_wizualna =~ wizual_wykres_osie + wizual_zdjecie_kontekst + wizual_retusz_sugestywny
"

typy_kryteriow <- c(
  "max", "min", "max", "max", "min",
  "min", "max", "min", "min"
)

macierz_tfn <- przygotuj_dane_mcda(
  dane = dane_dgp,
  skladnia = skladnia_clickbait,
  kolumna_alternatyw = "Alternatywa"
)

sprawdz(is.matrix(macierz_tfn), "przygotuj_dane_mcda zwraca macierz")
sprawdz(nrow(macierz_tfn) == parametry_testu$liczba_alternatyw, "macierz ma po jednym wierszu na alternatywe")
sprawdz(ncol(macierz_tfn) == 3 * length(typy_kryteriow), "macierz TFN ma trzy kolumny na kryterium")
sprawdz(!anyNA(macierz_tfn), "macierz TFN nie zawiera brakow danych")
sprawdz(all(is.finite(macierz_tfn)), "macierz TFN zawiera tylko wartosci skonczone")

wagi_critic <- oblicz_wagi_critic(macierz_tfn, typy_kryteriow)
wagi_entropii <- oblicz_wagi_entropii(macierz_tfn, typy_kryteriow)

sprawdz(abs(sum(wagi_critic) - 1) < 1e-8, "wagi CRITIC sumuja sie do 1")
sprawdz(abs(sum(wagi_entropii) - 1) < 1e-8, "wagi entropii sumuja sie do 1")
sprawdz(all(wagi_critic >= 0), "wagi CRITIC nie sa ujemne")
sprawdz(all(wagi_entropii >= 0), "wagi entropii nie sa ujemne")

wyniki <- list()

for (metoda_wag in c("critic", "entropia")) {
  wynik_topsis <- rozmyty_topsis(macierz_tfn, typy_kryteriow, metoda_wag = metoda_wag)
  wynik_vikor <- rozmyty_vikor(macierz_tfn, typy_kryteriow, metoda_wag = metoda_wag)
  wynik_waspas <- rozmyty_waspas(macierz_tfn, typy_kryteriow, metoda_wag = metoda_wag)

  sprawdz(inherits(wynik_topsis, "rozmyty_topsis_wynik"), paste("TOPSIS dziala dla wag", metoda_wag))
  sprawdz(inherits(wynik_vikor, "rozmyty_vikor_wynik"), paste("VIKOR dziala dla wag", metoda_wag))
  sprawdz(inherits(wynik_waspas, "rozmyty_waspas_wynik"), paste("WASPAS dziala dla wag", metoda_wag))

  sprawdz_zbior_rang(wynik_topsis$wyniki$Ranking, nrow(macierz_tfn), paste("TOPSIS zwraca pelny ranking dla wag", metoda_wag))
  sprawdz_zbior_rang(wynik_vikor$wyniki$Ranking, nrow(macierz_tfn), paste("VIKOR zwraca pelny ranking dla wag", metoda_wag))
  sprawdz_zbior_rang(wynik_waspas$wyniki$Ranking, nrow(macierz_tfn), paste("WASPAS zwraca pelny ranking dla wag", metoda_wag))

  sprawdz(all(is.finite(wynik_topsis$wyniki$Wynik)), paste("TOPSIS ma skonczone wyniki dla wag", metoda_wag))
  sprawdz(all(is.finite(wynik_vikor$wyniki$Def_Q)), paste("VIKOR ma skonczone Q dla wag", metoda_wag))
  sprawdz(all(is.finite(wynik_waspas$wyniki$Wynik)), paste("WASPAS ma skonczone wyniki dla wag", metoda_wag))

  wykres <- plot(wynik_vikor)
  sprawdz(inherits(wykres, "ggplot"), paste("wykres VIKOR zwraca obiekt ggplot dla wag", metoda_wag))

  tabela_topsis <- tabela_apa(wynik_topsis)
  tabela_vikor <- tabela_apa(wynik_vikor)
  tabela_waspas <- tabela_apa(wynik_waspas)
  sprawdz(inherits(tabela_topsis, "flextable"), paste("tabela APA TOPSIS dziala dla wag", metoda_wag))
  sprawdz(inherits(tabela_vikor, "flextable"), paste("tabela APA VIKOR dziala dla wag", metoda_wag))
  sprawdz(inherits(tabela_waspas, "flextable"), paste("tabela APA WASPAS dziala dla wag", metoda_wag))

  meta <- fuzzy_meta_ranking(macierz_tfn, typy_kryteriow, metoda_wag = metoda_wag)
  tabela_meta <- tabela_apa(meta)
  sprawdz(is.data.frame(meta$comparison), paste("meta-ranking zwraca tabele dla wag", metoda_wag))
  sprawdz(nrow(meta$comparison) == nrow(macierz_tfn), paste("meta-ranking ma komplet alternatyw dla wag", metoda_wag))
  sprawdz(abs(sum(meta$weights) - 1) < 1e-8, paste("meta-ranking przechowuje poprawne wagi dla", metoda_wag))
  sprawdz(inherits(tabela_meta, "flextable"), paste("tabela APA meta-rankingu dziala dla wag", metoda_wag))

  wyniki[[metoda_wag]] <- list(
    topsis = wynik_topsis,
    vikor = wynik_vikor,
    waspas = wynik_waspas,
    meta = meta
  )
}

wynik_vikor_z_wagami <- rozmyty_vikor(macierz_tfn, typy_kryteriow, wagi = wagi_critic)
sprawdz(inherits(wynik_vikor_z_wagami, "rozmyty_vikor_wynik"), "VIKOR dziala z recznie przekazanym wektorem wag")

alias_topsis <- fuzzy_topsis(macierz_tfn, typy_kryteriow, weights = wagi_critic)
alias_vikor <- fuzzy_vikor(macierz_tfn, typy_kryteriow, weights = wagi_critic)
alias_waspas <- fuzzy_waspas(macierz_tfn, typy_kryteriow, weights = wagi_critic)

sprawdz(is.data.frame(alias_topsis), "alias fuzzy_topsis dziala")
sprawdz(inherits(alias_vikor, "fuzzy_vikor_res"), "alias fuzzy_vikor dziala")
sprawdz(is.data.frame(alias_waspas), "alias fuzzy_waspas dziala")
sprawdz(inherits(tabela_apa(alias_vikor), "flextable"), "tabela APA dla aliasu fuzzy_vikor dziala")
sprawdz_zbior_rang(alias_topsis$Ranking, nrow(macierz_tfn), "alias fuzzy_topsis zwraca pelny ranking")
sprawdz_zbior_rang(alias_vikor$results$Ranking, nrow(macierz_tfn), "alias fuzzy_vikor zwraca pelny ranking")
sprawdz_zbior_rang(alias_waspas$Ranking, nrow(macierz_tfn), "alias fuzzy_waspas zwraca pelny ranking")

cat("\nNajlepsze alternatywy VIKOR/CRITIC:\n")
print(head(wyniki$critic$vikor$wyniki[order(wyniki$critic$vikor$wyniki$Ranking), ], 10))

cat("\nWagi CRITIC:\n")
print(round(wagi_critic, 4))

cat("\nWagi entropii Shannona:\n")
print(round(wagi_entropii, 4))

cat("\nZakonczono test DGP i funkcji pakietu bez bledow.\n")
