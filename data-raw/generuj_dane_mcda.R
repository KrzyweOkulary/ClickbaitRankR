# Procedura generowania danych dla ClickbaitRankR
# Uruchom z katalogu głównego pakietu:
# Rscript data-raw/generuj_dane_mcda.R
# albo wczytaj funkcje bez zapisu:
# source("data-raw/generuj_dane_mcda.R", local = TRUE)

parametry_dgp <- list(
  seed = 20260429,
  liczba_alternatyw = 20,
  liczba_ekspertow = 5,
  udzial_zrodel_sensacyjnych = 0.35,
  prawdopodobienstwo_braku = 0.03,
  prawdopodobienstwo_kodu_bledu = 0.01
)

obcinaj <- function(x, dol, gora) {
  pmin(gora, pmax(dol, x))
}

rhalf_t <- function(n, df = 3, scale = 1) {
  abs(stats::rt(n, df = df)) * scale
}

rhalf_cauchy <- function(n, scale = 1) {
  abs(stats::rt(n, df = 1)) * scale
}

skala_likerta <- function(eta, poziomy) {
  round(obcinaj(1 + (poziomy - 1) * stats::plogis(eta), 1, poziomy))
}

skala_proc <- function(eta, dol = 80, gora = 100) {
  obcinaj(dol + (gora - dol) * stats::plogis(eta), dol, gora)
}

zmienna_binarna_0_10 <- function(eta) {
  as.integer(stats::runif(length(eta)) < stats::plogis(eta)) * 10
}

dodaj_braki <- function(x, p_braku, p_bledu = 0) {
  los <- stats::runif(length(x))
  x[los < p_braku] <- NA
  if (p_bledu > 0) {
    x[los >= p_braku & los < p_braku + p_bledu] <- 99
  }
  x
}

generuj_dane_clickbait <- function(parametry = parametry_dgp) {
  set.seed(parametry$seed)

  n_alt <- parametry$liczba_alternatyw
  n_exp <- parametry$liczba_ekspertow
  n <- n_alt * n_exp

  alternatywy <- seq_len(n_alt)
  eksperci <- seq_len(n_exp)
  siatka <- expand.grid(Alternatywa = alternatywy, EkspertID = eksperci)
  siatka <- siatka[order(siatka$Alternatywa, siatka$EkspertID), ]

  mieszanka <- stats::runif(n_alt) < parametry$udzial_zrodel_sensacyjnych
  clickbait_latent <- ifelse(
    mieszanka,
    stats::rbeta(n_alt, shape1 = 5.5, shape2 = 2.2),
    stats::rbeta(n_alt, shape1 = 2.0, shape2 = 5.0)
  )
  jakosc_latent <- obcinaj(1 - clickbait_latent + stats::rnorm(n_alt, 0, 0.10), 0, 1)

  surowosc_eksperta <- stats::rt(n_exp, df = 4) * 0.28
  skale_kryteriow <- rhalf_t(23, df = 3, scale = 0.18) + 0.08
  skoki_sensacyjne <- rhalf_cauchy(n_alt, scale = 0.10)

  z <- clickbait_latent[siatka$Alternatywa]
  q <- jakosc_latent[siatka$Alternatywa]
  b <- surowosc_eksperta[siatka$EkspertID]
  impuls <- skoki_sensacyjne[siatka$Alternatywa]

  eta <- function(stala, beta_click, beta_jakosc = 0, nr_skali = 1) {
    stala + beta_click * z + beta_jakosc * q + b +
      stats::rt(n, df = 3) * skale_kryteriow[nr_skali] + impuls * sign(beta_click)
  }

  dane <- data.frame(
    EkspertID = siatka$EkspertID,
    Alternatywa = siatka$Alternatywa,

    koszt_subskrypcji = round(obcinaj(16 + 42 * q + 9 * rhalf_cauchy(n, 0.18), 12, 70), 2),
    dostep_do_tresci_bez_zalogowania = skala_likerta(eta(-0.2, 1.0, -0.3, 1), 5),

    autor_transparentnosc = skala_likerta(eta(-0.7, -1.1, 1.7, 2), 5),
    autor_ekspertyza = skala_likerta(eta(-0.8, -0.8, 1.9, 3), 5),
    autor_reputacja = skala_likerta(eta(-0.9, -0.9, 1.8, 4), 5),

    zrodla_zewnetrzne = round(obcinaj(2 + 13 * (1 - q) + rhalf_t(n, 3, 1.6), 0.5, 30), 2),
    multimedia_dowodowe = round(skala_proc(eta(0.3, -0.9, 1.6, 5), 80, 100), 2),
    cytowania_ekspertow = zmienna_binarna_0_10(eta(-0.4, -1.2, 1.7, 6)),

    przymiotniki_emocjonalne = skala_likerta(eta(-1.1, 2.2, -0.3, 7), 7),
    zbalansowanie_stron = skala_likerta(eta(-0.8, -1.2, 1.8, 8), 7),
    neutralnosc_naglowka = skala_likerta(eta(-0.7, -1.8, 1.4, 9), 7),

    fakt_wyrwanie_z_kontekstue = skala_likerta(eta(-1.3, 2.1, -0.2, 10), 5),
    fakt_selektywnosc = round(skala_proc(eta(-0.1, 1.4, -0.4, 11), 80, 100), 2),
    fakt_anegdotyczny = skala_likerta(eta(-1.0, 1.7, -0.1, 12), 5),

    wizual_wykres_osie = zmienna_binarna_0_10(eta(-1.4, 1.8, -0.2, 13)),
    wizual_zdjecie_kontekst = skala_likerta(eta(-1.1, 1.5, -0.3, 14), 5),
    wizual_retusz_sugestywny = skala_likerta(eta(-1.2, 1.6, -0.2, 15), 5),

    jezyk_hiperbola = skala_likerta(eta(-1.0, 2.1, -0.1, 16), 5),
    clickbait_luka_informacyjna = skala_likerta(eta(-0.9, 2.0, -0.2, 17), 5),
    clickbait_nacechowanie_wartosciujaco = skala_likerta(eta(-1.0, 1.8, -0.2, 18), 5),
    clickbait_wrogi_jezyk = skala_likerta(eta(-1.5, 1.6, -0.1, 19), 5)
  )

  dane$neutralnosc_naglowka <- dodaj_braki(
    dane$neutralnosc_naglowka,
    parametry$prawdopodobienstwo_braku,
    parametry$prawdopodobienstwo_kodu_bledu
  )

  dane
}

zapisz_dane_clickbait <- function(parametry = parametry_dgp,
                                  sciezka = "data/mcda_dane_surowe.rda") {
  mcda_dane_surowe <- generuj_dane_clickbait(parametry)
  dir.create(dirname(sciezka), showWarnings = FALSE, recursive = TRUE)
  save(mcda_dane_surowe, file = sciezka, compress = "xz")
  invisible(mcda_dane_surowe)
}

if (sys.nframe() == 0) {
  zapisz_dane_clickbait()
  message("Zapisano dane DGP do data/mcda_dane_surowe.rda")
}
