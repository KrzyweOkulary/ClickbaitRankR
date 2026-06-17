## Define skladnia_clickbait for clickbait analysis
## This script creates the default MCDA syntax for evaluating misinformation level
## in news portals.

skladnia_clickbait <- " Wiarygodnosc =~ autor_transparentnosc + autor_ekspertyza + autor_reputacja; Koszt =~ koszt_subskrypcji; Dostepnosc =~ dostep_do_tresci_bez_zalogowania; Jakosc_zrodel =~ multimedia_dowodowe + cytowania_ekspertow; Opoznienie_zrodel =~ zrodla_zewnetrzne; Sensacyjnosc =~ przymiotniki_emocjonalne + jezyk_hiperbola + clickbait_luka_informacyjna + clickbait_nacechowanie_wartosciujaco + clickbait_wrogi_jezyk; Zbalansowanie =~ zbalansowanie_stron + neutralnosc_naglowka; Manipulacja_faktami =~ fakt_wyrwanie_z_kontekstue + fakt_selektywnosc + fakt_anegdotyczny; Manipulacja_wizualna =~ wizual_wykres_osie + wizual_zdjecie_kontekst + wizual_retusz_sugestywny "

# Save to data directory using usethis/devtools workflow
usethis::use_data(skladnia_clickbait, overwrite = TRUE)
