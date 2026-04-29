# Procedura generowania danych

Ten katalog dokumentuje syntetyczny DGP używany do budowy przykładowego zbioru `mcda_dane_surowe`. Dane mają imitować oceny wielu ekspertów dla wielu alternatyw, czyli źródeł lub artykułów ocenianych pod kątem clickbaitowości.

## Parametry

Niech `I` oznacza liczbę alternatyw, `E` liczbę ekspertów, a `J` liczbę obserwowanych zmiennych. W skrypcie `generuj_dane_mcda.R` są one ustawiane przez:

- `liczba_alternatyw`
- `liczba_ekspertow`
- `udzial_zrodel_sensacyjnych`
- `prawdopodobienstwo_braku`
- `prawdopodobienstwo_kodu_bledu`

## Zmienna ukryta clickbaitowości

Dla alternatywy `i` generowany jest latentny poziom clickbaitowości:

```text
z_i ~ pi * Beta(5.5, 2.2) + (1 - pi) * Beta(2.0, 5.0),
q_i = clip(1 - z_i + epsilon_i, 0, 1),
epsilon_i ~ N(0, 0.10^2),
```

gdzie `z_i` opisuje presję sensacyjną, a `q_i` przybliża jakość lub wiarygodność źródła.

## Efekty ekspertów i ciężkie ogony

Oceny ekspertów nie są idealnie wymienne, dlatego każdy ekspert otrzymuje efekt surowości:

```text
b_e ~ t_4(0, 0.28).
```

Zmienność pozycji kryteriów ma rozkład half-t, a sporadyczne skoki sensacyjności mają rozkład half-Cauchy:

```text
sigma_j ~ |t_3| * 0.18 + 0.08,
h_i ~ |t_1| * 0.10.
```

To daje bardziej realistyczne, asymetryczne i ciężkoogonowe dane niż prosta symulacja normalna.

## Równanie obserwacji

Dla alternatywy `i`, eksperta `e` i zmiennej `j` powstaje latentny indeks:

```text
eta_iej = alpha_j + beta_j z_i + gamma_j q_i + b_e + sigma_j t_3 + sign(beta_j) h_i.
```

Następnie `eta_iej` jest przekształcane do odpowiedniej skali:

```text
Likert_K = round(clip(1 + (K - 1) logistic(eta_iej), 1, K)),
Procent = clip(a + (b - a) logistic(eta_iej), a, b),
Flaga_0_10 = 10 * Bernoulli(logistic(eta_iej)).
```

Wygenerowane dane są później przekształcane przez `przygotuj_dane_mcda()` do trójkątnych liczb rozmytych TFN, a defuzyfikacja w pakiecie wykorzystuje formułę:

```text
defuzz(l, m, u) = (l + 4m + u) / 6.
```
