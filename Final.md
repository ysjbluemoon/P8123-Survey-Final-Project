Final
================
Sijia Yue
11/24/2019

``` r
library(survey)
```

    ## Loading required package: grid

    ## Loading required package: Matrix

    ## Loading required package: survival

    ## 
    ## Attaching package: 'survey'

    ## The following object is masked from 'package:graphics':
    ## 
    ##     dotchart

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.2.1     ✔ purrr   0.3.2
    ## ✔ tibble  2.1.3     ✔ dplyr   0.8.3
    ## ✔ tidyr   1.0.0     ✔ stringr 1.4.0
    ## ✔ readr   1.3.1     ✔ forcats 0.4.0

    ## ── Conflicts ────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ tidyr::expand() masks Matrix::expand()
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ✖ tidyr::pack()   masks Matrix::pack()
    ## ✖ tidyr::unpack() masks Matrix::unpack()

### Data Preparation

``` r
cancer_df = read_csv("cancerxx.csv") %>% 
  janitor::clean_names() %>% 
  select(hhx, fmx, fpx, #identifiers
         wtfa_sa, #weights
         strat_p, psu_p, #for design
         region, 
         psahad, #ever has a psa test
         rpsa1_mt, #month of most recent psa test
         rpsa1n:psaexp)
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   FPX = col_character(),
    ##   FMX = col_character(),
    ##   FN_AGE10 = col_logical(),
    ##   FN_AGE12 = col_logical(),
    ##   FN_AGE14 = col_logical(),
    ##   FN_AGE15 = col_logical(),
    ##   FN_AGE25 = col_logical(),
    ##   FN_AGE28 = col_logical(),
    ##   FN_AGE33 = col_logical(),
    ##   FN_MAN10 = col_logical(),
    ##   FN_MAN12 = col_logical(),
    ##   FN_MAN14 = col_logical(),
    ##   FN_MAN15 = col_logical(),
    ##   FN_MAN25 = col_logical(),
    ##   FN_MAN28 = col_logical(),
    ##   FN_MAN33 = col_logical(),
    ##   HHX = col_character()
    ## )

    ## See spec(...) for full column specifications.

    ## Warning: 227 parsing failures.
    ##  row      col           expected actual           file
    ## 5956 FN_AGE10 1/0/T/F/TRUE/FALSE      9 'cancerxx.csv'
    ## 5956 FN_AGE12 1/0/T/F/TRUE/FALSE      9 'cancerxx.csv'
    ## 5956 FN_AGE14 1/0/T/F/TRUE/FALSE      9 'cancerxx.csv'
    ## 5956 FN_AGE15 1/0/T/F/TRUE/FALSE      9 'cancerxx.csv'
    ## 5956 FN_AGE25 1/0/T/F/TRUE/FALSE      9 'cancerxx.csv'
    ## .... ........ .................. ...... ..............
    ## See problems(...) for more details.

``` r
fam_dat = read_csv("familyxx.csv") %>%
    janitor::clean_names() %>% 
    select(hhx, fmx,  #identifiers
         rat_cat4, rat_cat5) # Ratio of family income to the poverty threshold (not sure the difference)
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   FMX = col_character(),
    ##   HHX = col_character()
    ## )
    ## See spec(...) for full column specifications.

``` r
pers_dat = read_csv("personsx.csv") %>%
   janitor::clean_names() %>% 
    select(hhx, fmx, fpx, #identifiers
         age_p, #age
         educ1, #education
         sex, #gender
         notcov, cover, cover65, cover65o,  #coverage > 65, 65+, alternate 65+
         la1ar, #limitation
         lcondrt, #limitation is chronic
         lachronr, #chronic limitation
         hiscodi3, #ethnicity recode,
         racreci3)#race recode
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   FPX = col_character(),
    ##   AGE_CHG = col_logical(),
    ##   FSPOUS2 = col_character(),
    ##   FCOHAB3 = col_character(),
    ##   FMX = col_character(),
    ##   HHREFLG = col_character(),
    ##   FMREFLG = col_character(),
    ##   FMRPFLG = col_character(),
    ##   FMOTHER1 = col_character(),
    ##   FFATHER1 = col_character(),
    ##   HHX = col_character(),
    ##   LCTIME5 = col_logical(),
    ##   LCUNIT5 = col_logical(),
    ##   LCTIME6 = col_logical(),
    ##   LCUNIT6 = col_logical(),
    ##   LCTIME10 = col_logical(),
    ##   LCUNIT10 = col_logical(),
    ##   LCTIME11 = col_logical(),
    ##   LCUNIT11 = col_logical(),
    ##   LCTIME90 = col_logical()
    ##   # ... with 68 more columns
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: 2523 parsing failures.
    ##  row      col           expected actual           file
    ## 1265 LCTIME5  1/0/T/F/TRUE/FALSE     96 'personsx.csv'
    ## 1265 LCUNIT5  1/0/T/F/TRUE/FALSE     6  'personsx.csv'
    ## 1265 LCDURA5  1/0/T/F/TRUE/FALSE     10 'personsx.csv'
    ## 1265 LCDURB5  1/0/T/F/TRUE/FALSE     4  'personsx.csv'
    ## 1422 LAUNIT31 1/0/T/F/TRUE/FALSE     4  'personsx.csv'
    ## .... ........ .................. ...... ..............
    ## See problems(...) for more details.

``` r
adult_dat = read_csv("samadult.csv") %>%
   janitor::clean_names() %>% 
  select(hhx, fmx, fpx, #identifiers
    ausualpl, ahcplrou, ahcplknd, #Usual source of care - different options
    fla1ar) #functional limitation
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   FPX = col_character(),
    ##   FMX = col_character(),
    ##   HHX = col_character(),
    ##   CNKIND31 = col_logical(),
    ##   CANAGE2 = col_logical(),
    ##   CANAGE4 = col_logical(),
    ##   CANAGE8 = col_logical(),
    ##   CANAGE9 = col_logical(),
    ##   CANAGE13 = col_logical(),
    ##   CANAGE17 = col_logical(),
    ##   CANAGE19 = col_logical(),
    ##   CANAGE24 = col_logical(),
    ##   CANAGE25 = col_logical(),
    ##   CANAGE27 = col_logical(),
    ##   ALTIME26 = col_logical(),
    ##   ALTIME27 = col_logical(),
    ##   ALTIME29 = col_logical(),
    ##   ALTIME30 = col_logical(),
    ##   ALTIME34 = col_logical(),
    ##   ALUNIT26 = col_logical()
    ##   # ... with 24 more columns
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: 650 parsing failures.
    ##  row      col           expected actual           file
    ## 1056 ALTIME34 1/0/T/F/TRUE/FALSE     6  'samadult.csv'
    ## 1056 ALUNIT34 1/0/T/F/TRUE/FALSE     3  'samadult.csv'
    ## 1056 ALDURB34 1/0/T/F/TRUE/FALSE     3  'samadult.csv'
    ## 1056 ALCHRC34 1/0/T/F/TRUE/FALSE     2  'samadult.csv'
    ## 1101 CANAGE24 1/0/T/F/TRUE/FALSE     35 'samadult.csv'
    ## .... ........ .................. ...... ..............
    ## See problems(...) for more details.

``` r
psa_df = cancer_df %>% 
  left_join(adult_dat, by = c("hhx", "fmx", "fpx")) %>% 
  left_join(pers_dat, by = c("hhx", "fmx", "fpx")) %>% 
  left_join(fam_dat, by = c("hhx", "fmx"))
head(psa_df)
```

    ## # A tibble: 6 x 36
    ##   hhx   fmx   fpx   wtfa_sa strat_p psu_p region psahad rpsa1_mt rpsa1n
    ##   <chr> <chr> <chr>   <dbl>   <dbl> <dbl>  <dbl>  <dbl>    <dbl>  <dbl>
    ## 1 0000… 01    01       7261     163     2      3     NA       NA     NA
    ## 2 0000… 01    01       5018     274     2      4     NA       NA     NA
    ## 3 0000… 01    01       2399     258     2      4      2       NA     NA
    ## 4 0000… 01    01       4077     184     1      3      2       NA     NA
    ## 5 0000… 01    01       2846      36     2      1     NA       NA     NA
    ## 6 0000… 01    02       9427     271     1      4     NA       NA     NA
    ## # … with 26 more variables: rpsa1t <dbl>, rpsa2 <dbl>, psareas <dbl>,
    ## #   psasugg <dbl>, psa5yr <dbl>, psaadv <dbl>, psadisav <dbl>,
    ## #   psaexp <dbl>, ausualpl <dbl>, ahcplrou <dbl>, ahcplknd <dbl>,
    ## #   fla1ar <dbl>, age_p <dbl>, educ1 <dbl>, sex <dbl>, notcov <dbl>,
    ## #   cover <dbl>, cover65 <dbl>, cover65o <dbl>, la1ar <dbl>,
    ## #   lcondrt <dbl>, lachronr <dbl>, hiscodi3 <dbl>, racreci3 <dbl>,
    ## #   rat_cat4 <dbl>, rat_cat5 <dbl>

### EDA

Count of people who has PSA before by times:

``` r
psa_df %>% 
  count(psahad)
```

    ## # A tibble: 6 x 2
    ##   psahad     n
    ##    <dbl> <int>
    ## 1      1  4412
    ## 2      2  4516
    ## 3      7    37
    ## 4      8   603
    ## 5      9   277
    ## 6     NA 23827

``` r
psa_df = psa_df %>% 
  mutate(age_cat = case_when(age_p >= 25 & age_p < 40 ~ "25–39",
                              age_p >= 40 & age_p < 50 ~ "40–49",
                              age_p >= 50 & age_p < 65 ~ "50–64",
                              age_p >= 65 ~ "65+"))
```

``` r
psa_df=psa_df %>% 
  mutate(educ_cat = case_when(educ1 < 13 ~ "Less than high school",
                              educ1 >= 13 & educ1 < 15 ~ "High school",
                              educ1 >= 15 & educ1 < 18 ~ "Some college",
                              educ1 >= 18 & educ1 <= 21 ~ "College graduate"))
```

``` r
psa_df=psa_df %>% 
  mutate(finc_cat = case_when(rat_cat5 <= 7 |  rat_cat5 %in% c(15, 16) ~ "<200%",
                              rat_cat5 %in% c(8, 9) ~ "200–299%", 
                              rat_cat5 %in% c(10, 11) ~ "300–399%",
                              rat_cat5 >= 18 & educ1 <= 21 ~ "400–499%",
                              rat_cat5 == 14  ~">=500%",
                              rat_cat5 == 17  ~">=200%, no further detail",
                              rat_cat5 %in% c(96, 99) ~ "Unknown"))
```

``` r
psa_df = psa_df %>% 
  mutate(ausualpl_cat  = case_when(ausualpl == 2 ~ "No",
                                 ausualpl %in% c(1, 3) ~ "Yes",
                                 ausualpl %in% c(7, 8, 9) ~ "Other"))
```

``` r
psa_df <- psa_df %>% 
  mutate(cover_cat  = case_when(notcov == 1 | cover == 4 | cover65 == 6 ~ "None",
                                cover == 2 | cover65 %in% 2:4 ~ "Public",
                                cover %in% c(1, 3) | cover65 %in% c(1, 5) ~ "Private/Military"))
```

``` r
psa_df <- psa_df %>% 
  mutate(lcond_chronic_cat = if_else(lcondrt == 1, "Yes", "No"))
```

``` r
psa_df <- psa_df %>% 
  mutate(race_cat = case_when(racreci3 == 1 ~ "White",
                              racreci3 == 2 ~ "Black",
                              racreci3 == 3 ~ "Asian",
                              racreci3 == 4 ~ "AN/AI"),
         eth_cat = case_when(hiscodi3 == 1 ~ "Hispanic",
                             hiscodi3 == 2 ~ "Non-Hispanic White",
                             hiscodi3 == 3 ~ "Non-Hispanic Black",
                             hiscodi3 == 4 ~ "Non-Hispanic Asian",
                             hiscodi3 == 5 ~ "Non-Hispanic AN/AI"))
```

``` r
psa_df =
  psa_df %>%
  filter(sex==1)
```

### Survey design

``` r
psa_df %>% count(rpsa1_mt)
```

    ## # A tibble: 16 x 2
    ##    rpsa1_mt     n
    ##       <dbl> <int>
    ##  1        1   249
    ##  2        2   243
    ##  3        3   218
    ##  4        4   240
    ##  5        5   206
    ##  6        6   266
    ##  7        7   211
    ##  8        8   176
    ##  9        9   211
    ## 10       10   269
    ## 11       11   221
    ## 12       12   183
    ## 13       96  1194
    ## 14       97     5
    ## 15       99   520
    ## 16       NA 10659

``` r
psa_df = 
  psa_df %>% 
  mutate(psa_1yr = case_when(rpsa1_mt %in% c(1:12) ~ 1,
                             rpsa1_mt %in% c(96,97,99) ~ 0))

psa_df %>% count(psa_1yr)
```

    ## # A tibble: 3 x 2
    ##   psa_1yr     n
    ##     <dbl> <int>
    ## 1       0  1719
    ## 2       1  2693
    ## 3      NA 10659

``` r
des <- svydesign(ids = ~psu_p, strata = ~strat_p, weights = ~wtfa_sa, nest = TRUE, data = psa_df)
```

### unstratified descriptive stats

``` r
age_pct = svyby(~psa_1yr, by = ~age_cat, svymean, na.rm = TRUE, design = des)
age_pct %>% knitr::kable()
```

|       | age\_cat |   psa\_1yr|         se|
|-------|:---------|----------:|----------:|
| 25–39 | 25–39    |  0.0000000|  0.0000000|
| 40–49 | 40–49    |  0.5189758|  0.0332043|
| 50–64 | 50–64    |  0.6227473|  0.0165301|
| 65+   | 65+      |  0.6463953|  0.0135625|

``` r
age_tot = svyby(~psa_1yr, by = ~age_cat, svytotal, na.rm = TRUE, design = des)
age_tot  %>% knitr::kable()
```

|       | age\_cat |  psa\_1yr|        se|
|-------|:---------|---------:|---------:|
| 25–39 | 25–39    |         0|       0.0|
| 40–49 | 40–49    |   1899983|  162931.2|
| 50–64 | 50–64    |   8793898|  340573.6|
| 65+   | 65+      |   8995107|  351915.4|

``` r
edu_pct = svyby(~psa_1yr, by = ~educ_cat, svymean, na.rm = TRUE, design = des)
edu_pct %>% knitr::kable()
```

|                       | educ\_cat             |   psa\_1yr|         se|
|-----------------------|:----------------------|----------:|----------:|
| College graduate      | College graduate      |  0.6399375|  0.0168319|
| High school           | High school           |  0.6253862|  0.0206409|
| Less than high school | Less than high school |  0.5731144|  0.0304473|
| Some college          | Some college          |  0.6077714|  0.0198116|

``` r
edu_tot = svyby(~psa_1yr, by = ~educ_cat, svytotal, na.rm = TRUE, design = des)
edu_tot %>% knitr::kable()
```

|                       | educ\_cat             |  psa\_1yr|        se|
|-----------------------|:----------------------|---------:|---------:|
| College graduate      | College graduate      |   8328932|  378892.3|
| High school           | High school           |   4409502|  258057.6|
| Less than high school | Less than high school |   1728912|  142318.9|
| Some college          | Some college          |   5203814|  274063.0|

``` r
finc_pct = svyby(~psa_1yr, by = ~finc_cat, svymean, na.rm = TRUE, design = des)
finc_pct  %>% knitr::kable()
```

|                              | finc\_cat                    |   psa\_1yr|         se|
|------------------------------|:-----------------------------|----------:|----------:|
| &lt;200%                     | &lt;200%                     |  0.5784159|  0.0231935|
| &gt;=200%, no further detail | &gt;=200%, no further detail |  0.5479313|  0.0596531|
| &gt;=500%                    | &gt;=500%                    |  0.6665735|  0.0165400|
| 200–299%                     | 200–299%                     |  0.6185232|  0.0298673|
| 300–399%                     | 300–399%                     |  0.5606895|  0.0303181|
| 400–499%                     | 400–499%                     |  0.5792006|  0.0284587|
| Unknown                      | Unknown                      |  0.6326029|  0.3286865|

``` r
finc_tot = svyby(~psa_1yr, by = ~finc_cat, svytotal, na.rm = TRUE, design = des)
finc_tot %>% knitr::kable()
```

|                              | finc\_cat                    |  psa\_1yr|         se|
|------------------------------|:-----------------------------|---------:|----------:|
| &lt;200%                     | &lt;200%                     |   2982172|  188371.78|
| &gt;=200%, no further detail | &gt;=200%, no further detail |    449879|   75510.99|
| &gt;=500%                    | &gt;=500%                    |   7652165|  362064.81|
| 200–299%                     | 200–299%                     |   2216520|  168282.96|
| 300–399%                     | 300–399%                     |   1889577|  145169.71|
| 400–499%                     | 400–499%                     |   2556128|  197867.83|
| Unknown                      | Unknown                      |     11316|   11316.00|

``` r
ausualp_pct <- svyby(~psa_1yr, by = ~ausualpl_cat, svymean, na.rm = TRUE, design = des)
ausualp_pct %>% knitr::kable()
```

|       | ausualpl\_cat |   psa\_1yr|         se|
|-------|:--------------|----------:|----------:|
| No    | No            |  0.3685949|  0.0534046|
| Other | Other         |  1.0000000|  0.0000000|
| Yes   | Yes           |  0.6327883|  0.0114651|

``` r
ausualp_tot <- svyby(~psa_1yr, by = ~ausualpl_cat, svytotal, na.rm = TRUE, design = des)
ausualp_tot %>% knitr::kable()
```

|       | ausualpl\_cat |  psa\_1yr|        se|
|-------|:--------------|---------:|---------:|
| No    | No            |    515320|  101538.3|
| Other | Other         |       680|     680.0|
| Yes   | Yes           |  19172988|  527127.1|

``` r
cover_pct <- svyby(~psa_1yr, by = ~cover_cat, svymean, na.rm = TRUE, design = des)
cover_pct %>% knitr::kable()
```

|                  | cover\_cat       |   psa\_1yr|         se|
|------------------|:-----------------|----------:|----------:|
| None             | None             |  0.4174773|  0.0552340|
| Private/Military | Private/Military |  0.6307811|  0.0130429|
| Public           | Public           |  0.6183174|  0.0198567|

``` r
cover_tot <- svyby(~psa_1yr, by = ~cover_cat, svytotal, na.rm = TRUE, design = des)
cover_tot %>% knitr::kable()
```

|                  | cover\_cat       |  psa\_1yr|         se|
|------------------|:-----------------|---------:|----------:|
| None             | None             |    434310|   75974.51|
| Private/Military | Private/Military |  14706175|  449301.96|
| Public           | Public           |   4503890|  248342.44|

``` r
lcond_chronic_pct <- svyby(~psa_1yr, by = ~lcond_chronic_cat, svymean, na.rm = TRUE, design = des)
lcond_chronic_pct %>% knitr::kable()
```

|     | lcond\_chronic\_cat |   psa\_1yr|         se|
|-----|:--------------------|----------:|----------:|
| No  | No                  |  0.7119669|  0.1302567|
| Yes | Yes                 |  0.5721954|  0.0195857|

``` r
lcond_chronic_tot <- svyby(~psa_1yr, by = ~lcond_chronic_cat, svytotal, na.rm = TRUE, design = des)
lcond_chronic_tot %>% knitr::kable()
```

|     | lcond\_chronic\_cat |  psa\_1yr|         se|
|-----|:--------------------|---------:|----------:|
| No  | No                  |     75445|   29041.88|
| Yes | Yes                 |   4118514|  237994.34|

``` r
race_pct <- svyby(~psa_1yr, by = ~race_cat, svymean, na.rm = TRUE, design = des)
race_pct %>% knitr::kable()
```

|       | race\_cat |   psa\_1yr|         se|
|-------|:----------|----------:|----------:|
| AN/AI | AN/AI     |  0.5875108|  0.0932342|
| Asian | Asian     |  0.5343534|  0.0513015|
| Black | Black     |  0.5985427|  0.0298753|
| White | White     |  0.6271250|  0.0122030|

``` r
race_tot <- svyby(~psa_1yr, by = ~race_cat, svytotal, na.rm = TRUE, design = des)
race_tot %>% knitr::kable()
```

|       | race\_cat |  psa\_1yr|         se|
|-------|:----------|---------:|----------:|
| AN/AI | AN/AI     |    208591|   45651.27|
| Asian | Asian     |    495282|   72313.37|
| Black | Black     |   1875057|  132037.16|
| White | White     |  17110058|  485446.76|

``` r
eth_pct <- svyby(~psa_1yr, by = ~eth_cat, svymean, na.rm = TRUE, design = des)
eth_pct %>% knitr::kable()
```

|                    | eth\_cat           |   psa\_1yr|         se|
|--------------------|:-------------------|----------:|----------:|
| Hispanic           | Hispanic           |  0.6085490|  0.0313007|
| Non-Hispanic AN/AI | Non-Hispanic AN/AI |  0.6539571|  0.0988646|
| Non-Hispanic Asian | Non-Hispanic Asian |  0.5143447|  0.0540981|
| Non-Hispanic Black | Non-Hispanic Black |  0.5909754|  0.0306581|
| Non-Hispanic White | Non-Hispanic White |  0.6295350|  0.0129043|

``` r
eth_tot <- svyby(~psa_1yr, by = ~eth_cat, svytotal, na.rm = TRUE, design = des)
eth_tot %>% knitr::kable()
```

|                    | eth\_cat           |  psa\_1yr|         se|
|--------------------|:-------------------|---------:|----------:|
| Hispanic           | Hispanic           |   1557989|  141501.23|
| Non-Hispanic AN/AI | Non-Hispanic AN/AI |    158255|   40721.94|
| Non-Hispanic Asian | Non-Hispanic Asian |    448489|   71509.03|
| Non-Hispanic Black | Non-Hispanic Black |   1804330|  126916.07|
| Non-Hispanic White | Non-Hispanic White |  15719925|  464708.80|

### Statified design

``` r
edu_pct_strat <- svyby(~psa_1yr, by = ~age_cat+educ_cat, svymean, na.rm = TRUE, design = des)
edu_pct_strat %>% knitr::kable()
```

|                             | age\_cat | educ\_cat             |   psa\_1yr|         se|
|-----------------------------|:---------|:----------------------|----------:|----------:|
| 25–39.College graduate      | 25–39    | College graduate      |  0.0000000|  0.0000000|
| 40–49.College graduate      | 40–49    | College graduate      |  0.5520173|  0.0488491|
| 50–64.College graduate      | 50–64    | College graduate      |  0.6223180|  0.0244773|
| 65+.College graduate        | 65+      | College graduate      |  0.6897178|  0.0210606|
| 25–39.High school           | 25–39    | High school           |  0.0000000|  0.0000000|
| 40–49.High school           | 40–49    | High school           |  0.5138127|  0.0894525|
| 50–64.High school           | 50–64    | High school           |  0.5965755|  0.0364042|
| 65+.High school             | 65+      | High school           |  0.6734629|  0.0241167|
| 25–39.Less than high school | 25–39    | Less than high school |  0.0000000|  0.0000000|
| 40–49.Less than high school | 40–49    | Less than high school |  0.6498828|  0.1121183|
| 50–64.Less than high school | 50–64    | Less than high school |  0.5998807|  0.0587750|
| 65+.Less than high school   | 65+      | Less than high school |  0.5403757|  0.0355496|
| 25–39.Some college          | 25–39    | Some college          |  0.0000000|  0.0000000|
| 40–49.Some college          | 40–49    | Some college          |  0.4110205|  0.0651628|
| 50–64.Some college          | 50–64    | Some college          |  0.6493104|  0.0286198|
| 65+.Some college            | 65+      | Some college          |  0.6086206|  0.0297379|

``` r
edu_tot_strat <- svyby(~psa_1yr, by = ~age_cat+educ_cat, svytotal, na.rm = TRUE, design = des)
edu_tot_strat %>% knitr::kable()
```

|                             | age\_cat | educ\_cat             |  psa\_1yr|         se|
|-----------------------------|:---------|:----------------------|---------:|----------:|
| 25–39.College graduate      | 25–39    | College graduate      |         0|       0.00|
| 40–49.College graduate      | 40–49    | College graduate      |    996951|  114762.87|
| 50–64.College graduate      | 50–64    | College graduate      |   3686023|  241292.38|
| 65+.College graduate        | 65+      | College graduate      |   3645958|  234670.19|
| 25–39.High school           | 25–39    | High school           |         0|       0.00|
| 40–49.High school           | 40–49    | High school           |    342134|   76928.30|
| 50–64.High school           | 50–64    | High school           |   1805340|  171659.88|
| 65+.High school             | 65+      | High school           |   2262028|  162375.54|
| 25–39.Less than high school | 25–39    | Less than high school |         0|       0.00|
| 40–49.Less than high school | 40–49    | Less than high school |    201851|   65914.08|
| 50–64.Less than high school | 50–64    | Less than high school |    652759|   94403.91|
| 65+.Less than high school   | 65+      | Less than high school |    874302|   86303.01|
| 25–39.Some college          | 25–39    | Some college          |         0|       0.00|
| 40–49.Some college          | 40–49    | Some college          |    359047|   76990.23|
| 50–64.Some college          | 50–64    | Some college          |   2638460|  198048.00|
| 65+.Some college            | 65+      | Some college          |   2206307|  169199.61|

``` r
finc_pct_strat <- svyby(~psa_1yr, by = ~age_cat+finc_cat, svymean, na.rm = TRUE, design = des)
finc_pct_strat  %>% knitr::kable()
```

|                                    | age\_cat | finc\_cat                    |   psa\_1yr|         se|
|------------------------------------|:---------|:-----------------------------|----------:|----------:|
| 25–39.&lt;200%                     | 25–39    | &lt;200%                     |  0.0000000|  0.0000000|
| 40–49.&lt;200%                     | 40–49    | &lt;200%                     |  0.4149264|  0.0792984|
| 50–64.&lt;200%                     | 50–64    | &lt;200%                     |  0.5960212|  0.0348593|
| 65+.&lt;200%                       | 65+      | &lt;200%                     |  0.5905947|  0.0323649|
| 25–39.&gt;=200%, no further detail | 25–39    | &gt;=200%, no further detail |  0.0000000|  0.0000000|
| 40–49.&gt;=200%, no further detail | 40–49    | &gt;=200%, no further detail |  0.5381050|  0.2620185|
| 50–64.&gt;=200%, no further detail | 50–64    | &gt;=200%, no further detail |  0.5469473|  0.1149468|
| 65+.&gt;=200%, no further detail   | 65+      | &gt;=200%, no further detail |  0.5495356|  0.0672903|
| 25–39.&gt;=500%                    | 25–39    | &gt;=500%                    |  0.0000000|  0.0000000|
| 40–49.&gt;=500%                    | 40–49    | &gt;=500%                    |  0.5672807|  0.0511483|
| 50–64.&gt;=500%                    | 50–64    | &gt;=500%                    |  0.6457134|  0.0227132|
| 65+.&gt;=500%                      | 65+      | &gt;=500%                    |  0.7339579|  0.0217020|
| 25–39.200–299%                     | 25–39    | 200–299%                     |  0.0000000|  0.0000000|
| 40–49.200–299%                     | 40–49    | 200–299%                     |  0.6535867|  0.0994868|
| 50–64.200–299%                     | 50–64    | 200–299%                     |  0.6616265|  0.0584957|
| 65+.200–299%                       | 65+      | 200–299%                     |  0.5825825|  0.0344583|
| 25–39.300–399%                     | 25–39    | 300–399%                     |  0.0000000|  0.0000000|
| 40–49.300–399%                     | 40–49    | 300–399%                     |  0.4269506|  0.0833838|
| 50–64.300–399%                     | 50–64    | 300–399%                     |  0.4918107|  0.0535872|
| 65+.300–399%                       | 65+      | 300–399%                     |  0.6585750|  0.0373922|
| 25–39.400–499%                     | 25–39    | 400–499%                     |  0.0000000|  0.0000000|
| 40–49.400–499%                     | 40–49    | 400–499%                     |  0.3338957|  0.0943717|
| 50–64.400–499%                     | 50–64    | 400–499%                     |  0.6143752|  0.0482454|
| 65+.400–499%                       | 65+      | 400–499%                     |  0.6008081|  0.0348079|
| 25–39.Unknown                      | 25–39    | Unknown                      |  0.0000000|  0.0000000|
| 40–49.Unknown                      | 40–49    | Unknown                      |  0.0000000|  0.0000000|
| 50–64.Unknown                      | 50–64    | Unknown                      |  1.0000000|  0.0000000|
| 65+.Unknown                        | 65+      | Unknown                      |  0.0000000|  0.0000000|

``` r
finc_tot_strat <- svyby(~psa_1yr, by = ~age_cat+finc_cat, svytotal, na.rm = TRUE, design = des)
finc_tot_strat %>% knitr::kable()
```

|                                    | age\_cat | finc\_cat                    |  psa\_1yr|         se|
|------------------------------------|:---------|:-----------------------------|---------:|----------:|
| 25–39.&lt;200%                     | 25–39    | &lt;200%                     |         0|       0.00|
| 40–49.&lt;200%                     | 40–49    | &lt;200%                     |    178776|   36918.10|
| 50–64.&lt;200%                     | 50–64    | &lt;200%                     |   1416662|  142647.77|
| 65+.&lt;200%                       | 65+      | &lt;200%                     |   1386734|  117683.25|
| 25–39.&gt;=200%, no further detail | 25–39    | &gt;=200%, no further detail |         0|       0.00|
| 40–49.&gt;=200%, no further detail | 40–49    | &gt;=200%, no further detail |     33786|   24523.32|
| 50–64.&gt;=200%, no further detail | 50–64    | &gt;=200%, no further detail |    126679|   43395.04|
| 65+.&gt;=200%, no further detail   | 65+      | &gt;=200%, no further detail |    289414|   58723.40|
| 25–39.&gt;=500%                    | 25–39    | &gt;=500%                    |         0|       0.00|
| 40–49.&gt;=500%                    | 40–49    | &gt;=500%                    |    843417|  105367.01|
| 50–64.&gt;=500%                    | 50–64    | &gt;=500%                    |   3847098|  245117.85|
| 65+.&gt;=500%                      | 65+      | &gt;=500%                    |   2961650|  189848.55|
| 25–39.200–299%                     | 25–39    | 200–299%                     |         0|       0.00|
| 40–49.200–299%                     | 40–49    | 200–299%                     |    228073|   71027.17|
| 50–64.200–299%                     | 50–64    | 200–299%                     |    870674|  117037.00|
| 65+.200–299%                       | 65+      | 200–299%                     |   1117773|   97369.03|
| 25–39.300–399%                     | 25–39    | 300–399%                     |         0|       0.00|
| 40–49.300–399%                     | 40–49    | 300–399%                     |    230868|   61513.46|
| 50–64.300–399%                     | 50–64    | 300–399%                     |    603498|   79556.03|
| 65+.300–399%                       | 65+      | 300–399%                     |   1055211|  100610.70|
| 25–39.400–499%                     | 25–39    | 400–499%                     |         0|       0.00|
| 40–49.400–499%                     | 40–49    | 400–499%                     |    147456|   45572.17|
| 50–64.400–499%                     | 50–64    | 400–499%                     |   1019641|  139751.49|
| 65+.400–499%                       | 65+      | 400–499%                     |   1389031|  128000.07|
| 25–39.Unknown                      | 25–39    | Unknown                      |         0|       0.00|
| 40–49.Unknown                      | 40–49    | Unknown                      |         0|       0.00|
| 50–64.Unknown                      | 50–64    | Unknown                      |     11316|   11316.00|
| 65+.Unknown                        | 65+      | Unknown                      |         0|       0.00|

``` r
ausualp_pct_strat <- svyby(~psa_1yr, by = ~age_cat+ausualpl_cat, svymean, na.rm = TRUE, design = des)
ausualp_pct_strat %>% knitr::kable()
```

|             | age\_cat | ausualpl\_cat |   psa\_1yr|         se|
|-------------|:---------|:--------------|----------:|----------:|
| 25–39.No    | 25–39    | No            |  0.0000000|  0.0000000|
| 40–49.No    | 40–49    | No            |  0.3740256|  0.0941044|
| 50–64.No    | 50–64    | No            |  0.3371015|  0.0706320|
| 65+.No      | 65+      | No            |  0.4646195|  0.0955717|
| 25–39.Other | 25–39    | Other         |  0.0000000|  0.0000000|
| 40–49.Other | 40–49    | Other         |  0.0000000|  0.0000000|
| 50–64.Other | 50–64    | Other         |  0.0000000|  0.0000000|
| 65+.Other   | 65+      | Other         |  1.0000000|  0.0000000|
| 25–39.Yes   | 25–39    | Yes           |  0.0000000|  0.0000000|
| 40–49.Yes   | 40–49    | Yes           |  0.5343839|  0.0354579|
| 50–64.Yes   | 50–64    | Yes           |  0.6399670|  0.0168938|
| 65+.Yes     | 65+      | Yes           |  0.6496142|  0.0137404|

``` r
ausualp_tot_strat <- svyby(~psa_1yr, by = ~age_cat+ausualpl_cat, svytotal, na.rm = TRUE, design = des)
ausualp_tot_strat %>% knitr::kable()
```

|             | age\_cat | ausualpl\_cat |  psa\_1yr|         se|
|-------------|:---------|:--------------|---------:|----------:|
| 25–39.No    | 25–39    | No            |         0|       0.00|
| 40–49.No    | 40–49    | No            |    131571|   39873.99|
| 50–64.No    | 50–64    | No            |    270649|   74409.18|
| 65+.No      | 65+      | No            |    113100|   35038.19|
| 25–39.Other | 25–39    | Other         |         0|       0.00|
| 40–49.Other | 40–49    | Other         |         0|       0.00|
| 50–64.Other | 50–64    | Other         |         0|       0.00|
| 65+.Other   | 65+      | Other         |       680|     680.00|
| 25–39.Yes   | 25–39    | Yes           |         0|       0.00|
| 40–49.Yes   | 40–49    | Yes           |   1768412|  160387.93|
| 50–64.Yes   | 50–64    | Yes           |   8523249|  337233.48|
| 65+.Yes     | 65+      | Yes           |   8881327|  346388.88|

``` r
cover_pct_strat <- svyby(~psa_1yr, by = ~age_cat+cover_cat, svymean, na.rm = TRUE, design = des)
cover_pct_strat %>% knitr::kable()
```

|                        | age\_cat | cover\_cat       |   psa\_1yr|         se|
|------------------------|:---------|:-----------------|----------:|----------:|
| 25–39.None             | 25–39    | None             |  0.0000000|  0.0000000|
| 40–49.None             | 40–49    | None             |  0.3692583|  0.1046581|
| 50–64.None             | 50–64    | None             |  0.4261227|  0.0665066|
| 65+.None               | 65+      | None             |  0.6039433|  0.2187940|
| 25–39.Private/Military | 25–39    | Private/Military |  0.0000000|  0.0000000|
| 40–49.Private/Military | 40–49    | Private/Military |  0.5363333|  0.0366754|
| 50–64.Private/Military | 50–64    | Private/Military |  0.6332304|  0.0173037|
| 65+.Private/Military   | 65+      | Private/Military |  0.6650912|  0.0178997|
| 25–39.Public           | 25–39    | Public           |  0.0000000|  0.0000000|
| 40–49.Public           | 40–49    | Public           |  0.4259465|  0.1087056|
| 50–64.Public           | 50–64    | Public           |  0.6378134|  0.0527096|
| 65+.Public             | 65+      | Public           |  0.6233890|  0.0208024|

``` r
cover_tot_strat <- svyby(~psa_1yr, by = ~age_cat+cover_cat, svytotal, na.rm = TRUE, design = des)
cover_tot_strat %>% knitr::kable()
```

|                        | age\_cat | cover\_cat       |  psa\_1yr|          se|
|------------------------|:---------|:-----------------|---------:|-----------:|
| 25–39.None             | 25–39    | None             |         0|       0.000|
| 40–49.None             | 40–49    | None             |     86515|   30293.816|
| 50–64.None             | 50–64    | None             |    333092|   69227.786|
| 65+.None               | 65+      | None             |     14703|    9902.959|
| 25–39.Private/Military | 25–39    | Private/Military |         0|       0.000|
| 40–49.Private/Military | 40–49    | Private/Military |   1691324|  160530.961|
| 50–64.Private/Military | 50–64    | Private/Military |   7828247|  334291.467|
| 65+.Private/Military   | 65+      | Private/Military |   5186604|  258328.577|
| 25–39.Public           | 25–39    | Public           |         0|       0.000|
| 40–49.Public           | 40–49    | Public           |    109021|   30839.426|
| 50–64.Public           | 50–64    | Public           |    601069|   95258.565|
| 65+.Public             | 65+      | Public           |   3793800|  222486.160|

``` r
lcond_chronic_pct_strat <- svyby(~psa_1yr, by = ~age_cat+lcond_chronic_cat, svymean, na.rm = TRUE, design = des)
lcond_chronic_pct_strat %>% knitr::kable()
```

|           | age\_cat | lcond\_chronic\_cat |   psa\_1yr|         se|
|-----------|:---------|:--------------------|----------:|----------:|
| 25–39.No  | 25–39    | No                  |  0.0000000|  0.0000000|
| 40–49.No  | 40–49    | No                  |  1.0000000|  0.0000000|
| 50–64.No  | 50–64    | No                  |  0.6556569|  0.2014876|
| 65+.No    | 65+      | No                  |  0.6724408|  0.1931692|
| 25–39.Yes | 25–39    | Yes                 |  0.0000000|  0.0000000|
| 40–49.Yes | 40–49    | Yes                 |  0.3701198|  0.0884249|
| 50–64.Yes | 50–64    | Yes                 |  0.6357627|  0.0315984|
| 65+.Yes   | 65+      | Yes                 |  0.5479066|  0.0243877|

``` r
lcond_chronic_tot_strat <- svyby(~psa_1yr, by = ~age_cat+lcond_chronic_cat, svytotal, na.rm = TRUE, design = des)
lcond_chronic_tot_strat %>% knitr::kable()
```

|           | age\_cat | lcond\_chronic\_cat |  psa\_1yr|         se|
|-----------|:---------|:--------------------|---------:|----------:|
| 25–39.No  | 25–39    | No                  |         0|       0.00|
| 40–49.No  | 40–49    | No                  |     14377|   14377.00|
| 50–64.No  | 50–64    | No                  |     20347|   11000.56|
| 65+.No    | 65+      | No                  |     40721|   22715.30|
| 25–39.Yes | 25–39    | Yes                 |         0|       0.00|
| 40–49.Yes | 40–49    | Yes                 |    153029|   44018.58|
| 50–64.Yes | 50–64    | Yes                 |   1797029|  159342.50|
| 65+.Yes   | 65+      | Yes                 |   2168456|  155021.27|

``` r
race_pct_strat <- svyby(~psa_1yr, by = ~age_cat+race_cat, svymean, na.rm = TRUE, design = des)
race_pct_strat %>% knitr::kable()
```

|             | age\_cat | race\_cat |   psa\_1yr|         se|
|-------------|:---------|:----------|----------:|----------:|
| 25–39.AN/AI | 25–39    | AN/AI     |  0.0000000|  0.0000000|
| 40–49.AN/AI | 40–49    | AN/AI     |  0.6951579|  0.1709104|
| 50–64.AN/AI | 50–64    | AN/AI     |  0.5647422|  0.1412485|
| 65+.AN/AI   | 65+      | AN/AI     |  0.5799006|  0.1410433|
| 25–39.Asian | 25–39    | Asian     |  0.0000000|  0.0000000|
| 40–49.Asian | 40–49    | Asian     |  0.3489210|  0.1170097|
| 50–64.Asian | 50–64    | Asian     |  0.6663906|  0.0830669|
| 65+.Asian   | 65+      | Asian     |  0.4967146|  0.0736204|
| 25–39.Black | 25–39    | Black     |  0.0000000|  0.0000000|
| 40–49.Black | 40–49    | Black     |  0.4779565|  0.0719063|
| 50–64.Black | 50–64    | Black     |  0.6095230|  0.0418887|
| 65+.Black   | 65+      | Black     |  0.6494906|  0.0369761|
| 25–39.White | 25–39    | White     |  0.0000000|  0.0000000|
| 40–49.White | 40–49    | White     |  0.5344666|  0.0381233|
| 50–64.White | 50–64    | White     |  0.6241212|  0.0180799|
| 65+.White   | 65+      | White     |  0.6514521|  0.0143436|

``` r
race_tot_strat <- svyby(~psa_1yr, by = ~age_cat+race_cat, svytotal, na.rm = TRUE, design = des)
race_tot_strat %>% knitr::kable()
```

|             | age\_cat | race\_cat |  psa\_1yr|         se|
|-------------|:---------|:----------|---------:|----------:|
| 25–39.AN/AI | 25–39    | AN/AI     |         0|       0.00|
| 40–49.AN/AI | 40–49    | AN/AI     |     33939|   15489.78|
| 50–64.AN/AI | 50–64    | AN/AI     |    108980|   38821.07|
| 65+.AN/AI   | 65+      | AN/AI     |     65672|   20659.05|
| 25–39.Asian | 25–39    | Asian     |         0|       0.00|
| 40–49.Asian | 40–49    | Asian     |     61796|   21380.35|
| 50–64.Asian | 50–64    | Asian     |    239816|   50677.14|
| 65+.Asian   | 65+      | Asian     |    193670|   45926.85|
| 25–39.Black | 25–39    | Black     |         0|       0.00|
| 40–49.Black | 40–49    | Black     |    268082|   52898.59|
| 50–64.Black | 50–64    | Black     |    966765|  102587.45|
| 65+.Black   | 65+      | Black     |    640210|   61275.50|
| 25–39.White | 25–39    | White     |         0|       0.00|
| 40–49.White | 40–49    | White     |   1536166|  154349.23|
| 50–64.White | 50–64    | White     |   7478337|  310939.65|
| 65+.White   | 65+      | White     |   8095555|  330006.59|

``` r
eth_pct_strat <- svyby(~psa_1yr, by = ~age_cat+eth_cat, svymean, na.rm = TRUE, design = des)
eth_pct_strat %>% knitr::kable()
```

|                          | age\_cat | eth\_cat           |   psa\_1yr|         se|
|--------------------------|:---------|:-------------------|----------:|----------:|
| 25–39.Hispanic           | 25–39    | Hispanic           |  0.0000000|  0.0000000|
| 40–49.Hispanic           | 40–49    | Hispanic           |  0.5674564|  0.0851067|
| 50–64.Hispanic           | 50–64    | Hispanic           |  0.6086124|  0.0463831|
| 65+.Hispanic             | 65+      | Hispanic           |  0.6272817|  0.0451628|
| 25–39.Non-Hispanic AN/AI | 25–39    | Non-Hispanic AN/AI |  0.0000000|  0.0000000|
| 40–49.Non-Hispanic AN/AI | 40–49    | Non-Hispanic AN/AI |  0.6399516|  0.2052159|
| 50–64.Non-Hispanic AN/AI | 50–64    | Non-Hispanic AN/AI |  0.6942218|  0.1483896|
| 65+.Non-Hispanic AN/AI   | 65+      | Non-Hispanic AN/AI |  0.6099224|  0.1657466|
| 25–39.Non-Hispanic Asian | 25–39    | Non-Hispanic Asian |  0.0000000|  0.0000000|
| 40–49.Non-Hispanic Asian | 40–49    | Non-Hispanic Asian |  0.2986095|  0.1145338|
| 50–64.Non-Hispanic Asian | 50–64    | Non-Hispanic Asian |  0.6490843|  0.0878398|
| 65+.Non-Hispanic Asian   | 65+      | Non-Hispanic Asian |  0.4868300|  0.0773751|
| 25–39.Non-Hispanic Black | 25–39    | Non-Hispanic Black |  0.0000000|  0.0000000|
| 40–49.Non-Hispanic Black | 40–49    | Non-Hispanic Black |  0.4815115|  0.0725764|
| 50–64.Non-Hispanic Black | 50–64    | Non-Hispanic Black |  0.5956569|  0.0431719|
| 65+.Non-Hispanic Black   | 65+      | Non-Hispanic Black |  0.6465787|  0.0372952|
| 25–39.Non-Hispanic White | 25–39    | Non-Hispanic White |  0.0000000|  0.0000000|
| 40–49.Non-Hispanic White | 40–49    | Non-Hispanic White |  0.5318357|  0.0418596|
| 50–64.Non-Hispanic White | 50–64    | Non-Hispanic White |  0.6265678|  0.0194826|
| 65+.Non-Hispanic White   | 65+      | Non-Hispanic White |  0.6532711|  0.0148749|

``` r
eth_tot_strat <- svyby(~psa_1yr, by = ~age_cat+eth_cat, svytotal, na.rm = TRUE, design = des)
eth_tot_strat %>% knitr::kable()
```

|                          | age\_cat | eth\_cat           |  psa\_1yr|         se|
|--------------------------|:---------|:-------------------|---------:|----------:|
| 25–39.Hispanic           | 25–39    | Hispanic           |         0|       0.00|
| 40–49.Hispanic           | 40–49    | Hispanic           |    238856|   65297.93|
| 50–64.Hispanic           | 50–64    | Hispanic           |    742515|   92069.59|
| 65+.Hispanic             | 65+      | Hispanic           |    576618|   77994.83|
| 25–39.Non-Hispanic AN/AI | 25–39    | Non-Hispanic AN/AI |         0|       0.00|
| 40–49.Non-Hispanic AN/AI | 40–49    | Non-Hispanic AN/AI |     25376|   14232.16|
| 50–64.Non-Hispanic AN/AI | 50–64    | Non-Hispanic AN/AI |     77950|   34401.06|
| 65+.Non-Hispanic AN/AI   | 65+      | Non-Hispanic AN/AI |     54929|   19027.75|
| 25–39.Non-Hispanic Asian | 25–39    | Non-Hispanic Asian |         0|       0.00|
| 40–49.Non-Hispanic Asian | 40–49    | Non-Hispanic Asian |     49092|   19233.92|
| 50–64.Non-Hispanic Asian | 50–64    | Non-Hispanic Asian |    219765|   51032.24|
| 65+.Non-Hispanic Asian   | 65+      | Non-Hispanic Asian |    179632|   44840.89|
| 25–39.Non-Hispanic Black | 25–39    | Non-Hispanic Black |         0|       0.00|
| 40–49.Non-Hispanic Black | 40–49    | Non-Hispanic Black |    268082|   52898.59|
| 50–64.Non-Hispanic Black | 50–64    | Non-Hispanic Black |    910804|   98196.67|
| 65+.Non-Hispanic Black   | 65+      | Non-Hispanic Black |    625444|   59919.22|
| 25–39.Non-Hispanic White | 25–39    | Non-Hispanic White |         0|       0.00|
| 40–49.Non-Hispanic White | 40–49    | Non-Hispanic White |   1318577|  143280.33|
| 50–64.Non-Hispanic White | 50–64    | Non-Hispanic White |   6842864|  299280.32|
| 65+.Non-Hispanic White   | 65+      | Non-Hispanic White |   7558484|  321247.76|
