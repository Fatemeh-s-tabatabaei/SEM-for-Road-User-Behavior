# SEM for Road User Behavior

Structural equation modelling (SEM) of self-reported road-safety behaviour among **cyclists, pedestrians, drivers and motorcyclists**. The project extends the Theory of Planned Behaviour (TPB) with habit, acceptability of violations, enforcement, and perceived risk, and tests how these factors relate to **intention** (next month) and **risky behaviour** (past month).

Each road-user group answered its own version of the questionnaire, so every analysis is run **separately per group**.

> **Status:** work in progress. The measurement model (CFA) is largely settled; the structural model (`SEM.R`) is still being refined. See [Current findings](#current-findings).

---

## Contents

1. [Research questions and hypotheses](#research-questions-and-hypotheses)
2. [Constructs](#constructs)
3. [Pipeline overview](#pipeline-overview)
4. [Repository structure](#repository-structure)
5. [Requirements](#requirements)
6. [How to run](#how-to-run)
7. [Configuration](#configuration)
8. [Outputs](#outputs)
9. [Methodological notes](#methodological-notes)
10. [Current findings](#current-findings)
11. [Data availability, citation, license](#data-availability-citation-license)

---

## Research questions and hypotheses

The structural model tests the following paths (expected sign in brackets). They are defined in `Model Test/SEM.R` (`hypotheses`) and can be edited there.

| ID | Path | Meaning | Expected |
|----|------|---------|----------|
| H1 | INT ~ CORE | Risk-favourable attitude, control and habit weaken safe intention | − |
| H2 | INT ~ SN | Friends' risky behaviour weakens intention | − |
| H3 | PA ~ SA | City-level acceptance raises personal acceptance | + |
| H4 | INT ~ PA | Personal acceptance weakens intention | − |
| H5 | SA → PA → INT | Indirect effect (H3 × H4) | − |
| H6 | INT ~ MR | Willingness to act against others' violations strengthens intention | + |
| H7 | INT ~ PENF | Perceived police enforcement strengthens intention | + |
| H8 | RBF ~ CORE | Risk-favourable attitude, control and habit raise risky behaviour | + |
| H9 | RBF ~ HAB | Habit effect beyond CORE | + |
| H10 | RBF ~ SN | Norms and risky behaviour | + |
| H11 | RBF ~ PA | Personal acceptance raises risky behaviour | + |
| H12 | RBF ~ PENF | Perceived enforcement lowers risky behaviour | − |
| H13 | NBV ~ PA | Personal acceptance and nuisance behaviour (drivers only) | + |
| H14 | INT ~ PRISK | Perceived risk strengthens intention | + |

H7, H12 and H14 are **not tested for pedestrians**, because the enforcement and cause items describe cycling and driving rather than walking.

## Constructs

| Code | Construct | Item prefix |
|------|-----------|-------------|
| ATT | Attitude | `att` |
| SN | Subjective (social) norm | `sn` |
| PBC | Perceived behavioural control | `pbc` |
| HAB | Habit | `hab` |
| INT | Intention (next month) | `int` |
| SA | Social acceptability ("people in your city") | `sa` |
| PA | Personal acceptability | `pa` |
| RBF | Risky behaviour frequency (past month) | `rbf` |
| NBV | Nuisance behaviour (e.g. parking violations) | `nbv` |
| MR | Willingness to act against others' violations | `mr` |
| PENF | Perceived enforcement | `penf` |
| CAUSE / PRISK | Perceived causes of crashes; PRISK = the six human-factor cause items | `cause` |
| INFRA | Infrastructure | `infra` |
| AE | Binary items, treated as single items rather than a scale | `aenf` |

Items are tied to specific behaviours (alertness, speed, phone use, helmet/seatbelt, parking, etc.). This mapping lives in `behaviour_map.csv`.

## Pipeline overview

```
 raw survey data
      │
      ▼
 1. Data_Preparation.py    cleaning, duplicates, straight-lining flags, covariates
 2. Item_Preparation.py    merge sparse categories, reverse-coded items
      │                    → ItemPrep/<Group>_items_ready.csv
      ▼
 3. Correlations.py        item-rest screening        → Screening/item_rest*.csv
    Straightlinecheck.py   how strict is the straight-lining filter?
    Pruningcandidates.py   rule-based item-drop proposals
      │                    → Screening/pruning_candidates.csv, drop_items_PROPOSED.csv
      ▼   (approve drops by copying rows to Model Test/drop_items.csv)
 4. CFA.R                  measurement model per scale, block, and full set
    Discriminant.R         are the factors statistically distinct?
    Alternatives.R         competing measurement structures (M0–M6)
    Behavior_Factor_CFA.R  behaviour-specific factors
      ▼
 5. SEM.R                  structural model, hypotheses H1–H14
```

## Repository structure

```
.
├── Data_Preparation.py          Step 1: respondent-level cleaning and flags
├── Item_Preparation.py          Step 2: item recoding and category merging
├── ItemPrep/                    <Group>_items_ready.csv (inputs to all R scripts)
├── Screening/                   Item-rest screening and pruning output
│   ├── Correlations.py
│   ├── Pruningcandidates.py
│   └── ...
├── Model Test/
│   ├── Common.R                 Shared settings and helpers (sourced by all R scripts)
│   ├── CFA.R
│   ├── Discriminant.R
│   ├── Alternatives.R
│   ├── Behavior_Factor_CFA.R
│   ├── SEM.R
│   ├── Straightlinecheck.py
│   ├── behaviour_map.csv        Item → behaviour mapping
│   └── drop_items.csv           Approved item drops (group,item)
├── cfa_results*/                CFA output
├── discriminant_results/        Discriminant-validity output
├── alternatives_results/        Model-comparison output
└── sem_results/                 Structural-model output
```

> **TODO:** adjust the tree above to match the actual folder layout (some scripts may live in the repo root).

## Requirements

- **R** with `lavaan` (≥ 0.6-13 if you enable the ESEM option in `Alternatives.R`) and `dplyr`
- **Python 3** with `pandas` and `numpy`

> **TODO:** pin versions (e.g. with `renv` / `requirements.txt`).

## How to run

Run all commands from the repository root, since scripts use relative paths.

**1. Prepare the data**

```bash
python Data_Preparation.py
python Item_Preparation.py
```

This writes `ItemPrep/<Group>_items_ready.csv` for Cyclist, Pedestrian, Driver and Motorcyclist.

**2. Screen items**

```bash
python Correlations.py                 # item-rest correlations (all / clean / adults samples)
python "Model Test/Straightlinecheck.py"   # optional: pick a threshold for sl_max_blocks
python "Model Test/Pruningcandidates.py"   # propose drops; does NOT apply them
```

Review `Screening/pruning_candidates.csv`. Every proposal needs a content check: an item that is statistically weak but essential to the construct's meaning should stay. Approve drops by copying rows into `Model Test/drop_items.csv` (columns: `group,item`).

**3. Fit and evaluate the measurement model**

```bash
Rscript "Model Test/CFA.R"
Rscript "Model Test/Discriminant.R"
Rscript "Model Test/Alternatives.R"
Rscript "Model Test/Behavior_Factor_CFA.R"
```

**4. Fit the structural model**

```bash
Rscript "Model Test/SEM.R"
```

Any setting can be overridden from the command line:

```bash
Rscript -e 'measurement <- "as_is"; source("Model Test/SEM.R")'
Rscript -e 'diagnose <- TRUE; source("Model Test/SEM.R")'
Rscript -e 'sample_def <- "clean_adults"; source("Model Test/SEM.R")'
```

## Configuration

Shared settings live in `Model Test/Common.R` and use "set only if not already set", so a script (or the command line) can override them by assigning the variable first.

| Setting | Default | Meaning |
|---------|---------|---------|
| `sample_def` | `"clean"` | `all`, `no_dups`, `clean` (no dups, no straight-liners), `adults`, `clean_adults` |
| `sl_max_blocks` | `NA` | Also drop respondents who straight-lined this many item blocks or more (sensitivity runs) |
| `include_reversed` | `TRUE` | Use reverse-coded `_r` items as indicators |
| `drop_file` | `Model Test/drop_items.csv` | Approved item drops |
| `missing_method` | `"listwise"` | Missing-data handling |
| `min_items` | `3` | Minimum items per factor |

`SEM.R` adds its own options:

| Setting | Default | Meaning |
|---------|---------|---------|
| `measurement` | `"second_order"` | `second_order`: ATT, PBC, HAB are facets of a higher-order factor CORE. `as_is`: all factors separate |
| `behaviour_res` | `TRUE` | Residual correlations between belief items about the *same behaviour* (from `behaviour_map.csv`) |
| `parallel_sa_pa` | `TRUE` | Residual correlations between parallel SA/PA item pairs |
| `saturate` | `TRUE` | Add control paths (`C_<outcome>_<predictor>`) so no relationship is forced to zero just because no hypothesis names it |
| `lv_bounds` | `FALSE` | Keep latent variances positive (use only for a Heywood case, and report it) |
| `equal_loadings` | Motorcyclist: MR, PENF | Tau-equivalence constraint for short scales with Heywood cases (report it) |
| `diagnose` | `FALSE` | Fit measurement → INT part → RBF part → full model to find which step breaks |

Output folder names encode the settings, e.g. `sem_results/clean_second_order_res_sat_eq/`.

## Outputs

| Folder | Key files |
|--------|-----------|
| `cfa_results*/<Group>/` | `fit.csv`, `loadings.csv`, `reliability.csv` (omega, AVE), `modindices.csv`, HTMT and Fornell–Larcker tables |
| `discriminant_results/<sample>/` | `fit.csv`, `factor_cor_ci.csv`, `chisq_cut_tests.csv` |
| `alternatives_results/<sample>/` | `fit.csv`, `lrt.csv`, `factor_cor.csv`, `esem_loadings.csv` |
| `sem_results/<run>/` | `fit.csv`, `paths.csv` (hypothesis and control paths with CIs), `r2.csv`, `indirect.csv`, `vif.csv` |
| `Screening/` | `item_rest.csv`, `item_rest_review.csv`, `pruning_candidates.csv`, `drop_items_PROPOSED.csv` |

## Methodological notes

- **Estimator.** All models use lavaan with `WLSMV`, treating the 1–5 answers as **ordered categories**. Sparse response categories are merged in `Item_Preparation.py`.
- **Sample.** The default sample drops duplicate respondents and straight-liners. `Straightlinecheck.py` supports a sensitivity analysis, since a block of identical answers can be honest.
- **Item pruning rules** (`Pruningcandidates.py`, set before looking at results):
  - R1: weak item (low loading and low item-rest correlation).
  - R2: near-duplicate pair (large modification index and residual correlation).
  - R3: a factor may not fall below 4 items (3 is the identification floor).
  - R4: binary AE items are not a reflective scale and stay out of the CFA.
- **Discriminant validity** follows Rönkkö & Cho (2022): the CI of the factor correlation, plus chi-square difference tests with the correlation fixed at 0.90 and 1.00.
- **Reliability and validity thresholds.** Omega ≥ .70, AVE ≥ .50, loading ≥ .50, HTMT ≤ .85.
- **Intention vs. behaviour.** INT (next month) and RBF/NBV (past month) are treated as parallel outcomes linked only by residual correlations, because a cross-sectional survey cannot order them.
- **Heywood cases.** Constraints used to resolve them (`equal_loadings`, `lv_bounds`) are choices you impose and should report as such.

## Current findings

- Most scales hold together and are reliable.
- A few items fit poorly: helmet/seatbelt items within risky behaviour (RBF) and one enforcement item (PENF).
- **Attitude, perceived control and habit are too highly correlated to separate** (for example, cyclist PBC–HAB r ≈ .90, ATT–HAB r ≈ .89). The likely cause is the questionnaire design: the same behaviours are asked about in every scale, so answers about one behaviour hang together regardless of construct. This motivates the second-order CORE factor and the behaviour-residual models in `Alternatives.R` and `SEM.R`.

> **TODO:** add a short summary of the structural results once the final model is chosen.

## Data availability, citation, license

- **Data:** TODO (state whether the survey data are public, available on request, or restricted).
- **Citation:** TODO (paper or preprint reference).
- **License:** TODO.
- **Contact:** TODO.