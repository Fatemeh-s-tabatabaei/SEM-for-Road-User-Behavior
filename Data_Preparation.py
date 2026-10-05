# Before running the data cleaning, the raw data was manually reviewed and analysed.
# The renamed structured dataset is saved in SurveyData/renamed_excel.xlsx.

from __future__ import annotations

import argparse
import re
from pathlib import Path
from typing import NamedTuple

import numpy as np
import pandas as pd

# =============================================================================
# 0. CONFIGURATION

MODES = ["Cyclist", "Pedestrian", "Driver", "Motorcyclist"]

SHEETS = {
    "Cyclist": "Cyclist (dlt+edited)",
    "Pedestrian": "pedestrian spss (dlt+edit)",
    "Driver": "Driver SPSS(dlt+edit)",
    "Motorcyclist": "Motor (dlt+edited)",
}

SURVEY_YEAR_IR = 1402      # Iranian calendar year of data collection
LEGAL_LICENSE_AGE = 18     # minimum age for a car or motorcycle license
ACC_EXTREME = 10           # any single accident count >= 10 is flagged

NA_NOT_APPLICABLE = -8
NA_MISSING = -9


def seq(prefix: str, n: int) -> list[str]:
    return [f"{prefix}{i}" for i in range(1, n + 1)]

# define the column names for the blocks of items that are shared across all four forms
BF = ["bf_walk", "bf_bike", "bf_motor", "bf_bus", "bf_taxi", "bf_car", "bf_metro"]
PSAFE = ["psafe_walk", "psafe_bike", "psafe_motor", "psafe_taxi",
         "psafe_metro", "psafe_bus", "psafe_car"]
ACC_PARTS = ["inj_ped", "inj_bike", "inj_motor", "inj_taxi", "inj_bus", "inj_car",
             "fin_ped", "fin_bike", "fin_motor", "fin_taxi", "fin_bus", "fin_car"]
ACC = ["acc_total"] + ACC_PARTS
SHARED_TAIL = seq("aenf", 4) + PSAFE + seq("cause", 8) + seq("penf", 4) + ACC
INT_MR = seq("int", 4) + seq("mr", 3) # 
# Cyclist and motorcyclist forms print "friends use phones" (sn5) between hab2 and hab3
HAB_WITH_SN5 = ["hab1", "hab2", "sn5", "hab3", "hab4", "hab5", "hab6", "hab7"]

# Column names by POSITION in the English sheet. Headers are not used for
# matching because several are wrong: the pedestrian sheet has an extra
# "prprtymotor" header so the last three accident headers are shifted by one,
# and two "Attitude toward enforcement" headers are numbered twice.
LAYOUT = {
    "Cyclist": (["gender", "age", "edu", "job",
                 "purp_shop", "purp_recr", "purp_exer", "purp_work", "purp_school",
                 "bike_personal", "bike_shared", "bike_both", "car_lic_raw"]
                + BF + seq("rbf", 8) + seq("sa", 6) + seq("pa", 6) + seq("sn", 4)
                + seq("att", 6) + seq("pbc", 7) + HAB_WITH_SN5 + INT_MR
                + seq("infra", 2) + SHARED_TAIL),
    "Pedestrian": (["gender", "age", "edu", "job", "car_lic_raw"]
                   + BF + seq("rbf", 8) + seq("sa", 7) + seq("pa", 7) + seq("sn", 5)
                   + seq("att", 6) + seq("pbc", 5) + seq("hab", 5) + INT_MR
                   + seq("infra", 4) + SHARED_TAIL),
    "Driver": (["gender", "age", "edu", "job", "car_lic_raw"]
               + BF + seq("rbf", 6) + seq("nbv", 5) + seq("sa", 9) + seq("pa", 9)
               + seq("sn", 4) + seq("att", 6) + seq("pbc", 7) + seq("hab", 7)
               + INT_MR + SHARED_TAIL),
    "Motorcyclist": (["gender", "age", "edu", "job",
                      "motor_lic_yn", "motor_lic_raw", "car_lic_yn", "car_lic_raw"]
                     + BF + seq("rbf", 9) + seq("sa", 8) + seq("pa", 8) + seq("sn", 4)
                     + seq("att", 6) + seq("pbc", 7) + HAB_WITH_SN5 + INT_MR
                     + SHARED_TAIL),
}

# ---------------------------------------------------------------------------
# Answer coding (FROM PERSIAN CHECK: the Persian coding sheet gives the codes; the English sheet keeps the same numbers)


GENDER = {1: "female", 2: "male"}
EDU = {1: "no formal education", 2: "primary school", 3: "middle school",
       4: "high-school diploma", 5: "bachelor's degree", 6: "master's degree or higher"}
EDU_ORD = {1: 1, 2: 1, 3: 2, 4: 3, 5: 4, 6: 5}   # codes 1-2 merged (very few)

JOB = {
    "Cyclist": {1: "unemployed", 2: "self-employed", 3: "laborer",
                4: "government employee", 5: "private-sector employee",
                6: "university student", 7: "school student", 8: "retired",
                9: "homemaker"},
    "Driver": {1: "unemployed", 2: "government employee", 3: "private-sector employee",
               4: "taxi driver", 5: "bus driver", 6: "freight vehicle driver",
               7: "self-employed", 8: "laborer", 9: "student", 10: "retired",
               11: "homemaker"},
    "Motorcyclist": {1: "unemployed", 2: "motorcycle courier", 3: "self-employed",
                     4: "laborer", 5: "government employee",
                     6: "private-sector employee", 7: "university student",
                     8: "school student", 9: "retired"},
}
JOB["Pedestrian"] = JOB["Cyclist"]
JOB_GROUP = {
    "employed": ["government employee", "private-sector employee"],
    "self_employed_laborer": ["self-employed", "laborer", "motorcycle courier",
                              "taxi driver", "bus driver", "freight vehicle driver"],
    "student": ["university student", "school student", "student"],
    "not_working": ["unemployed", "retired", "homemaker"],
}
PROFESSIONAL_DRIVER_JOBS = [4, 5, 6]           # Driver only

BF_CODING = ("1=every day; 2=4 days a week; 3=1-3 days a week; "
             "4=a few days a month; 5=a few days a year (never)")

# ---------------------------------------------------------------------------
# Item groups: one row per group of items (prefix). Everything the script
# needs to know about a group is here. Row order = order in the figures.
# ---------------------------------------------------------------------------
class Group(NamedTuple):
    label: str     # construct name (codebook, figures)
    scale: str     # answer scale
    factor: bool   # multi-item construct in the measurement model
    shared: bool   # same wording in all four forms: merges decided jointly
    core: bool     # part of the "risky core" used in the direction check
 
 
AGREE = "1=strongly disagree ... 5=strongly agree"
NEVER = "1=never ... 5=always"
ACCEPT = "1=unacceptable ... 5=acceptable"
 
GROUPS = {
    #                 label                          scale                                    factor shared core
    "att":   Group("Attitude",                    AGREE,                                   True,  False, True),
    "sn":    Group("Social norms",                AGREE,                                   True,  False, False),
    "pbc":   Group("Perceived control",           AGREE,                                   True,  False, True),
    "hab":   Group("Habit",                       AGREE,                                   True,  False, True),
    "int":   Group("Intention",                   AGREE,                                   True,  False, False),
    "sa":    Group("Social acceptance",           ACCEPT,                                  True,  False, False),
    "pa":    Group("Personal acceptance",         ACCEPT,                                  True,  False, False),
    "rbf":   Group("Risky behaviour",             NEVER,                                   True,  False, True),
    "nbv":   Group("Parking violations",          NEVER,                                   True,  False, False),
    "infra": Group("Infrastructure safety",       AGREE,                                   True,  False, False),
    "mr":    Group("Moral responsibility",        AGREE,                                   True,  True,  False),
    "cause": Group("Perceived causes",            "1=not important ... 5=very important",  True,  True,  False),
    "penf":  Group("Perceived enforcement",       "1=very unlikely ... 5=very likely",     True,  True,  False),
    "aenf":  Group("Attitude toward enforcement", "1=agree; 0=disagree (source: 1=agree, 2=disagree)",
                                                                                           False, True,  False),
    "psafe": Group("Feeling of safety",           "1=very unsafe ... 5=very safe",         False, True,  False),
}
FACTOR_ROLES = {g.upper() for g, grp in GROUPS.items() if grp.factor}
 
# A factor group with fewer items than this in a mode is used as single
# indicators instead (cyclists have only two infrastructure items).
MIN_FACTOR_ITEMS = 3 

REVERSE = {                      # mode -> {item: reason}
    "Cyclist": {"sn4": "My family complies with traffic laws"},
    "Pedestrian": {
        "sn5": "My family follows traffic rules",
        "sn4": "friends wear light clothing: safe-worded",
        "sa6": "Persian wording is the safe behaviour",
        "pa6": "Persian wording is the safe behaviour",
    },
    "Driver": {"sn4": "My family complies with traffic laws"},
    "Motorcyclist": {"sn4": "My family follows traffic laws"},
}

NOTES_ALL_MODES = {
    "mr3": "angry confrontation; weaker link to mr1/mr2, check loading in the model",
}


# =============================================================================
# Small helpers

def group_of(col: str) -> str | None:
    """Item group of a column: 'sn4' -> 'sn' """
    if col in BF:
        return "bf"
    if col in PSAFE:
        return "psafe"
    m = re.match(r"^([a-z]+)\d+(_r)?$", col) # the rest are matched by prefix + number, optionally with _r for reversed copies
    return m.group(1) if m else None
 
 
def decided(mode: str, action: str) -> list[str]:
    """Items of a mode reverse category order."""
    return [i for i, (a, _) in REVERSE.get(mode, {}).items() if a == action]

 
def opposite_blocks(mode: str) -> list[str]:
    """Blocks that contain an opposite-worded item (from the "reverse" decisions)."""
    return list(dict.fromkeys(group_of(i) for i in REVERSE[mode]))
 
 
def items_of(df: pd.DataFrame, prefix: str) -> list[str]:
    """Columns named prefix + number (rbf1, rbf2, ...), in column order."""
    pat = re.compile(rf"^{prefix}\d+$")
    return [c for c in df.columns if pat.match(c)]
 
 
def scale_items(df: pd.DataFrame, order: list[str] | None = None) -> list[str]:
    """Every 1-5 answer scale (aenf excluded), in column order or in `order`."""
    prefixes = ("rbf", "nbv", "sa", "pa", "sn", "att", "pbc", "hab", "int",
                "mr", "infra", "cause", "penf")
    pat = re.compile(rf"^({'|'.join(prefixes)})\d+$")
    cols = order if order is not None else list(df.columns)
    return [c for c in cols if c in df.columns and (pat.match(c) or c in BF or c in PSAFE)]
 
 
def to_na(s: pd.Series) -> pd.Series:
    """coded -> model: -8 / -9 become NaN."""
    return s.where(~s.isin([NA_NOT_APPLICABLE, NA_MISSING]))
 
 
class Log:
    def __init__(self):
        self.rows = []
 
    def add(self, rid, var, old, new, reason):
        self.rows.append({"resp_id": rid, "variable": var, "old_value": old,
                          "new_value": new, "reason": reason})
 
    def frame(self):
        return pd.DataFrame(self.rows, columns=["resp_id", "variable", "old_value",
                                                "new_value", "reason"])
 

# =============================================================================
# PART A - CLEANING

def load_mode(path: Path, mode: str) -> tuple[pd.DataFrame, pd.DataFrame]:
    """Read one mode from the renamed workbook (values exactly as entered)."""
    df = pd.read_excel(path, sheet_name=mode, dtype=object)
    cmap = pd.read_excel(path, sheet_name="column_map")
    cmap = cmap[cmap["mode"] == mode].sort_values("questionnaire_position")
    missing = set(cmap["variable"]) - set(df.columns)
    if missing:
        raise ValueError(f"{mode}: columns missing from the sheet: {sorted(missing)}")
    return df, cmap


def as_number(v):
    """Return float for numeric entries, None for text ('-', '1, 4', 'no')."""
    if v is None or (isinstance(v, float) and np.isnan(v)):
        return np.nan
    if isinstance(v, (int, float, np.integer, np.floating)):
        return float(v)
    try:
        return float(str(v).strip())
    except ValueError:
        return None


def clean_scales(df: pd.DataFrame, log: Log) -> pd.DataFrame:
    """A2: 1-5 scales. 0 on a feeling-of-safety item = does not use the mode (-8); 
    0 elsewhere = skipped (-9); 1.25 / 3.5 = two boxes ticked (-9);
    text = dash or two answers (-9); 6, 7, 51 = typing errors (-9)."""
    for col in scale_items(df):
        out = []
        for rid, v in zip(df["resp_id"], df[col]):
            x = as_number(v)
            if x is None:
                log.add(rid, col, v, NA_MISSING, "non-numeric entry (dash or two answers)")
                out.append(NA_MISSING)
            elif np.isnan(x):
                out.append(NA_MISSING)
            elif x == 0:
                if col in PSAFE:
                    log.add(rid, col, 0.0, NA_NOT_APPLICABLE,
                            "0 on mode-safety item = does not use the mode")
                    out.append(NA_NOT_APPLICABLE)
                else:
                    log.add(rid, col, 0.0, NA_MISSING,
                            "0 on an item that applies to everyone = skipped")
                    out.append(NA_MISSING)
            elif x != int(x):
                log.add(rid, col, x, NA_MISSING, "fractional value (two adjacent boxes ticked)")
                out.append(NA_MISSING)
            elif not 1 <= x <= 5:
                log.add(rid, col, x, NA_MISSING, "out of 1-5 range (typo)")
                out.append(NA_MISSING)
            else:
                out.append(int(x))
        df[col] = pd.Series(out, index=df.index, dtype="int64")
    return df.copy()


def clean_enforcement_attitude(df: pd.DataFrame, log: Log) -> pd.DataFrame:
    """A3: aenf1-4 are agree/disagree. Persian coding 1=agree, 2=disagree ->
    1=agree, 0=disagree. 0 in the source = skipped."""
    for col in items_of(df, "aenf"):
        out = []
        for rid, v in zip(df["resp_id"], df[col]):
            x = as_number(v)
            if x == 1:
                out.append(1)
            elif x == 2:
                out.append(0)
            else:
                reason = ("0 on an item that applies to everyone = skipped" if x == 0
                          else "invalid entry on agree/disagree item")
                log.add(rid, col, v, NA_MISSING, reason)
                out.append(NA_MISSING)
        df[col] = pd.Series(out, index=df.index, dtype="int64")
    return df


def clean_numeric(df: pd.DataFrame, cols: list[str], log: Log) -> pd.DataFrame:
    """Age, demographics, 0/1 boxes and accident counts: must be whole numbers."""
    for col in cols:
        out = []
        for rid, v in zip(df["resp_id"], df[col]):
            x = as_number(v)
            if x is None or (not np.isnan(x) and x != int(x)) or (not np.isnan(x) and x < 0):
                log.add(rid, col, v, NA_MISSING, "invalid count / code")
                out.append(NA_MISSING)
            elif np.isnan(x):
                out.append(NA_MISSING)
            else:
                out.append(int(x))
        df[col] = pd.Series(out, index=df.index, dtype="int64")
    return df


def clean_license(df: pd.DataFrame, mode: str, kind: str, log: Log,
                  checks: list[dict]) -> pd.DataFrame:
    """A4: one license (car or motor).

    Year field (FROM PERSIAN CHECK): the number is YEARS HELD (1402 - year
    obtained), 'no' or 0 = no license. The motorcyclist form also has a
    separate yes/no question (Persian coding 1=no, 2=yes); the yes/no answer
    decides <kind>_lic, the year is set to -9 when the two disagree.
    Years held that imply licensing before age 18 (or before birth) are set
    to age - 18; respondents under 18 who report a license get -9.
    """
    raw_years = df[f"{kind}_lic_years_raw"]
    yn = df.get(f"{kind}_lic_yn_raw")
    lic, years, lic_age, issue = [], [], [], []

    for i, rid in enumerate(df["resp_id"]):
        age = df.at[i, "age"]
        v = raw_years.iat[i]
        x = as_number(v)
        # 'no', 0 or blank = no year; any other text is treated the same way
        no_year = x is None or x == 0 or np.isnan(x)
        says = None if yn is None else (1 if as_number(yn.iat[i]) == 2 else 0)
        has = (0 if no_year else 1) if says is None else says
        rec = {"resp_id": rid, "license": kind, "says_has_license": has, "age": age,
               "years_held_reported": np.nan if no_year else x,
               "year_obtained_iranian": np.nan if no_year else SURVEY_YEAR_IR - x,
               "age_when_licensed_reported": np.nan if no_year else age - x,
               "issue": np.nan, "action": np.nan, "years_held_cleaned": np.nan}

        if has == 0 and no_year:                             # consistent "no"
            y, a, iss = NA_NOT_APPLICABLE, NA_NOT_APPLICABLE, np.nan
        elif has == 0 and not no_year:                       # says no, gives a year
            y, a, iss = NA_MISSING, NA_MISSING, "says no, but a year is given"
            log.add(rid, f"{kind}_lic_years", x, NA_MISSING, iss)
        elif has == 1 and no_year:                           # says yes, no year
            y, a, iss = NA_MISSING, NA_MISSING, "says yes, no year given"
        else:
            y, iss = int(x), np.nan
            age_lic = age - x
            if age_lic < LEGAL_LICENSE_AGE:
                iss = ("license older than respondent" if age_lic < 0
                       else "licensed before age 18")
                if age < LEGAL_LICENSE_AGE:
                    y = NA_MISSING
                    rec["action"] = "years held set to missing (respondent under 18)"
                else:
                    y = int(age - LEGAL_LICENSE_AGE)
                    rec["action"] = "years held set to age - 18"
                log.add(rid, f"{kind}_lic_years", x, y, rec["action"])
            a = NA_MISSING if y == NA_MISSING else int(age - y)
        rec["issue"] = iss
        if not (isinstance(iss, float) and np.isnan(iss)):
            rec["years_held_cleaned"] = y if isinstance(rec["action"], str) else np.nan
            checks.append({**rec, "mode": mode})
        lic.append(has); years.append(y); lic_age.append(a); issue.append(iss)

    df = df.copy()
    df[f"{kind}_lic"] = lic
    df[f"{kind}_lic_years"] = years
    df[f"{kind}_lic_age"] = lic_age
    df[f"{kind}_lic_issue"] = issue
    return df.copy()


def derive(df: pd.DataFrame, mode: str, q_order: list[str]) -> pd.DataFrame:
    """A5 + A6: mode use, accidents, quality flags, covariates, reversed copies."""
    df = df.copy()
    m = df.apply(to_na)          # NaN view for calculations
 
    # job label
    df["job_label"] = df["job"].map(JOB[mode])
 
    # mode use: 1 = uses the mode (bf 1-4), 0 = bf 5
    for b in BF:
        df["user_" + b[3:]] = np.where(m[b].isna(), np.nan, (m[b] <= 4).astype(int))
 
    # reversed copies (6 - x)
    for it in REVERSE[mode]:                  # in derive(): was decided(mode, "reverse")
        df[it + "_r"] = np.where(m[it].isna(), NA_MISSING, 6 - m[it]).astype(int)
 
    # accidents
    parts = m[ACC_PARTS].sum(axis=1, min_count=1)
    df["acc_parts_sum"] = parts
    df["acc_total_rec"] = np.fmax(m["acc_total"], parts)
    tot = m["acc_total"]
    df["acc_check"] = np.select(
        [tot.eq(parts), tot.gt(0) & parts.eq(0), tot.lt(parts), tot.gt(parts)],
        ["ok", "total>0_no_breakdown", "total<parts", "total>parts"], default="missing")
    df["acc_any"] = (df["acc_total_rec"] > 0).astype(int)
    df["acc_extreme"] = (m[ACC].max(axis=1) >= ACC_EXTREME).astype(int)
 
    # respondent flags
    df["minor"] = (m["age"] < 18).astype(int)
 
    tpb = sum((items_of(df, p) for p in ["sn", "att", "pbc", "hab", "int"]), [])
    v = m[tpb]
    df["straightline"] = (v.max(axis=1) == v.min(axis=1)).astype(int)
 
    # identical earlier record (all original answers equal)
    raw_cols = [c for c in q_order if c in df.columns]
    key = df[raw_cols].astype(str).agg("|".join, axis=1)
    first = {}
    dup = []
    for rid, k in zip(df["resp_id"], key):
        dup.append(first.get(k, np.nan))
        first.setdefault(k, rid)
    df["dup_of"] = dup
 
    blocks = {b: items_of(df, b) for b in
              ["rbf", "nbv", "sa", "pa", "sn", "att", "pbc", "hab", "int", "mr"]}
    blocks = {b: its for b, its in blocks.items() if its}
    same = pd.DataFrame({b: m[its].max(axis=1) == m[its].min(axis=1)
                         for b, its in blocks.items()})
    df["sl_blocks"] = same.sum(axis=1)
    df["sl_block_names"] = same.apply(
        lambda r: ", ".join(b for b in same.columns if r[b]) or np.nan, axis=1)
    opp = np.zeros(len(df), dtype=bool)
    for b in opposite_blocks(mode):
        first_val = m[blocks[b]].max(axis=1)
        opp |= same[b].values & (first_val != 3).values
    df["sl_opposite"] = opp.astype(int)
 
    # covariates: gender, edu_ord, job_group, professional_driver, lic_under_age
    df = df.copy()
    df["female"] = (df["gender"] == 1).astype(int)
    df["edu_ord"] = df["edu"].map(EDU_ORD)
    label_to_group = {lab: g for g, labs in JOB_GROUP.items() for lab in labs}
    df["job_group"] = df["job_label"].map(label_to_group)
    for g in JOB_GROUP:
        df["job_" + g] = (df["job_group"] == g).astype(int)
    if mode == "Driver":
        df["professional_driver"] = df["job"].isin(PROFESSIONAL_DRIVER_JOBS).astype(int)
    any_lic = df["car_lic"] == 1
    if "motor_lic" in df:
        any_lic |= df["motor_lic"] == 1
    df["lic_under_age"] = (any_lic & (df["age"] < LEGAL_LICENSE_AGE)).astype(int)
    return df
 
 
def clean_mode(path: Path, mode: str, lic_checks: list[dict]):
    df, cmap = load_mode(path, mode)
    q_order = list(cmap["variable"])
    log = Log()
 
    demo = ["gender", "age", "edu", "job"] + [c for c in df.columns
                                              if c.startswith(("purp_", "bike_"))]
    df = clean_numeric(df, demo + ACC, log)
    df = clean_scales(df, log)
    df = clean_enforcement_attitude(df, log)
 
    checks: list[dict] = []
    if mode == "Motorcyclist":
        df = clean_license(df, mode, "motor", log, checks)
    df = clean_license(df, mode, "car", log, checks)
    lic_checks.extend(checks)
    df = df.drop(columns=[c for c in df.columns if c.endswith("_raw")])
 
    df = derive(df, mode, q_order)
    coded = df
    model = df.copy()
    num = model.select_dtypes("number").columns
    model[num] = model[num].apply(to_na)
 
    # flag summary
    fs = [(f, int(model[f].sum())) for f in ["minor", "lic_under_age", "straightline",
                                             "sl_opposite", "acc_extreme"]]
    if mode == "Driver":
        fs.append(("professional_driver", int(model["professional_driver"].sum())))
    fs += [("dup_of", int(model["dup_of"].notna().sum())),
           ("acc_check != ok", int((model["acc_check"] != "ok").sum()))]
    for kind in ["motor", "car"]:
        if f"{kind}_lic_issue" in model:
            for iss, n in model[f"{kind}_lic_issue"].value_counts().sort_index().items():
                fs.append((f"{kind}_lic: {iss}", int(n)))
    flag_summary = pd.DataFrame(fs, columns=["flag", "n"])
 
    lic_df = pd.DataFrame(checks).drop(columns="mode", errors="ignore")
    return coded, model, log.frame(), flag_summary, lic_df
 

# =============================================================================
# MAIN
# =============================================================================

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--input", default=Path("SurveyData/survey_renamed.xlsx"), type=Path)
    ap.add_argument("--out-dir", default=Path("CleaningResults"), type=Path)
    args = ap.parse_args()
    args.out_dir.mkdir(parents=True, exist_ok=True)

    models, lic_all = {}, []
    for mode in MODES:
        coded, model, log, fs, lic = clean_mode(args.input, mode, lic_all)
        with pd.ExcelWriter(args.out_dir / f"{mode}_clean.xlsx") as xw:
            model.to_excel(xw, sheet_name="model_data", index=False)
            coded.to_excel(xw, sheet_name="coded_data", index=False)
            log.to_excel(xw, sheet_name="cleaning_log", index=False)
            fs.to_excel(xw, sheet_name="flag_summary", index=False)
            lic.to_excel(xw, sheet_name="license_check", index=False)
        model.to_csv(args.out_dir / f"{mode}_model_data.csv", index=False)
        models[mode] = model
        print(f"{mode:13s} n={len(model):4d}  log entries={len(log):4d}")

    pd.DataFrame(lic_all).to_csv(args.out_dir / "license_check.csv", index=False)

if __name__ == "__main__":
    main()