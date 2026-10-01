"""
Clean the road-user survey sheets (Cyclist, Pedestrian, Driver, Motorcyclist).

Codes in coded_data:  -8 = not applicable (no license, does not use the mode)
                      -9 = nonresponse / invalid (skipped, two boxes ticked, typo)
In model_data (and the CSV) both are blank.

Response codes and their meaning are taken from the Persian raw sheets (column C, "coding"), which
are aligned question by question with the English analysis sheets. The Driver workbook has no
Persian sheet, so its labels come from the English questionnaire in Driver.xlsx/Sheet1.

Respondents who failed the attention check are assumed to be already removed from the English
sheets. The script only records how many respondents each Persian raw sheet had versus the
English sheet (sample_sizes.csv) for the data-cleaning report.

Usage:  python clean_road_user_data.py --data_dir . --out_dir cleaned
"""
import argparse, os, re, warnings
import numpy as np
import pandas as pd

SKIP, MISS = -8, -9
warnings.filterwarnings("ignore", category=pd.errors.PerformanceWarning)

# English sheet, id prefix, Persian raw sheet (rows = questions, respondents from column D),
# and the Persian rows that have no English column, with the reason.
SURVEYS = {
    "Cyclist":      ("Cyclist (dlt+edited)",       "C", "cyclist",
                     {13: "no-license tick box (merged into license year)", 39: "blank row", 68: "attention check"}),
    "Pedestrian":   ("pedestrian spss (dlt+edit)", "P", "pedestrian",
                     {5: "no-license tick box (merged into license year)", 34: "blank row", 55: "attention check",
                      **{r: "underpass/overpass preference (not in English sheet)" for r in range(88, 94)}}),
    "Driver":       ("Driver SPSS(dlt+edit)",      "D", None, {}),
    "Motorcyclist": ("Motor (dlt+edited)",         "M", "motor", {39: "blank row", 65: "attention check"}),
}
# The license question asks for the year obtained (Iranian calendar); the English sheets store
# years held = 1402 - year obtained. Fieldwork was in 2023, i.e. 1402 (use --survey_year 1401 only
# if questionnaires were collected before 21 March 2023).
ENGLISH_BASE_YEAR = 1402
SURVEY_YEAR = 1402
MIN_LIC_AGE = 18        # minimum legal age for a car or motorcycle license
# Years held that imply licensing before MIN_LIC_AGE are capped at age - MIN_LIC_AGE (licensed at 18).
# Respondents younger than MIN_LIC_AGE who report a license get years held = missing.

# Persian response labels (raw sheets, column C) -> English
FA_EN = {
    "خانم": "female", "آقا": "male",
    "بدون مدرک": "no formal education", "ابتدایی": "primary school", "راهنمایی": "middle school",
    "دیپلم": "high-school diploma", "لیسانس": "bachelor's degree", "ارشد و بالاتر": "master's degree or higher",
    "بیکار": "unemployed", "شغل آزاد": "self-employed", "کارگر": "laborer", "کارمند دولتی": "government employee",
    "کارمند بخش خصوصی": "private-sector employee", "کارمند خصوصی": "private-sector employee",
    "دانشجو": "university student", "دانش آموز": "school student", "بازنشسته": "retired",
    "خانه دار": "homemaker", "پیک موتورسوار": "motorcycle courier",
    "خرید": "shopping", "تفریح": "recreation", "ورزش": "exercise", "رفتن محل کار": "commuting to work",
    "رفتن مدرسه یا دانشگاه": "going to school or university",
    "دوچرخه شخصی": "personal bicycle", "دوچرخه اشتراکی": "shared bicycle", "هر دو مورد": "both",
    "خیر": "no", "بله": "yes",
    "هر روز": "every day", "4 روز در هفته": "4 days a week", "1 تا 3 روز در هفته": "1-3 days a week",
    "چند روز در ماه": "a few days a month", "چند روز در سال(هیچ وقت)": "a few days a year (never)",
    "هرگز": "never", "همیشه": "always", "غیر قابل قبول": "unacceptable", "قابل قبول": "acceptable",
    "کاملا مخالف": "strongly disagree", "کاملا موافق": "strongly agree", "موافقم": "agree", "مخالفم": "disagree",
}
# Driver.xlsx has no Persian sheet: occupation order from the English questionnaire (Sheet1).
# Checked against the data: 9 = youngest, 10 = oldest, 11 = all female, 4-6 = all drive daily.
DRIVER_JOBS = ("1=unemployed; 2=government employee; 3=private-sector employee; 4=taxi driver; 5=bus driver; "
               "6=freight vehicle driver; 7=self-employed; 8=laborer; 9=student; 10=retired; 11=homemaker")
DEMOGRAPHICS = {"gender": "1=female; 2=male", "edu": ("1=no formal education; 2=primary school; 3=middle school; "
                "4=high-school diploma; 5=bachelor's degree; 6=master's degree or higher")}

# construct tag in the header's last "(...)" -> (code, construct, scale)
AGREE = "1=strongly disagree ... 5=strongly agree"
CONSTRUCTS = {
    "behavioral frequency": ("bf", "Mode use", "1=every day ... 5=a few days a year (never)"),
    "risky behavior frequency": ("rbf", "Risky behavior", "1=never ... 5=always"),
    "normative behavior violation frequency": ("nbv", "Normative violations", "1=never ... 5=always"),
    "social acceptance": ("sa", "Social acceptance", "1=unacceptable ... 5=acceptable"),
    "personal acceptance": ("pa", "Personal acceptance", "1=unacceptable ... 5=acceptable"),
    "social norms": ("sn", "Social norms", AGREE),
    "att": ("att", "Attitude", AGREE),
    "attitude": ("att", "Attitude", AGREE),
    "perceived behavioral control": ("pbc", "Perceived behavioral control", AGREE),
    "habit": ("hab", "Habit", AGREE),
    "intention": ("int", "Intention", AGREE),
    "moral responsibility": ("mr", "Moral responsibility", AGREE),
    "attitude toward enforcement": ("aenf", "Attitude toward enforcement", "1=agree, 0=disagree"),
    "perceived risk": ("psafe", "Feeling of safety", "1=very unsafe ... 5=very safe"),
    "risk/perceived causality": ("cause", "Perceived causality", "1=not important ... 5=very important"),
    "perceived enforcement": ("penf", "Perceived enforcement", "1=very unlikely ... 5=very likely"),
}
BF_MODES = ["walk", "bike", "motor", "bus", "taxi", "car", "metro"]
PSAFE_MODES = ["walk", "bike", "motor", "taxi", "metro", "bus", "car"]
ACC = ["acc_total"] + [f"{k}_{m}" for k in ("inj", "fin") for m in ("ped", "bike", "motor", "taxi", "bus", "car")]
REVERSE = {"Cyclist": ["sn4"], "Driver": ["sn4"], "Motorcyclist": ["sn4", "hab7"], "Pedestrian": ["sn4", "sn5"]}
PLAIN = {"car license?": "Lyear", "Do you use a bicycle for shopping?": "purp_shop",
         "Do you use a bicycle for recreation?": "purp_recr", "Do you use a bicycle for exercise?": "purp_exer",
         "Do you use a bicycle for commuting to work?": "purp_work",
         "Do you use a bicycle for traveling to school or university?": "purp_school", "Do you use a personal bicycle?": "bike_personal",
         "Do you use a shared bicycle (e.g., Bikeshare systems)?": "bike_shared",
         "Do you use both a personal and a shared bicycle?": "bike_both"}


def short_name(header, counter):
    """Short code from an English header, e.g. '... (perceived behavioral control3)' -> 'pbc3'."""
    h = str(header).strip()
    tag = re.findall(r"\(([^()]*)\)", h)
    m = re.match(r"(.*?)\s*(\d*)$", re.sub(r"\s+", " ", tag[-1]).strip().lower()) if tag else None
    if not m or m.group(1) not in CONSTRUCTS:
        return PLAIN.get(h, h)
    code, n = CONSTRUCTS[m.group(1)][0], int(m.group(2))
    if code == "aenf":                                       # duplicated labels in source: number by position
        counter["aenf"] += 1; n = counter["aenf"]
    if code == "bf": return f"bf_{BF_MODES[n - 1]}"
    if code == "psafe": return f"psafe_{PSAFE_MODES[n - 1]}"
    if code == "att" and n >= 7: return f"infra{ {7: 1, 8: 2, 9: 3, 11: 4}[n] }"   # sidewalk/street items
    return f"{code}{n}"


def scale_items(cols):
    return [c for c in cols if re.match(r"(bf_|psafe_|(rbf|nbv|sa|pa|sn|att|infra|pbc|hab|int|mr|aenf|cause|penf)\d+$)", c)]


def translate_coding(fa):
    """Persian coding cell -> English value labels, e.g. 'خانم = 1 | آقا = 2' -> '1=female; 2=male'."""
    out = []
    for line in re.split(r"[\n|]", str(fa).replace("\u200c", " ")):
        line = re.sub(r"\s+", " ", line).strip()
        rng = re.match(r"^(?:از\s*)?(.+?)\s*=\s*(\d+)\s+تا\s+(.+?)\s*=\s*(\d+)$", line)   # 'X = 1 تا Y = 5'
        pairs = [rng.group(1, 2), rng.group(3, 4)] if rng else re.findall(r"(.+?)\s*=\s*(\d+)", line)
        out += [f"{n}={FA_EN.get(lbl.strip(), lbl.strip())}" for lbl, n in pairs]
    if out:
        return " ... ".join(out) if rng else "; ".join(out)
    return "1-5 as marked (direction not stated)" if "1 تا 5" in str(fa) else "number as written"


def persian_codebook(mode, path, n_cols):
    """Align the Persian sheet to the English columns; return per-column Persian info and unused rows."""
    _, _, raw_sheet, skip = SURVEYS[mode]
    if raw_sheet is None:
        return pd.DataFrame(index=range(n_cols), columns=["q_no", "question_fa", "coding_fa", "coding_en"]), pd.DataFrame()
    raw = pd.read_excel(path, sheet_name=raw_sheet).iloc[:, :3]
    raw.columns = ["q_no", "question_fa", "coding_fa"]
    raw[["q_no", "coding_fa"]] = raw[["q_no", "coding_fa"]].ffill()
    raw["question_fa"] = raw.question_fa.astype(str).str.replace(r"\s+", " ", regex=True).str.strip()
    raw["coding_en"] = raw.coding_fa.map(translate_coding)
    used = [r for r in raw.index if r not in skip][:n_cols]
    unused = raw.drop(index=used).assign(reason=lambda d: [skip.get(r, "not in English sheet") for r in d.index])
    return raw.loc[used].reset_index(drop=True), unused[["q_no", "question_fa", "coding_fa", "reason"]]


def apply_rules(df, cols, rules, log):
    """Apply (mask, new value, reason) rules in order (first match wins) and log every change."""
    done = pd.DataFrame(False, index=df.index, columns=cols)
    for mask, value, reason in rules:
        mask = mask & ~done
        r, c = np.nonzero(mask.to_numpy())
        log += [(df.resp_id.iat[i], cols[j], df[cols[j]].iat[i], value, reason) for i, j in zip(r, c)]
        df[cols] = df[cols].mask(mask, value)
        done |= mask


def sample_sizes(mode, data_dir, n_english):
    """Respondents in the Persian raw sheet (non-empty columns from D) vs the English sheet."""
    raw_sheet = SURVEYS[mode][2]
    if raw_sheet is None:
        return dict(mode=mode, raw_persian_n=None, english_n=n_english, removed=None, pct_removed=None,
                    note="No Persian raw sheet in Driver.xlsx")
    raw = pd.read_excel(os.path.join(data_dir, f"{mode}.xlsx"), sheet_name=raw_sheet).iloc[:, 3:]
    n_raw = int(raw.notna().any().sum())
    return dict(mode=mode, raw_persian_n=n_raw, english_n=n_english, removed=n_raw - n_english,
                pct_removed=round(100 * (n_raw - n_english) / n_raw, 1),
                note="English sheet assumed to exclude attention-check failures")


def license(df, kind, years, log, has=None):
    """'no' or 0 = no license. Adds {kind}_lic, _lic_years, _lic_age (age when licensed) and _lic_issue.
    Years: -8 without license, -9 when the yes/no answer and the year field conflict.
    Years implying a license before MIN_LIC_AGE are capped so that the license dates from MIN_LIC_AGE."""
    y = pd.to_numeric(years.astype(str).str.strip().replace("no", ""), errors="coerce").replace(0, np.nan)
    y = y - (ENGLISH_BASE_YEAR - SURVEY_YEAR)                 # years held at the time of the survey
    has = y.notna().astype(int) if has is None else has
    age_at = df.age - y
    ok = (has == 1) & y.notna()
    issue = np.select([(has == 1) & y.isna(), (has == 0) & y.notna(), ok & (age_at < 0), ok & (age_at < MIN_LIC_AGE)],
                      ["says yes, no year given", "says no, but a year is given", "license older than respondent",
                       f"licensed before age {MIN_LIC_AGE}"], "")
    early = ok & (age_at < MIN_LIC_AGE)
    too_young = early & (df.age < MIN_LIC_AGE)
    capped = y.mask(early, df.age - MIN_LIC_AGE).mask(too_young, MISS)
    action = np.select([too_young, early], ["years held set to missing (respondent under 18)",
                                            f"years held set to age - {MIN_LIC_AGE}"], "")
    log += [(r, f"{kind}_lic_years", o, int(n), a) for r, o, n, a in
            zip(df.resp_id[early], y[early], capped[early], action[early.to_numpy()])]
    years_out = capped.where(ok, np.where(has == 0, SKIP, MISS)).mask((has == 0) & y.notna(), MISS).astype(int)
    df[f"{kind}_lic"] = has
    df[f"{kind}_lic_years"] = years_out
    df[f"{kind}_lic_age"] = (df.age - years_out).where(years_out >= 0, years_out)
    df[f"{kind}_lic_issue"] = issue
    return pd.DataFrame({"resp_id": df.resp_id, "license": kind, "says_has_license": has, "age": df.age,
                         "years_held_reported": y, "year_obtained_iranian": SURVEY_YEAR - y,
                         "age_when_licensed_reported": age_at, "issue": issue, "action": action,
                         "years_held_cleaned": capped.where(ok)})[issue != ""]


def clean(mode, data_dir):
    sheet, prefix, _, _ = SURVEYS[mode]
    path = os.path.join(data_dir, f"{mode}.xlsx")
    src = pd.read_excel(path, sheet_name=sheet)
    fa, unused = persian_codebook(mode, path, src.shape[1])
    counter = {"aenf": 0}
    names = [short_name(c, counter) for c in src.columns[:-13]] + ACC   # accidents: last 13 columns
    df = src.set_axis(names, axis=1)
    df.insert(0, "resp_id", [f"{prefix}{i:03d}" for i in range(1, len(df) + 1)])
    log = []

    # 1. scale items -------------------------------------------------------------------
    items = scale_items(df.columns)
    X = df[items].apply(pd.to_numeric, errors="coerce")
    is_psafe = pd.Series([c.startswith("psafe") for c in items], index=items)
    is_aenf = pd.Series([c.startswith("aenf") for c in items], index=items)
    apply_rules(df, items, [
        (X.isna(), MISS, "non-numeric entry (dash or two answers)"),
        ((X == 0) & is_psafe, SKIP, "0 on mode-safety item = does not use the mode"),
        (X == 0, MISS, "0 on an item that applies to everyone = skipped"),
        (X % 1 != 0, MISS, "fractional value (two adjacent boxes ticked)"),
        ((X > 2) & is_aenf, MISS, "invalid on 1/2 scale"),
        ((X < 1) | (X > 5), MISS, "out of 1-5 range (typo)"),
    ], log)
    df[items] = df[items].astype(int)
    aenf = list(is_aenf[is_aenf].index)
    df[aenf] = df[aenf].replace({2: 0})                      # 1=agree, 2=disagree -> 1/0

    # 2. licenses ----------------------------------------------------------------------
    if mode == "Motorcyclist":
        lic_check = pd.concat([license(df, kind, df.pop(yr), log, df.pop(yn).map({1: 0, 2: 1}))
                               for kind, yn, yr in (("motor", "motorlisence", "Ymlisence"), ("car", "carlisence", "Yclisence"))])
    else:
        lic_check = license(df, "car", df.pop("Lyear"), log)
    jobs = DRIVER_JOBS if mode == "Driver" else fa.coding_en[names.index("job")]
    df["job_label"] = df.job.map({int(k): v for k, v in re.findall(r"(\d+)=([^;]+)", jobs)})

    # 3. mode use, accidents, reversed items, respondent flags ---------------------------
    df[ACC] = df[ACC].apply(pd.to_numeric, errors="coerce").fillna(MISS).astype(int)
    parts, total = df[ACC[1:]].clip(lower=0).sum(axis=1), df.acc_total
    agree_items = [c for c in items if re.match(r"(sn|att|pbc|hab|int)\d+$", c)]
    key = df[["gender", "age", "edu", "job"] + items].astype(str).agg("|".join, axis=1)
    first = df.resp_id.groupby(key).transform("first")
    new = {f"user_{m}": (df[f"bf_{m}"] < 5).astype(int).where(df[f"bf_{m}"] > 0, MISS) for m in BF_MODES}
    new |= {f"{c}_r": (6 - df[c]).where(df[c] > 0, df[c]) for c in REVERSE[mode]}
    new |= {
        "acc_parts_sum": parts,
        "acc_total_rec": np.maximum(total, parts),
        "acc_check": np.select([total == parts, (total > 0) & (parts == 0), total < parts],
                               ["ok", "total>0_no_breakdown", "total<parts"], "total>parts"),
        "acc_any": (np.maximum(total, parts) > 0).astype(int),
        "acc_extreme": (df[ACC].max(axis=1) >= 10).astype(int),
        "minor": (df.age < 18).astype(int),
        "straightline": (df[agree_items].where(df[agree_items] > 0).std(axis=1) == 0).astype(int),
        "dup_of": first.where(first != df.resp_id, ""),
    }
    df = pd.concat([df, pd.DataFrame(new, index=df.index)], axis=1)
    log = pd.DataFrame(log, columns=["resp_id", "variable", "old_value", "new_value", "reason"])
    fa.index = names
    return df.copy(), codebook(mode, df, dict(zip(names, src.columns)), fa), log, lic_check, unused


NOTES = {
    "bf_": "Persian wording: usual frequency under normal conditions (before the COVID-19 outbreak)",
    "rbf": "Past month", "sa": "Asks about people in Shiraz", "pa": "Asks about the respondent personally",
    "purp_": "1 = selected, 0 = not selected (multiple choice in the Persian form)",
    "bike_": "1 = selected, 0 = not selected (single choice in the Persian form: 1=personal, 2=shared, 3=both)",
    "_lic_age": "age - years held = age when licensed; -8 no license, -9 missing/conflict",
    "_lic_issue": f"conflict between yes/no and year, or reported licensing before age {MIN_LIC_AGE}",
    "job_label": "occupation label from the Persian coding (Driver: English questionnaire)",
    "car_lic": "1=has license; 0=no license (0 or 'no' in the year field)",
    "motor_lic": "1=yes; 0=no (Persian coding: 1=no, 2=yes)",
    "acc_": "count as written in the questionnaire", "inj_": "count as written in the questionnaire",
    "fin_": "count as written in the questionnaire",
    "psafe_": "-8 = 0 in source (does not use mode). Non-users' ratings kept; filter with user_*",
    "infra": "Infrastructure safety, not attitude; model as a separate factor",
    "user_": "1 = uses mode (bf 1-4), 0 = bf 5",
    "_r": "Reverse-coded copy (6 - original)",
    "acc_total_rec": "max(acc_total, acc_parts_sum)",
    "straightline": "1 = same answer on every SN/ATT/PBC/Habit/Intention item",
    "dup_of": "resp_id of an identical earlier row",
    "_lic_years": f"survey year - year obtained (Iranian calendar), capped at age - {MIN_LIC_AGE}; -8 no license, -9 missing/conflict",
}


FIXED_DIRECTION = {"psafe": "1=very unsafe ... 5=very safe", "cause": "1=not important ... 5=very important",
                   "penf": "1=very unlikely ... 5=very likely"}   # Persian sheet only says "enter 1 to 5"


def codebook(mode, df, original, fa):
    rows = []
    for c in df.columns:
        m = re.match(r"([a-z]+)", c).group(1)
        info = next((v for v in CONSTRUCTS.values() if v[0] == m), ("", "", ""))
        note = next((v for k, v in NOTES.items() if c.startswith(k) or (k.startswith("_") and c.endswith(k))), "")
        p = fa.loc[c] if c in fa.index else pd.Series(dtype=object)
        coding = p.get("coding_en") if isinstance(p.get("coding_en"), str) else (info[2] or DEMOGRAPHICS.get(c, ""))
        if c == "job" and mode == "Driver": coding = DRIVER_JOBS
        if m in FIXED_DIRECTION: coding = FIXED_DIRECTION[m] + " (direction from the questionnaire; Persian sheet: enter 1-5)"
        if m == "aenf": coding = "1=agree; 0=disagree (Persian coding: 1=agree, 2=disagree)"
        if m in ("purp", "bike") or c.endswith("_r"): coding = ""
        if mode == "Driver" and c.startswith(("bf_", "user_")):
            note = ("UNVERIFIED for drivers (no Persian sheet): bf_car fits 5=every day, the other items fit "
                    "5=never; check the paper forms")
        if mode == "Pedestrian" and c in ("fin_taxi", "fin_bus", "fin_car"):
            note = "Source header shifted by one; relabelled by position"
        rows.append((c, info[1], str(original.get(c, "")), coding, note,
                     p.get("q_no", ""), p.get("question_fa", ""), p.get("coding_fa", "")))
    return pd.DataFrame(rows, columns=["variable", "construct", "question_en", "coding", "note",
                                       "block_fa", "question_fa", "coding_fa"]).fillna("")


def export(mode, df, cb, log, lic_check, unused, out_dir):
    num = df.select_dtypes("number").columns
    model = df.copy()
    model[num] = model[num].mask(model[num].isin([SKIP, MISS]))
    flags = [c for c in ["minor", "straightline", "acc_extreme"] if c in df]
    issues = lic_check.groupby(["license", "issue"]).size()
    summary = pd.DataFrame({"flag": flags + ["dup_of", "acc_check != ok"] + [f"{k}_lic: {i}" for k, i in issues.index],
                            "n": [int((df[f] == 1).sum()) for f in flags]
                                 + [int((df.dup_of != "").sum()), int((df.acc_check != "ok").sum())] + list(issues)})
    with pd.ExcelWriter(os.path.join(out_dir, f"{mode}_clean.xlsx")) as w:
        for name, d in [("model_data", model), ("coded_data", df), ("codebook", cb), ("cleaning_log", log),
                        ("flag_summary", summary), ("license_check", lic_check), ("persian_rows_not_used", unused)]:
            d.to_excel(w, sheet_name=name, index=False)
    model.to_csv(os.path.join(out_dir, f"{mode}_model_data.csv"), index=False)
    return summary


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("--data_dir", default="SurveyData")
    ap.add_argument("--out_dir", default="cleaned")
    ap.add_argument("--survey_year", type=int, default=SURVEY_YEAR, help="fieldwork year, Iranian calendar")
    args = ap.parse_args()
    SURVEY_YEAR = args.survey_year
    os.makedirs(args.out_dir, exist_ok=True)
    sizes, checks = [], []
    for mode in SURVEYS:
        df, cb, log, lic_check, unused = clean(mode, args.data_dir)
        s = export(mode, df, cb, log, lic_check, unused, args.out_dir)
        checks.append(lic_check.assign(mode=mode))
        sizes.append(sample_sizes(mode, args.data_dir, len(df)))
        print(f"{mode}: n={len(df)}, changes={len(log)}, " + ", ".join(f"{f}={n}" for f, n in s.values))
    sizes = pd.DataFrame(sizes).astype({"raw_persian_n": "Int64", "removed": "Int64"})
    sizes.to_csv(os.path.join(args.out_dir, "sample_sizes.csv"), index=False)
    print(sizes.to_string(index=False))
    pd.concat(checks).to_csv(os.path.join(args.out_dir, "license_check.csv"), index=False)