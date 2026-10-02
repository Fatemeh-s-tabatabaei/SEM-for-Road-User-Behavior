import argparse, os
import numpy as np, pandas as pd

MODES = ["Cyclist", "Pedestrian", "Driver", "Motorcyclist"]
GENDER = {1: "Female", 2: "Male"}
EDU = {1: "No formal education", 2: "Primary", 3: "Lower secondary", 4: "High-school diploma",
       5: "Bachelor's degree", 6: "Master's or higher"}
JOB_CP = {1: "Unemployed", 2: "Self-employed", 3: "Laborer", 4: "Government employee",
          5: "Private-sector employee", 6: "University student", 7: "School student", 8: "Retired", 9: "Homemaker"}
JOBS = {"Cyclist": JOB_CP, "Pedestrian": JOB_CP,
        "Motorcyclist": {1: "Unemployed", 2: "Motorcycle courier", 3: "Self-employed", 4: "Laborer",
                         5: "Government employee", 6: "Private-sector employee", 7: "University student",
                         8: "School student", 9: "Retired"},
        "Driver": {1: "Unemployed", 2: "Government employee", 3: "Private-sector employee", 4: "Taxi driver",
                   5: "Bus driver", 6: "Freight vehicle driver", 7: "Self-employed", 8: "Laborer",
                   9: "Student", 10: "Retired", 11: "Homemaker"}}
PROFESSIONAL = [4, 5, 6]          # driver occupation codes: taxi, bus, freight
# license held by each group: (indicator, years held, label)
LICENSES = {"Cyclist": [("car_lic", "car_lic_years", "car")], "Pedestrian": [("car_lic", "car_lic_years", "car")],
            "Driver": [(None, "car_lic_years", "car")],
            "Motorcyclist": [("motor_lic", "motor_lic_years", "motorcycle"), ("car_lic", "car_lic_years", "car")]}
MIN_LIC_AGE = 16                  # years held implying a license before this age are treated as unreliable
SMALL = 30                        # a subgroup smaller than this is too small for separate estimates


def pct(k, n):
    return f"{k} ({100 * k / n:.1f})" if n else "0"


def describe(mode, d):
    n = len(d)
    rows = [("Sample", "", "n", str(n))]
    a = d["age"]
    rows += [("Age (years)", "", "Mean (SD)", f"{a.mean():.1f} ({a.std():.1f})"),
             ("", "", "Range", f"{a.min():.0f}--{a.max():.0f}"),
             ("", "", "Under 18, n (%)", pct(int((a < 18).sum()), n))]
    for var, labels, title in [("gender", GENDER, "Gender"), ("edu", EDU, "Education"), ("job", JOBS[mode], "Occupation")]:
        for code, lab in labels.items():
            rows.append((title if code == min(labels) else "", lab, "n (%)", pct(int((d[var] == code).sum()), n)))
        miss = int(d[var].isna().sum())
        if miss: rows.append(("", "Missing", "n (%)", pct(miss, n)))

    for ind, yrs, lab in LICENSES[mode]:
        title = f"Experience: {lab} license"
        if ind:
            rows.append((title, "Holds a license", "n (%)", pct(int((d[ind] == 1).sum()), n)))
            title = ""
        y = d[yrs]
        bad = y.notna() & (d["age"] - y < MIN_LIC_AGE)
        ok = y[y.notna() & ~bad]
        rows += [(title, "Years held", "Mean (SD)", f"{ok.mean():.1f} ({ok.std():.1f})" if len(ok) else "--"),
                 ("", "", "Range", f"{ok.min():.0f}--{ok.max():.0f}" if len(ok) else "--"),
                 ("", "", "Valid n", str(len(ok)))]
        if bad.any(): rows.append(("", f"Implies licensing before {MIN_LIC_AGE} (excluded)", "n", str(int(bad.sum()))))

    if mode == "Driver":
        prof = d["job"].isin(PROFESSIONAL)
        rows.append(("Professional status", "Professional driver (taxi, bus, freight)", "n (%)", pct(int(prof.sum()), n)))
        rows.append(("", "Non-professional", "n (%)", pct(int((~prof & d["job"].notna()).sum()), n)))
    return pd.DataFrame(rows, columns=["Characteristic", "Category", "Statistic", "Value"])


def imbalances(mode, d):
    n = len(d)
    f, m = int((d["gender"] == 1).sum()), int((d["gender"] == 2).sum())
    u18 = int((d["age"] < 18).sum())
    notes, limits = [], []
    share = f / max(f + m, 1)
    if min(f, m) < SMALL:
        notes.append(f"Effectively an all-{'male' if f < m else 'female'} sample ({min(f, m)} "
                     f"{'women' if f < m else 'men'})")
        limits.append("gender cannot be used as a grouping variable or moderator; results describe "
                      f"{'male' if f < m else 'female'} riders only")
    elif share < .25 or share > .75:
        notes.append(f"Gender split {100 * share:.0f}% female")
        limits.append("gender comparisons rest on a small group; gender as a covariate only")
    if u18 / n > .05:
        notes.append(f"{100 * u18 / n:.0f}% under 18")
        limits.append("run sensitivity analyses without minors")
    if mode == "Driver":
        p = int(d["job"].isin(PROFESSIONAL).sum())
        notes.append(f"{p} professional drivers")
        limits.append("professional drivers too few for a separate model; include professional status as a covariate"
                      if p < 100 else "professional status can be compared")
    if mode == "Motorcyclist":
        c = int((d["job"] == 2).sum())
        notes.append(f"{c} motorcycle couriers")
        limits.append("couriers ride for work; include occupation as a covariate")
    if "dup_of" in d and d["dup_of"].fillna("").astype(str).str.len().gt(0).any():
        k = int(d["dup_of"].fillna("").astype(str).str.len().gt(0).sum())
        notes.append(f"{k} duplicate records flagged in cleaning")
        limits.append("drop the duplicates before modelling")
    return {"Group": mode, "n": n, "Female / male": f"{f} / {m}", "Female %": round(100 * share, 1),
            "Under 18": u18, "Other": "; ".join(notes) or "--", "Limits later analyses": "; ".join(limits) or "--"}


def main(data_dir, out_dir):
    os.makedirs(out_dir, exist_ok=True)
    tables, imb = {}, []
    for mode in MODES:
        d = pd.read_csv(os.path.join(data_dir, f"{mode}_model_data.csv"))
        tables[mode] = describe(mode, d)
        imb.append(imbalances(mode, d))
    imb = pd.DataFrame(imb)
    best = (imb["Female %"] - 50).abs().idxmin()            # mark the most balanced gender split
    imb.loc[best, "Other"] = "Most balanced gender split" + ("" if imb.loc[best, "Other"] == "--" else "; " + imb.loc[best, "Other"])
    with pd.ExcelWriter(os.path.join(out_dir, "sample_description.xlsx")) as xw:
        imb.to_excel(xw, sheet_name="imbalances", index=False)
        for mode, t in tables.items():
            t.to_excel(xw, sheet_name=mode, index=False)
    print(imb[["Group", "n", "Female / male", "Under 18", "Other"]].to_string(index=False))


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("--data_dir", default="cleaned")
    ap.add_argument("--out_dir", default="sample")
    a = ap.parse_args()
    main(a.data_dir, a.out_dir)