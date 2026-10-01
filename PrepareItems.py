"""Pprepare items for measurement modelling:

1. align item directions (reverse-code where the wording runs in the safe direction)
2. assign each item to the factor it actually measures
3. screen response distributions and merge sparse categories

(anything outside 1-5, or 1-2 for attitude toward enforcement, or non-integer -> missing).
"""

import argparse, os, re
import numpy as np, pandas as pd
 
# ---------------------------------------------------------------- 0. configuration
MODES = ["Cyclist", "Pedestrian", "Driver", "Motorcyclist"]
ITEM = r"(rbf|nbv|sa|pa|sn|att|infra|pbc|hab|int|mr|aenf|cause|penf)\d+|psafe_\w+"   # Phase 1 item names
MIN_CELL = 5
SHARED = ("mr", "aenf", "psafe_", "cause", "penf")  # identical wording in all four questionnaires -> common categories
 
# Step 4: items reversed so that higher = riskier (only items whose wording AND data run the safe way)
REVERSE = {"Cyclist": ["sn4"], "Driver": ["sn4"], "Motorcyclist": ["sn4"], "Pedestrian": ["sn5"]}
 
# Step 5: proposed factors. Items not listed in any factor or SINGLE are excluded (see EXCLUDE).
R = lambda p, a, b: [f"{p}{i}" for i in range(a, b + 1)]
BASE = {"SA": None, "PA": None, "SN": R("sn", 1, 3), "ATT": R("att", 1, 6), "PBC": R("pbc", 1, 7),
        "HAB": R("hab", 1, 7), "INT": R("int", 1, 4), "RBF": None, "MR": R("mr", 1, 3),
        "CAUS": R("cause", 1, 8), "PENF": R("penf", 1, 4)}
FACTORS = {
    "Cyclist": {**BASE, "SA": R("sa", 1, 6), "PA": R("pa", 1, 6), "SN": R("sn", 1, 3) + ["sn5"], "RBF": R("rbf", 1, 8)},
    "Pedestrian": {**BASE, "SA": R("sa", 1, 5) + ["sa7"], "PA": R("pa", 1, 5) + ["pa7"], "PBC": R("pbc", 1, 5),
                   "HAB": R("hab", 1, 5), "RBF": R("rbf", 1, 8), "INFRA": R("infra", 1, 4)},
    "Driver": {**BASE, "SA": R("sa", 1, 9), "PA": R("pa", 1, 9), "RBF": R("rbf", 1, 6), "NBV": R("nbv", 1, 5)},
    "Motorcyclist": {**BASE, "SA": R("sa", 1, 8), "PA": R("pa", 1, 8), "SN": R("sn", 1, 3) + ["sn5"], "RBF": R("rbf", 1, 9)},
}
FOCAL = {"Cyclist": "psafe_bike", "Pedestrian": "psafe_walk", "Driver": "psafe_car", "Motorcyclist": "psafe_motor"}
SINGLE = {m: R("aenf", 1, 4) + [FOCAL[m]] + (["sn5_r"] if m == "Pedestrian" else ["sn4_r"]) for m in MODES}
SINGLE["Cyclist"] += ["infra1", "infra2"]  # only two infrastructure items, r = .26: not a factor
EXCLUDE = {
    "Pedestrian": {"sn4": "friends wear light clothing: correlates the wrong way with the other norm items",
                   "sa6": "Persian wording is the safe behaviour ('definitely wears light clothing'); English is reversed",
                   "pa6": "Persian wording is the safe behaviour ('definitely wears light clothing'); English is reversed"},
}
NOTES = {
    ("Motorcyclist", "hab7"): "NOT reversed: Persian reads 'I usually do not use a helmet'; English label is a mistranslation",
    ("Pedestrian", "hab5"): "Persian 'I'm not used to wearing light clothing at night' = risky; matches English",
    ("Cyclist", "rbf1"): "Persian 'you did not have a helmet' can be read as not owning one",
    ("Motorcyclist", "rbf4"): "Persian 'you did not have a helmet'; contradicts hab7 for 48 respondents",
    ("Cyclist", "rbf7"): "carrying loads may be work-related rather than risk-taking",
    ("Motorcyclist", "rbf8"): "carrying goods may be work-related (couriers) rather than risk-taking",
}
for m in MODES:
    NOTES[(m, "mr3")] = "angry confrontation; weaker link to mr1/mr2, check loading in Step 10"
 
 
# ---------------------------------------------------------------- 1. load Phase 1 output
def load(mode, data_dir):
    d = pd.read_csv(os.path.join(data_dir, f"{mode}_model_data.csv"))
    d = d.drop(columns=[c for c in d.columns if c.endswith("_r")])
    items = [c for c in d.columns if re.fullmatch(ITEM, c)]
    needed = {i for f in FACTORS[mode].values() for i in f} | set(REVERSE.get(mode, [])) | \
             {i for i in SINGLE[mode] if not i.endswith("_r")} | set(EXCLUDE.get(mode, {}))
    missing = sorted(needed - set(items))
    if missing:
        raise SystemExit(f"{mode}_model_data.csv lacks {missing}; rename them in the configuration block")
    bad = [c for c in items if not d[c].dropna().isin(values(c)).all()]
    if bad:
        raise SystemExit(f"{mode}: unexpected values in {bad}; run Phase 1 first")
    acc = d["acc_total_rec"] if "acc_total_rec" in d else pd.Series(dtype=float)   # reconciled total from Phase 1
    return d, items, acc
 
 
def values(c):
    return [0, 1] if c.startswith("aenf") else [1, 2, 3, 4, 5]
 
 
# ---------------------------------------------------------------- 2. merge rule
def merge_map(counts):
    """Merge any category with < MIN_CELL answers into its neighbour nearer the scale centre."""
    groups = [[k] for k in counts.index]
    n = lambda g: sum(counts[k] for k in g)
    while len(groups) > 2:
        small = [i for i, g in enumerate(groups) if n(g) < MIN_CELL]
        if not small: break
        i = min(small, key=lambda j: n(groups[j]))
        j = i + 1 if i < len(groups) / 2 else i - 1
        j = min(max(j, 0), len(groups) - 1)
        groups[min(i, j)] += groups[max(i, j)]; groups.pop(max(i, j)); groups[min(i, j)].sort()
    return {k: new for new, g in enumerate(groups, 1) for k in g}
 
 
# ---------------------------------------------------------------- 3. run
def main(data_dir, out_dir):
    os.makedirs(out_dir, exist_ok=True)
    data, maps, direc, cats, accs = {}, [], [], [], []
    for mode in MODES:
        d, items, acc = load(mode, data_dir)
        d = d.assign(**{c + "_r": 6 - d[c] for c in REVERSE.get(mode, [])})
        data[mode] = (d, items)
        accs.append({"mode": mode, "n": int(acc.notna().sum()), "zero_%": round((acc == 0).mean() * 100, 1),
                     "mean": round(acc.mean(), 2), "variance": round(acc.var(), 2), "max": acc.max(),
                     "any_accident_n": int((acc > 0).sum())})
 
    # category counts (pooled minimum across modes for shared-wording items)
    def counts(mode, c):
        return data[mode][0][c].value_counts().reindex(values(c.removesuffix("_r")), fill_value=0)
    pooled = {}
    for c in {c for m in MODES for c in data[m][1] if c.startswith(SHARED)}:
        pooled[c] = pd.concat([counts(m, c) for m in MODES if c in data[m][1]], axis=1).min(axis=1)
 
    for mode in MODES:
        d, items = data[mode]
        fac = {i: f for f, its in FACTORS[mode].items() for i in its}
        risk = [c for c in items if re.fullmatch(r"(hab|pbc|rbf)\d+|att[1-6]", c)]
        out = d.drop(columns=items).copy()
        for c in items + [x + "_r" for x in REVERSE.get(mode, [])]:
            base = c.removesuffix("_r")
            role = (fac.get(c) or ("single indicator" if c in SINGLE[mode] else
                    "excluded" if base in EXCLUDE.get(mode, {}) else
                    "replaced by reversed copy" if base in REVERSE.get(mode, []) and c == base else
                    "descriptive only (non-focal mode)"))
            reason = EXCLUDE.get(mode, {}).get(c) or NOTES.get((mode, base), "")
            if c.endswith("_r"): reason = "reversed: wording runs in the safe direction"
            maps.append({"mode": mode, "item": c, "role": role, "note": reason})
            if role in ("excluded", "replaced by reversed copy"):
                continue
            r_risk = d[c].corr(d[[x for x in risk if x != c]].mean(axis=1), method="spearman")
            r_rest = (d[c].corr(d[[x for x in FACTORS[mode][role] if x != c]].mean(axis=1), method="spearman")
                      if role in FACTORS[mode] else np.nan)
            direc.append({"mode": mode, "role": role, "item": c, "item_rest_r": round(r_rest, 2),
                          "r_with_risky_core": round(r_risk, 2), "flag": "item-rest < .30" if r_rest < .30 else ""})
            own = counts(mode, c)
            cnt = pooled.get(c, own)
            mp = merge_map(cnt)
            out[c] = d[c].map(mp) if len(set(mp.values())) < len(mp) else d[c]   # keep codes when nothing merged
            flags = []
            if own.max() / max(own.sum(), 1) > .80: flags.append("> 80% in one category")
            if len(own) == 5 and own.idxmax() == 1 and own.iloc[-1] > own.iloc[-2] and own.iloc[-1] > own.iloc[2]:
                flags.append("rises again at top category")
            cats.append({"mode": mode, "item": c, "role": role, **{f"n_{k}": int(v) for k, v in own.items()},
                         "missing": int(d[c].isna().sum()),
                         "merge": "" if len(set(mp.values())) == len(mp) else
                                  " | ".join("+".join(str(k) for k in mp if mp[k] == g) for g in sorted(set(mp.values()))),
                         "basis": "pooled across modes" if c in pooled else "this mode", "flag": "; ".join(flags)})
        out.to_csv(os.path.join(out_dir, f"{mode}_items_ready.csv"), index=False)
 
    with pd.ExcelWriter(os.path.join(out_dir, "item_prep.xlsx")) as xw:
        for name, rows in [("item_map", maps), ("direction_check", direc), ("categories", cats), ("accidents", accs)]:
            pd.DataFrame(rows).to_excel(xw, sheet_name=name, index=False)
    c = pd.DataFrame(cats)
    print(c.groupby("mode").agg(items=("item", "size"), merged=("merge", lambda s: (s != "").sum()),
                                flagged=("flag", lambda s: (s != "").sum())).to_string())
 
 
if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("--data_dir", default="cleaned")
    ap.add_argument("--out_dir", default="ItemPrep")
    a = ap.parse_args()
    main(a.data_dir, a.out_dir)
 