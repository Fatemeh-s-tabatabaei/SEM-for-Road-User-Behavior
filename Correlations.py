import argparse
import re
import sys
from pathlib import Path

import numpy as np
import pandas as pd

SRC_DIR = Path("ItemPrep")
OUT_FILE = Path("Screening") / "item_rest.csv"
OUT_REVIEW = Path("Screening") / "item_rest_review.csv"
FILES = {g: f"{g.capitalize()}_items_ready.csv"
         for g in ("cyclist", "pedestrian", "driver", "motorcyclist")}
STEMS = {"rbf": "RBF", "nbv": "NVF", "sa": "SA", "pa": "PA", "sn": "SN",
         "att": "ATT", "pbc": "PBC", "hab": "HAB", "int": "INT", "mr": "MR",
         "infra": "ENV", "aenf": "AE", "cause": "PC", "penf": "PE"}
WEAK, BORDER, MIN_ITEMS = 0.25, 0.30, 3
ITEM_RE = re.compile(rf"^({'|'.join(STEMS)})(\d+)(?:_r)?$")
SAMPLES = {  # name -> function returning a boolean "drop" mask
    "all":    lambda d: pd.Series(False, index=d.index),
    "clean":  lambda d: d["dup_of"].notna() | (d["straightline"] == 1),
    "adults": lambda d: d["minor"] == 1,
}


def read_group(group):
    path = SRC_DIR / FILES[group]
    if not path.exists():
        sys.exit(f"File not found: {path.resolve()} (check SRC_DIR / FILES)")
    keep = lambda c: ITEM_RE.match(c) or c in ("dup_of", "straightline", "minor")
    for enc in ("utf-8-sig", "cp1256"):
        try:
            return pd.read_csv(path, usecols=keep, encoding=enc)
        except UnicodeDecodeError:
            pass
    sys.exit(f"Could not decode {path.name}")


def scales(df):
    """construct -> item columns, ordered by item number."""
    out = {}
    for c in df.columns:
        m = ITEM_RE.match(c)
        if m:
            out.setdefault(STEMS[m[1]], []).append((int(m[2]), c))
    return {k: [c for _, c in sorted(v)] for k, v in out.items()}


def item_rest(X):
    """n, r and r_rev (item reversed as 6 - x) for every column of X."""
    total, cnt = X.sum(axis=1), X.notna().sum(axis=1)
    rows = []
    for c in X:
        rest = (total - X[c].fillna(0)) / (cnt - X[c].notna())  # mean of others
        rows.append((c, (X[c].notna() & rest.notna()).sum(),
                     X[c].corr(rest, method="spearman"),
                     (6 - X[c]).corr(rest, method="spearman")))
    return rows


def review(res, base):
    """Items needing attention: one row per item, with level and reasons."""
    k3 = ["group", "construct", "item"]
    order = res.drop_duplicates(k3).set_index(k3).index
    r = res.pivot(index=k3, columns="sample", values="r").reindex(order)
    fl = (res.assign(flagged=res.flag != "")
             .pivot(index=k3, columns="sample", values="flagged").reindex(order))
    cols = list(res["sample"].unique())          # keep run order: main sample first
    r, fl = r[cols], fl[cols]
    others = [c for c in cols if c != base]
    share = fl[base].astype(float).groupby(level=["group", "construct"]).transform("mean")
    rows = []
    for idx in r.index:
        rb, why = r.loc[idx, base], []
        bad = bool(fl.loc[idx, base]) or pd.isna(rb)
        if pd.isna(rb):
            why.append(f"r undefined in {base} sample (no variation or too few cases)")
        elif bad:
            why.append(f"{'negative' if rb < 0 else 'weak'} in {base} sample ({rb:.3f})"
                       + ("; check keying" if rb < 0 else ""))
        elif rb < BORDER:
            why.append(f"borderline in {base} sample ({rb:.3f})")
        diff = [c for c in others if fl.loc[idx, c] != fl.loc[idx, base]]
        if diff:
            why.append("flag status differs in " +
                       ", ".join(f"{c} ({r.loc[idx, c]:.3f})" for c in diff))
        if bad and share.loc[idx] >= 0.5:
            why.append("most items of this scale are flagged (scale-level problem)")
        if why:
            rows.append([*idx, "investigate" if bad else "caution",
                         *r.loc[idx].round(3), "; ".join(why)])
    out = pd.DataFrame(rows, columns=k3 + ["level"] + [f"r_{c}" for c in r.columns]
                       + ["reasons"])
    return out.sort_values("level", key=lambda c: c.ne("investigate"), kind="stable")


def main():
    ap = argparse.ArgumentParser(description="Item-rest screening")
    ap.add_argument("--samples", nargs="+", default=list(SAMPLES),
                    choices=list(SAMPLES))
    args = ap.parse_args()

    out = []
    for g in FILES:
        raw = read_group(g)
        for s in args.samples:
            d = raw.loc[~SAMPLES[s](raw)]
            for k, cols in scales(raw).items():
                if len(cols) < MIN_ITEMS:
                    continue
                X = d[cols].apply(pd.to_numeric, errors="coerce")
                valid = [0, 1] if k == "AE" else [1, 2, 3, 4, 5]
                bad = int((X.notna() & ~X.isin(valid)).sum().sum())
                if bad:
                    print(f"  ! {g}/{s}/{k}: {bad} out-of-range values set to NA")
                    X = X.where(X.isin(valid))
                out += [(s, g, k, c, n, r, rr) for c, n, r, rr in item_rest(X)]
            print(f"{g:13s} {s:7s} N={len(d)}")

    res = pd.DataFrame(out, columns=["sample", "group", "construct", "item", "n", "r", "r_rev"])
    res[["r", "r_rev"]] = res[["r", "r_rev"]].round(3)
    res["flag"] = np.select([res.r < 0, res.r < WEAK], ["negative", "weak"], "")
    res.to_csv(OUT_FILE, index=False)
    rev = review(res, res["sample"].iloc[0])
    rev.to_csv(OUT_REVIEW, index=False)

    key = ["group", "item"]
    base = res[res["sample"] == res["sample"].iloc[0]]
    print(f"r_rev == -r on all {len(res)} rows: {bool(((res.r + res.r_rev).abs() < 1e-9).all())}")
    print(f"\nFlagged items ({base['sample'].iloc[0]} sample):")
    print(base.loc[base.flag != "", key + ["construct", "n", "r", "flag"]]
          .to_string(index=False))
    for s in args.samples[1:]:
        alt = res[res["sample"] == s]
        a, b = (set(map(tuple, t.loc[t.flag != "", key].values)) for t in (base, alt))
        print(f"\n{s}: flag changes vs {base['sample'].iloc[0]} -> "
              f"gained {sorted(b - a) or 'none'} | lost {sorted(a - b) or 'none'}")
    print(f"\nReview file: {(rev.level == 'investigate').sum()} items to investigate, "
          f"{(rev.level == 'caution').sum()} with caution")
    print("Saved:", OUT_FILE.resolve(), "and", OUT_REVIEW.name)


if __name__ == "__main__":
    main()