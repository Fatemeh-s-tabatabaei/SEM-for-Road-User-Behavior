"""
Reads the cleaned model data (*_model_data.csv), performs category merging for sparse answers (< 5 responses), 
calculates pre-merge item-rest direction checks, and prepares the final *_items_ready.csv datasets.
"""
import argparse
import re
from pathlib import Path
from typing import NamedTuple

import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

# baseline functions copied here from the cleaning script.
MODES = ["Cyclist", "Pedestrian", "Driver", "Motorcyclist"]

# mode -> (Plural Label for Figures, Focal Safety Item)
MODE_INFO = {
    "Cyclist": ("Cyclists", "psafe_bike"),
    "Pedestrian": ("Pedestrians", "psafe_walk"),
    "Driver": ("Drivers", "psafe_car"),
    "Motorcyclist": ("Motorcyclists", "psafe_motor"),
}

MIN_CATEGORY_N = 5
MIN_FACTOR_ITEMS = 3
ONE_CATEGORY_MAX = 0.80
ITEM_REST_MIN = 0.30
DROP_DUPLICATES = True

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


def group_of(col: str) -> str | None:
    if col in ["psafe_walk", "psafe_bike", "psafe_motor", "psafe_taxi", "psafe_metro", "psafe_bus", "psafe_car"]:
        return "psafe"
    m = re.match(r"^([a-z]+)\d+(_r)?$", col)
    return m.group(1) if m else None

def items_of(df: pd.DataFrame, prefix: str) -> list[str]:
    pat = re.compile(rf"^{prefix}\d+$")
    return [c for c in df.columns if pat.match(c)]

def decided(mode: str, action: str) -> list[str]:
    return [i for i, (a, _) in REVERSE.get(mode, {}).items() if a == action]

# ===============================================================================================
# new code here for preparation of items, merging categories, and checking item-rest correlations

def item_map(model: pd.DataFrame, mode: str) -> pd.DataFrame:
    # Factor items get their group name in capitals as role (ATT, SN, ...).
    # Exceptions come from REVERSE, the own-mode safety item and the MIN_FACTOR_ITEMS rule.
    
    focal = MODE_INFO[mode][1]
    reversals = REVERSE.get(mode, {})
    rows = []
    
    for c in model.columns:
        g = group_of(c)
        if g not in GROUPS or c.endswith("_r"):
            continue
            
        # Get the reason if it's reversed, otherwise check general notes, else NaN
        note = reversals.get(c, NOTES_ALL_MODES.get(c, np.nan))
        
        if c in reversals:
            role = "replaced by reversed copy"
        elif g == "psafe":
            role = "single indicator" if c == focal else "descriptive only (non-focal mode)"
        elif not GROUPS[g].factor or len(items_of(model, g)) < MIN_FACTOR_ITEMS:
            role = "single indicator"
        else:
            role = g.upper()
            
        rows.append({"mode": mode, "item": c, "role": role, "note": note})
        
    for c in reversals:
        rows.append({"mode": mode, "item": c + "_r", "role": "single indicator",
                     "note": "reversed: wording runs in the safe direction"})
                     
    return pd.DataFrame(rows)


def counts(s: pd.Series, cats) -> np.ndarray:
    return np.array([(s == k).sum() for k in cats])

# combine sparse categories with their adjacent neighbors
def merge_plan(count_rows: list[np.ndarray]) -> list[list[int]]:
    """Merge categories with < MIN_CATEGORY_N answers (in ANY of the count rows)
    into the neighbour towards the scale midpoint (3);
    a sparse group centred on 3 joins its smaller neighbour."""
    groups = [[1], [2], [3], [4], [5]]
    total = np.sum(count_rows, axis=0)

    def gsum(row, g): # sum of the counts in a category group g (or scale 1:5) for a given row
        return sum(row[k - 1] for k in g)

    while len(groups) > 2:
        sparse = [i for i, g in enumerate(groups)
                  if min(gsum(r, g) for r in count_rows) < MIN_CATEGORY_N] # minimum of people chosing this option
        if not sparse:
            break
        i = sparse[0]
        centre = np.mean(groups[i])
        if i == 0:
            j = 1
        elif i == len(groups) - 1:
            j = i - 1
        elif centre < 3:
            j = i + 1
        elif centre > 3:
            j = i - 1
        else:
            j = i - 1 if gsum(total, groups[i - 1]) <= gsum(total, groups[i + 1]) else i + 1
        a, b = sorted((i, j))
        groups[a:b + 1] = [groups[a] + groups[b]]
    return groups


def plan_text(groups) -> str | float: # return how many categories were merged, or NaN if all 5 remain
    if len(groups) == 5:
        return np.nan
    return " | ".join("+".join(map(str, g)) for g in groups)


def recode_with(s: pd.Series, groups) -> pd.Series: # recode a series according to the given category groups
    mapping = {k: new for new, g in enumerate(groups, start=1) for k in g}
    return s.map(mapping)


def spearman(a: pd.Series, b: pd.Series) -> float:
    return a.corr(b, method="spearman")


def prepare_items(models: dict[str, pd.DataFrame], out_dir: Path):
    maps = {m: item_map(models[m], m) for m in MODES}
    analysed = {m: maps[m][~maps[m]["role"].isin(["excluded", "replaced by reversed copy"])]
                for m in MODES}
    
    plans, cat_rows = {}, []
    for m in MODES:
        for _, r in analysed[m].iterrows():
            it = r["item"]
            if it.startswith("aenf"):
                continue
            pooled = GROUPS[group_of(it)].shared
            rows = ([counts(models[k][it], range(1, 6)) for k in MODES if it in models[k]]
                    if pooled else [counts(models[m][it], range(1, 6))])
            plans[(m, it)] = (merge_plan(rows), "pooled across modes" if pooled else "this mode")

    ready = {}
    for m in MODES:
        mod = models[m]
        prep = mod.copy()
        for _, r in analysed[m].iterrows():
            it = r["item"]
            s = mod[it]
            if it.startswith("aenf"):
                n = counts(s, [0, 1])
                share = n.max() / n.sum() if n.sum() > 0 else 0
                cat_rows.append({"mode": m, "item": it, "role": r["role"],
                                 "n_1": n[1], "missing": int(s.isna().sum()),
                                 "merge": np.nan, "basis": "pooled across modes",
                                 "flag": "> 80% in one category" if share > ONE_CATEGORY_MAX else np.nan,
                                 "n_0": n[0]})
                continue
            groups, basis = plans[(m, it)]
            n = counts(s, range(1, 6))
            flags = []
            if n.sum() > 0 and n.max() / n.sum() > ONE_CATEGORY_MAX:
                flags.append("> 80% in one category")
            if n[0] >= n.max() and n[4] > n[3] and n[4] > n[2]:
                flags.append("rises again at top category")
            cat_rows.append({"mode": m, "item": it, "role": r["role"],
                             **{f"n_{k}": n[k - 1] for k in range(1, 6)},
                             "missing": int(s.isna().sum()), "merge": plan_text(groups),
                             "basis": basis, "flag": "; ".join(flags) or np.nan})
            if len(groups) < 5:
                prep[it] = recode_with(s, groups)
        ready[m] = prep
    categories = pd.DataFrame(cat_rows)

    # item-rest and direction check
    dir_rows = []
    for m in MODES:
        prep = models[m]
        core_items = [i for i, role in zip(analysed[m]["item"], analysed[m]["role"])
                      if role in FACTOR_ROLES and GROUPS[group_of(i)].core]
        for _, r in analysed[m].iterrows():
            it, role = r["item"], r["role"]
            ir = np.nan
            if role in FACTOR_ROLES:
                others = analysed[m].loc[(analysed[m]["role"] == role)
                                         & (analysed[m]["item"] != it), "item"]
                if not others.empty:
                    ir = spearman(prep[it], prep[list(others)].mean(axis=1))
            
            rc = np.nan
            if core_items:
                core = prep[[c for c in core_items if c != it]].mean(axis=1)
                rc = spearman(prep[it], core)
                
            dir_rows.append({"mode": m, "role": role, "item": it,
                             "item_rest_r": round(ir, 2) if pd.notna(ir) else np.nan, 
                             "r_with_risky_core": round(rc, 2) if pd.notna(rc) else np.nan,
                             "flag": "item-rest < .30" if pd.notna(ir) and ir < ITEM_REST_MIN else np.nan})
    direction = pd.DataFrame(dir_rows)

    # accidents overview 
    acc = pd.DataFrame([{
        "mode": m, "n": len(models[m]),
        "zero_%": round(100 * (models[m]["acc_total_rec"] == 0).mean(), 1) if "acc_total_rec" in models[m] else np.nan,
        "mean": round(models[m]["acc_total_rec"].mean(), 2) if "acc_total_rec" in models[m] else np.nan,
        "variance": round(models[m]["acc_total_rec"].var(), 2) if "acc_total_rec" in models[m] else np.nan,
        "max": int(models[m]["acc_total_rec"].max()) if "acc_total_rec" in models[m] and not models[m]["acc_total_rec"].isna().all() else np.nan,
        "any_accident_n": int(models[m]["acc_any"].sum()) if "acc_any" in models[m] else np.nan,
    } for m in MODES])

    # write results
    with pd.ExcelWriter(out_dir / "item_prep.xlsx") as xw:
        pd.concat(maps.values()).to_excel(xw, sheet_name="item_map", index=False)
        direction.to_excel(xw, sheet_name="direction_check", index=False)
        categories.to_excel(xw, sheet_name="categories", index=False)
        acc.to_excel(xw, sheet_name="accidents", index=False)

    for m in MODES:
        items = list(analysed[m]["item"])
        meta = [c for c in models[m].columns
                if c not in items and c not in maps[m]["item"].values]
        out = ready[m][meta + items]
        if DROP_DUPLICATES and "dup_of" in out.columns:
            out = out[out["dup_of"].isna()]
        out.to_csv(out_dir / f"{m}_items_ready.csv", index=False)

    for m in MODES:
        plot_distributions(m, models[m], analysed[m], categories, direction,
                           out_dir / f"fig_distributions_{m}.png")
    return maps, categories, direction


# =============================================================================
# Figure: response distribution of measurement-model items
# =============================================================================

def plot_distributions(mode, model, analysed, categories, direction, path):

    cat = categories[categories["mode"] == mode].set_index("item")
    drc = direction[direction["mode"] == mode].set_index("item")
    sections = []
    for g, grp in GROUPS.items():
        label = grp.label
        its = list(analysed.loc[analysed["role"] == g.upper(), "item"])
        if its:
            sections.append((label, its))

    colors = ["#d4d9e8", "#a6bddb", "#6fa3cc", "#2b7bba", "#08427e"]
    n_rows = sum(len(its) + 1.4 for _, its in sections)
    
    if n_rows == 0:
        return
        
    fig, ax = plt.subplots(figsize=(11, 0.26 * n_rows + 2.2))
    y, yt, yl, heads = 0.0, [], [], []
    for label, its in sections:
        heads.append((y, label))
        y += 1.0
        for it in its:
            s = model[it].dropna()
            pct = np.array([(s == k).mean() for k in range(1, 6)]) if len(s) > 0 else np.zeros(5)
            left = 0.0
            for k in range(5):
                if pct[k] > 0:
                    ax.barh(y, pct[k], left=left, color=colors[k], height=0.78,
                            edgecolor="white", linewidth=0.6)
                left += pct[k]
            tag = it
            if isinstance(cat.at[it, "merge"], str):
                tag += "\u2020"
            if drc.at[it, "flag"] == "item-rest < .30":
                tag += "*"
            yt.append(y); yl.append(tag)
            y += 1.0
        y += 0.4
        
    ax.set_yticks(yt, yl, fontsize=9)
    for yy, label in heads:
        ax.text(-0.012, yy, label, ha="right", va="center", fontsize=10,
                fontweight="bold", transform=ax.get_yaxis_transform())
    ax.set_ylim(y - 0.4, -1.0)
    ax.set_xlim(0, 1)
    ax.set_xticks([0, .25, .5, .75, 1], ["0%", "25%", "50%", "75%", "100%"])
    
    for x in (.25, .5, .75):
        ax.axvline(x, color="#dddddd", lw=0.8, zorder=0)
    for side in ("top", "right", "left"):
        ax.spines[side].set_visible(False)
        
    ax.tick_params(axis="y", length=0)
    ax.set_title(f"{MODE_INFO[mode][0]} (n = {len(model)}): response distribution of "
                 "measurement-model items", loc="left", fontsize=12, fontweight="bold")
                 
    handles = [plt.Rectangle((0, 0), 1, 1, color=c) for c in colors]
    fig.legend(handles, list("12345"), title="Response category (1 = lowest, 5 = highest)",
               loc="lower left", ncol=5, frameon=False, bbox_to_anchor=(0.06, 0.025),
               title_fontsize=9, fontsize=10)
    fig.text(0.01, 0.005,
             "1 = never / unacceptable / strongly disagree / not important / very unlikely;  "
             "5 = always / acceptable / strongly agree / very important / very likely\n"
             "\u2020 category with fewer than 5 answers merged before modelling      "
             "* item-rest correlation below .30", fontsize=8, color="#444444")
             
    fig.tight_layout(rect=(0, 0.06, 1, 1))
    fig.savefig(path, dpi=150)
    plt.close(fig)


# =============================================================================
# MAIN
# =============================================================================

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--in-dir", default=Path("CleaningResults"), type=Path,
                        help="Directory containing the *_model_data.csv files from Part A")
    parser.add_argument("--out-dir", default=Path("ItemPrep"), type=Path,
                        help="Directory to save the *_items_ready.csv files and plots")
    args = parser.parse_args()

    args.out_dir.mkdir(parents=True, exist_ok=True)
    
    models = {}
    for mode in MODES:
        csv_path = args.in_dir / f"{mode}_model_data.csv"
        if not csv_path.exists():
            print(f"Warning: Could not find {csv_path}. Make sure Part A was run successfully.")
            continue
        # Read the cleaned model data from Part A
        models[mode] = pd.read_csv(csv_path)

    if models:
        print(f"Loaded {len(models)} modes. Starting Item Preparation...")
        prepare_items(models, args.out_dir)
        print(f"Done! Check the '{args.out_dir.resolve()}' folder for results.")
    else:
        print(f"No *_model_data.csv data found in {args.in_dir.resolve()}.")

if __name__ == "__main__":
    main()