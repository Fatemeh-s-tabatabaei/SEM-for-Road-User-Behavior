"""PruningCandidates.py -- apply a pruning rule, written down in advance, to the
screening and CFA output, and propose (not apply) item drops.

Rules (constants can be adjusted and the outputs shall be interpreted):
  R1 weak item     single-factor loading < LOW_LOADING and item-rest r < WEAK_R
                   in every screening sample  -> drop candidate
                   only one of the two        -> review
                   no CFA loading (e.g. _r items the old CFA.R skipped) and
                   item-rest < WEAK_R in every sample -> drop candidate
  R2 near-duplicate  within-factor residual correlation with MI >= MI_MIN and
                   |sepc.all| >= SEPC_MIN in a multi-factor model -> the item of
                   the pair with the lower single-factor loading is a candidate
  R3 scale size    a factor may not drop below MIN_KEEP items; candidates that
                   would break this are marked "blocked"
  R4 AE scale      binary items, item-rest near zero in every group: not a
                   reflective scale -> keep out of CFA, use as single items

Every proposal needs a content check: an item that is weak but essential to
what the construct means should stay. Approved item rows to drop are copied to
drop_items.csv (columns group,item), which common.R reads.

Inputs : Screening/item_rest.csv, cfa_results/<Group>/{loadings,modindices}.csv
Outputs: Screening/pruning_candidates.csv, Screening/drop_items_PROPOSED.csv\

Running order:
CFA.R  →  Pruningcandidates.py  →  review proposals  →  fill drop_items.csv  →  CFA.R again
"""
import argparse
import re
from pathlib import Path

import pandas as pd

SCREEN = Path("Screening") / "item_rest.csv"
CFA_DIR = Path("cfa_results") / "no_dups"
OUT = Path("Screening") / "pruning_candidates.csv"
OUT_DROP = Path("Screening") / "drop_items_PROPOSED.csv"

LOW_LOADING, WEAK_R = 0.40, 0.30
MI_MIN, SEPC_MIN = 50, 0.30
MIN_KEEP = 4          # 3 is the floor for identification; 4 keeps fit testable
REFLECTIVE_EXCLUDE = {"AE"}   # screening construct labels to treat under R4

PREFIX_TO_FACTOR = {"att": "ATT", "sn": "SN", "pbc": "PBC", "hab": "HAB",
                    "int": "INT", "sa": "SA", "pa": "PA", "rbf": "RBF",
                    "nbv": "NBV", "infra": "INFRA", "mr": "MR",
                    "cause": "CAUSE", "penf": "PENF", "aenf": "AE"}
ITEM_RE = re.compile(r"^([a-z]+?)(\d+)(_r)?$")


def factor_of(item):
    m = ITEM_RE.match(str(item))
    return PREFIX_TO_FACTOR.get(m[1]) if m else None


def read_csv(path):
    for enc in ("utf-8-sig", "cp1256"):
        try:
            return pd.read_csv(path, encoding=enc)
        except UnicodeDecodeError:
            pass
    raise SystemExit(f"Could not decode {path}")


def load_cfa(kind):
    frames = []
    for d in sorted(CFA_DIR.glob("*")):
        f = d / f"{kind}.csv"
        if f.exists():
            frames.append(read_csv(f))
    if not frames:
        return pd.DataFrame()
    x = pd.concat(frames, ignore_index=True)
    x["group"] = x["group"].str.lower()
    return x


def main():
    global CFA_DIR
    ap = argparse.ArgumentParser(description="Propose item drops")
    ap.add_argument("--cfa-dir", type=Path, default=CFA_DIR,
                    help="folder holding <Group>/loadings.csv, e.g. cfa_results/clean")
    CFA_DIR = ap.parse_args().cfa_dir
    if not SCREEN.exists():
        raise SystemExit(f"{SCREEN} not found -- run Correlations.py first")
    scr = read_csv(SCREEN)
    scr["group"] = scr["group"].str.lower()
    scr["factor"] = scr["item"].map(factor_of)

    # item-rest summary across samples
    ir = (scr.groupby(["group", "factor", "construct", "item"])["r"]
             .agg(r_min="min", r_max="max").reset_index())

    load = load_cfa("loadings")
    if not load.empty:
        one = load[load["model"].str.match(r"^M[01]_one_")].copy()
        one["_pref"] = one["model"].str.startswith("M0")
        one = (one.sort_values("_pref", ascending=False)
                  [["group", "item", "std_loading"]]
                  .drop_duplicates(["group", "item"]))
        ir = ir.merge(one, on=["group", "item"], how="left")
        no_cfa = sorted(set(ir["group"]) - set(one["group"]))
        if no_cfa:
            print(f"WARNING: no CFA loadings for {no_cfa}: "
                  "R1 uses item-rest only, R2 skipped")
    else:
        ir["std_loading"] = float("nan")
        print("No cfa_results/*/loadings.csv found: R1 uses item-rest only")

    rows = []
    # R4
    for _, x in ir[ir["construct"].isin(REFLECTIVE_EXCLUDE)].iterrows():
        rows.append([x.group, x.factor, x["item"], "R4 not a reflective scale",
                     f"item-rest {x.r_min:.2f} to {x.r_max:.2f}", "exclude from CFA"])
    ir = ir[~ir["construct"].isin(REFLECTIVE_EXCLUDE)]

    # R1
    for _, x in ir.iterrows():
        weak_r = x.r_max < WEAK_R
        has_l = pd.notna(x.std_loading)
        weak_l = has_l and x.std_loading < LOW_LOADING
        ev = f"item-rest {x.r_min:.2f} to {x.r_max:.2f}" + \
             (f"; loading {x.std_loading:.2f}" if has_l else "; no CFA loading")
        if weak_r and (weak_l or not has_l):
            rows.append([x.group, x.factor, x["item"], "R1 weak item", ev, "drop candidate"])
        elif weak_r or weak_l:
            rows.append([x.group, x.factor, x["item"], "R1 weak item", ev, "review"])

    # R2
    mi = load_cfa("modindices")
    mi = mi[mi["model"].str.startswith("M0_")] if not mi.empty else mi
    if not mi.empty:
        res = mi[(mi["op"] == "~~") & (mi["mi"] >= MI_MIN) & (mi["sepc.all"].abs() >= SEPC_MIN)]
        res = res[res["lhs"].map(factor_of) == res["rhs"].map(factor_of)]
        res = res.sort_values("mi", ascending=False).drop_duplicates(["group", "lhs", "rhs"])
        lmap = ir.set_index(["group", "item"])["std_loading"].to_dict()
        for _, x in res.iterrows():
            la, lb = lmap.get((x.group, x.lhs)), lmap.get((x.group, x.rhs))
            if la is None or lb is None or pd.isna(la) or pd.isna(lb):
                cand = None
            else:
                cand = x.lhs if la < lb else x.rhs
            ev = (f"{x.lhs}~~{x.rhs} MI {x.mi:.0f}, sepc {x['sepc.all']:.2f} "
                  f"({x.model}); loadings {la if la is None else round(la, 2)} / "
                  f"{lb if lb is None else round(lb, 2)}")
            rows.append([x.group, factor_of(x.lhs), cand or f"{x.lhs} or {x.rhs}",
                         "R2 near-duplicate pair", ev,
                         "drop candidate" if cand else "review"])
        print("Note: modindices.csv keeps only the top 20 MIs per model, "
              "so R2 can miss pairs.")
    else:
        print("No cfa_results/*/modindices.csv found: R2 skipped")

    out = pd.DataFrame(rows, columns=["group", "factor", "item", "rule", "evidence", "proposal"])

    # R3: block candidates that would shrink a factor below MIN_KEEP
    size = ir.groupby(["group", "factor"])["item"].nunique().to_dict()
    is_cand = out["proposal"] == "drop candidate"
    order = out[is_cand].assign(_p=lambda d: d["rule"].str[:2])   # R1 before R2
    for (g, f), grp in order.sort_values("_p").groupby(["group", "factor"]):
        left = size.get((g, f), 0)
        dropped = set()
        for i, x in grp.iterrows():
            if x["item"] in dropped:
                continue
            if left - 1 < MIN_KEEP:
                out.loc[i, "proposal"] = f"blocked (would leave {left - 1} items)"
            else:
                left -= 1
                dropped.add(x["item"])

    out = out.drop_duplicates(["group", "item", "rule"])
    out = out.sort_values(["proposal", "group", "factor", "item"],
                          key=lambda c: c.ne("drop candidate") if c.name == "proposal" else c)
    out.to_csv(OUT, index=False)
    drops = out[out["proposal"] == "drop candidate"][["group", "item"]].drop_duplicates()
    drops.to_csv(OUT_DROP, index=False)

    with pd.option_context("display.width", 200, "display.max_colwidth", 70):
        print(out.to_string(index=False))
    print(f"\n{len(drops)} drop candidates -> {OUT_DROP}  (review, then copy approved "
          f"rows to drop_items.csv)")


if __name__ == "__main__":
    main()