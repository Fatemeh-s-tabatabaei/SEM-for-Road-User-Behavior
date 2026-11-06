"""StraightlineCheck.py -- how lenient is the straightline flag?

For each group, from the ready files:
  * how many respondents the current `straightline` flag removes
  * the distribution of sl_blocks (number of item blocks answered with one
    identical value), and how many would be removed at each threshold
  * which sl_blocks values the current flag corresponds to
  * how often each block is straight-lined

Use it to pick `sl_max_blocks` in common.R for a sensitivity run. A block of
identical answers can be honest (someone who never uses a phone while riding),
so the point is to check whether results change, not to delete everyone.

Output: Screening/straightline_check.csv and Screening/straightline_blocks.csv
"""
from pathlib import Path

import pandas as pd

SRC = Path("ItemPrep")
OUT = Path("Screening")
GROUPS = ("cyclist", "pedestrian", "driver", "motorcyclist")


def read_group(g):
    path = SRC / f"{g.capitalize()}_items_ready.csv"
    if not path.exists():
        print(f"  missing: {path}")
        return None
    for enc in ("utf-8-sig", "cp1256"):
        try:
            return pd.read_csv(path, encoding=enc)
        except UnicodeDecodeError:
            pass
    raise SystemExit(f"Could not decode {path}")


def main():
    OUT.mkdir(exist_ok=True)
    rows, blocks = [], []
    for g in GROUPS:
        d = read_group(g)
        if d is None:
            continue
        if "dup_of" in d:
            d = d[d["dup_of"].isna()]
        if "sl_blocks" not in d:
            print(f"  {g}: no sl_blocks column -- skipped")
            continue
        n = len(d)
        sl = pd.to_numeric(d.get("sl_blocks"), errors="coerce").fillna(0).astype(int)
        flag = pd.to_numeric(d.get("straightline"), errors="coerce").fillna(0) == 1
        names = d.get("sl_block_names", pd.Series("", index=d.index)).fillna("")
        listed = names.str.split(",").explode().str.strip()
        listed = listed[listed != ""]
        n_blocks = listed.nunique()

        print(f"\n{g}: N = {n}, blocks seen straight-lined = {n_blocks}, "
              f"current flag removes {int(flag.sum())}")
        if flag.any():
            print(f"  flagged rows have sl_blocks {sorted(int(v) for v in sl[flag].unique())}; "
                  f"unflagged max = {sl[~flag].max()}")
        for k in range(1, max(int(sl.max()), 1) + 1):
            removed = int((sl >= k).sum())
            rows.append([g, n, n_blocks, k, int((sl == k).sum()), removed,
                         round(100 * removed / n, 1), int(flag.sum())])
        for b, c in listed.value_counts().items():
            blocks.append([g, b, int(c), round(100 * c / n, 1)])

    t = pd.DataFrame(rows, columns=["group", "n", "blocks_seen", "k", "n_with_exactly_k",
                                    "n_removed_if_drop_sl_blocks_ge_k", "pct_removed",
                                    "removed_by_current_flag"])
    b = pd.DataFrame(blocks, columns=["group", "block", "n_straightlined", "pct"])
    t.to_csv(OUT / "straightline_check.csv", index=False)
    b.to_csv(OUT / "straightline_blocks.csv", index=False)
    print("\n" + t.to_string(index=False))
    print("\nSaved:", (OUT / "straightline_check.csv").resolve(), "and straightline_blocks.csv")


if __name__ == "__main__":
    main()