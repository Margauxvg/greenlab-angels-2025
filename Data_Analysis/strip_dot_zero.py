#!/usr/bin/env python3
"""
strip_dot_zero.py
Replace values in one CSV column so that trailing ".0" is removed when the value is an integer.
Examples:
  python strip_dot_zero.py input.csv -c time_ms -o output.csv
  python strip_dot_zero.py data.csv -c 3 -o cleaned.csv           # 0-based column index
Options:
  --sep       CSV separator (default ",")
  --decimal   Decimal mark in numbers (default ".")
  --encoding  File encoding (default "utf-8")
"""
import argparse
import sys
import math
import re
import pandas as pd

def strip_dot_zero_cell(x):
    """Return x with trailing .0 removed if x represents an integer value."""
    if pd.isna(x):
        return x
    # Try numeric path first
    try:
        # Convert to float safely from string or numeric
        f = float(x)
        if math.isfinite(f) and f.is_integer():
            return int(f)  # write as integer, no .0 in CSV
        return x
    except Exception:
        # Fall back to regex on the string representation
        s = str(x)
        m = re.fullmatch(r"\s*(-?\d+)\.0\s*", s)
        if m:
            return m.group(1)
        return x

def main():
    ap = argparse.ArgumentParser(description="Strip trailing .0 in a single CSV column.")
    ap.add_argument("input_csv", help="Path to input CSV")
    ap.add_argument("-c", "--column", required=True, help="Target column name or 0-based index")
    ap.add_argument("-o", "--output_csv", required=True, help="Path to write cleaned CSV")
    ap.add_argument("--sep", default=",", help="CSV separator (default ',')")
    ap.add_argument("--decimal", default=".", help="Decimal mark in numbers (default '.')")
    ap.add_argument("--encoding", default="utf-8", help="File encoding (default 'utf-8')")
    args = ap.parse_args()

    try:
        df = pd.read_csv(args.input_csv, sep=args.sep, decimal=args.decimal, encoding=args.encoding)
    except Exception as e:
        sys.exit(f"Failed to read CSV: {e}")

    # Resolve target column
    colnames = list(df.columns)
    target = args.column
    if target.isdigit() and int(target) < len(colnames):
        colname = colnames[int(target)]
    else:
        colname = target
        if colname not in df.columns:
            sys.exit(f"Column '{colname}' not found. Available columns: {colnames}")

    # Apply transform
    df[colname] = df[colname].apply(strip_dot_zero_cell)

    # Write output
    try:
        df.to_csv(args.output_csv, index=False, sep=args.sep, encoding=args.encoding)
    except Exception as e:
        sys.exit(f"Failed to write CSV: {e}")

if __name__ == "__main__":
    main()
