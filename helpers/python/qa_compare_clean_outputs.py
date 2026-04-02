#!/usr/bin/env python3
import argparse
import csv
from pathlib import Path


def to_int(x):
    try:
        return int(x)
    except Exception:
        return 0


def to_float(x):
    try:
        return float(x)
    except Exception:
        return None


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--detail', required=True)
    parser.add_argument('--summary', required=True)
    parser.add_argument('--pass-file', required=True)
    parser.add_argument('--max-orig-only-keys', type=int, required=True)
    parser.add_argument('--max-new-only-keys', type=int, required=True)
    parser.add_argument('--max-unit-mismatch-common', type=int, required=True)
    parser.add_argument('--max-negative-new-mean-common', type=int, required=True)
    parser.add_argument('--max-abs-mean-rel-diff-over-threshold', type=int, required=True)
    parser.add_argument('--mean-rel-diff-threshold', type=float, required=True)
    args = parser.parse_args()

    detail_rows = []
    with open(args.detail, newline='') as f:
        reader = csv.DictReader(f)
        for row in reader:
            detail_rows.append(row)

    orig_only = sum(1 for r in detail_rows if to_int(r['orig_n']) > 0 and to_int(r['new_n']) == 0)
    new_only = sum(1 for r in detail_rows if to_int(r['new_n']) > 0 and to_int(r['orig_n']) == 0)
    common_rows = [r for r in detail_rows if to_int(r['orig_n']) > 0 and to_int(r['new_n']) > 0]
    unit_mismatch_common = sum(1 for r in common_rows if (r.get('orig_units') or '') != (r.get('new_units') or ''))
    negative_new_mean_common = sum(1 for r in common_rows if (to_float(r.get('new_mean')) is not None and to_float(r.get('new_mean')) < 0))
    abs_mean_rel_diff_over = sum(
        1 for r in common_rows
        if (to_float(r.get('mean_rel_diff')) is not None and abs(to_float(r.get('mean_rel_diff'))) > args.mean_rel_diff_threshold)
    )

    failures = []
    if orig_only > args.max_orig_only_keys:
        failures.append(f'orig_only_keys={orig_only} > {args.max_orig_only_keys}')
    if new_only > args.max_new_only_keys:
        failures.append(f'new_only_keys={new_only} > {args.max_new_only_keys}')
    if unit_mismatch_common > args.max_unit_mismatch_common:
        failures.append(f'unit_mismatch_common={unit_mismatch_common} > {args.max_unit_mismatch_common}')
    if negative_new_mean_common > args.max_negative_new_mean_common:
        failures.append(f'negative_new_mean_common={negative_new_mean_common} > {args.max_negative_new_mean_common}')
    if abs_mean_rel_diff_over > args.max_abs_mean_rel_diff_over_threshold:
        failures.append(
            f'abs_mean_rel_diff_over_{args.mean_rel_diff_threshold:g}={abs_mean_rel_diff_over} > '
            f'{args.max_abs_mean_rel_diff_over_threshold}'
        )

    print('QA_ORIG_ONLY_KEYS', orig_only)
    print('QA_NEW_ONLY_KEYS', new_only)
    print('QA_UNIT_MISMATCH_COMMON', unit_mismatch_common)
    print('QA_NEGATIVE_NEW_MEAN_COMMON', negative_new_mean_common)
    print('QA_ABS_MEAN_REL_DIFF_OVER_THRESHOLD', abs_mean_rel_diff_over)

    if failures:
        for msg in failures:
            print('QA_FAIL', msg)
        raise SystemExit(1)

    pass_path = Path(args.pass_file)
    pass_path.parent.mkdir(parents=True, exist_ok=True)
    pass_path.write_text('qa_pass\n', encoding='utf-8')
    print('QA_PASS_FILE', pass_path)


if __name__ == '__main__':
    main()
