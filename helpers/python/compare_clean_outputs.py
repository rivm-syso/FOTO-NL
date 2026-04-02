#!/usr/bin/env python3
import argparse
import csv
from collections import defaultdict
from math import isfinite
from pathlib import Path


def agg_file(path):
    stats = {}
    with path.open(newline='') as f:
        reader = csv.DictReader(f, delimiter=';')
        for row in reader:
            ds = row['DataSource']
            substance = row['AquoCode']
            key = (ds, substance)
            try:
                val = float(row['MeasuredValue'])
            except Exception:
                continue
            if not isfinite(val):
                continue
            unit = row.get('Unit', '')
            rec = stats.get(key)
            if rec is None:
                rec = {
                    'n': 0,
                    'sum': 0.0,
                    'min': val,
                    'max': val,
                    'units': set(),
                }
                stats[key] = rec
            rec['n'] += 1
            rec['sum'] += val
            if val < rec['min']:
                rec['min'] = val
            if val > rec['max']:
                rec['max'] = val
            if unit:
                rec['units'].add(unit)
    return stats


def rec_to_row(ds, substance, orig, new):
    def unpack(rec):
        if rec is None:
            return (0, '', '', '', '', '')
        n = rec['n']
        mean = rec['sum'] / n if n else None
        mn = rec['min']
        mx = rec['max']
        span = mx - mn
        units = '|'.join(sorted(rec['units']))
        return (n, mean, mn, mx, span, units)

    orig_n, orig_mean, orig_min, orig_max, orig_span, orig_units = unpack(orig)
    new_n, new_mean, new_min, new_max, new_span, new_units = unpack(new)
    mean_diff = None if orig_mean in ('', None) or new_mean in ('', None) else new_mean - orig_mean
    min_diff = None if orig_min in ('', None) or new_min in ('', None) else new_min - orig_min
    max_diff = None if orig_max in ('', None) or new_max in ('', None) else new_max - orig_max
    span_diff = None if orig_span in ('', None) or new_span in ('', None) else new_span - orig_span
    mean_rel_diff = None
    if mean_diff is not None and orig_mean not in (0, None, ''):
        mean_rel_diff = mean_diff / orig_mean
    return {
        'DataSource': ds,
        'AquoCode': substance,
        'orig_n': orig_n,
        'new_n': new_n,
        'orig_mean': orig_mean,
        'new_mean': new_mean,
        'mean_diff': mean_diff,
        'mean_rel_diff': mean_rel_diff,
        'orig_min': orig_min,
        'new_min': new_min,
        'min_diff': min_diff,
        'orig_max': orig_max,
        'new_max': new_max,
        'max_diff': max_diff,
        'orig_span': orig_span,
        'new_span': new_span,
        'span_diff': span_diff,
        'orig_units': orig_units,
        'new_units': new_units,
    }


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--orig', required=True)
    parser.add_argument('--new', required=True)
    parser.add_argument('--outdir', required=True)
    args = parser.parse_args()

    orig_path = Path(args.orig)
    new_path = Path(args.new)
    out_dir = Path(args.outdir)
    out_dir.mkdir(parents=True, exist_ok=True)

    orig = agg_file(orig_path)
    new = agg_file(new_path)
    all_keys = sorted(set(orig) | set(new))
    rows = [rec_to_row(ds, substance, orig.get((ds, substance)), new.get((ds, substance))) for ds, substance in all_keys]

    detail_path = out_dir / 'clean_vs_original_by_board_substance.csv'
    with detail_path.open('w', newline='') as f:
        writer = csv.DictWriter(f, fieldnames=list(rows[0].keys()))
        writer.writeheader()
        writer.writerows(rows)

    board_summary = []
    by_board = defaultdict(list)
    for row in rows:
        by_board[row['DataSource']].append(row)
    for ds, items in sorted(by_board.items()):
        common = [r for r in items if r['orig_n'] and r['new_n']]
        only_orig = sum(1 for r in items if r['orig_n'] and not r['new_n'])
        only_new = sum(1 for r in items if r['new_n'] and not r['orig_n'])
        abs_mean_diffs = [abs(r['mean_diff']) for r in common if r['mean_diff'] is not None]
        abs_span_diffs = [abs(r['span_diff']) for r in common if r['span_diff'] is not None]
        board_summary.append({
            'DataSource': ds,
            'common_substances': len(common),
            'orig_only_substances': only_orig,
            'new_only_substances': only_new,
            'mean_abs_mean_diff': (sum(abs_mean_diffs) / len(abs_mean_diffs)) if abs_mean_diffs else None,
            'mean_abs_span_diff': (sum(abs_span_diffs) / len(abs_span_diffs)) if abs_span_diffs else None,
            'orig_total_rows': sum(r['orig_n'] for r in items),
            'new_total_rows': sum(r['new_n'] for r in items),
        })

    summary_path = out_dir / 'clean_vs_original_board_summary.csv'
    with summary_path.open('w', newline='') as f:
        writer = csv.DictWriter(f, fieldnames=list(board_summary[0].keys()))
        writer.writeheader()
        writer.writerows(board_summary)

    common_rows = [r for r in rows if r['orig_n'] and r['new_n'] and r['mean_diff'] is not None]
    common_rows_sorted = sorted(common_rows, key=lambda r: abs(r['mean_diff']), reverse=True)
    span_rows_sorted = sorted(common_rows, key=lambda r: abs(r['span_diff']) if r['span_diff'] is not None else -1, reverse=True)

    print('DETAIL_CSV', detail_path)
    print('SUMMARY_CSV', summary_path)
    print('TOTAL_KEYS', len(rows))
    print('COMMON_KEYS', len(common_rows))
    print('ORIG_ONLY_KEYS', sum(1 for r in rows if r['orig_n'] and not r['new_n']))
    print('NEW_ONLY_KEYS', sum(1 for r in rows if r['new_n'] and not r['orig_n']))
    print('TOP_MEAN_DIFFS_BEGIN')
    for r in common_rows_sorted[:20]:
        print(r['DataSource'], r['AquoCode'], r['orig_mean'], r['new_mean'], r['mean_diff'], sep='\t')
    print('TOP_MEAN_DIFFS_END')
    print('TOP_SPAN_DIFFS_BEGIN')
    for r in span_rows_sorted[:20]:
        print(r['DataSource'], r['AquoCode'], r['orig_span'], r['new_span'], r['span_diff'], sep='\t')
    print('TOP_SPAN_DIFFS_END')


if __name__ == '__main__':
    main()
