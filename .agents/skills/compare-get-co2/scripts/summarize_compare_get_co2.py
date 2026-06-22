#!/usr/bin/env python3
"""Summarize a cached get_co2 comparison directory as Markdown."""

from __future__ import annotations

import argparse
import csv
from pathlib import Path
from typing import Iterable


def read_metadata(path: Path) -> dict[str, str]:
    metadata_file = path / "metadata.csv"
    if not metadata_file.exists():
        raise SystemExit(f"Missing metadata file: {metadata_file}")

    with metadata_file.open(newline="", encoding="utf-8") as handle:
        rows = csv.DictReader(handle)
        return {
            row.get("key", ""): row.get("value", "")
            for row in rows
            if row.get("key")
        }


def read_table(path: Path) -> list[dict[str, str]]:
    if not path.exists():
        return []

    with path.open(newline="", encoding="utf-8") as handle:
        return list(csv.DictReader(handle))


def to_float(value: str | None) -> float | None:
    if value is None or value == "":
        return None
    try:
        return float(value)
    except ValueError:
        return None


def fmt_number(value: str | None, scale: float = 1.0, digits: int = 2) -> str:
    number = to_float(value)
    if number is None:
        return "NA"
    return f"{number / scale:,.{digits}f}"


def fmt_pct(value: str | None) -> str:
    number = to_float(value)
    if number is None:
        return "NA"
    return f"{number:,.2f}%"


def top_abs(rows: Iterable[dict[str, str]], field: str, limit: int) -> list[dict[str, str]]:
    return sorted(
        rows,
        key=lambda row: abs(to_float(row.get(field)) or 0.0),
        reverse=True,
    )[:limit]


def metric_range(
    rows: list[dict[str, str]],
    field: str,
    scale: float = 1.0,
    digits: int = 2,
) -> str:
    values = [value for row in rows if (value := to_float(row.get(field))) is not None]
    if not values:
        return "NA"
    return f"{min(values) / scale:,.{digits}f} to {max(values) / scale:,.{digits}f}"


def table_status(path: Path) -> str:
    return "present" if path.exists() else "missing"


def no_rows_message() -> str:
    return "- No rows available."


def summarize(comparison_dir: Path, limit: int) -> str:
    metadata = read_metadata(comparison_dir)
    eu = read_table(comparison_dir / "comparison_totals_eu.csv")
    eu_components = read_table(comparison_dir / "comparison_totals_eu_component.csv")
    countries = read_table(comparison_dir / "comparison_totals_country.csv")
    components = read_table(comparison_dir / "comparison_totals_component.csv")
    monthly_eu_component = read_table(comparison_dir / "monthly_eu_component_metrics.csv")
    monthly_country = read_table(comparison_dir / "monthly_country_metrics.csv")
    monthly_component = read_table(comparison_dir / "monthly_component_metrics.csv")

    lines: list[str] = []
    base_label = f"{metadata.get('base_ref', 'base')} ({metadata.get('base_short_sha', 'unknown')})"
    target_label = (
        f"{metadata.get('target_ref', 'target')} "
        f"({metadata.get('target_short_sha', 'unknown')})"
    )

    lines.append("# get_co2 Comparison Summary")
    lines.append("")
    lines.append(f"- Directory: `{comparison_dir}`")
    lines.append(f"- Base: {base_label}")
    lines.append(f"- Target: {target_label}")
    lines.append(f"- date_to: {metadata.get('date_to', 'unknown')}")
    if metadata.get("runner"):
        lines.append(f"- runner: `{metadata['runner']}`")
    if metadata.get("target_worktree_hash"):
        lines.append(f"- target_worktree_hash: `{metadata['target_worktree_hash']}`")
    if metadata.get("driver_script_hash"):
        lines.append(f"- driver_script_hash: `{metadata['driver_script_hash']}`")
    if metadata.get("report_hash"):
        lines.append(f"- report_hash: `{metadata['report_hash']}`")
    if metadata.get("base_run_hash") or metadata.get("target_run_hash"):
        lines.append(
            "- run_hashes: "
            f"base `{metadata.get('base_run_hash', 'unknown')}`, "
            f"target `{metadata.get('target_run_hash', 'unknown')}`"
        )

    lines.append("")
    lines.append("## EU Total")
    if eu:
        row = eu[0]
        lines.append(
            "- Diff: "
            f"{fmt_number(row.get('diff'), scale=1_000_000)} Mt CO2 "
            f"({fmt_pct(row.get('pct_diff'))})"
        )
        lines.append(
            "- Compared rows: "
            f"{row.get('n_compared', 'NA')} of base {row.get('n_base', 'NA')} "
            f"and target {row.get('n_target', 'NA')}"
        )
    else:
        lines.append("- `comparison_totals_eu.csv` is missing.")

    lines.append("")
    lines.append(f"## Top EU Component Diffs ({min(limit, len(eu_components))})")
    if eu_components:
        for row in top_abs(eu_components, "diff", limit):
            label = " / ".join(
                part
                for part in [row.get("fuel"), row.get("sector")]
                if part
            )
            lines.append(
                f"- {label or 'unknown'}: "
                f"{fmt_number(row.get('diff'), scale=1_000_000)} Mt CO2 "
                f"({fmt_pct(row.get('pct_diff'))})"
            )
    elif (comparison_dir / "comparison_totals_eu_component.csv").exists():
        lines.append(no_rows_message())
    else:
        lines.append("- `comparison_totals_eu_component.csv` is missing.")

    lines.append("")
    lines.append(f"## Top Country Diffs ({min(limit, len(countries))})")
    if countries:
        for row in top_abs(countries, "diff", limit):
            lines.append(
                f"- {row.get('iso2', 'unknown')}: "
                f"{fmt_number(row.get('diff'), scale=1_000_000)} Mt CO2 "
                f"({fmt_pct(row.get('pct_diff'))})"
            )
    elif (comparison_dir / "comparison_totals_country.csv").exists():
        lines.append(no_rows_message())
    else:
        lines.append("- `comparison_totals_country.csv` is missing.")

    lines.append("")
    lines.append(f"## Top Component Diffs ({min(limit, len(components))})")
    if components:
        for row in top_abs(components, "diff", limit):
            label = " / ".join(
                part
                for part in [row.get("iso2"), row.get("fuel"), row.get("sector")]
                if part
            )
            lines.append(
                f"- {label or 'unknown'}: "
                f"{fmt_number(row.get('diff'), scale=1_000_000)} Mt CO2 "
                f"({fmt_pct(row.get('pct_diff'))})"
            )
    elif (comparison_dir / "comparison_totals_component.csv").exists():
        lines.append(no_rows_message())
    else:
        lines.append("- `comparison_totals_component.csv` is missing.")

    lines.append("")
    lines.append("## Monthly Metrics")
    if monthly_country:
        lines.append(
            f"- Country RMSE range: {metric_range(monthly_country, 'rmse', scale=1_000_000)} Mt CO2"
        )
        lines.append(f"- Country R2 range: {metric_range(monthly_country, 'r2', digits=3)}")
    else:
        lines.append("- `monthly_country_metrics.csv` is missing.")
    if monthly_eu_component:
        lines.append(
            "- EU component RMSE range: "
            f"{metric_range(monthly_eu_component, 'rmse', scale=1_000_000)} Mt CO2"
        )
        lines.append(
            f"- EU component R2 range: {metric_range(monthly_eu_component, 'r2', digits=3)}"
        )
    else:
        lines.append("- `monthly_eu_component_metrics.csv` is missing.")
    if monthly_component:
        lines.append(
            "- Component RMSE range: "
            f"{metric_range(monthly_component, 'rmse', scale=1_000_000)} Mt CO2"
        )
        lines.append(f"- Component R2 range: {metric_range(monthly_component, 'r2', digits=3)}")
    else:
        lines.append("- `monthly_component_metrics.csv` is missing.")

    lines.append("")
    lines.append("## Artifact Status")
    for filename in [
        "metadata.csv",
        "complete.ok",
        "comparison_totals_eu.csv",
        "comparison_totals_eu_component.csv",
        "comparison_totals_country.csv",
        "comparison_totals_component.csv",
        "monthly_eu_component_metrics.csv",
        "monthly_country_metrics.csv",
        "monthly_component_metrics.csv",
    ]:
        lines.append(f"- `{filename}`: {table_status(comparison_dir / filename)}")

    return "\n".join(lines)


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Summarize a cached get_co2 comparison directory as Markdown."
    )
    parser.add_argument("comparison_dir", type=Path)
    parser.add_argument("--limit", type=int, default=5)
    args = parser.parse_args()

    if args.limit < 1:
        raise SystemExit("--limit must be at least 1")

    comparison_dir = args.comparison_dir
    if not comparison_dir.exists() or not comparison_dir.is_dir():
        raise SystemExit(f"Comparison directory does not exist: {comparison_dir}")

    print(summarize(comparison_dir, args.limit))


if __name__ == "__main__":
    main()
