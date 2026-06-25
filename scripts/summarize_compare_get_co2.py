#!/usr/bin/env python3
"""Summarize a cached get_co2 comparison directory as Markdown."""

from __future__ import annotations

import argparse
import csv
from pathlib import Path
from typing import Iterable


IMAGE_SUFFIXES = {".png", ".jpg", ".jpeg", ".svg"}
PRIORITY_PLOTS = (
    Path("plots/eu_timeseries.png"),
    Path("plots/country_timeseries_small_multiples.png"),
    Path("plots/fuel_sector_month_scatter_small_multiples.png"),
)


def read_table(path: Path) -> list[dict[str, str]]:
    if not path.exists():
        return []
    with path.open(newline="", encoding="utf-8") as handle:
        return list(csv.DictReader(handle))


def to_float(value: str | None) -> float | None:
    if value in (None, ""):
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
    return sorted(rows, key=lambda row: abs(to_float(row.get(field)) or 0.0), reverse=True)[:limit]


def metric_range(rows: list[dict[str, str]], field: str, scale: float = 1.0, digits: int = 2) -> str:
    values = [value for row in rows if (value := to_float(row.get(field))) is not None]
    if not values:
        return "NA"
    return f"{min(values) / scale:,.{digits}f} to {max(values) / scale:,.{digits}f}"


def md_table(headers: list[str], rows: list[list[str]]) -> list[str]:
    if not rows:
        return ["_No rows available._"]
    lines = ["| " + " | ".join(headers) + " |", "| " + " | ".join(["---"] * len(headers)) + " |"]
    lines.extend("| " + " | ".join(row) + " |" for row in rows)
    return lines


def artifact_status(comparison_dir: Path) -> list[list[str]]:
    names = [
        "complete.ok", "summary.md",
        "comparison_totals_eu.csv", "comparison_totals_eu_component.csv",
        "comparison_totals_country.csv", "comparison_totals_component.csv",
        "monthly_eu_component_metrics.csv", "monthly_country_metrics.csv",
        "monthly_component_metrics.csv", "plots",
    ]
    return [[f"`{name}`", "present" if (comparison_dir / name).exists() else "missing"] for name in names]


def plot_lines(comparison_dir: Path) -> list[str]:
    plots = sorted((comparison_dir / "plots").glob("*")) if (comparison_dir / "plots").exists() else []
    image_plots = [p for p in plots if p.suffix.lower() in IMAGE_SUFFIXES]
    if not image_plots:
        return ["_No generated plot images found._"]

    priority_plots = set(PRIORITY_PLOTS)
    lines: list[str] = []

    for rel in PRIORITY_PLOTS:
        plot = comparison_dir / rel
        if plot.exists():
            lines.append(f"![{rel}]({rel})")
        else:
            lines.append(f"- Missing expected priority plot: `{rel}`")

    other_plots = [plot for plot in image_plots if plot.relative_to(comparison_dir) not in priority_plots]
    if not other_plots:
        return lines

    lines.extend([
        "",
        "<details>",
        "<summary>Other plots</summary>",
        "",
    ])
    for plot in other_plots:
        rel = plot.relative_to(comparison_dir)
        lines.append(f"![{rel}]({rel})")
    lines.extend([
        "",
        "</details>",
    ])
    return lines


def label(row: dict[str, str], fields: list[str]) -> str:
    return " / ".join(part for field in fields if (part := row.get(field))) or "unknown"


def summarize(
    comparison_dir: Path,
    limit: int,
    *,
    base_ref: str,
    target_ref: str,
    base_short_sha: str,
    target_short_sha: str,
    date_to: str,
    runner: str,
    report_hash: str,
    base_run_hash: str,
    target_run_hash: str,
) -> str:
    eu = read_table(comparison_dir / "comparison_totals_eu.csv")
    eu_components = read_table(comparison_dir / "comparison_totals_eu_component.csv")
    countries = read_table(comparison_dir / "comparison_totals_country.csv")
    components = read_table(comparison_dir / "comparison_totals_component.csv")
    monthly_eu_component = read_table(comparison_dir / "monthly_eu_component_metrics.csv")
    monthly_country = read_table(comparison_dir / "monthly_country_metrics.csv")
    monthly_component = read_table(comparison_dir / "monthly_component_metrics.csv")

    base_label = f"{base_ref} ({base_short_sha})"
    target_label = f"{target_ref} ({target_short_sha})"
    eu_row = eu[0] if eu else {}
    diff = fmt_number(eu_row.get("diff"), scale=1_000_000)
    pct = fmt_pct(eu_row.get("pct_diff"))
    compared = eu_row.get("n_compared", "NA")
    n_base = eu_row.get("n_base", "NA")
    n_target = eu_row.get("n_target", "NA")

    lines = [
        "# get_co2 Comparison Summary",
        "",
        f"**Headline:** EU total diff is **{diff} Mt CO2 ({pct})** for `{target_label}` vs `{base_label}`.",
        "",
        "## Top-line metrics",
        f"- Base: `{base_label}`",
        f"- Target: `{target_label}`",
        f"- date_to: `{date_to}`",
        f"- Runner: `{runner}`",
        f"- EU total diff: **{diff} Mt CO2 ({pct})**",
        f"- Row coverage: compared **{compared}** rows; base **{n_base}**, target **{n_target}**",
        f"- Report hash: `{report_hash}`",
        f"- Run hashes: base `{base_run_hash}`, target `{target_run_hash}`",
        f"- Directory: `{comparison_dir}`",
        "",
        "## Key plots",
        *plot_lines(comparison_dir),
        "",
        "<details>",
        "<summary>Run context</summary>",
        "",
        *md_table(
            ["Field", "Value"],
            [
                ["`base_ref`", f"`{base_ref}`"],
                ["`target_ref`", f"`{target_ref}`"],
                ["`base_short_sha`", f"`{base_short_sha}`"],
                ["`target_short_sha`", f"`{target_short_sha}`"],
                ["`date_to`", f"`{date_to}`"],
                ["`runner`", f"`{runner}`"],
                ["`report_hash`", f"`{report_hash}`"],
                ["`base_run_hash`", f"`{base_run_hash}`"],
                ["`target_run_hash`", f"`{target_run_hash}`"],
            ],
        ),
        "",
        "</details>",
        "",
        "<details>",
        f"<summary>Top EU component diffs ({min(limit, len(eu_components))})</summary>",
        "",
        *md_table(["Component", "Diff (Mt CO2)", "% diff", "Rows"], [[label(r, ["fuel", "sector"]), fmt_number(r.get("diff"), 1_000_000), fmt_pct(r.get("pct_diff")), r.get("n_compared", "NA")] for r in top_abs(eu_components, "diff", limit)]),
        "",
        "</details>",
        "",
        "<details>",
        f"<summary>Top country diffs ({min(limit, len(countries))})</summary>",
        "",
        *md_table(["Country", "Diff (Mt CO2)", "% diff", "Rows"], [[label(r, ["iso2"]), fmt_number(r.get("diff"), 1_000_000), fmt_pct(r.get("pct_diff")), r.get("n_compared", "NA")] for r in top_abs(countries, "diff", limit)]),
        "",
        "</details>",
        "",
        "<details>",
        f"<summary>Top country/fuel/sector component diffs ({min(limit, len(components))})</summary>",
        "",
        *md_table(["Component", "Diff (Mt CO2)", "% diff", "Rows"], [[label(r, ["iso2", "fuel", "sector"]), fmt_number(r.get("diff"), 1_000_000), fmt_pct(r.get("pct_diff")), r.get("n_compared", "NA")] for r in top_abs(components, "diff", limit)]),
        "",
        "</details>",
        "",
        "<details>",
        "<summary>Monthly metric ranges</summary>",
        "",
        *md_table(["Table", "RMSE range (Mt CO2)", "R2 range"], [
            ["Country", metric_range(monthly_country, "rmse", 1_000_000), metric_range(monthly_country, "r2", digits=3)],
            ["EU component", metric_range(monthly_eu_component, "rmse", 1_000_000), metric_range(monthly_eu_component, "r2", digits=3)],
            ["Country/fuel/sector component", metric_range(monthly_component, "rmse", 1_000_000), metric_range(monthly_component, "r2", digits=3)],
        ]),
        "",
        "</details>",
        "",
        "<details>",
        "<summary>Artifact status</summary>",
        "",
        *md_table(["Artifact", "Status"], artifact_status(comparison_dir)),
        "",
        "</details>",
    ]
    return "\n".join(lines) + "\n"


def main() -> None:
    parser = argparse.ArgumentParser(description="Summarize a cached get_co2 comparison directory as Markdown.")
    parser.add_argument("comparison_dir", type=Path)
    parser.add_argument("--limit", type=int, default=5)
    parser.add_argument("--base-ref", required=True)
    parser.add_argument("--target-ref", required=True)
    parser.add_argument("--base-short-sha", required=True)
    parser.add_argument("--target-short-sha", required=True)
    parser.add_argument("--date-to", required=True)
    parser.add_argument("--runner", required=True)
    parser.add_argument("--report-hash", required=True)
    parser.add_argument("--base-run-hash", required=True)
    parser.add_argument("--target-run-hash", required=True)
    args = parser.parse_args()
    if args.limit < 1:
        raise SystemExit("--limit must be at least 1")
    if not args.comparison_dir.exists() or not args.comparison_dir.is_dir():
        raise SystemExit(f"Comparison directory does not exist: {args.comparison_dir}")
    print(
        summarize(
            args.comparison_dir,
            args.limit,
            base_ref=args.base_ref,
            target_ref=args.target_ref,
            base_short_sha=args.base_short_sha,
            target_short_sha=args.target_short_sha,
            date_to=args.date_to,
            runner=args.runner,
            report_hash=args.report_hash,
            base_run_hash=args.base_run_hash,
            target_run_hash=args.target_run_hash,
        ),
        end="",
    )


if __name__ == "__main__":
    main()
