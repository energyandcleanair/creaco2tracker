"""Publish comparison Markdown with native GitHub image attachments."""

import argparse
import json
import os
from pathlib import Path
import re
import subprocess
import tempfile
from urllib.parse import unquote, urlsplit


MARKER = "<!-- creaco2tracker:get-co2-comparison -->"
IMAGE = re.compile(r"!\[[^\]\n]*\]\(([^)\n]+)\)")


def gh(*args, **kwargs):
    return subprocess.check_output(["gh", *args], text=True, **kwargs).strip()


def check_auth(repo):
    # The automatic workflow token is an App installation token. GitHub rejects
    # that token type for attachments regardless of workflow permissions.
    token = os.environ.get("GH_TOKEN", "")
    fine_grained = token.startswith("github_pat_")
    if not fine_grained and not token.startswith(("ghp_", "gho_")):
        raise ValueError(
            "IMAGE_UPLOAD_TOKEN must contain a personal access or OAuth token; "
            "the automatic GITHUB_TOKEN cannot upload attachments. Fine-grained PATs "
            "need access to this repository and Pull requests: Read and write."
        )
    response = gh("api", "--include", "user")
    headers, body = re.split(r"\r?\n\r?\n", response, maxsplit=1)
    scopes = set()
    for line in headers.splitlines():
        name, _, value = line.partition(":")
        if name.lower() == "x-oauth-scopes":
            scopes.update(scope.strip() for scope in value.split(","))
    metadata = json.loads(gh("repo", "view", repo, "--json", "viewerPermission,isPrivate"))
    required = {"repo"} if metadata["isPrivate"] else {"repo", "public_repo"}
    # Fine-grained PATs do not expose classic OAuth scopes. Repository selection
    # and role are checked here; GitHub enforces Pull requests: write on publish.
    if not fine_grained and not scopes.intersection(required):
        raise ValueError("IMAGE_UPLOAD_TOKEN lacks public_repo (public repo) or repo scope.")
    if metadata["viewerPermission"] not in {"ADMIN", "MAINTAIN", "WRITE"}:
        raise ValueError("The owner of IMAGE_UPLOAD_TOKEN needs write access to this repository.")
    if "--attach" not in gh("pr", "comment", "--help"):
        raise ValueError("GitHub CLI 2.99.0 or newer is required for attachments.")
    return json.loads(body)["login"]


def attachments(summary):
    """Collect the inline local images produced by scripts/compare_lib."""
    paths = []
    for match in IMAGE.finditer(summary.read_text()):
        destination = match.group(1).strip().removeprefix("<").removesuffix(">")
        if urlsplit(destination).scheme or destination.startswith(("//", "#")):
            continue
        path = (summary.parent / unquote(destination)).resolve()
        if not path.is_file():
            raise ValueError(f"Comparison image is missing: {path}")
        if path.stat().st_size > 10 * 1024 * 1024:
            raise ValueError(f"Comparison image exceeds GitHub's 10 MB limit: {path}")
        if path not in paths:
            paths.append(path)
    if len(paths) > 50:
        raise ValueError(f"Report has {len(paths)} images; gh accepts at most 50 attachments.")
    return paths


def publish(repo, pr, summary, login):
    summary = summary.resolve()
    paths = attachments(summary)
    pages = json.loads(gh(
        "api", "--paginate", "--slurp", f"repos/{repo}/issues/{pr}/comments"
    ))
    own_comments = [
        comment for page in pages for comment in page
        if comment["user"]["login"] == login
    ]
    reports = [comment for comment in own_comments if MARKER in comment["body"]]
    # --edit-last must never overwrite another comment made by the PAT's owner.
    edit_last = bool(own_comments and MARKER in own_comments[-1]["body"])
    with tempfile.TemporaryDirectory(dir=summary.parent) as temporary:
        body = Path(temporary) / "comment.md"
        body.write_text(f"{MARKER}\n\n{summary.read_text()}")
        args = ["pr", "comment", str(pr), "--repo", repo, "--body-file", str(body)]
        if edit_last:
            args.append("--edit-last")
        for path in paths:
            args.extend(["--attach", str(path)])
        # gh resolves Markdown image paths relative to cwd, not the body file.
        print(gh(*args, cwd=summary.parent))
    # When another comment intervenes, create the replacement before removing
    # old reports. Failed uploads leave the previous report intact.
    for report in reports:
        if edit_last and report["id"] == own_comments[-1]["id"]:
            continue
        gh("api", "--method", "DELETE", f"repos/{repo}/issues/comments/{report['id']}")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repo", required=True)
    parser.add_argument("--pr", type=int)
    parser.add_argument("--summary", type=Path)
    parser.add_argument("--check-auth", action="store_true")
    args = parser.parse_args()
    login = check_auth(args.repo)
    if not args.check_auth:
        if args.pr is None or args.summary is None:
            parser.error("--pr and --summary are required when publishing")
        publish(args.repo, args.pr, args.summary, login)


if __name__ == "__main__":
    main()
