"""Offline checks for comparison attachments and comment ownership."""

import importlib.util
import json
from pathlib import Path
import subprocess
import tempfile
import unittest
from unittest.mock import patch


spec = importlib.util.spec_from_file_location(
    "publisher", Path(__file__).resolve().parents[1] / "publish-comparison-comment.py"
)
publisher = importlib.util.module_from_spec(spec)
spec.loader.exec_module(publisher)


class PublishComparisonTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.summary = self.root / "summary.md"
        self.image = self.root / "plot.png"
        self.image.write_bytes(b"test image")
        self.summary.write_text("# Report\n\n![Chart](plot.png)\n")

    def test_collects_only_referenced_images_and_deduplicates_paths(self):
        self.summary.write_text(
            "![a](plot.png)\n![b](./plot.png)\n![remote](https://example.com/chart.png)"
        )
        (self.root / "unused.png").write_bytes(b"unused")
        self.assertEqual(publisher.attachments(self.summary), [self.image])

    def test_resolves_combined_summary_paths(self):
        nested = self.root / "outputs"
        nested.mkdir()
        summary = nested / "combined.md"
        summary.write_text("![Chart](../plot.png)")
        self.assertEqual(publisher.attachments(summary), [self.image])

    def test_missing_image_fails_before_publishing(self):
        self.image.unlink()
        with self.assertRaisesRegex(ValueError, "missing"):
            publisher.attachments(self.summary)

    def test_attachment_count_limit(self):
        self.summary.write_text("\n".join(f"![{i}]({i}.png)" for i in range(51)))
        for i in range(51):
            (self.root / f"{i}.png").write_bytes(b"test")
        with self.assertRaisesRegex(ValueError, "at most 50"):
            publisher.attachments(self.summary)

    def test_image_size_limit(self):
        with self.image.open("wb") as image:
            image.truncate(10 * 1024 * 1024 + 1)
        with self.assertRaisesRegex(ValueError, "10 MB"):
            publisher.attachments(self.summary)

    def check_token(self, scopes="public_repo", permission="WRITE", private=False,
                    token="ghp_test"):
        responses = [
            f'HTTP/2 200 OK\nX-OAuth-Scopes: {scopes}\n\n{{"login":"reporter"}}',
            json.dumps({"viewerPermission": permission, "isPrivate": private}),
            "--attach file",
        ]
        with patch.dict("os.environ", {"GH_TOKEN": token}), \
                patch.object(publisher, "gh", side_effect=responses):
            return publisher.check_auth("owner/repo")

    def test_public_repo_scope_and_write_access_pass(self):
        self.assertEqual(self.check_token(), "reporter")

    def test_fine_grained_token_has_no_classic_scopes(self):
        self.assertEqual(self.check_token(scopes="", token="github_pat_test"), "reporter")

    def test_fine_grained_token_still_needs_repository_write_access(self):
        with self.assertRaisesRegex(ValueError, "write access"):
            self.check_token(scopes="", token="github_pat_test", permission="READ")

    def test_workflow_token_rejected_without_api_calls(self):
        with patch.dict("os.environ", {"GH_TOKEN": "ghs_test"}), \
                patch.object(publisher, "gh") as gh:
            with self.assertRaisesRegex(ValueError, "automatic GITHUB_TOKEN"):
                publisher.check_auth("owner/repo")
            gh.assert_not_called()

    def test_insufficient_scope_rejected(self):
        with self.assertRaisesRegex(ValueError, "lacks"):
            self.check_token(scopes="read:org")

    def test_read_only_repository_access_rejected(self):
        with self.assertRaisesRegex(ValueError, "write access"):
            self.check_token(permission="READ")

    def test_private_repository_requires_repo_scope(self):
        with self.assertRaisesRegex(ValueError, "lacks"):
            self.check_token(private=True)
        self.assertEqual(self.check_token(scopes="repo", private=True), "reporter")

    def exercise_publish(self, comments, fail=False):
        calls = []

        def gh(*args, **kwargs):
            calls.append(args)
            if args[:2] == ("api", "--paginate"):
                return json.dumps([comments])
            if args[:2] == ("pr", "comment"):
                self.assertEqual(kwargs["cwd"], self.root)
                body = Path(args[args.index("--body-file") + 1]).read_text()
                self.assertIn(publisher.MARKER, body)
                self.assertIn("![Chart](plot.png)", body)
                self.assertEqual(args[args.index("--attach") + 1], str(self.image))
                if fail:
                    raise subprocess.CalledProcessError(1, "gh")
                return "https://github.com/owner/repo/pull/27#issuecomment-99"
            return ""

        with patch.object(publisher, "gh", side_effect=gh):
            if fail:
                with self.assertRaises(subprocess.CalledProcessError):
                    publisher.publish("owner/repo", 27, self.summary, "reporter")
            else:
                publisher.publish("owner/repo", 27, self.summary, "reporter")
        return calls

    def comment(self, identifier, body, login="reporter"):
        return {"id": identifier, "body": body, "user": {"login": login}}

    def test_updates_latest_own_report(self):
        calls = self.exercise_publish([self.comment(1, publisher.MARKER)])
        self.assertIn("--edit-last", calls[1])
        self.assertEqual(len(calls), 2)

    def test_preserves_unrelated_and_other_users_comments(self):
        calls = self.exercise_publish([
            self.comment(1, publisher.MARKER),
            self.comment(2, "Human discussion"),
            self.comment(3, publisher.MARKER, login="someone_else"),
        ])
        self.assertNotIn("--edit-last", calls[1])
        self.assertEqual(calls[2], (
            "api", "--method", "DELETE", "repos/owner/repo/issues/comments/1"
        ))
        self.assertEqual(len(calls), 3)

    def test_failed_upload_keeps_previous_report(self):
        calls = self.exercise_publish([
            self.comment(1, publisher.MARKER), self.comment(2, "Human discussion")
        ], fail=True)
        self.assertEqual(len(calls), 2)


if __name__ == "__main__":
    unittest.main()
