import os
import subprocess
from typing import Dict, List, Optional, Tuple
from unidiff import PatchSet

class GitWrapper:
    def __init__(self, repo_path: str):
        if not os.path.exists(repo_path):
            raise ValueError(f"Repository path does not exist: {repo_path}")
        self.repo_path = repo_path
        # Check if it's a valid git repository
        try:
            self._run_git_command(["rev-parse", "--is-inside-work-tree"], check=True)
        except subprocess.CalledProcessError:
            raise ValueError(f"Repository at {repo_path} is not a valid git repository.")

    def _run_git_command(self, command: List[str], check: bool = False) -> str:
        """Helper to run git commands"""
        git_command = ["git", "-C", self.repo_path] + command
        result = subprocess.run(git_command, capture_output=True, text=True, check=check)
        return result.stdout.strip()

    def get_latest_commit_from_branch(self, branch_name: str) -> str:
        """Fetch remote branch and get latest commit hash"""
        try:
            self._run_git_command(["fetch", "origin", branch_name])
        except subprocess.CalledProcessError as e:
            print(f"Warning: Failed to fetch branch '{branch_name}': {e.stderr}")

        full_ref = f"origin/{branch_name}"
        try:
            return self._run_git_command(["rev-parse", full_ref])
        except subprocess.CalledProcessError:
            return self._run_git_command(["rev-parse", branch_name])

    def get_common_ancestor(self, branch1: str, branch2: str) -> str:
        """Get the merge-base (common ancestor) of two branches"""
        try:
            self._run_git_command(["fetch", "origin", branch1])
            self._run_git_command(["fetch", "origin", branch2])
        except subprocess.CalledProcessError as e:
            print(f"Warning: fetch failed: {e.stderr}")

        ref1 = f"origin/{branch1}"
        ref2 = f"origin/{branch2}"
        return self._run_git_command(["merge-base", ref1, ref2])

    def get_changed_files_from_commits(self, to_commit: str, from_commit: str) -> Dict[str, List[str]]:
        """Get categorized list of changed files between two commits"""
        diff_output = self._run_git_command(["diff", "--name-status", from_commit, to_commit])
        changes = {
            "added": [],
            "deleted": [],
            "modified": []
        }
        for line in diff_output.splitlines():
            status, file_path = line.split('\t')
            if status == "A":
                changes["added"].append(file_path)
            elif status == "D":
                changes["deleted"].append(file_path)
            elif status == "M":
                changes["modified"].append(file_path)
        return changes

    def get_changed_files_from_commits_raw(self, from_commit: str, to_commit: str) -> str:
        """Get raw git diff between two commits"""
        return self._run_git_command(["diff", from_commit, to_commit])

    def get_structured_diff(self, from_commit: str, to_commit: str) -> Tuple[dict, dict]:
        """Parse Git diff into structured added/removed changes with line numbers"""
        added_changes = {}
        removed_changes = {}

        raw_diff = self._run_git_command(["diff", "--unified=0", from_commit, to_commit])
        patch = PatchSet(raw_diff)

        for patched_file in patch:
            filename = patched_file.path.split("/")[-1]
            for hunk in patched_file:
                for line in hunk:
                    if line.is_added:
                        added_changes.setdefault(filename, []).append((line.target_line_no, line.value.rstrip()))
                    elif line.is_removed:
                        removed_changes.setdefault(filename, []).append((line.source_line_no, line.value.rstrip()))

        return added_changes, removed_changes

    def get_file_content(self, file_path: str, commit: Optional[str] = "HEAD") -> str:
        """Get the content of a file at a specific commit"""
        try:
            return self._run_git_command(["show", f"{commit}:{file_path}"], check=True)
        except subprocess.CalledProcessError as e:
            raise FileNotFoundError(f"File '{file_path}' not found at commit '{commit}'. Error: {e.stderr}")
