# Planning worktree instructions

This is the orphan `planning` branch for the iso8601 repository. The source repository instructions are [AGENTS.md](../../AGENTS.md); they govern source compatibility, evidence, and authority. This worktree contains plans and historical planning records only.

Canonical layout: `projectNN-<slug>/project-plan.md` and sibling `ledger.md`; each arc has `arc-plan.md` and `ledger.md`; each document-backed slice has `slice-plan.md`, `ledger.md`, and durable artifacts under `artifacts/`. New live work uses first-class CC prompts only when assigned. Archived workbench documents below `artifacts/legacy/` are historical source material, not current assignments.

The planning branch is orphaned and must remain free of inherited implementation files. Inspect Git status and worktree ownership before mutations. Preserve historical records and provenance. Keep acceptance rows open unless independent reproduction and any required Operator gate are documented.

Commit only intended planning paths; when other changes may be present, use explicit paths and `git commit --only`. Assistant-authored commits require this contiguous trailer block:

```text
Co-authored-by: Codex <noreply@openai.com>
Co-authored-by: Billo AI <ai-engineering@billo.systems>
```

Local commit authority does not authorize push, merge, release, or publication.
