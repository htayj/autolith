# Unreleased

## Mission exhaustion

- Choose `:mission-exhaustion-policy :evaluate` for a bounded, independently funded review at a mission budget boundary. Configure an explicit strong `:mission-exhaustion-model`; without it, exhaustion falls back to asking the user.
- Capture original limits and extension ceilings when starting each mission. Defaults permit two extensions, each at most half the original limit, up to twice the original budget.
- Review progress and verification in one tool-free request, retain a private redacted transcript, and settle evaluator usage separately from mission work. Interrupted or uncertain evaluation falls back to asking the user.
- Record validated, evidence-cited harness papercuts from that response through the private deduplicating store. The decision notice counts only confirmed new reports.
