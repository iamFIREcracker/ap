# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Added
- First release
- `--today DATE` option to run the planning as if today was `DATE`
- `--productivity 100` option to configure how productive people will be (max
  is `100`)
- `--disable-skip-weekends` option to have the planner schedule work for
  weekends as well as business days
- `--good-enough` option to have the planner use some aggressive heuristics to
  speed up the computation (yes, it might return a sub-optimal schedule)
