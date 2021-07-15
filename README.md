# Mobility_Bias_Viz

Unbiased mobility viz is a visualization spin-off from the [Vancom X UBC DSSG unbiased mobility project](https://github.com/cedaracademysociety/vancom-ubc-dssg).

## Introduction
// TODO


## Technical Requirement
// TODO


## Data Requirement
data/* folder is gitignored for confidential concerns. The code expects the following data files:
- surrey_desc.csv
- surrey_data.csv 
- all_businesses.csv

(For dev only, subject to change)


## Development Workflow

### For Development
1. Assign an issue (they are mostly feature request atm) you want to work on to yourself.
1. Make sure local dev branch is synced up with the upstream dev, and the app is working under a sanity check.
1. Create a feature branch from there.
1. Create a **draft** PR from this feature branch linked to the issue.
1. Local development.
1. Pull merge any changes in dev branch, resolve merge conflicts if any.
1. Request for code reviews. Go to Step 1 for next issue while waiting for reviewers.
1. Make changes according to code reviews until approval.
1. Merge the feature branch into dev .
1. Start or continue from step 1.

### For Code Review
1. Stash current dev work (if needed). Pull the feature branch to local. Checkout to the feature branch.
2. In R Studio, **clear objects from the local space**. Run App, make sure that:
    - No previous functions are broken (No regression).
    - The new feature is working as intended.
3. Give code review comments, with potential issues (attach a screenshot if it helps), refactor suggestions, or feature improvement suggestions. 
4. Appove the merge if changes fulfill expectations.
