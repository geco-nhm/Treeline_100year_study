# Release checklist

Status of the data and code release for Tjessem et al., "Tales from the past: remapping dynamic tree- and forest lines in response to changing climate and current land use" (EGUsphere egusphere-2026-2717, under review in Biogeosciences).

| | DOI |
|---|---|
| Version v0.1 (cite this one in the manuscript) | [10.5281/zenodo.24155132](https://doi.org/10.5281/zenodo.24155132) |
| Concept DOI (always resolves to the latest version) | [10.5281/zenodo.24155131](https://doi.org/10.5281/zenodo.24155131) |

## Done

- [x] Repository made public; Zenodo connected to GitHub; release `v0.1` (commit d3922d0) archived on Zenodo.
- [x] Manuscript cites the version DOI; manuscript, editor email and Ingrid email updated.

## Now (before the revised manuscript is uploaded)

- [ ] Confirm with Ingrid that the "Alternative model" in Table D1 is the distance-to-summit model. If not, fix Table D1, the editor email and the analysis, and make a new release.
- [ ] Licence in the repository. The `v0.1` zip contains no licence file; CC BY 4.0 is only in the Zenodo metadata.
  - [ ] Commit `license: CC-BY-4.0` in `CITATION.cff` (already edited locally).
  - [ ] Add a `LICENSE` file with the CC BY 4.0 text.
  - [ ] Replace "To be added" in the README licence section.
- [ ] `CITATION.cff`: add `version`, `date-released` and the concept DOI (`identifiers`), plus ORCIDs and affiliations.
- [ ] README: add a Zenodo DOI badge and a "How to cite" line.
- [ ] README: check the field years. It says the sites were revisited 2016–2019, but the latest records in `Dataset_V6` are from 2013–2023 (2013: 18 sites, 2015: 62, 2016: 149, 2017: 11, 2018: 24, 2019: 1, 2023: 10). Align with the paper.
- [ ] Zenodo record: edit the metadata (no new version needed).
  - [ ] Replace the description (currently the change log) with the text in `RELEASE_DESCRIPTION.md`.
  - [ ] Add ORCIDs and affiliations of all authors.
  - [ ] Related works: "Is supplement to" the preprint, DOI 10.5194/egusphere-2026-2717.
  - [ ] Keywords: treeline, forest line, mountain birch, *Betula pubescens*, climate change, land use, grazing, Norway, resurvey.
- [ ] Delete the branch `release/paper-v1` (the tag `v0.1` marks that version), or keep it if preferred.

## After review, if the analysis or data change

- [ ] Commit the changes to `main`, update the README change log.
- [ ] Publish a new GitHub release (e.g. `v0.2`). Zenodo creates a new version DOI under the same concept DOI.
- [ ] Update the version DOI in the manuscript's code and data availability statement and reference list.

## At acceptance

- [ ] Publish the final GitHub release `v1.0.0` from `main` (release notes: `RELEASE_DESCRIPTION.md`).
- [ ] Cite the `v1.0.0` version DOI in the final manuscript.
- [ ] `CITATION.cff`: update `preferred-citation` to Biogeosciences (volume, pages, DOI) and set `version: v1.0.0`.
- [ ] Zenodo: change "Is supplement to" from the preprint to the final article DOI.
- [ ] Remove this file from the repository.

## How a release works (reminder)

1. GitHub → Releases → Draft a new release → new tag (e.g. `v1.0.0`) on `main` → title and notes → Publish release (not a draft, not a pre-release).
2. Zenodo archives it within minutes (Zenodo → GitHub shows the record). Metadata can be edited afterwards; files cannot.
