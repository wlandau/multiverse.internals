# r.releases.internals 0.0.10

* Automatically merge GitLab URLs with non-"upcoming" releases.
* `build_universe()` omits `"branch": "release"` from listings originating from custom owners. The new `release_exceptions` can accept `"https://github.com/cran"`, for example.

# r.releases.internals 0.0.9

* Change the package name to `r.releases.internals`.
* Get RemoteSha hashes for checking versions (@jeroen, https://github.com/r-universe-org/help/issues/377).
* Makes `review_pull_requests()` robust to a package description with no URL listed.

# r.releases.utils 0.0.8

* Use R-releases and not `r-releases` to refer to the project.
* Edit bot messages.
* Add `record_versions()`.

# r.releases.utils 0.0.7

* Check if a release exists during automated checks.

# r.releases.utils 0.0.6

* Check URL exists as part of package verification.

# r.releases.utils 0.0.5

* Write pretty JSON to `packages.json` (@llrs, https://github.com/r-releases/help/issues/4).

# r.releases.utils 0.0.4

* Relax some assertions so `build_universe()` can run on reasonable cases.

# r.releases.utils 0.0.3

* Checks URL matches the package description for CRAN packages.
* `check_package()` checks the URL and name directly, not a file.
* Add more strict URL assertions.
* Accept custom JSON entries but flag them for manual review.
* Print progress messages from `build_universe()`.

# r.releases.utils 0.0.2

* Trim white space when processing URLs.

# r.releases.utils 0.0.1

* First version.
