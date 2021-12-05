# 0.16.0

* The invitation and membership fields were made optional for `OrganizationEvent` (resolves #37)
* The target repository field was made optional for `PullRequestTarget` (resolves #51)
* Support for marketplace purchase event (by Alistair Burrowes, resolves #46)
* The maintaining sponsor [onrock.online](https://onrock.online) has rebranded to [Cuedo Business Solutions](https://cuedo.com.au)

# 0.15.0

* Corrected false failures in the test suite when using `time-1.9` and later (by Julien Debon)

# 0.14.0

* Corrected description for CheckRunEventAction
* Fixed a decoding error for CheckRunEventActionRerequested
* Corrected a new hlint failure that was introduced by a recent hlint release

# 0.13.0

* Support for `node_id`, which is used by the GraphQL API (by Alistair Burrowes)
* Handle the case where `issue.body` is `null` (by Rob Berry)
* Support for `mergeable_state` option in pull requests (by Matthew Bauer)

# 0.12.0

* Support for CheckSuiteEvent and CheckRunEvent was added (resolves #25)

# 0.11.0

* `OwnerType` of `Bot` is now supported (by Domen Kozar, resolves #22)
* GHC-7.10 requires less changes to build the bundled examples (resolves #6)
* Fixed issues to do with updated stack package format and CI build (resolves #23, et al.)
* Added a security policy clarifying how security issues should be disclosed (resolves #24)
* The latest release is now available on the `release` branch (resolves #10)

# 0.10.1

* Fixed a bug parsing related PR with empty body (by Rob Berry)
* Updated copyright and contact information

# 0.10.0

* Significantly improved documentation coverage and added module descriptions (by Christian Sakai, resolves #5)
* Fixed a bug where the installation id field of `PullRequestEvent` was not optional (by Rob Berry)
* Implementing `head` and `base` fields to `PullRequestReview` (by Rob Berry and Thomas DuBuisson)

# 0.9.1

* Fixed a bug whereby the test fixtures were not included in the source distribution (resolves #8)
* Marked the modules as Trustworthy indicating that they do not perform unsafe operations (resolves #9)
* Include stack files for older resolvers in CI and the source distribution
* Removed flags from the cabal file that prevented older GHC from being able to build (contributes #6)

# 0.9.0

* This is an alpha testing release with an incomplete feature set to recieve feedback on the API.
* The API may change in the recent future. When alpha testing has commenced, the version will start at `1.0.0`.
