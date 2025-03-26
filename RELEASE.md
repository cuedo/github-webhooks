### How to do a release!

1. First, make your changes
2. Add a new entry to `CHANGELOG.md` with the next release number
3. Update `package.yaml` to the next release number and update contributor list and copyright year (if relevant)
4. Add any new `yaml` files for new `stack` releases 
  - Update `stack.yaml` with the *latest LTS version* rather than creating a new yaml for it
  - Ensure that these changes are reflected in all the workflows (`.github/worflows`)
5. Run `stack build`
6. Check the release number in `github-webhooks.cabal` matches the one in `package.yaml`
7. Merge the updated package files (specifically `package.yaml`, `stack.yaml`, `github-webhooks.cabal`)
8. If they complete successfully, run `stack upload . --documentation` in the root directory
9. Create GitHub release under the new release number

