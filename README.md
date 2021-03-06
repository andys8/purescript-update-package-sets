# purescript-update-package-sets

Check purescript package sets for newer versions.

## ⚠️ Deprecated

This project [was merged into `purescript/package-sets`](https://github.com/purescript/package-sets/pull/733).

<https://github.com/purescript/package-sets/tree/master/ci>

---

## Usage

```shell
npm install

npm start <github-token>
# or
spago run -a <github-token>
```

## Results

CI is daily checking if there are updates available:
<https://github.com/andys8/purescript-update-package-sets/actions>

The script will also keep [this github issue](https://github.com/purescript/package-sets/issues/728) up-to-date.

- (Re-)open or close it, if packages are outdated
- Update the content with packages and their newest available versions
