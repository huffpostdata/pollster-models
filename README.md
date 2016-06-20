# MOVED

This repository has moved to https://git.huffpo.net/huffpostdata/pollster-models

## Did you just update to find this repo empty?

Switch to the new one. You'll lose all your local changes; some git fu can save
them, but we haven't bothered to calculate the precise fu.

```sh
git checkout master
git remote rename origin github
git remote add origin git@git.huffpo.net:huffpostdata/pollster-models.git
git fetch origin
git reset --hard origin/master
```
