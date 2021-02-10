---
title: Commit Message Conventions
layout: single
toc: true
permalink: /guide/user/introduction/commit-message-conventions/
---

Commit message conventions are a powerful mean to leverage automation by just providing relevant informations in messages alongside [commits](https://git-scm.com/docs/git-commit). Nyx can take advantage of commit message conventions to automatically generate changelogs and infer the new version numbers.

Although not all conventions contemplate the full set of informations, the full set of fields that can be modelled in each commit message is:

* `type`: the kind of changes that the commit brings
* `scope`: the perimeter of the changes introduced by the commit
* `description`: a short, single line description of the changes introduced by the commit
* `body`: a more extensive, multi line description of the changes
* `fields`: some additional structured fields commplying with the [git trailer format](https://git-scm.com/docs/git-interpret-trailers)

Nyx can also infer the version number identifier to bump based on the above informations.

Nyx is not compelling about conventions so you can commit without complying to any. On the other hand you can have multiple conventions configured and enabled at the same time so you can grab informations according to different conventions in different commits.

Below are the presets that come preconfigured with Nyx and that you can enable or disable as you like.














#### Conventional Commits

[Conventional Commits](https://www.conventionalcommits.org/) is the most complete convention when using [Semantic Versioning]({{ site.baseurl }}/reference/version-schemes/#semantic-versioning-semver) as it models all of the optional informations. Although heavily inspired by [Angular convention](https://github.com/angular/angular/blob/master/CONTRIBUTING.md#commit) it is a generalization suitable for any kind of project.

The message format is:

```text
<type>[optional scope]: <description>

[optional body]

[optional footer(s)]
```

where the `footer(s)` are the structured `fields`. As you can see all of the informations are contemplated in *Conventional Commit*.

In short:

* a commit is meant to introduce a **major** change in code when:
  * the `type` (and the optional `scope`) is followed by a `!`, before the `:`, examples: `feat!: a new feature`, `refactor(engine)!: new engine`
  * there is a footer entry starting with `BREAKING CHANGE` (or `BREAKING-CHANGE`), examples: `BREAKING CHANGE: very intrusive change`
* a commit is meant to introduce a **minor** change in code when the `type` is `feat`, with no `!`, example: `feat: a new feature`, `feat(ui): a new button`
* a commit is meant to introduce a **patch** in code when the `type` is `fix`, with no `!`, example: `fix: bug fix`, `fix(parser): bug fix in parser`
* when the commit message does not match any of the bove rules (including when other values are used for the `type`, like `build`, `chore`, `ci`, `docs`, `style`, `refactor`, `perf`, `test`) it's not meant to bump and version number identifier but just record some changes

See the [configuration]({{ site.baseurl }}/reference/configuration/#conventional-commits) section for more.

#### Gitmoji

[Gitmoji](https://gitmoji.dev/) is emerging thanks to its simplicity and it's popular among those teams using emojis. It doesn't model all of the informations that one may need but it's very easy to use. Gitmoji is not tied to a particular [version scheme]({{ site.baseurl }}/reference/version-schemes/) nor it's automation oriented.

The specification gives you maximum freedom when using this spec as it's simply a list of emojis that you can embed in commit messages to represent some meaningful information. You just need to start the commit message with one of the listed emojis, considering that:

* a commit is meant to introduce a **major** change in code when the emoji is `:boom:` (ntroduce breaking changes)
* a commit is meant to introduce a **minor** change in code when the emoji is `:sparkles:` (introduce new features)
* a commit is meant to introduce a **patch** in code when the emoji is `:bug:` (fix a bug), `:ambulance:` (critical hotfix), `:lock:` (fix security issues), `:adhesive_bandage:` (simple fix for a non-critical issue)
* all other emojis do not bump the version number and they are just recorded as such

See the [configuration]({{ site.baseurl }}/reference/configuration/#gitmoji) section for more.

## Further information

You may find [this article](https://nitayneeman.com/posts/understanding-semantic-commit-messages-using-git-and-angular/) useful if you're approaching commit conventions.
