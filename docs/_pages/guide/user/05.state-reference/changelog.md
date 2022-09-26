---
title: Changelog
layout: single
toc: true
permalink: /guide/user/state-reference/changelog/
---

## Changelog attributes

The following attributes are children of the [`changelog`]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/global-attributes.md %}#changelog) element:

| Name                                                                | Type    | Values                                                    |
| ------------------------------------------------------------------- | ------- | --------------------------------------------------------- |
| [`changelog/releases`](#releases)                                   | list    | The [releases](#releases) in the changelog                |

Please note that the `changelog` object is only present when a [new version]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/global-attributes.md %}#new-version) has been [inferred]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer) and the changelog generation has been enabled by setting the changelog [`path`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/changelog.md %}#path) option.
{: .notice--info}

### Releases

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `changelog/releases`                                                                     |
| Type                          | list                                                                                     |
| Related configuration options |                                                                                          |
| Initialized by task           | [make]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#make){: .btn .btn--small} |

The ordered list of all changelog releases. The list is reverse ordered, so the newest release appears as the first element in the list, while the oldest is the last in the list.

Each release item has the following attributes:

| Name                                                                | Type    | Values                                                    |
| ------------------------------------------------------------------- | ------- | --------------------------------------------------------- |
| `changelog/releases/<ID>/date`                                      | string  | The release date (as a formatted string)                  |
| `changelog/releases/<ID>/name`                                      | string  | The release name                                          |
| [`changelog/releases/<ID>/sections`](#sections)                     | list    | The commit [sections](#sections) within a release         |

### Sections

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `changelog/releases/<ID>/sections`                                                       |
| Type                          | list                                                                                     |
| Related configuration options | [commitMessageConventions/NAME/expression]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/commit-message-conventions.md %}#expression){: .btn .btn--success .btn--small} [sections]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/changelog.md %}#sections){: .btn .btn--success .btn--small} |
| Initialized by task           | [make]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#make){: .btn .btn--small} |

The sections of changes (commits) within a release. This is a the list of [commits](#commit-objects) within the section for the release.

The section names are the commit *types* as they are inferred by the [commit message conventions]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/commit-message-conventions.md %}) (more specifically, by the regular [`expression`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/commit-message-conventions.md %}#expression)) but they can be remapped according to the [`sections`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/changelog.md %}#sections) defined for the changelog. If mapping is enabled (when [`sections`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/changelog.md %}#sections) have been defined) all commits not falling into an explicitly defined section will not appear in the changelog.

Each section item has the following attributes:

| Name                                                                | Type    | Values                                                    |
| ------------------------------------------------------------------- | ------- | --------------------------------------------------------- |
| `changelog/releases/<ID>/sections/<NAME>/name`                      | string  | The section name                                          |
| [`changelog/releases/<ID>/sections/<NAME>/commits`](#commit-objects)| list    | The list of [commits](#commit-objects) for the section    |

## Commit objects

Each commit in the changelog has the following properties:

| Name                                                                | Type    | Values                                                          |
| ------------------------------------------------------------------- | ------- | --------------------------------------------------------------- |
| `sha`                                                               | string  | The SHA-1 identifier of the commit                              |
| `date`                                                              | integer | The integer representing the commit timestamp                   |
| `parents`                                                           | list    | The list of SHA-1 identifiers of parent commits                 |
| `authorAction`                                                      | object  | The container for the commit author fields (see below)          |
| `authorAction/identity`                                             | object  | The container for the commit author identity fields (see below) |
| `authorAction/identity/name`                                        | string  | The commit author name                                          |
| `authorAction/identity/email`                                       | string  | The commit author email (optional)                              |
| `authorAction/timeStamp`                                            | object  | The commit author timestamp (optional)                          |
| `authorAction/timeStamp/timeStamp`                                  | date    | The actual commit author timestamp                              |
| `authorAction/timeStamp/timeZone`                                   | string  | The commit author time zone (optional)                          |
| `commitAction`                                                      | object  | The container for the committer fields (see below)              |
| `commitAction/identity`                                             | object  | The container for the committer identity fields (see below)     |
| `commitAction/identity/name`                                        | string  | The committer name                                              |
| `commitAction/identity/email`                                       | string  | The committer email (optional)                                  |
| `commitAction/timeStamp`                                            | object  | The committer timestamp (optional)                              |
| `commitAction/timeStamp/timeStamp`                                  | date    | The actual committer timestamp                                  |
| `commitAction/timeStamp/timeZone`                                   | string  | The committer time zone (optional)                              |
| `message`                                                           | object  | The container for the commit message (see below)                |
| `message/fullMessage`                                               | string  | The entire commit message                                       |
| `message/shortMessage`                                              | string  | The first line of the commit message                            |
| `message/footers`                                                   | map     | The commit footers, each modelled as a name and value pair      |
| `tags`                                                              | list    | The list of tags applied to the commit                          |
