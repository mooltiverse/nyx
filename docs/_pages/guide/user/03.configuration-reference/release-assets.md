---
title: Release Assets
layout: single
toc: true
permalink: /guide/user/configuration-reference/release-assets/
---

Release assets are the artifacts to be attached to releases upon [publication]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#publish). These may be local files (produced during the build process) or remote URLs. The configuration allows using dynamic [templates]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}) so you can have multiple degrees of control over the released contents.

Release assets are configured within the `releaseAssets` *section*. The section allows one sub-section for each release asset.

Unless a [release type]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}) defines a subset of release assets to publish by means of the [`assets`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#assets) option, all the release assets herein configured are published when the [publish]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#publish) command runs.

[Publication services]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#publication-services) may offer different support or have limitations for release assets. Please check the [services]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/services.md %}) page for details.

The outcome of the published artifacts is also summarized in the state [`releaseAssets`]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-assets.md %}) attribute.

### Release asset definition

Within the `releaseAssets` block you can define as many assets you need, each in its own separate block. The `name` identifies the asset configuration and is used solely for configuration purposes while it does not affect any artifacts characteristics.

Configuring release assets gives Nyx informations about:

* whether the asset is a local file to be uploaded or is a remote URL to be attached *as is* to the published release
* the asset name and description (or label)
* the asset MIME type

Each release asset has the following attributes:

| Name                                                                                       | Type    | Command Line Option                                                   | Environment Variable                                                    | Default                                              |
| ------------------------------------------------------------------------------------------ | ------- | --------------------------------------------------------------------- | ----------------------------------------------------------------------- | ---------------------------------------------------- |
| [`releaseAssets/<NAME>/description`](#description)                                         | string  | `--release-assets-<NAME>-description=<TEMPLATE>`                      | `NYX_RELEASE_ASSETS_<NAME>_DESCRIPTION=<TEMPLATE>`                      | N/A                                                    |
| [`releaseAssets/<NAME>/fileName`](#file-name)                                              | string  | `--release-assets-<NAME>-fileName=<TEMPLATE>`                         | `NYX_RELEASE_ASSETS_<NAME>_FILE_NAME=<TEMPLATE>`                        | N/A                                                    |
| [`releaseAssets/<NAME>/path`](#path)                                                       | string  | `--release-assets-<NAME>-path=<TEMPLATE>`                             | `NYX_RELEASE_ASSETS_<NAME>_PATH=<TEMPLATE>`                             | N/A                                                    |
| [`releaseAssets/<NAME>/type`](#type)                                                       | string  | `--release-assets-<NAME>-type=<TEMPLATE>`                             | `NYX_RELEASE_ASSETS_<NAME>_TYPE=<TEMPLATE>`                             | N/A                                                    |

#### Name

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `releaseAssets/<NAME>`                                                                   |
| Type                      | string                                                                                   |
| Default                   | N/A                                                                                      |
| Command Line Option       | `--release-assets-<NAME>=<NAME>`                                                         |
| Environment Variable      | `NYX_RELEASE_ASSETS_<NAME>=<NAME>`                                                       |
| Configuration File Option | `releaseAssets/<NAME>`                                                                   |
| Related state attributes  |                                                                                          |

The name that identifies the asset within the configuration. This is actually not a field to be set within assets but instead the key of the map element.

This value never appears on published artifacts as it's only used internally by Nyx.

This option is **mandatory**.

#### Description

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `releaseAssets/<NAME>/description`                                                       |
| Type                      | string                                                                                   |
| Default                   | N/A                                                                                      |
| Command Line Option       | `--release-assets-<NAME>-description=<TEMPLATE>`                                         |
| Environment Variable      | `NYX_RELEASE_ASSETS_<NAME>_DESCRIPTION=<TEMPLATE>`                                       |
| Configuration File Option | `releaseAssets/items/<NAME>/description`                                                 |
| Related state attributes  | [description]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-assets.md %}#description){: .btn .btn--info .btn--small} |

This attribute gives the description (or label) for the artifact.

This is the user facing identifier for the artifact upon publication, when supported. The way this description appears in the generated release depends on the [publication service]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/services.md %}) and may even be ignored by some services.

Here you can pass a [template]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}) to generate this attribute dynamically at runtime.

#### File name

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `releaseAssets/<NAME>/fileName`                                                          |
| Type                      | string                                                                                   |
| Default                   | N/A                                                                                      |
| Command Line Option       | `--release-assets-<NAME>-fileName=<TEMPLATE>`                                            |
| Environment Variable      | `NYX_RELEASE_ASSETS_<NAME>_FILE_NAME=<TEMPLATE>`                                         |
| Configuration File Option | `releaseAssets/items/<NAME>/fileName`                                                 |
| Related state attributes  | [fileName]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-assets.md %}#file-name){: .btn .btn--info .btn--small} |

This attribute gives the name to the artifact file.

Although it's suggested to make this name coherent with the [path](#path) you are not required to.

This is the user facing file name for the artifact upon publication (i.e. the name of the file being downloaded), when supported. This name is used for the published artifacts only and doesn't need to exist locally nor match the [path](#path). The way this name appears in the generated release depends on the [publication service]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/services.md %}) and may even be ignored by some services.

Here you can pass a [template]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}) to generate this attribute dynamically at runtime (i.e. when the file name contains the version number).

#### Path

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `releaseAssets/<NAME>/path`                                                              |
| Type                      | string                                                                                   |
| Default                   | N/A                                                                                      |
| Command Line Option       | `--release-assets-<NAME>-path=<TEMPLATE>`                                                |
| Environment Variable      | `NYX_RELEASE_ASSETS_<NAME>_PATH=<TEMPLATE>`                                              |
| Configuration File Option | `releaseAssets/items/<NAME>/path`                                                        |
| Related state attributes  | [path]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-assets.md %}#path){: .btn .btn--info .btn--small} |

This attribute defines the source path for the artifact, which may be a local file or a remote URL.

The way this attribute affects the artifacts in the generated release depends on the [publication service]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/services.md %}) but as a rule of thumb local files are uploaded and attached to the release while remote URLs are just listed as attachments and can be clicked to open the remote URL (also to download a file that was previously uploaded somewhere). Not all services support all types of paths.

When this is a local file the file doesn't need to exist until the [publish]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#publish) command runs so you can define an abstract file name here that will be generated during the build process. When the remote service supports custom [file names](#file-name) this file name will not affect the name of the file that will be published and, in that case, the two fields don't need to match.

Here you can pass a [template]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}) to generate this attribute dynamically at runtime.

#### Type

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `releaseAssets/<NAME>/type`                                                              |
| Type                      | string                                                                                   |
| Default                   | N/A                                                                                      |
| Command Line Option       | `--release-assets-<NAME>-type=<TEMPLATE>`                                                |
| Environment Variable      | `NYX_RELEASE_ASSETS_<NAME>_TYPE=<TEMPLATE>`                                              |
| Configuration File Option | `releaseAssets/items/<NAME>/type`                                                        |
| Related state attributes  | [type]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-assets.md %}#type){: .btn .btn--info .btn--small} |

This attribute defines the [MIME type](https://www.iana.org/assignments/media-types/media-types.xhtml) for the artifact.

The way this attribute affects the artifacts in the generated release depends on the [publication service]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/services.md %}) and may even be ignored. When in doubt just set this to `application/octet-stream`.

Here you can pass a [template]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}) to generate this attribute dynamically at runtime.
