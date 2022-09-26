---
title: Release Assets
layout: single
toc: true
permalink: /guide/user/state-reference/release-assets/
---

The `releaseAssets` element is the list of artifacts that have been actually published along with the release. For each asset you have the file name, the description (or label), the MIME type and the URL to download the published asset.

The assets being published are the ones configured by means of the [`releaseAssets`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-assets.md %}) configuration with a couple of details to keep in mind:

* each [release type]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}) may use the global definition of release assets (by default) or just a subset of assets (in case it defines the [`assets`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#assets) option)
* [publication services]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#publication-services) may offer different support or have limitations for release assets. Please check the [services]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/services.md %}) page for details

This list is only populated when [`newRelease`]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/global-attributes.md %}#new-release) is `true` and only after [publish]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#publish) has run.

Following is the list of attributes for each published asset.

## Release asset attributes

The following attributes are children of each item in the `releaseAssets` list:

| Name                                                                | Type    | Values                                                    |
| ------------------------------------------------------------------- | ------- | --------------------------------------------------------- |
| [`releaseAssets/<#>/description`](#description)                     | string  | The (short) description (or label) of the published asset |
| [`releaseAssets/<#>/fileName`](#file-name)                          | string  | The name of the published asset                           |
| [`releaseAssets/<#>/path`](#path)                                   | string  | The URL of the published asset                            |
| [`releaseAssets/<#>/type`](#type)                                   | string  | The MIME type of the published asset                      |

### Description

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `releaseAssets/<#>/description`                                                          |
| Type                          | string                                                                                   |
| Related configuration options | [description]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-assets.md %}#description){: .btn .btn--success .btn--small} [assets]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#assets){: .btn .btn--success .btn--small} |
| Initialized by task           | [publish]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#publish){: .btn .btn--small} |

The short description of the asset, or its label, depending on the semantics used by the [publication service]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/services.md %}).

Unless the [publication service]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/services.md %}) has a custom treatment for this attribute, this is the same of the configured asset [description]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-assets.md %}#description). If the configured value is a [template]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}), this value is the outcome of rendering the source.

### File name

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `releaseAssets/<#>/fileName`                                                             |
| Type                          | string                                                                                   |
| Related configuration options | [fileName]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-assets.md %}#file-name){: .btn .btn--success .btn--small} [assets]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#assets){: .btn .btn--success .btn--small} |
| Initialized by task           | [publish]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#publish){: .btn .btn--small} |

The name of the file, or simply the short name of the asset, that has been published.

Unless the [publication service]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/services.md %}) has a custom treatment for this attribute, this is the same of the configured asset [file name]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-assets.md %}#file-name). If the configured value is a [template]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}), this value is the outcome of rendering the source.

### Path

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `releaseAssets/<#>/path`                                                                 |
| Type                          | string                                                                                   |
| Related configuration options | [path]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-assets.md %}#path){: .btn .btn--success .btn--small} [assets]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#assets){: .btn .btn--success .btn--small} |
| Initialized by task           | [publish]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#publish){: .btn .btn--small} |

The URL that the asset is available to. This URL can be used to download the asset once it's published.

When the [configured asset]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-assets.md %}) is a local file, this URL is generated and assigned by the [publication service]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/services.md %}) once the asset has been uploaded. Otherwise, if the configured asset is a remote URL this should be the same as the configured value.

### Type

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `releaseAssets/<#>/type`                                                                 |
| Type                          | string                                                                                   |
| Related configuration options | [type]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-assets.md %}#type){: .btn .btn--success .btn--small} [assets]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#assets){: .btn .btn--success .btn--small} |
| Initialized by task           | [publish]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#publish){: .btn .btn--small} |

The MIME type of the asset.

Unless the [publication service]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/services.md %}) has a custom treatment for this attribute, this is the same of the configured asset [file name]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-assets.md %}#file-name). If the configured value is a [template]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}), this value is the outcome of rendering the source.
