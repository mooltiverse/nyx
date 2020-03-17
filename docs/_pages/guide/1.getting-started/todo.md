
In most cases Nyx is used to just generate a new [semantic version number]({{ site.baseurl }}/in-depth/design-principles/#semantic-version) and publish the new release by creating a new [Git tag]({{ site.baseurl }}/in-depth/design-principles/#git-tags). We will see there is plenty of options to customize and extend Nyx behavior but for now let's start with this simple use case.

In order to determine the new version number Nyx needs to know the latest previously released version and to do so it inspects the Git repository looking for existing tags representing releases (and ignoring all other tags). If no previous release tag is found Nyx uses the default initial version or you can also override the value and force a specific version.

The next step is to collect the facts telling Nyx which rules to apply in order to increment the last release number and define the new version number. These facts come from:
* the commit history and the messages that have been defined along with each commit
* the current Git branch
* the current workspace status
* the configuration


------------------------------------------



## Branching model
Nyx relies on [Git branches](https://git-scm.com/book/it/v2/Git-Branching-Basic-Branching-and-Merging) as to determine the type of release and the version to generate.

## Branching strategy support
*Nyx* support for Git branching models comes with built-in configurations for the most popular workflows but is also fully customizable, making no assumptions on which branches you use and at which stage. You can manage *release* branches as well as *pre-release* or *maintenance* (*post-release*) branches.

Other branches (like *feature branches*) can still have their version numbers assigned, properly identified to avoid clashes with official releases. So called *dirty* workspaces (with uncommitted changes) can also build with specific version numbers, to let developers iterate locally before committing. These branches are not meant to publish official releases but, instead, give developers *continuity*.

Local environments are important to *Nyx* to give developers a consistent build process that seamlessly adapts when on local or CI environments. This is meant to grant consistency of the build process and avoid workarounds driving to *works on my machine* situations.

## Release types
