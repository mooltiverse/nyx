---
title: Requirements
layout: single
toc: true
permalink: /guide/user/introduction/requirements/
---

## System requirements

### Command line binaries

Supported platforms are those listed in the [available release assets](https://github.com/mooltiverse/nyx/releases/latest), namely:

* `darwin` `amd64`
* `darwin` `arm64`
* `dragonfly` `amd64`
* `freebsd` `386`
* `freebsd` `amd64`
* `freebsd` `arm`
* `linux` `386`
* `linux` `amd64`
* `linux` `arm`
* `linux` `arm64`
* `linux` `ppc64`
* `linux` `ppc64le`
* `linux` `mips`
* `linux` `mipsle`
* `linux` `mips64`
* `linux` `mips64le`
* `netbsd` `386`
* `netbsd` `amd64`
* `netbsd` `arm`
* `openbsd` `386`
* `openbsd` `amd64`
* `openbsd` `arm`
* `openbsd` `arm64`
* `solaris` `amd64`
* `windows` `386`
* `windows` `amd64`
* `windows` `arm`
* `windows` `arm64`

### Docker

There are no known incompatibilities for the Docker image therefore it is compatible with any container engine.

### Java and Gradle

The recommended runtime is Java VM `15` or newer and Gradle `7.0` or newer. Tests are conducted up to Java VM `19` and Gradle versions up to `8.0`.

Although you may encounter some limitations (i.e. about support for some encryption or hashing algorithms when using SSH authentication), older versions are also supported. Specifically, Java VM `11` or newer are supported and Gradle version `6.0` and later is supported with the exception of versions from `6.5.x` to `6.9`.x (due to the [ASM](https://asm.ow2.io/index.html) version used by Gradle).

Java VMs older than `11` and Gradle versions older than `6.0` are not supported.

[Here](https://gradle.org/releases/) you can find the list of available releases and [here](https://docs.gradle.org/current/userguide/compatibility.html) is the Gradle compatibility matrix.

### GitHub Action

This is a [Docker Action](https://docs.github.com/en/actions/creating-actions/about-custom-actions#types-of-actions) so it's only available for Linux runners.

Please refer to the [GitHub Action home page](https://github.com/mooltiverse/nyx-github-action).
