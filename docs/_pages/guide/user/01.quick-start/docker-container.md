---
title: Docker Container
layout: single
toc: true
permalink: /guide/user/quick-start/docker-container/
---

In this section you can find instructions to get started using the Docker image in minutes. Consider these your first steps based or a standard scenario but there is much more you can do and control with Nyx. For more on using the Docker image see [Using the Docker image]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#using-the-docker-image).

We assume you already have Docker installed and have base familiarity with it, otherwise take a look at the [Docker Documentation](https://docs.docker.com/).

## Pull the image

First off, you need to get the Docker image. We will be using the `latest` version here. To fetch the image from [Docker Hub](https://hub.docker.com/repository/docker/mooltiverse/nyx) (the default), open a shell and run:

```bash
$ docker pull mooltiverse/nyx:latest
latest: Pulling from mooltiverse/nyx
ab6db1bc80d0: Pull complete
[...]
Digest: sha256:976f4821d643e02fc55c884cd6c9af5e958011132145150b7dd97e01d71ba055
Status: Downloaded newer image for mooltiverse/latest
mooltiverse/latest
```

In case you want to pull it from the [GitHub Container Registry](https://github.com/mooltiverse/nyx/pkgs/container/nyx), use the GitHub's registry like:

```bash
$ docker pull ghcr.io/mooltiverse/nyx::latest
latest: Pulling from mooltiverse/nyx
ab6db1bc80d0: Pull complete
[...]
Digest: sha256:976f4821d643e02fc55c884cd6c9af5e958011132145150b7dd97e01d71ba055
Status: Downloaded newer image for ghcr.io/mooltiverse/latest
ghcr.io/mooltiverse/latest
```

Please note that when using the GitHub Container Registry you may need to authenticate, according to [these instructions](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-container-registry).

Now make sure the image is locally available:

```bash
$ docker image ls
REPOSITORY                                            TAG                 IMAGE ID       CREATED        SIZE
mooltiverse/nyx                                       latest              a14cbc284e81   2 days ago     7.35MB
```

## Run Nyx

Now you're ready to run Nyx from within a container.

```bash
$ docker run -it --rm -v /local/path/to/project:/project mooltiverse/nyx:latest
```

For details on command line options and advanced use see the [Docker run reference](https://docs.docker.com/engine/reference/run/).

The `nyx` executable within the container is executed by default passing the [`infer`]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer) command so the above command line is equivalent to `docker run -it --rm -v /local/path/to/project:/project mooltiverse/nyx:latest infer`.

To run a different command, pass it on the command line. For example, to run the [publish]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#publish) command, run the container as:

```bash
$ docker run -it --rm -v /local/path/to/project:/project mooltiverse/nyx:latest publish
```

When running on Linux or Mac hosts you may also want to map the host user ID to the container user in order to avoid issue with file permissions. You can do this by adding this option to the command line: `-u $(id -u):$(id -g)`. See the [Docker run reference](https://docs.docker.com/engine/reference/run/) for more.
{: .notice--info}

### Mounting the Git project folder

What's important to note in the above command is the `-v /local/path/to/project:/project` option, which mounts a local path (`/local/path/to/project` in this example) to the `/project` path within the container. You need to change `/local/path/to/project` to whatever path contains the Git repository for your project, otherwise Nyx will have no access to the project files and won't be able to run.

In case you have a *Dockerized* development environment, chances are that your project files live inside a Docker volume instead of a local host folder. In this case you can mount the Docker volume instead of the local folder, by bassing the volume name, so the above command would look like:

```bash
$ docker run -it --rm -v project-volume:/project mooltiverse/nyx:latest [...]
```

where `project-volume` is the name of the Docker volume where the Git project lives.

For more on volumes and mounts please see [the official Docker documentation](https://docs.docker.com/storage/volumes/).

## Next steps

From here you can use Nyx just like from the [command line]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#using-the-command-line), with only a few caveats. Depending on the chosen (combination of) [configuration methods]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}) you can pass options on the [command line]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}#command-line-options), as one or more [configuration files]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}#supported-file-grammars) or as [environment variables]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}#environment-variables). When using environment variables you can pass them to the Docker container as one or more `-e` [flags](https://docs.docker.com/engine/reference/run/#env-environment-variables).