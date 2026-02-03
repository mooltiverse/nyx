FROM alpine:3.20 AS base

# These build arguments are resolved by Docker based on the values passed with
# the --platform option in 'docker buildx build'.
# The BUILD* arguments are related to the host building the container, while
# the TARGET* arguments are about the container and its target architecture.
# See:
# - https://docs.docker.com/build/guide/multi-platform/
# - https://docs.docker.com/engine/reference/builder/#automatic-platform-args-in-the-global-scope
# - https://docs.docker.com/reference/dockerfile/#automatic-platform-args-in-the-global-scope
ARG BUILDPLATFORM
ARG BUILDOS
ARG BUILDARCH
ARG BUILDVARIANT
ARG TARGETPLATFORM
ARG TARGETOS
ARG TARGETARCH
ARG TARGETVARIANT


# The volume to mount is also the working dir
WORKDIR /project
VOLUME /project

# Put the binary into the image
COPY --from=dependencies --chmod=755 binaries/go/nyx/nyx-linux-amd64 /usr/bin/nyx

# Add the script used as an entrypoint by the GitHub Action
COPY action-entrypoint.sh /action-entrypoint.sh
RUN chmod ugo+x /action-entrypoint.sh

# Also install Git, as it's required by the libraries used in Go
RUN apk update && \
    apk add git

# Run Nyx, using the Infer command by default.
ENTRYPOINT ["/usr/bin/nyx"]
