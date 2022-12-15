FROM alpine:3.16.2

# Link the image to the repository
LABEL org.opencontainers.image.source https://github.com/mooltiverse/nyx

# The volume to mount is also the working dir
WORKDIR /project
VOLUME /project

# Put the binary into the image
COPY nyx-linux-amd64 /usr/bin/nyx

# Also install Git, as it's required by the libraries used in Go
RUN apk update \
    apk add git

# Run Nyx, using the Infer command by default.
ENTRYPOINT ["/usr/bin/nyx"]
