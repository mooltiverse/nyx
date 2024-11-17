FROM alpine:3.20

# The volume to mount is also the working dir
WORKDIR /project
VOLUME /project

# Put the binary into the image
COPY --from=bin nyx-linux-amd64 /usr/bin/nyx

# Also install Git, as it's required by the libraries used in Go
RUN apk update && \
    apk add git

# Run Nyx, using the Infer command by default.
ENTRYPOINT ["/usr/bin/nyx"]
