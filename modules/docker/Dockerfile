FROM alpine:3.15

# Link the image to the repository
LABEL org.opencontainers.image.source https://github.com/mooltiverse/nyx

# The volume to mount is also the working dir
WORKDIR /project
VOLUME /project

# Put the binary into the image
COPY nyx-linux-amd64 /nyx

# Run Infer by default
CMD ["/nyx", "infer"]
