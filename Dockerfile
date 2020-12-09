FROM nixos/nix
WORKDIR /app
COPY . .
RUN nix-env -i git
RUN nix-build
CMD ["result/bin/build-websites"]
