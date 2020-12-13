FROM nixos/nix
WORKDIR /app
COPY . .
RUN nix-env -iA cachix -f https://cachix.org/api/v1/install
RUN cachix use websites
RUN nix-env -i git
ARG CACHIX_AUTH_TOKEN
ENV CACHIX_AUTH_TOKEN=$CACHIX_AUTH_TOKEN
RUN nix-build | cachix push websites
CMD ["nix-shell"]