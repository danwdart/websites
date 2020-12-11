FROM nixos/nix
WORKDIR /app
COPY . .
RUN nix-build
CMD ["sh"]
