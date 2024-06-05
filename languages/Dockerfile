FROM nixos/nix:2.21.2

# see https://discourse.nixos.org/t/cross-compilation-failing-with-nix-and-docker-on-macos/22169/4
# when using heredoc, syntax highlighting by treesitter in neovim does not work
RUN echo 'experimental-features = nix-command flakes' >> /etc/nix/nix.conf \
    && echo 'filter-syscalls = false' >> /etc/nix/nix.conf

# setup binary cache
ARG ATTIC_TOKEN
RUN nix run nixpkgs#attic-client -- login fly https://s3igo-nix-cache.fly.dev $ATTIC_TOKEN
RUN nix run nixpkgs#attic-client -- use docker

# setup copilot.lua
ARG COPILOT_TOKEN
RUN mkdir -p /root/.config/github-copilot \
    && echo $COPILOT_TOKEN > /root/.config/github-copilot/hosts.json

# setup online-judge-tools
ARG ATCODER_USERNAME
ARG ATCODER_PASSWORD
# RUN nix run nixpkgs#online-judge-tools -- login \
#     --username $ATCODER_USERNAME \
#     --password $ATCODER_PASSWORD \
#     https://atcoder.jp

ARG ATCODER_TOKEN
RUN mkdir -p /root/.local/share/online-judge-tools \
   && echo $ATCODER_TOKEN > /root/.local/share/online-judge-tools/cookie.jar

# setup fidget.nvim
RUN mkdir -p /root/.cache/nvim
