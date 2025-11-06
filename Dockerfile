FROM ubuntu:24.04

RUN apt-get update \
    && apt-get install -y \
        curl \
        direnv \
        git \
        just \
        nix \
        sudo \
        vim \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN echo "experimental-features = nix-command flakes" >> /etc/nix/nix.conf
RUN useradd -m -G nix-users,nixbld,sudo -p '$y$j9T$CBgfBXIuPXEWC3Sj3j.cw.$nhbujzxGXVKvIhEvf.DIBENIUnculIT3djAfKmu7As4' dev
# RUN chown dev /nix -R
USER dev
RUN echo 'eval "$(direnv hook bash)"' >> ~/.bashrc
WORKDIR /mnt/src
