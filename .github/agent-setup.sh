#!/usr/bin/env bash
# Set up R, uvr and the project packages on a fresh Ubuntu cloud agent.
# Shared by the Copilot cloud agent (.github/workflows/copilot-setup-steps.yml)
# and the Claude cloud environment setup script. Run from the repository root:
#   bash .github/agent-setup.sh
set -euo pipefail

SUDO=$(command -v sudo || true)

# System libraries for packages in uvr.lock that may build from source:
# curl, openssl, xml2, sf/units/s2, magick, pdftools, systemfonts/textshaping/
# ragg/gdtools.
# shortcut: hand-written list, regenerate with `uvr sysdeps --all` once the
# installed uvr has that command.
$SUDO apt-get update
$SUDO apt-get install -y --no-install-recommends \
  build-essential git curl ca-certificates \
  libcurl4-openssl-dev libssl-dev libxml2-dev \
  libgdal-dev libgeos-dev libproj-dev libudunits2-dev \
  libmagick++-dev libpoppler-cpp-dev \
  libfontconfig1-dev libfreetype6-dev libharfbuzz-dev libfribidi-dev \
  libpng-dev libtiff5-dev libjpeg-dev libwebp-dev libcairo2-dev

# uvr CLI, linked onto the default PATH so later agent shells find it.
curl -fsSL https://raw.githubusercontent.com/nbafrank/uvr/main/install.sh | sh
$SUDO ln -sf "$HOME/.local/bin/uvr" /usr/local/bin/uvr

uvr r install "$(cat .r-version)"
UVR_UNATTENDED=1 uvr sync --frozen
