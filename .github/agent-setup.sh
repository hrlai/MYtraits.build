#!/usr/bin/env bash
# Set up R, uvr and the project packages on a fresh Ubuntu cloud agent.
# Shared by the Copilot cloud agent (.github/workflows/copilot-setup-steps.yml)
# and the Claude cloud environment setup script. Run from the repository root:
#   bash .github/agent-setup.sh
set -euo pipefail

SUDO=$(command -v sudo || true)

# uvr CLI, linked onto the default PATH so later agent shells find it.
curl -fsSL https://raw.githubusercontent.com/nbafrank/uvr/main/install.sh | UVR_VERSION=v0.4.8 sh
$SUDO ln -sf "$HOME/.local/bin/uvr" /usr/local/bin/uvr

uvr r install "$(cat .r-version)"
# Packages come as P3M Linux binaries; uvr installs the system libraries they
# link against (from uvr.lock) instead of a hand-kept apt list.
UVR_UNATTENDED=1 uvr sync --frozen --install-system-deps
