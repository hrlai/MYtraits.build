#!/usr/bin/env bash
# Set up R, uvr and the project packages on a fresh Ubuntu cloud agent.
# Shared by the Copilot cloud agent (.github/workflows/copilot-setup-steps.yml)
# and the Claude cloud environment setup script. Run from the repository root:
#   bash .github/agent-setup.sh
set -euo pipefail

SUDO=$(command -v sudo || true)

# UTF-8 locale. Cloud images default to POSIX, under which R reads
# config/traits.yml only up to its first non-ASCII character, with a warning.
# Set here for this script and, through GITHUB_ENV, for later Copilot steps.
# Claude sessions do not inherit it: .claude/settings.json sets it with a
# SessionStart hook.
export LANG=C.UTF-8 LC_ALL=C.UTF-8
if [ -n "${GITHUB_ENV:-}" ]; then
  printf 'LANG=C.UTF-8\nLC_ALL=C.UTF-8\n' >> "$GITHUB_ENV"
fi

# uvr CLI, linked onto the default PATH so later agent shells find it.
curl -fsSL https://raw.githubusercontent.com/nbafrank/uvr/main/install.sh | UVR_VERSION=v0.4.8 sh
$SUDO ln -sf "$HOME/.local/bin/uvr" /usr/local/bin/uvr

uvr r install "$(cat .r-version)"
# Link R and Rscript onto the default PATH so Quarto finds the pinned R.
for bin in R Rscript; do
  $SUDO ln -sf "$HOME/.uvr/r-versions/$(cat .r-version)/bin/$bin" \
    "/usr/local/bin/$bin"
done
# Packages come as P3M Linux binaries; uvr installs the system libraries they
# link against (from uvr.lock) instead of a hand-kept apt list.
# Retry: CRAN downloads sometimes fail with a transient 503.
for attempt in 1 2 3; do
  UVR_UNATTENDED=1 uvr sync --frozen --install-system-deps && break
  [ "$attempt" -lt 3 ] || exit 1
  sleep $((attempt * 15))
done

# Quarto CLI, for rendering README.qmd and other .qmd pages. The tarball
# bundles its own pandoc and deno, so no apt packages are needed.
QUARTO_VERSION=1.8.25
curl -fsSL -o /tmp/quarto.tar.gz \
  "https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-amd64.tar.gz"
$SUDO mkdir -p /opt/quarto
$SUDO tar -xzf /tmp/quarto.tar.gz -C /opt/quarto --strip-components=1
$SUDO ln -sf /opt/quarto/bin/quarto /usr/local/bin/quarto
rm -f /tmp/quarto.tar.gz
