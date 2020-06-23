.DEFAULT_GOAL = help

# called from top level Makefile
# include etc/nix.mk

# unstable or 20.03
NIXV = unstable

init: utils ## initialize project
	if  [ "$(NIXV)" = "unstable" ]; \
	then nix-prefetch-git https://github.com/nixos/nixpkgs-channels.git refs/heads/nixpkgs-$(NIXV) > nix/nix.json; \
	else nix-prefetch-git https://github.com/nixos/nixpkgs-channels.git refs/heads/nixos-$(NIXV) > nix/nix.json; \
	fi
	nix-shell --run "cabal install fswatcher"

nix-shell: ## nix-shell
	nix-shell --pure shell.nix

utils: ## install utilities
	nix-env --install nix-prefetch-git

install-nix: ## install nix--run this target first
	curl -L https://nixos.org/nix/install > /tmp/install
	chmod +x /tmp/install
	/tmp/install --darwin-use-unencrypted-nix-store-volume

cabal-update: ## cabal update
	cabal v2-update

cabal-config: ## user cabal config
	cabal user-config -update

clobber: ## clobber
	echo noop

help: ## help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-18s\033[0m %s\n", $$1, $$2}'
