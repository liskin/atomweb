all:
	stack build --copy-bins --local-bin-path "$(shell pwd)/bin"

ghci:
	stack ghci atomweb

ghcid:
	ghcid --restart package.yaml -c "make ghci"
