LTS=9

all:
	stack --stack-yaml stack-lts-$(LTS).yaml build --copy-bins --local-bin-path "$(shell pwd)/bin"

ghci:
	stack --stack-yaml stack-lts-$(LTS).yaml ghci

ghcid:
	ghcid -c "make ghci"
