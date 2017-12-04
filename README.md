# (The) env

A tool to help manage environments. Currently only supports setting
kubernetes and pass environments but will likely get support for more stuff as
the Author needs them.

## How to install

* Install [stack](https://docs.haskellstack.org/en/stable/README/)
* Clone this repo
* cd to the cloned repo and run: `stack install`
* Add `~/.local/bin` to your PATH.

## How it works

Denv uses a hook that it executes during each shell re-print. It check to see if a
temporary file in `~/.denv` exists. This file contains information on which environment
variables to set and which to unset. Once it processes the file it will remove it.

See below on how to activate different environments.

## Kube env

Add alias to `.zshrc` or `.bashrc`:

```bash
alias k='kubectl --namespace=${KUBECTL_NAMESPACE:-default}'
```

And add the hook at the end of your `.zshrc` or `.bashrc` file:

```bash
eval "$(denv hook ZSH)"
```

Or

```bash
eval "$(denv hook BASH)"
```

`NOTE`: Make sure to use all caps when choosing the SHELL.

Activate kube env:

```bash
denv kube -p ~/.kube/my-cluster.yaml -n kube-system
```

Run command:

```bash
k get pods
```

## Pass env

Requires that you have [pass](https://www.passwordstore.org/) installed.

Use the current dir as the password store:

```bash
denv pass
```

Or specify a path to the password store:

```bash
denv pass -p /path/to/password-store
```

## Deactivate env

```bash
denv deactivate
```

## LIMITATIONS

`IMPORTANT`: `deactivate` should always be called first between invoking activate for different envs.

