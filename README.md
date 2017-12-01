# (The) env

A tool to help manage environments. Currently only supports setting
kubernetes and pass environments but will likely get support for more stuff as
the Author needs them.

## Kube env

Add alias to `.zshrc` or `.bashrc`:

  alias k='kubectl --namespace=${KUBECTL_NAMESPACE:-default}'

Activate kube env:

  eval $(denv kube -p ~/.kube/my-cluster.yaml -n kube-system)

Run command:

  k get pods

## Pass env

Requires that you have [pass](https://www.passwordstore.org/) installed.

  eval $(denv pass)

Or

  eval $(denv pass -p /path/to/password-store)

## Deactivate env

  eval $(denv deactivate)

`NOTE`: `deactivate` should always be called first between invoking activate for different envs.

