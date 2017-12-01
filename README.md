# (The) env

A tool to help manage environments. Currently only supports setting
kubernetes environments but will likely get support for more stuff as
the Author needs them.

## Kube env

Add alias to `.zshrc` or `.bashrc`:

  alias k='kubectl --namespace=${KUBECTL_NAMESPACE:-default}'

Activate kube env:

  denv kube -p ~/.kube/my-cluster.yaml -n kube-system

Run command:

  k get pods

## Pass env

Requires that you have [pass](https://www.passwordstore.org/) installed.

  denv pass

Or

  denv pass -p /path/to/password-store

