# (The) env

A tool to help manage environments. Currently only supports setting
kubernetes and pass environments but will likely get support for more stuff as
the Author needs them.

Usage guide and rationale behind this can be found [here](https://denibertovic.com/posts/haskell-showroom-how-to-switch-between-kubernetes-clusters/) and [here](https://denibertovic.com/posts/switching-between-different-aws-accounts/).

## How to install

* Download a binary from the [releases page](https://github.com/denibertovic/denv/releases)
* Add the below line at the end of your `~/.bashrc`, `~/.zshrc` or `~/.config/fish/conf.d/denv.fish` :

For ZSH:

```bash
eval "$(denv hook ZSH)"
```

For BASH:

```bash
eval "$(denv hook BASH)"
```

For FISH:

```fish
denv hook FISH | source
```

`NOTE`: Make sure to use all caps when choosing the SHELL.

### To compile the latest source:

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

## Raw

Example config file in `/tmp/env`:

```bash
export FOO=foo
```

Activate env with:

```bash
denv source /tmp/env
```

`NOTE`: In essence this just exports whatever is written in the env file and keeps track of it
so that we can unset it later.

## Terraform

Example config file in `prod/env`:

```bash
export ENVIRONMENT=prod
export TF_VAR_some_var_foo=$ENVIRONMENT-foo

```

Activate env with:

```bash
denv source prod/env
```

`NOTE`: This command has been removed in in the TODO version in favor of
using the above "raw" `source` command.

## Vault env

Example config file in `~/.vault/example`:

```bash
export VAULT_ADDR=https://vault.example.com
export VAULT_TOKEN=supersecretotken
export VAULT_SKIP_VERIFY=true
```

Activate env with:

```bash
denv source ~/.vault/example
```

`NOTE`: As with the `terraform` command, this too has been removed in favor of the "raw" `source` command.

## AWS env

### Requirements

You need to configure your `~/.aws/config` and `~/.aws/credentials` before we proceed. Here's an
example:

```
#### ~/.aws/config ####

[profile project1]
region=us-east-1

[profile project2]
region=us-west-2

[profile project3]
region=us-west-2

[profile project1-prod-admin]
role_arn=arn:aws:iam::....:role/admin
mfa_serial=arn:aws:iam::....:mfa/deni
source_profile=project2

[profile project2-prod-admin]
role_arn=arn:aws:iam::....:role/admin
source_profile=project2

[profile project3-test]
source_profile=project3


##### ~/.aws/credentials #####

[project1]
aws_access_key_id=.....
aws_secret_access_key=.....

[project2]
aws_access_key_id=.....
aws_secret_access_key=.....

[project3]
aws_access_key_id=.....
aws_secret_access_key=.....

```

Make sure to set the correct permissions: `chmod 600 ~/.aws/config && chmod 600 ~/.aws/credentials`
`NOTE`: I consider it best practice to delete the `default` profile if you have one.

In the example above we have have configured AWS access for 3 projects.
For `project1` we are accessing the prod environment and are required to enter a MFA token
and assume a role to be able to do anything. Denv will take care of this for us.
In this example denv will first request STS (temporary) credentials, called session credentials,
which we will then use to assume the `admin` role. All further requests are done using the key
and secret from the admin role.

The session credentials and role credentials are both cached in `~/.aws-env/`. Once the role
credentials expire (default is 1 hour but this will be configurable in a future release) we will
use the temporary session credentials to request new ones. The session credentials will expire
in 36 hours (this will be configurable in a future release).
It's worth noting that during these 36 hours you will not be prompted for your MFA token code.

In the second example for `project2`, since we are not using a MFA token, we will not be able to use
temporary session credentials and will use your raw credentials to assume the role.
Conversely, if there is no role to assume, like with project 3, denv will just export your aws_access_key_id and aws_secret_access_key that you specified in the credentials file.

### How to use

Aws environments can be activated in 2 ways:

* Eval form
* Exec form

To activate the eval form run the following command:

`denv aws -p project1-prod-admin`

So now you have the required environment variables injected in your shell.
Try running `export | grep AWS*` to see what they are.

And try running a command like `aws ec2 describe-instances` to verify it works.

To use the exec form run the following command:

`denv aws -p project1-prod-admin -- aws ec2 describe-instances`

The benefit of this is that the variables are only visible to the process you are
calling (in this case the aws cli tool), and are never exported into your current shell.
I prefer using the tool this way.

`NOTE`: As with all the other commands denv tracks the injected variables so that
`denv deactivate` can unset them from your shell.

## Deactivate env

```bash
denv deactivate
```

## LIMITATIONS

`IMPORTANT`: `deactivate` should always be called first between invoking activate for different envs.
The only exception to this is the `aws` exec form described above.

## ZSH completions

Copy `completions/_denv` to `~/.oh-my-zsh/custom/plugins/denv/_denv`
