module github.com/mooltiverse/nyx/src/go/nyx

go 1.27.3

replace github.com/mooltiverse/nyx/src/go/errors => ../errors

replace github.com/mooltiverse/nyx/src/go/utils => ../utils

replace github.com/mooltiverse/nyx/src/go/version => ../version

require (
	github.com/aymerick/raymond v2.0.2+incompatible
	github.com/bmatcuk/doublestar/v4 v4.10.4
	github.com/dlclark/regexp2 v1.12.0
	github.com/go-git/go-git/v5 v5.19.3
	github.com/google/go-github v17.0.0+incompatible
	github.com/mooltiverse/nyx/src/go/errors v0.0.0-20261006085516-45e71d646b92
	github.com/mooltiverse/nyx/src/go/utils v0.0.0-20261006085516-45e71d646b92
	github.com/mooltiverse/nyx/src/go/version v0.0.0-20261006085516-45e71d646b92
	github.com/sirupsen/logrus v1.10.2
	github.com/stretchr/testify v1.12.1
	gitlab.com/gitlab-org/api/client-go v1.46.0
	golang.org/x/crypto v0.58.0
	golang.org/x/exp v0.0.0-20261109185158-a52e3aef5920
	golang.org/x/oauth2 v0.38.0
	gopkg.in/yaml.v3 v3.0.1
)

require (
	dario.cat/mergo v1.0.2 // indirect
	github.com/Microsoft/go-winio v0.6.4 // indirect
	github.com/ProtonMail/go-crypto v1.5.2 // indirect
	github.com/cloudflare/circl v1.6.5 // indirect
	github.com/cyphar/filepath-securejoin v0.7.0 // indirect
	github.com/emirpasic/gods v1.18.1 // indirect
	github.com/go-git/gcfg v1.5.1-0.20230307220236-3a3c6141e376 // indirect
	github.com/go-git/go-billy/v5 v5.9.2 // indirect
	github.com/golang/groupcache v0.0.0-20241129210726-2c02b8208cf8 // indirect
	github.com/google/go-querystring v1.2.0 // indirect
	github.com/hashicorp/go-cleanhttp v0.5.2 // indirect
	github.com/hashicorp/go-retryablehttp v0.7.8 // indirect
	github.com/jbenet/go-context v0.0.0-20150711004518-d14ea06fba99 // indirect
	github.com/kevinburke/ssh_config v1.6.0 // indirect
	github.com/pjbgf/sha1cd v0.7.0 // indirect
	github.com/sergi/go-diff v1.4.0 // indirect
	github.com/skeema/knownhosts v1.3.3 // indirect
	github.com/xanzy/ssh-agent v0.3.3 // indirect
	go.yaml.in/yaml/v3 v3.0.5 // indirect
	golang.org/x/net v0.62.0 // indirect
	golang.org/x/sys v0.50.0 // indirect
	golang.org/x/time v0.16.0 // indirect
	gopkg.in/warnings.v0 v0.1.2 // indirect
)
