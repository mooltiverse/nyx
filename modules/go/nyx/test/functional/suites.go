/*
 * Copyright 2020 Mooltiverse
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package functional_test

import (
	"os" // https://pkg.go.dev/os

	gittools "github.com/mooltiverse/nyx/modules/go/nyx/test/integration/git/tools"
	utl "github.com/mooltiverse/nyx/modules/go/utils"
)

/*
This list of test suites tries to narrow down the number of suites by:
  - adding the most complexity to suites so simples suites can be skipped, and so use a combination
    of configuration means to test them all together, along with their prioriries
*/
var (
	/*
		An ordered list of functional test suites.
		These is a small set of tests to run also when quick tests are selected.
	*/
	wellKnownFunctionalTestSuitesQuick = []*TestSuite{
		// This suite runs Nyx Infer using a common configuration
		&TestSuite{
			Name:       "Infer",
			Scenario:   gittools.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS(),
			NyxCommand: nil, // run Infer as the default command
			Args: []string{
				"--dry-run=false",
				"--resume=false",
				"--summary-file=.nyx-summary.txt",
				"--state-file=.nyx-state.yml",
				"--debug",
			},
			Env: map[string]string{
				"NYX_PRESET": "extended",
			},
			Files: map[string]string{
				// the .gitignore file, required to avoid conflicts due to locked files (i.e. 'The process cannot access the file because another process has locked a portion of the file') etc
				".gitignore": `.gitignore
.gradle
build.gradle
settings.gradle
.nyx-summary.txt
.nyx-state.yml
`,
				// the .nyx-shared.yaml is the standard shared configuration file
				".nyx-shared.yaml": `---
commitMessageConventions:
  enabled:
    - conventionalCommits
  items:
    conventionalCommits:
      expression: "(?m)^(?<type>[a-zA-Z0-9_]+)(!)?(\\((?<scope>[a-z ]+)\\))?:( (?<title>.+))$(?s).*"
      bumpExpressions:
        major: "(?s)(?m)^[a-zA-Z0-9_]+(!|.*^(BREAKING( |-)CHANGE: )).*"
        minor: "(?s)(?m)^feat(?!!|.*^(BREAKING( |-)CHANGE: )).*"
        patch: "(?s)(?m)^fix(?!!|.*^(BREAKING( |-)CHANGE: )).*"
initialVersion: "1.0.0"
releaseLenient: true
releasePrefix: v
scheme: SEMVER
`,
				// the .nyx.json is the standard configuration file
				".nyx.json": `{
    "releaseTypes": {
        "items": {
            "mainline": {
                "collapseVersions":false,
                "filterTags":"^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$",
                "gitCommit":"true",
                "gitCommitMessage":"Release version {{version}}",
                "gitPush":"false",
                "gitTag":"true",
                "gitTagMessage":"Tag version {{version}}",
                "matchBranches":"^(master|main)$",
                "publish":"true"
            }
        }
    }
}
`,
			},
			RemoteRepoName:     nil,
			HostingRepoService: nil,
			FileContentChecks: map[string][]string{
				".nyx-state.yml": []string{
					"bump: minor",
					"initialVersion: 1.0.0",
					"preset: extended",
					"releaseLenient: true",
					"releasePrefix: v",
					"coreVersion: true",
					"newVersion: true",
					"newRelease: true",
					"previousVersion: 0.0.4",
					"primeVersion: 0.0.4",
					"version: v0.1.0",
				},
				".nyx-summary.txt": []string{
					"bump             = minor",
					"core version     = true",
					"new release      = true",
					"new version      = true",
					"scheme           = SEMVER",
					"timestamp        = ", // we don't need to test for the timestamp value here
					"current version  = v0.1.0",
					"previous version = 0.0.4",
					"prime version    = 0.0.4",
				},
			},
			RepositoryTags:       []string{}, // no new tag is created by Infer
			RemoteRepositoryTags: []string{}, // not using remotes for this suite
			HostedReleaseTags:    []string{}, // not using hosting services for this suite
		},

		// This suite runs Nyx Publish using a common configuration
		&TestSuite{
			Name:       "Common",
			Scenario:   gittools.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS(),
			NyxCommand: utl.PointerToString("publish"),
			Args: []string{
				"--dry-run=false",
				"--resume=false",
				"--summary-file=.nyx-summary.txt",
				"--state-file=.nyx-state.yml",
				"--debug",
			},
			Env: map[string]string{
				"NYX_PRESET": "extended",
			},
			Files: map[string]string{
				// the .gitignore file, required to avoid conflicts due to locked files (i.e. 'The process cannot access the file because another process has locked a portion of the file') etc
				".gitignore": `.gitignore
.gradle
build.gradle
settings.gradle
.nyx-summary.txt
.nyx-state.yml
`,
				// the .nyx-shared.yaml is the standard shared configuration file
				".nyx-shared.yaml": `---
commitMessageConventions:
  enabled:
    - conventionalCommits
  items:
    conventionalCommits: 
      expression: "(?m)^(?<type>[a-zA-Z0-9_]+)(!)?(\\((?<scope>[a-z ]+)\\))?:( (?<title>.+))$(?s).*"
      bumpExpressions: 
        major: "(?s)(?m)^[a-zA-Z0-9_]+(!|.*^(BREAKING( |-)CHANGE: )).*"
        minor: "(?s)(?m)^feat(?!!|.*^(BREAKING( |-)CHANGE: )).*"
        patch: "(?s)(?m)^fix(?!!|.*^(BREAKING( |-)CHANGE: )).*"
initialVersion: "1.0.0"
releaseLenient: true
releasePrefix: v
scheme: SEMVER
`,
				// the .nyx.json is the standard configuration file
				".nyx.json": `{
	"changelog":{
		"path":"CHANGELOG.md",
		"sections":{
			"Added":"^feat$",
			"Fixed":"^fix$"
		},
		"substitutions":{
			"(?m)#([0-9]+)(?s)": "[#%s](https://example.com/issues/%s)"
		}
	},
    "releaseTypes": {
        "items": {
            "mainline": {
                "collapseVersions":false,
                "filterTags":"^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$",
                "gitCommit":"true",
                "gitCommitMessage":"Release version {{version}}",
                "gitPush":"false",
                "gitTag":"true",
                "gitTagMessage":"Tag version {{version}}",
                "matchBranches":"^(master|main)$",
                "publish":"true"
			}
		}
	},
	"substitutions":{
		"enabled":[
			"node_version",
			"text_version"
		]
	}
}				  
`,
				// the version.txt file, used to test for substitutions, initialized with a fake version number to be replaced by Make
				"version.txt": `91.92.93`,
			},
			RemoteRepoName:     nil,
			HostingRepoService: nil,
			FileContentChecks: map[string][]string{
				".nyx-state.yml": []string{
					"bump: minor",
					"initialVersion: 1.0.0",
					"preset: extended",
					"releaseLenient: true",
					"releasePrefix: v",
					"coreVersion: true",
					"newVersion: true",
					"newRelease: true",
					"previousVersion: 0.0.4",
					"primeVersion: 0.0.4",
					"version: v0.1.0",
				},
				".nyx-summary.txt": []string{
					"bump             = minor",
					"core version     = true",
					"new release      = true",
					"new version      = true",
					"scheme           = SEMVER",
					"timestamp        = ", // we don't need to test for the timestamp value here
					"current version  = v0.1.0",
					"previous version = 0.0.4",
					"prime version    = 0.0.4",
				},
				"CHANGELOG.md": []string{
					"## v0.1.0",
					"### Added",
					"* [] feat: Untagged commit [#2](https://example.com/issues/2)",
					"### Fixed",
					"* [] fix: Untagged commit [#1](https://example.com/issues/1)",
				},
				"version.txt": []string{
					"v0.1.0",
				},
			},
			RepositoryTags:       []string{"v0.1.0"},
			RemoteRepositoryTags: []string{}, // not using remotes for this suite
			HostedReleaseTags:    []string{}, // not using hosting services for this suite
		},

		// This suite runs Nyx using a common configuration with also a remote repository (locally stored)
		&TestSuite{
			Name:       "Remote",
			Scenario:   gittools.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS(),
			NyxCommand: utl.PointerToString("publish"),
			Args: []string{
				"--dry-run=false",
				"--resume=false",
				"--summary-file=.nyx-summary.txt",
				"--state-file=.nyx-state.yml",
				"--debug",
			},
			Env: map[string]string{
				"NYX_PRESET": "extended",
			},
			Files: map[string]string{
				// the .gitignore file, required to avoid conflicts due to locked files (i.e. 'The process cannot access the file because another process has locked a portion of the file') etc
				".gitignore": `.gitignore
.gradle
build.gradle
settings.gradle
.nyx-summary.txt
.nyx-state.yml
`,
				// the .nyx-shared.yaml is the standard shared configuration file
				".nyx-shared.yaml": `---
commitMessageConventions:
  enabled:
    - conventionalCommits
  items:
    conventionalCommits: 
      expression: "(?m)^(?<type>[a-zA-Z0-9_]+)(!)?(\\((?<scope>[a-z ]+)\\))?:( (?<title>.+))$(?s).*"
      bumpExpressions: 
        major: "(?s)(?m)^[a-zA-Z0-9_]+(!|.*^(BREAKING( |-)CHANGE: )).*"
        minor: "(?s)(?m)^feat(?!!|.*^(BREAKING( |-)CHANGE: )).*"
        patch: "(?s)(?m)^fix(?!!|.*^(BREAKING( |-)CHANGE: )).*"
initialVersion: "1.0.0"
releaseLenient: true
releasePrefix: v
scheme: SEMVER
`,
				// the .nyx.json is the standard configuration file
				".nyx.json": `{
	"changelog":{
		"path":"CHANGELOG.md",
		"sections":{
			"Added":"^feat$",
			"Fixed":"^fix$"
		},
		"substitutions":{
			"(?m)#([0-9]+)(?s)": "[#%s](https://example.com/issues/%s)"
		}
	},
    "releaseTypes": {
		"remoteRepositories":[
            "replica"
        ],
        "items": {
            "mainline": {
                "collapseVersions":false,
                "filterTags":"^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$",
                "gitCommit":"true",
                "gitCommitMessage":"Release version {{version}}",
                "gitPush":"true",
                "gitTag":"true",
                "gitTagMessage":"Tag version {{version}}",
                "matchBranches":"^(master|main)$",
                "publish":"true"
			}
		}
	},
	"substitutions":{
		"enabled":[
			"node_version",
			"text_version"
		]
	}
}				  
`,
				// the version.txt file, used to test for substitutions, initialized with a fake version number to be replaced by Make
				"version.txt": `91.92.93`,
			},
			RemoteRepoName:     utl.PointerToString("replica"),
			HostingRepoService: nil,
			FileContentChecks: map[string][]string{
				".nyx-state.yml": []string{
					"bump: minor",
					"initialVersion: 1.0.0",
					"preset: extended",
					"releaseLenient: true",
					"releasePrefix: v",
					"coreVersion: true",
					"newVersion: true",
					"newRelease: true",
					"previousVersion: 0.0.4",
					"primeVersion: 0.0.4",
					"version: v0.1.0",
				},
				".nyx-summary.txt": []string{
					"bump             = minor",
					"core version     = true",
					"new release      = true",
					"new version      = true",
					"scheme           = SEMVER",
					"timestamp        = ", // we don't need to test for the timestamp value here
					"current version  = v0.1.0",
					"previous version = 0.0.4",
					"prime version    = 0.0.4",
				},
				"CHANGELOG.md": []string{
					"## v0.1.0",
					"### Added",
					"* [] feat: Untagged commit [#2](https://example.com/issues/2)",
					"### Fixed",
					"* [] fix: Untagged commit [#1](https://example.com/issues/1)",
				},
				"version.txt": []string{
					"v0.1.0",
				},
			},
			RepositoryTags:       []string{"v0.1.0"},
			RemoteRepositoryTags: []string{"v0.1.0"},
			HostedReleaseTags:    []string{}, // not using hosting services for this suite
		},
	}

	/*
		An ordered list of functional test suites.
		These is a small set of tests to run also when quick tests are selected.
	*/
	wellKnownFunctionalTestSuitesNonQuick = []*TestSuite{
		// This suite runs Nyx using a custom configuration with also a remote repository (locally stored)
		&TestSuite{
			Name:       "Custom",
			Scenario:   gittools.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED(),
			NyxCommand: utl.PointerToString("publish"),
			Args: []string{
				"--dry-run=false",
				"--resume=false",
				"--summary-file=.nyx-summary.txt",
				"--state-file=.nyx-state.yml",
				"--debug",
			},
			Env: map[string]string{
				"NYX_PRESET": "extended",
			},
			Files: map[string]string{
				// the .gitignore file, required to avoid conflicts due to locked files (i.e. 'The process cannot access the file because another process has locked a portion of the file') etc
				".gitignore": `.gitignore
.gradle
build.gradle
settings.gradle
.nyx-summary.txt
.nyx-state.yml
`,
				// the .nyx-shared.yaml is the standard shared configuration file
				".nyx-shared.yaml": `---
commitMessageConventions:
  enabled:
    - testConvention
  items:
    testConvention:
      expression: ".*"
      bumpExpressions:
        patch: ".*"
`,
				// the .nyx.json is the standard configuration file
				".nyx.json": `{
	"changelog":{
		"path":"CHANGELOG.md",
		"sections":{
			"Added":"^feat$",
			"Fixed":"^fix$"
		},
		"substitutions":{
			"(?m)#([0-9]+)(?s)": "[#%s](https://example.com/issues/%s)"
		}
	},
    "releaseTypes": {
		"remoteRepositories":[
            "replica"
        ],
        "items": {
            "mainline": {
                "collapseVersions":false,
                "filterTags":"^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$",
                "gitCommit":"true",
                "gitCommitMessage":"Release version {{version}}",
                "gitPush":"true",
                "gitTag":"true",
                "gitTagMessage":"Tag version {{version}}",
                "matchBranches":"^(master|main)$",
                "publish":"true"
			}
		}
	},
	"substitutions":{
		"enabled":[
			"node_version",
			"text_version"
		]
	}
}				  
`,
				// the version.txt file, used to test for substitutions, initialized with a fake version number to be replaced by Make
				"version.txt": `91.92.93`,
			},
			RemoteRepoName:     utl.PointerToString("replica"),
			HostingRepoService: nil,
			FileContentChecks: map[string][]string{
				".nyx-state.yml": []string{
					"branch: master",
					"bump: patch",
					"initialVersion: 0.1.0",
					"preset: extended",
					"coreVersion: true",
					"newVersion: true",
					"newRelease: true",
					"previousVersion: 0.0.5",
					"primeVersion: 0.0.5",
					"version: 0.0.6",
				},
				".nyx-summary.txt": []string{
					"bump             = patch",
					"core version     = true",
					"new release      = true",
					"new version      = true",
					"scheme           = SEMVER",
					"timestamp        = ", // we don't need to test for the timestamp value here
					"current version  = 0.0.6",
					"previous version = 0.0.5",
					"prime version    = 0.0.5",
				},
				"CHANGELOG.md": []string{
					"## 0.0.6",
					"No changes.",
				},
				"version.txt": []string{
					"v0.1.0",
				},
			},
			RepositoryTags:       []string{"0.0.6"},
			RemoteRepositoryTags: []string{"0.0.6"},
			HostedReleaseTags:    []string{}, // not using hosting services for this suite
		},

		// This suite runs Nyx using a common configuration also with GitHub as the hosting service
		&TestSuite{
			Name:       "GitHub",
			Scenario:   gittools.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS(),
			NyxCommand: utl.PointerToString("publish"),
			Args: []string{
				"--dry-run=false",
				"--resume=false",
				"--summary-file=.nyx-summary.txt",
				"--state-file=.nyx-state.yml",
				"--debug",
			},
			Env: map[string]string{
				"GH_TOKEN":                        os.Getenv("gitHubTestUserToken"), // The 'gitHubTestUserToken' variable is set in Gradle build scripts
				"NYX_PRESET":                      "extended",
				"NYX_GIT_REMOTES_origin_USER":     "{{#environmentVariable}}GH_TOKEN{{/environmentVariable}}",
				"NYX_GIT_REMOTES_origin_PASSWORD": "",
			},
			Files: map[string]string{
				// the .gitignore file, required to avoid conflicts due to locked files (i.e. 'The process cannot access the file because another process has locked a portion of the file') etc
				".gitignore": `.gitignore
.gradle
build.gradle
settings.gradle
.nyx-summary.txt
.nyx-state.yml
`,
				// the .nyx-shared.yaml is the standard shared configuration file
				".nyx-shared.yaml": `---
commitMessageConventions:
  enabled:
    - conventionalCommits
  items:
    conventionalCommits: 
      expression: "(?m)^(?<type>[a-zA-Z0-9_]+)(!)?(\\((?<scope>[a-z ]+)\\))?:( (?<title>.+))$(?s).*"
      bumpExpressions: 
        major: "(?s)(?m)^[a-zA-Z0-9_]+(!|.*^(BREAKING( |-)CHANGE: )).*"
        minor: "(?s)(?m)^feat(?!!|.*^(BREAKING( |-)CHANGE: )).*"
        patch: "(?s)(?m)^fix(?!!|.*^(BREAKING( |-)CHANGE: )).*"
initialVersion: "1.0.0"
releaseLenient: true
releasePrefix: v
scheme: SEMVER
`,
				// the .nyx.json is the standard configuration file
				".nyx.json": `{
	"changelog":{
		"path":"CHANGELOG.md",
		"sections":{
			"Added":"^feat$",
			"Fixed":"^fix$"
		},
		"substitutions":{
			"(?m)#([0-9]+)(?s)": "[#%s](https://example.com/issues/%s)"
		}
	},
	"releaseAssets": {
		"asset1":{
			"fileName":"CHANGELOG.md",
			"description":"CHANGELOG",
			"type":"text/plain",
			"path":"CHANGELOG.md"
		}
	},
    "releaseTypes": {
		"remoteRepositories":[
            "origin"
        ],
        "items": {
            "mainline": {
                "collapseVersions":false,
                "filterTags":"^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$",
                "gitCommit":"true",
                "gitCommitMessage":"Release version {{version}}",
                "gitPush":"false",
                "gitTag":"true",
                "gitTagMessage":"Tag version {{version}}",
                "matchBranches":"^(master|main)$",
                "publish":"true"
			}
		}
	},
	"substitutions":{
		"enabled":[
			"node_version",
			"text_version"
		]
	}
}				  
`,
				// the version.txt file, used to test for substitutions, initialized with a fake version number to be replaced by Make
				"version.txt": `91.92.93`,
			},
			RemoteRepoName:     nil, //utl.PointerToString("origin"), // use 'origin', from the cloned repository from the hosting service
			HostingRepoService: utl.PointerToString("github"),
			FileContentChecks: map[string][]string{
				".nyx-state.yml": []string{
					"bump: minor",
					"initialVersion: 1.0.0",
					"preset: extended",
					"releaseLenient: true",
					"releasePrefix: v",
					"coreVersion: true",
					"newVersion: true",
					"newRelease: true",
					"previousVersion: 0.0.4",
					"primeVersion: 0.0.4",
					"version: v0.1.0",
				},
				".nyx-summary.txt": []string{
					"bump             = minor",
					"core version     = true",
					"new release      = true",
					"new version      = true",
					"scheme           = SEMVER",
					"timestamp        = ", // we don't need to test for the timestamp value here
					"current version  = v0.1.0",
					"previous version = 0.0.4",
					"prime version    = 0.0.4",
				},
				"CHANGELOG.md": []string{
					"## v0.1.0",
					"### Added",
					"* [] feat: Untagged commit [#2](https://example.com/issues/2)",
					"### Fixed",
					"* [] fix: Untagged commit [#1](https://example.com/issues/1)",
				},
				"version.txt": []string{
					"v0.1.0",
				},
			},
			RepositoryTags:       []string{"v0.1.0"},
			RemoteRepositoryTags: nil, // here we don't use the locally managed remote repository
			HostedReleaseTags:    nil, // we can't test these because we'd need to pass the 'publicationServices' and 'services' configuration, which requires the repo name and owner in advance, but they are created dynamically during the test
		},

		// This suite runs Nyx using a common configuration also with GitLab as the hosting service
		&TestSuite{
			Name:       "GitLab",
			Scenario:   gittools.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS(),
			NyxCommand: utl.PointerToString("publish"),
			Args: []string{
				"--dry-run=false",
				"--resume=false",
				"--summary-file=.nyx-summary.txt",
				"--state-file=.nyx-state.yml",
				"--debug",
			},
			Env: map[string]string{
				"GL_TOKEN":                        os.Getenv("gitLabTestUserToken"), // The 'gitLabTestUserToken' variable is set in Gradle build scripts
				"NYX_PRESET":                      "extended",
				"NYX_GIT_REMOTES_origin_USER":     "PRIVATE-TOKEN",
				"NYX_GIT_REMOTES_origin_PASSWORD": "{{#environmentVariable}}GL_TOKEN{{/environmentVariable}}",
			},
			Files: map[string]string{
				// the .gitignore file, required to avoid conflicts due to locked files (i.e. 'The process cannot access the file because another process has locked a portion of the file') etc
				".gitignore": `.gitignore
.gradle
build.gradle
settings.gradle
.nyx-summary.txt
.nyx-state.yml
`,
				// the .nyx-shared.yaml is the standard shared configuration file
				".nyx-shared.yaml": `---
commitMessageConventions:
  enabled:
    - conventionalCommits
  items:
    conventionalCommits: 
      expression: "(?m)^(?<type>[a-zA-Z0-9_]+)(!)?(\\((?<scope>[a-z ]+)\\))?:( (?<title>.+))$(?s).*"
      bumpExpressions: 
        major: "(?s)(?m)^[a-zA-Z0-9_]+(!|.*^(BREAKING( |-)CHANGE: )).*"
        minor: "(?s)(?m)^feat(?!!|.*^(BREAKING( |-)CHANGE: )).*"
        patch: "(?s)(?m)^fix(?!!|.*^(BREAKING( |-)CHANGE: )).*"
initialVersion: "1.0.0"
releaseLenient: true
releasePrefix: v
scheme: SEMVER
`,
				// the .nyx.json is the standard configuration file
				".nyx.json": `{
	"changelog":{
		"path":"CHANGELOG.md",
		"sections":{
			"Added":"^feat$",
			"Fixed":"^fix$"
		},
		"substitutions":{
			"(?m)#([0-9]+)(?s)": "[#%s](https://example.com/issues/%s)"
		}
	},
	"releaseAssets": {
		"asset1":{
			"fileName":"CHANGELOG.md",
			"description":"CHANGELOG",
			"type":"text/plain",
			"path":"CHANGELOG.md"
		}
	},
    "releaseTypes": {
		"remoteRepositories":[
            "origin"
        ],
        "items": {
            "mainline": {
                "collapseVersions":false,
                "filterTags":"^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$",
                "gitCommit":"true",
                "gitCommitMessage":"Release version {{version}}",
                "gitPush":"false",
                "gitTag":"true",
                "gitTagMessage":"Tag version {{version}}",
                "matchBranches":"^(master|main)$",
                "publish":"true"
			}
		}
	},
	"substitutions":{
		"enabled":[
			"node_version","text_version"
		]
	}
}				  
`,
				// the version.txt file, used to test for substitutions, initialized with a fake version number to be replaced by Make
				"version.txt": `91.92.93`,
			},
			RemoteRepoName:     nil, //utl.PointerToString("origin"), // use 'origin', from the cloned repository from the hosting service
			HostingRepoService: utl.PointerToString("gitlab"),
			FileContentChecks: map[string][]string{
				".nyx-state.yml": []string{
					"bump: minor",
					"initialVersion: 1.0.0",
					"preset: extended",
					"releaseLenient: true",
					"releasePrefix: v",
					"coreVersion: true",
					"newVersion: true",
					"newRelease: true",
					"previousVersion: 0.0.4",
					"primeVersion: 0.0.4",
					"version: v0.1.0",
				},
				".nyx-summary.txt": []string{
					"bump             = minor",
					"core version     = true",
					"new release      = true",
					"new version      = true",
					"scheme           = SEMVER",
					"timestamp        = ", // we don't need to test for the timestamp value here
					"current version  = v0.1.0",
					"previous version = 0.0.4",
					"prime version    = 0.0.4",
				},
				"CHANGELOG.md": []string{
					"## v0.1.0",
					"### Added",
					"* [] feat: Untagged commit [#2](https://example.com/issues/2)",
					"### Fixed",
					"* [] fix: Untagged commit [#1](https://example.com/issues/1)",
				},
				"version.txt": []string{
					"v0.1.0",
				},
			},
			RepositoryTags:       []string{"v0.1.0"},
			RemoteRepositoryTags: nil, // here we don't use the locally managed remote repository
			HostedReleaseTags:    nil, // we can't test these because we'd need to pass the 'publicationServices' and 'services' configuration, which requires the repo name and owner in advance, but they are created dynamically during the test
		}}
)

/*
Returns the well known test suites.

The returned slice contains different items depending on whether the quickTest flag has been set as an environment variable
*/
func WellKnownFunctionalTestSuites() []*TestSuite {
	if os.Getenv("quickTests") == "true" {
		return wellKnownFunctionalTestSuitesQuick
	} else {
		return append(wellKnownFunctionalTestSuitesQuick, wellKnownFunctionalTestSuitesNonQuick...)
	}
}
