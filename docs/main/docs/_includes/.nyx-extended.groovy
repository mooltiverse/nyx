nyx {
  changelog {
    append = 'head'
    path = 'CHANGELOG.md'
    sections = [
      'Added' : '^feat$',
      'Fixed' : '^fix$',
    ]
    substitutions = [
      '(?m)#([0-9]+)(?s)': '[#%s](https://example.com/issues/%s)'
    ]
  }
  commitMessageConventions {
    enabled = [ 'conventionalCommits' ]
    items {
      conventionalCommits {
        expression = '(?m)^(?<type>[a-zA-Z0-9_]+)(\\((?<scope>[a-zA-Z0-9 \\-_]+)\\))?(!)?:( (?<title>.+))$(?s).*'
        bumpExpressions {
          major = '(?s)(?m)^[a-zA-Z0-9_]+(\\([a-zA-Z0-9 \\-_]+\\))?(!: .*|.*^(BREAKING( |-)CHANGE: )).*'
          minor = '(?s)(?m)^feat(!{0})(\\([a-zA-Z0-9 \\-_]+\\))?: (?!.*^(BREAKING( |-)CHANGE: )).*'
          patch = '(?s)(?m)^fix(!{0})(\\([a-zA-Z0-9 \\-_]+\\))?: (?!.*^(BREAKING( |-)CHANGE: )).*'
        }
      }
    }
  }
  configurationFile = '.nyx.json'
  dryRun = false
  git {
    remotes {
      origin {
        authenticationMethod = 'USER_PASSWORD'
        user = 'jdoe'
        password = 'somepassword'
      }
      replica {
        authenticationMethod = 'PUBLIC_KEY'
        privateKey = '{{#fileContent}}.ssh/id_ed25519{{/fileContent}}'
        passphrase = 'somesecretpassphrase'
      }
    }
  }
  initialVersion = '1.0.0'
  preset = 'extended'
  releaseAssets {
    asset1 {
      fileName = "asset.txt"
      description = "Text Asset"
      type = "text/plain"
      path = "asset.txt"
    }
    asset2 {
      fileName = "asset.bin"
      description = "Binary Asset"
      type = "application/octet-stream"
      path = "asset.bin"
    }
  }
  releaseLenient = true
  releasePrefix = 'v'
  releaseTypes {
    enabled = [ 'mainline', 'maturity', 'integration', 'hotfix', 'feature', 'release', 'maintenance', 'internal' ]
    publicationServices = [ 'github', 'gitlab' ]
    remoteRepositories = [ 'origin', 'replica' ]
    items {
      mainline {
        collapseVersions = false
        filterTags = '^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$'
        gitCommit = 'false'
        gitPush = 'true'
        gitPushForce = 'true'
        gitTag = 'true'
        gitTagForce = 'true'
        gitTagMessage = 'Tag version {{version}}'
        gitTagNames = [ '{{version}}', 'stable', 'latest' ]
        matchBranches = '^(master|main)$'
        matchEnvironmentVariables {
          CI = '^true$'
        }
        matchWorkspaceStatus = 'CLEAN'
        publish = 'true'
        releaseName = 'Release {{version}}'
        versionRangeFromBranchName = false
      }
      maturity {
        collapseVersions = true
        collapsedVersionQualifier = '{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}'
        description = 'Maturity release {{version}}'
        filterTags = '^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$'
        gitCommit = 'false'
        gitPush = 'true'
        gitTag = 'true'
        gitTagNames = [ '{{version}}' ]
        matchBranches = '^(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)$'
        matchWorkspaceStatus = 'CLEAN'
        publish = 'true'
        publishPreRelease = 'true'
        versionRangeFromBranchName = false
      }
      integration {
        assets = ""
        collapseVersions = true
        collapsedVersionQualifier = '{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}'
        description = 'Integration release {{version}}'
        filterTags = '^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(develop|development|integration|latest)(\\.([0-9]\\d*))?)$'
        gitCommit = 'false'
        gitPush = 'true'
        gitTag = 'true'
        gitTagNames = [ '{{version}}' ]
        matchBranches = '^(develop|development|integration|latest)$'
        matchWorkspaceStatus = 'CLEAN'
        publish = 'true'
        publishPreRelease = 'true'
        versionRangeFromBranchName = false
      }
      feature {
        collapseVersions = true
        collapsedVersionQualifier = '{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}'
        description = 'Feature release {{version}}'
        filterTags = '^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(feat|feature)(([0-9a-zA-Z]*)(\\.([0-9]\\d*))?)?)$'
        gitCommit = 'false'
        gitPush = 'false'
        gitTag = 'false'
        matchBranches = '^(feat|feature)((-|\\/)[0-9a-zA-Z-_]+)?$'
        publish = 'false'
        publishPreRelease = 'true'
        versionRangeFromBranchName = false
      }
      hotfix {
        assets = "asset1"
        collapseVersions = true
        collapsedVersionQualifier = '{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}'
        description = 'Hotfix release {{version}}'
        filterTags = '^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(fix|hotfix)(([0-9a-zA-Z]*)(\\.([0-9]\\d*))?)?)$'
        gitCommit = 'false'
        gitPush = 'true'
        gitTag = 'true'
        gitTagNames = [ '{{version}}' ]
        matchBranches = '^(fix|hotfix)((-|\\/)[0-9a-zA-Z-_]+)?$'
        matchWorkspaceStatus = 'CLEAN'
        publish = 'true'
        publishPreRelease = 'true'
        versionRangeFromBranchName = false
      }
      release {
        collapseVersions = true
        collapsedVersionQualifier = '{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}'
        description = 'Release {{version}}'
        filterTags = '^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(rel|release)((\\.([0-9]\\d*))?)?)$'
        gitCommit = 'false'
        gitPush = 'true'
        gitTag = 'true'
        gitTagNames = [ '{{version}}' ]
        matchBranches = '^(rel|release)(-|\\/)({{configuration.releasePrefix}})?([0-9|x]\\d*)(\\.([0-9|x]\\d*)(\\.([0-9|x]\\d*))?)?$'
        matchWorkspaceStatus = 'CLEAN'
        publish = 'false'
        versionRangeFromBranchName = true
      }
      maintenance {
        collapseVersions = false
        description = 'Maintenance release {{version}}'
        filterTags = '^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$'
        gitCommit = 'false'
        gitPush = 'true'
        gitTag = 'true'
        gitTagNames = [ '{{version}}' ]
        matchBranches = '^[a-zA-Z]*([0-9|x]\\d*)(\\.([0-9|x]\\d*)(\\.([0-9|x]\\d*))?)?$'
        matchWorkspaceStatus = 'CLEAN'
        publish = 'true'
        versionRangeFromBranchName = true
      }
      internal {
        collapseVersions = true
        collapsedVersionQualifier = 'internal'
        description = 'Internal release {{version}}'
        gitCommit = 'false'
        gitPush = 'false'
        gitTag = 'false'
        identifiers {
          '0' {
            position = 'BUILD'
            qualifier = 'branch'
            value = '{{#sanitize}}{{branch}}{{/sanitize}}'
          }
          '1' {
            position = 'BUILD'
            qualifier = 'commit'
            value = '{{#short7}}{{releaseScope.finalCommit}}{{/short7}}'
          }
          '2' {
            position = 'BUILD'
            qualifier = 'timestamp'
            value = '{{#timestampYYYYMMDDHHMMSS}}{{timestamp}}{{/timestampYYYYMMDDHHMMSS}}'
          }
          '3' {
            position = 'BUILD'
            qualifier = 'user'
            value = '{{#sanitizeLower}}{{environmentUser}}{{/sanitizeLower}}'
          }
        }
        publish = 'false'
        publishDraft = 'true'
        versionRangeFromBranchName = false
      }
    }
  }
  resume = true
  scheme = 'SEMVER'
  services {
    github {
      type = 'GITHUB'
      options {
        AUTHENTICATION_TOKEN = '{{#environmentVariable}}GITHUB_TOKEN{{/environmentVariable}}'
        REPOSITORY_NAME = 'myrepo'
        REPOSITORY_OWNER = 'acme'
      }
    }
    gitlab {
      type = 'GITLAB'
      options {
        AUTHENTICATION_TOKEN = 'abcde-a1b2c3d4e5f6g7h8i9j0'
        REPOSITORY_NAME = 'myrepo'
        REPOSITORY_OWNER = 'acme'
      }
    }
  }
  sharedConfigurationFile = 'example-shared.config.json'
  summary = true
  summaryFile = '.nyx-summary.txt'
  stateFile = '.nyx-state.yml'
  substitutions {
    enabled = [ 'npm', 'rust' ]
    items {
      npm {
        files = 'package.json'
        match = '"version"(\s)*:(\s)*"(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)(?:-((?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\.(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\+([0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))?"'
        replace = '"version": "{{version}}"'
      }
      rust {
        files = 'Cargo.toml'
        match = 'version(\s)*=(\s)*("|\')(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)(?:-((?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\.(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\+([0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))?("|\')'
        replace = 'version = "{{version}}"'
      }
    }
  }
  verbosity = 'INFO'
  version = '1.8.12'
}
