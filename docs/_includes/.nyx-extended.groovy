nyx {
  commitMessageConventions {
    enabled = [ 'conventionalCommits' ]
    items {
      conventionalCommits {
        expression = '(?m)^(?<type>[a-zA-Z0-9_]+)(!)?(\\\\((?<scope>[a-z ]+)\\\\))?:( (?<title>.+))$(?s).*'
        bumpExpressions {
          major = '(?s)(?m)^[a-zA-Z0-9_]+(!|.*^(BREAKING( |-)CHANGE: )).*'
          minor = '(?s)(?m)^feat(?!!|.*^(BREAKING( |-)CHANGE: )).*'
          patch = '(?s)(?m)^fix(?!!|.*^(BREAKING( |-)CHANGE: )).*'
        }
      }
    }
  }
  configurationFile = '.nyx.json'
  dryRun = false
  initialVersion = '1.0.0'
  preset = 'extended'
  releaseLenient = true
  releasePrefix = 'v'
  releaseTypes {
    enabled = [ 'mainline', 'maturity', 'integration', 'hotfix', 'feature', 'release', 'maintenance', 'internal' ]
    items {
      mainline {
        collapseVersions = false
        filterTags = '^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$'
        gitCommit = 'false'
        gitCommitMessage = 'Release version {{version}}'
        gitPush = 'true'
        gitTag = 'true'
        gitTagMessage = 'Tag version {{version}}'
        matchBranches = '^(master|main)$'
        matchEnvironmentVariables {
          CI = '^true$'
        }
        matchWorkspaceStatus = 'CLEAN'
        publish = 'true'
        versionRangeFromBranchName = false
      }
      maturity {
        collapseVersions = true
        collapsedVersionQualifier = '{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}'
        filterTags = '^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$'
        gitCommit = 'false'
        gitPush = 'true'
        gitTag = 'true'
        matchBranches = '^(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)$'
        matchWorkspaceStatus = 'CLEAN'
        publish = 'true'
        versionRangeFromBranchName = false
      }
      integration {
        collapseVersions = true
        collapsedVersionQualifier = '{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}'
        filterTags = '^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(develop|development|integration|latest)(\\.([0-9]\\d*))?)$'
        gitCommit = 'false'
        gitPush = 'true'
        gitTag = 'true'
        matchBranches = '^(develop|development|integration|latest)$'
        matchWorkspaceStatus = 'CLEAN'
        publish = 'true'
        versionRangeFromBranchName = false
      }
      hotfix {
        collapseVersions = true
        collapsedVersionQualifier = '{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}'
        filterTags = '^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(fix|hotfix)(([0-9a-zA-Z]*)(\\.([0-9]\\d*))?)?)$'
        gitCommit = 'false'
        gitPush = 'true'
        gitTag = 'true'
        matchBranches = '^(fix|hotfix)((-|\\/)[0-9a-zA-Z-_]+)?$'
        matchWorkspaceStatus = 'CLEAN'
        publish = 'true'
        versionRangeFromBranchName = false
      }
      feature {
        collapseVersions = true
        collapsedVersionQualifier = '{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}'
        filterTags = '^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(feat|feature)(([0-9a-zA-Z]*)(\\.([0-9]\\d*))?)?)$'
        gitCommit = 'false'
        gitPush = 'false'
        gitTag = 'false'
        matchBranches = '^(feat|feature)((-|\\/)[0-9a-zA-Z-_]+)?$'
        publish = 'false'
        versionRangeFromBranchName = false
      }
      release {
        collapseVersions = true
        collapsedVersionQualifier = '{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}'
        filterTags = '^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(rel|release)((\\.([0-9]\\d*))?)?)$'
        gitCommit = 'false'
        gitPush = 'true'
        gitTag = 'true'
        matchBranches = '^(rel|release)(-|\\/)({{configuration.releasePrefix}})?([0-9|x]\\d*)(\\.([0-9|x]\\d*)(\\.([0-9|x]\\d*))?)?$'
        matchWorkspaceStatus = 'CLEAN'
        publish = 'false'
        versionRangeFromBranchName = true
      }
      maintenance {
        collapseVersions = false
        filterTags = '^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$'
        gitCommit = 'false'
        gitPush = 'true'
        gitTag = 'true'
        matchBranches = '^[a-zA-Z]*([0-9|x]\\d*)(\\.([0-9|x]\\d*)(\\.([0-9|x]\\d*))?)?$'
        matchWorkspaceStatus = 'CLEAN'
        publish = 'true'
        versionRangeFromBranchName = true
      }
      internal {
        collapseVersions = true
        collapsedVersionQualifier = 'internal'
        gitCommit = 'false'
        gitPush = 'false'
        gitTag = 'false'
        identifiers {
          enabled = [ 'branch', 'commit', 'user', 'timestamp' ]
          items {
            branch {
              position = 'BUILD'
              qualifier = 'branch'
              value = '{{#sanitize}}{{branch}}{{/sanitize}}'
            }
            commit {
              position = 'BUILD'
              qualifier = 'commit'
              value = '{{#short7}}{{releaseScope.finalCommit}}{{/short7}}'
            }
            timestamp {
              position = 'BUILD'
              qualifier = 'timestamp'
              value = '{{#timestampYYYYMMDDHHMMSS}}{{timestamp}}{{/timestampYYYYMMDDHHMMSS}}'
            }
            user {
              position = 'BUILD'
              qualifier = 'user'
              value = '{{#sanitizeLower}}{{environment.user}}{{/sanitizeLower}}'
            }
          }
        }
        publish = 'false'
        versionRangeFromBranchName = false
      }
    }
  }
  resume = true
  scheme = 'SEMVER'
  sharedConfigurationFile = 'example-shared.config.json'
  stateFile = '.nyx-state.yml'
  verbosity = 'INFO'
  version = '1.8.12'
}