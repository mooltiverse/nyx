nyx {
  changelog {
    path = 'build/CHANGELOG.md'
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
        expression = '(?m)^(?<type>[a-zA-Z0-9_]+)(!)?(\\\\((?<scope>[a-z ]+)\\\\))?:( (?<title>.+))$(?s).*'
        bumpExpressions {
          major = '(?s)(?m)^[a-zA-Z0-9_]+(!|.*^(BREAKING( |-)CHANGE: )).*'
          minor = '(?s)(?m)^feat(?!!|.*^(BREAKING( |-)CHANGE: )).*'
          patch = '(?s)(?m)^fix(?!!|.*^(BREAKING( |-)CHANGE: )).*'
        }
      }
    }
  }
  dryRun = false
  git {
    remotes {
      origin {
        user = 'jdoe'
        password = 'somepassword'
      }
    }
  }
  initialVersion = '0.1.0'
  releaseLenient = true
  releasePrefix = 'v'
  releaseTypes {
    enabled = [ 'mainline', 'internal' ]
    publicationServices = [ 'github' ]
    items {
      mainline {
        collapseVersions = false
        filterTags = '^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$'
        gitCommit = 'true'
        gitCommitMessage = 'Release version {{version}}'
        gitPush = 'true'
        gitTag = 'true'
        gitTagMessage = 'Tag release {{version}}'
        matchBranches = '^(master|main)$'
        matchEnvironmentVariables {
          CI = '^true$'
        }
        matchWorkspaceStatus = 'CLEAN'
        publish = 'true'
        versionRangeFromBranchName = false
      }
      internal {
        collapseVersions = false
        collapsedVersionQualifier = 'internal'
        description = 'Internal release {{version}}'
        filterTags = '^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$'
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
            value = '{{#sanitizeLower}}{{environment.user}}{{/sanitizeLower}}'
          }
        }
        publish = 'false'
        versionRangeFromBranchName = false
      }
    }
  }
  scheme = 'SEMVER'
  services {
    github {
      type = 'GITHUB'
      options {
        AUTHENTICATION_TOKEN = '{{#environment.variable}}GITHUB_TOKEN{{/environment.variable}}'
        REPOSITORY_NAME = 'myrepo'
        REPOSITORY_OWNER = 'acme'
      }
    }
  }
  verbosity = 'INFO'
}
