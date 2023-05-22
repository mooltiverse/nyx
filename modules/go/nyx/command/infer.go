/*
 * Copyright 2020 Mooltiverse
 *
 * Licensed under the Apache License, Version 2.0 (the "License")
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

package command

import (
	"fmt"     // https://pkg.go.dev/fmt
	"strconv" // https://pkg.go.dev/strconv
	"strings" // https://pkg.go.dev/strings

	regexp2 "github.com/dlclark/regexp2" // https://pkg.go.dev/github.com/dlclark/regexp2, we need to use this instead of the standard 'regexp' to have support for lookarounds (look ahead), even if this implementation is a little slower
	log "github.com/sirupsen/logrus"     // https://pkg.go.dev/github.com/sirupsen/logrus

	errs "github.com/mooltiverse/nyx/modules/go/errors"
	ent "github.com/mooltiverse/nyx/modules/go/nyx/entities"
	gitent "github.com/mooltiverse/nyx/modules/go/nyx/entities/git"
	git "github.com/mooltiverse/nyx/modules/go/nyx/git"
	stt "github.com/mooltiverse/nyx/modules/go/nyx/state"
	ver "github.com/mooltiverse/nyx/modules/go/version"
)

/*
The Infer command takes care of inferring and computing informations in order to make a new release.

After this task is executed the state object has:
- the version attribute set to the release version

This class is not meant to be used in multi-threaded environments.
*/
type Infer struct {
	// Extend abstractCommand by composition
	abstractCommand
}

const (
	// The common prefix used for all the internal state attributes managed by this class.
	INFER_INTERNAL_ATTRIBUTE_PREFIX = "infer"

	// The common prefix used for all the internal state attributes managed by this class, representing an input.
	INFER_INTERNAL_INPUT_ATTRIBUTE_PREFIX = INFER_INTERNAL_ATTRIBUTE_PREFIX + "." + "input"

	// The common prefix used for all the internal state attributes managed by this class, representing an output.
	INFER_INTERNAL_OUTPUT_ATTRIBUTE_PREFIX = INFER_INTERNAL_ATTRIBUTE_PREFIX + "." + "output"

	// The name used for the internal state attribute where we store the configured bump.
	INFER_INTERNAL_INPUT_ATTRIBUTE_CONFIGURED_BUMP = INFER_INTERNAL_INPUT_ATTRIBUTE_PREFIX + "." + "configured" + "." + "bump"

	// The name used for the internal state attribute where we store the configured initial version.
	INFER_INTERNAL_INPUT_ATTRIBUTE_CONFIGURED_INITIAL_VERSION = INFER_INTERNAL_INPUT_ATTRIBUTE_PREFIX + "." + "configured" + "." + "initialVersion"

	// The name used for the internal state attribute where we store the configured release lenient.
	INFER_INTERNAL_INPUT_ATTRIBUTE_CONFIGURED_RELEASE_LENIENT = INFER_INTERNAL_INPUT_ATTRIBUTE_PREFIX + "." + "configured" + "." + "releaseLenient"

	// The name used for the internal state attribute where we store the configured release prefix.
	INFER_INTERNAL_INPUT_ATTRIBUTE_CONFIGURED_RELEASE_PREFIX = INFER_INTERNAL_INPUT_ATTRIBUTE_PREFIX + "." + "configured" + "." + "releasePrefix"

	// The name used for the internal state attribute where we store the configured scheme.
	INFER_INTERNAL_INPUT_ATTRIBUTE_CONFIGURED_SCHEME = INFER_INTERNAL_INPUT_ATTRIBUTE_PREFIX + "." + "configured" + "." + "scheme"

	// The name used for the internal state attribute where we store the configured version.
	INFER_INTERNAL_INPUT_ATTRIBUTE_CONFIGURED_VERSION = INFER_INTERNAL_INPUT_ATTRIBUTE_PREFIX + "." + "configured" + "." + "version"

	// The name used for the internal state attribute where we store current branch name.
	INFER_INTERNAL_INPUT_ATTRIBUTE_REPOSITORY_CURRENT_BRANCH = INFER_INTERNAL_INPUT_ATTRIBUTE_PREFIX + "." + "repository" + "." + "current" + "." + "branch"

	// The name used for the internal state attribute where we store the SHA-1 of the last commit in the current branch by the time this command was last executed.
	INFER_INTERNAL_INPUT_ATTRIBUTE_REPOSITORY_LAST_COMMIT = INFER_INTERNAL_INPUT_ATTRIBUTE_PREFIX + "." + "repository" + "." + "last" + "." + "commit"

	// The name used for the internal state attribute where we store the version.
	INFER_INTERNAL_INPUT_ATTRIBUTE_STATE_VERSION = INFER_INTERNAL_OUTPUT_ATTRIBUTE_PREFIX + "." + "state" + "." + "version"

	/*
		This regular expression is used to match a branch name in order to see if it's a wildcard that may represent
		a valid version range.

		The expression uses named groups major, minor and patch for those parts meant to represent
		their respective semantic version identifiers, while there are other anonymous (non capturing groups) for those parts
		that are tolerated but ignored.

		Each part can be present or not. When it's not present it's assumed that any identifier may appear in that position.
		When present it may be a number, in which case only those numbers are allowed in the version range check, or it may
		be an x, which acts as a wildcard, allowing any number to appear in there.

		For example:
		- 1.x: means that only version with major number 1 are accepted, while the minor and patch numbers can be anything
		- x.2.x: means that any major and patch numbers are allowed, while the minor number can be any valid number
		- rel/v1.2.3: is tolerated but the 'rel/v' is just ignored, while the version number can only be 1.2.3
		- v1.x-abc.123+def.456: tolerates any pre-release and build parts and the 'v' prefix, while the major number must
		  be 1 (while the minor and patch can be anything)
	*/
	SEMVER_DYNAMIC_VERSION_RANGE_FROM_BRANCH_NAME_REGEX = "^(?:.*?)(?<major>x|[0-9]+)(?:\\.(?<minor>x|[0-9]+)(?:\\.(?<patch>x|[0-9]+))?)?(?:(?:-|\\+).*)?$"
)

/*
Standard constructor.

Arguments are as follows:

- state the state reference
- repository the repository reference

Error is:

- NilPointerError: if a given argument is nil
*/
func NewInfer(state *stt.State, repository *git.Repository) (*Infer, error) {
	if state == nil {
		return nil, &errs.NilPointerError{Message: fmt.Sprintf("the State object cannot be nil")}
	}
	if repository == nil {
		return nil, &errs.NilPointerError{Message: fmt.Sprintf("the Repository object cannot be nil")}
	}
	log.Debugf("new Infer command object")

	res := &Infer{}
	res.abstractCommand.repository = repository
	res.abstractCommand.state = state
	return res, nil
}

/*
Scans the Git commit history in order to detect:
  - the previous version (and the prime version, when the release type is configured to use collapsed versioning)
  - the commits within the release scope (the ones after the commit that brings the latest version tag)
  - the significant commits since the previous version (and since the prime version, when the
    release type is configured to use collapsed versioning)
  - the identifiers that are supposed to be bumped based on the significant commits since the previous and prime version

To do so the commit message convention is used to decide whether a commit is significant or not.

Outputs from this task are all stored in the State object, with more detail:
  - the version is defined with the new version identifier for the new release; if the user has overridden
    the version by configuration that value is simply used and no inference is done; if the version is not overridden
    by configuration and no previous versions can be found in the history the initial version from the
    configuration is used
  - the releaseScope/commits is defined with the commits within the scope
  - the releaseScope/significantCommits is defined with the commits within the scope that yield to some
    version identified to be bumped, if any;
  - the releaseScope/previousVersion and releaseScope/previousVersionCommit are defined with the
    tag and SHA-1 of the previous release, if any; if no previous release is found or the version was overridden
    by the user configuration they will be nil;
  - the releaseScope/primeVersion and releaseScope/primeVersionCommit are defined with the
    tag and SHA-1 of the prime release, if any; if no prime release is found or the version was overridden
    by the user configuration they will be nil;
  - the releaseScope/initialCommit is defined with the SHA-1 of the commit right after the
    releaseScope/previousVersionCommit or, when releaseScope/previousVersionCommit can't be
    inferred, the repository root commit SHA-1 is used; if the user overrides the version by configuration
    this value remains nil
  - the releaseType

Arguments are as follows:

  - scheme the versioning scheme in use. It can't be nil or empty
  - bump the identifier to bump. It may be nil. Set this argument to a non nil value only to override
    the internal logic of determining the identifier to bump, otherwise just pass it nil and this method will infer it from
    the commit history
  - @param releaseLenient when true prefixes, even others than the releasePrefix, are tolerated when parsing and comparing
    the prime and previous version
  - releasePrefix the release prefix that has been configured. This is considered when parsing and comparing the prime and previous
    version. It may be nil or empty
  - collapsedVersioning pass true if the release type is configured to use collapsed versioning, false otherwise
  - filterTagsExpression a regular expression that filters tags in the commit history in order to find the previous version.
    If nil all tags are considered to be included in the commit history, otherwise only those matched by the expression
    are considered while others are ignored.
  - commitMessageConventions the map of all commit message conventions that have to be evaluated when scanning commits. It
    may be nil or empty when no convention is used, in which case significant commits and bump identifiers are not detected
  - previousSignificantCommits a list of commits that this method will fill with every commit that is significant since
    the previous version, according to the given commitMessageConventions. It should be empty and must not be nil.
    This list is returned by this method with the outcomes of the repository scan as the first return value.
  - previousBumpIdentifiers a set of version component identifiers that this method will fill for every commit that is
    significant since the previous version, according to the given commitMessageConventions. It should be empty and must
    not be nil. This list is returned by this method with the outcomes of the repository scan  as the second return value.
  - primeSignificantCommits a list of commits that this method will fill with every commit that is significant since
    the prime version, according to the given commitMessageConventions. It should be empty and must not be nil.
    This list is returned by this method with the outcomes of the repository scan as the third return value.
  - primeBumpIdentifiers a set of version component identifiers that this method will fill for every commit that is
    significant since the prime version, according to the given commitMessageConventions. It should be empty and must
    not be nil. This list is returned by this method with the outcomes of the repository scan as the fourth return value.

Error is:

- DataAccessError in case the configuration can't be loaded for some reason.
- IllegalPropertyError in case the configuration has some illegal options.
- GitError in case of unexpected issues when accessing the Git repository.
- ReleaseError if the task is unable to complete for reasons due to the release process.
*/
func (c *Infer) scanRepository(scheme *ver.Scheme, bump *string, releaseLenient *bool, releasePrefix *string, collapsedVersioning *bool, filterTagsExpression *string, commitMessageConventions map[string]*ent.CommitMessageConvention, previousSignificantCommits []gitent.Commit, previousBumpIdentifiers []string, primeSignificantCommits []gitent.Commit, primeBumpIdentifiers []string) ([]gitent.Commit, []string, []gitent.Commit, []string, error) {
	if scheme == nil {
		return nil, nil, nil, nil, &errs.NilPointerError{Message: fmt.Sprintf("the scheme cannot be nil")}
	}
	if previousSignificantCommits == nil {
		return nil, nil, nil, nil, &errs.NilPointerError{Message: fmt.Sprintf("the list of previous significant commits cannot be nil")}
	}
	if previousBumpIdentifiers == nil {
		return nil, nil, nil, nil, &errs.NilPointerError{Message: fmt.Sprintf("the set of previous bump identifiers cannot be nil")}
	}
	if primeSignificantCommits == nil {
		return nil, nil, nil, nil, &errs.NilPointerError{Message: fmt.Sprintf("the list of prime significant commits cannot be nil")}
	}
	if primeBumpIdentifiers == nil {
		return nil, nil, nil, nil, &errs.NilPointerError{Message: fmt.Sprintf("the set of previous bump identifiers cannot be nil")}
	}
	previousSignificantCommitsResult := previousSignificantCommits
	previousBumpIdentifiersResult := previousBumpIdentifiers
	primeSignificantCommitsResult := primeSignificantCommits
	primeBumpIdentifiersResult := primeBumpIdentifiers

	releaseScope, err := c.State().GetReleaseScope()
	if err != nil {
		return nil, nil, nil, nil, err
	}

	log.Debugf("walking the commit history...")
	(*c.Repository()).WalkHistory(nil, nil, func(cc gitent.Commit) bool {
		log.Debugf("stepping by commit '%s'", cc.GetSHA())
		log.Debugf("commit '%s' has '%d' tags: '%s'", cc.GetSHA(), len(cc.GetTags()), cc.GetTags())

		// Inspect the tags in order to determine what kind of commit this is.
		// If this commit has tags that make it the 'previous version commit' then the release scope
		// previousVersion and previousVersionCommit are set and this commit closes the release scope
		// (without being part of it), otherwise this is just another commit that belongs to the scope
		// The primeVersion and primeVersionCommit are also detected: if the release type is using
		// collapsed versioning their search may go beyond (backward) the previousVersion and previousVersionCommit
		// otherwise they are the same.
		// If the commit has multiple valid version tags they are all evaluated and compared to select the greatest
		for _, tag := range cc.GetTags() {
			if (*releaseLenient && ver.IsLegalWithLenience(*scheme, tag.GetName(), *releaseLenient)) || (!*releaseLenient && ver.IsLegalWithPrefix(*scheme, tag.GetName(), releasePrefix)) {
				log.Debugf("evaluating tag '%s': tag is a valid version according to the '%s' scheme and will be passed to the next evaluation steps. The tag is applied to commit '%s'", tag.GetName(), (*scheme).String(), cc.GetSHA())

				var previousVersionComparison int
				if *releaseLenient {
					v1 := tag.GetName()
					previousVersionComparison = ver.CompareWithSanitization(*scheme, &v1, releaseScope.GetPreviousVersion(), *releaseLenient)
				} else {
					v1 := tag.GetName()
					previousVersionComparison = ver.CompareWithPrefix(*scheme, &v1, releaseScope.GetPreviousVersion(), releasePrefix)
				}
				if previousVersionComparison > 0 {
					if releaseScope.GetPreviousVersion() == nil {
						log.Debugf("evaluating tag '%s': tag is greater than previously selected previousVersion tag '%s' and will be passed to the next evaluation steps. The tag is applied to commit '%s'", tag.GetName(), "<none>", cc.GetSHA())
					} else {
						log.Debugf("evaluating tag '%s': tag is greater than previously selected previousVersion tag '%s' and will be passed to the next evaluation steps. The tag is applied to commit '%s'", tag.GetName(), *releaseScope.GetPreviousVersion(), cc.GetSHA())
					}

					if filterTagsExpression == nil || "" == strings.TrimSpace(*filterTagsExpression) {
						log.Debugf("evaluating tag '%s': the selected release type does not specify any additional filter for tags (or the tags filter template yields to an empty regular expression after evaluation) so the tag will be used as the previousVersion and the commit '%s' is used as the previousVersionCommit", tag.GetName(), cc.GetSHA())
						t := tag.GetName()
						releaseScope.SetPreviousVersion(&t)
						releaseScope.SetPreviousVersionCommit(&cc)
					} else {
						re, err := regexp2.Compile(*filterTagsExpression, 0)
						if err != nil {
							log.Errorf("cannot compile regular expression '%s': %v", *filterTagsExpression, err)
						}
						match, err := re.MatchString(tag.GetName())
						if err != nil {
							log.Errorf("cannot evaluate regular expression '%s' against '%s': %v", *filterTagsExpression, tag.GetName(), err)
						}
						if match {
							log.Debugf("evaluating tag '%s': the selected release type specifies an additional filter '%s' (after template evaluation) for tags and the tag successfully matches so the tag will be used as the previousVersion and the commit '%s' is used as the previousVersionCommit", tag.GetName(), *filterTagsExpression, cc.GetSHA())
							t := tag.GetName()
							releaseScope.SetPreviousVersion(&t)
							releaseScope.SetPreviousVersionCommit(&cc)
						} else {
							log.Debugf("evaluating tag '%s': the selected release type specifies an additional filter '%s' (after template evaluation) for tags but the tag doesn't match it so it will be ignored. The tag is applied to commit '%s'", tag.GetName(), *filterTagsExpression, cc.GetSHA())
						}
					}
				} else {
					if releaseScope.GetPreviousVersion() == nil {
						log.Debugf("evaluating tag '%s': tag is less than previously selected previousVersion tag '%s' so it will be ignored. The tag is applied to commit '%s'", tag.GetName(), "<none>", cc.GetSHA())
					} else {
						log.Debugf("evaluating tag '%s': tag is less than previously selected previousVersion tag '%s' so it will be ignored. The tag is applied to commit '%s'", tag.GetName(), *releaseScope.GetPreviousVersion(), cc.GetSHA())
					}
				}

				if collapsedVersioning != nil && *collapsedVersioning {
					log.Debugf("evaluating tag '%s': the selected release type uses collapsed versioning so the tag will be passed to the next evaluation steps to check if it's a valid primeVersion. The tag is applied to commit '%s'", tag.GetName(), cc.GetSHA())

					if (*releaseLenient && ver.IsCoreWithLenience(*scheme, tag.GetName(), *releaseLenient)) || (!*releaseLenient && ver.IsCoreWithPrefix(*scheme, tag.GetName(), releasePrefix)) {
						log.Debugf("evaluating tag '%s': tag is a valid core version according to the '%s' scheme and the selected release type uses collapsed versioning so the tag will be passed to the next evaluation steps to check if it's a valid primeVersion. The tag is applied to commit '%s'", tag.GetName(), (*scheme).String(), cc.GetSHA())

						var primeVersionComparison int
						if *releaseLenient {
							v1 := tag.GetName()
							primeVersionComparison = ver.CompareWithSanitization(*scheme, &v1, releaseScope.GetPrimeVersion(), *releaseLenient)
						} else {
							v1 := tag.GetName()
							primeVersionComparison = ver.CompareWithPrefix(*scheme, &v1, releaseScope.GetPrimeVersion(), releasePrefix)
						}
						if primeVersionComparison > 0 {
							if releaseScope.GetPrimeVersion() == nil {
								log.Debugf("evaluating tag '%s': tag is greater than previously selected primeVersion tag '%s' so it will be used as the primeVersion and the commit '%s' is used as the primeVersionCommit", tag.GetName(), "<none>", cc.GetSHA())
							} else {
								log.Debugf("evaluating tag '%s': tag is greater than previously selected primeVersion tag '%s' so it will be used as the primeVersion and the commit '%s' is used as the primeVersionCommit", tag.GetName(), *releaseScope.GetPrimeVersion(), cc.GetSHA())
							}

							t := tag.GetName()
							releaseScope.SetPrimeVersion(&t)
							releaseScope.SetPrimeVersionCommit(&cc)

							if !releaseScope.HasPreviousVersion() {
								log.Debugf("evaluating tag '%s': a primeVersion has been encountered before any valid previousVersion so this tag will also be used as the previousVersion the commit '%s' as the prreviousVersionCommit", tag.GetName(), cc.GetSHA())

								t := tag.GetName()
								releaseScope.SetPreviousVersion(&t)
								releaseScope.SetPreviousVersionCommit(&cc)
							}
						} else {
							if releaseScope.GetPrimeVersion() == nil {
								log.Debugf("evaluating tag '%s': tag is less than previously selected primeVersion tag '%s' so it will be ignored for the primeVersion", tag.GetName(), "<none>")
							} else {
								log.Debugf("evaluating tag '%s': tag is less than previously selected primeVersion tag '%s' so it will be ignored for the primeVersion", tag.GetName(), *releaseScope.GetPrimeVersion())
							}
						}
					} else {
						log.Debugf("evaluating tag '%s': tag is not a valid core version according to the '%s' scheme so the tag will be ignored for the primeVersion. The tag is applied to commit '%s'", tag.GetName(), (*scheme).String(), cc.GetSHA())
					}
				}
			} else {
				log.Debugf("evaluating tag '%s': tag is not a valid version according to the '%s' scheme and will be ignored. The tag is applied to commit '%s'", tag.GetName(), (*scheme).String(), cc.GetSHA())
			}
		}

		// If this is a commit within the scope let's add it to the scope and inspect it
		if !(releaseScope.HasPreviousVersion() && releaseScope.HasPreviousVersionCommit()) {
			log.Debugf("commit '%s' has no valid version tags so it's added to the release scope", cc.GetSHA())
			commits := releaseScope.GetCommits()
			commitToAppend := cc // avoid duplicate appends of the same item
			commits = append(commits, &commitToAppend)
			releaseScope.SetCommits(commits)
		}

		// if the 'bump' was not overridden by user, evaluate the commit message against the configured conventions to see which identifier must be dumped, if any
		if bump == nil {
			if commitMessageConventions != nil {
				// Let's find the identifier to bump (unless the bump was overridden by user).
				// We need to consider all commits within the scope and, when using collapsed versioning,
				// also those between the primeVersionCommit and the finalCommit
				if (!(releaseScope.HasPreviousVersion() && releaseScope.HasPreviousVersionCommit())) || (collapsedVersioning != nil && *collapsedVersioning && (!(releaseScope.HasPrimeVersion() && releaseScope.HasPrimeVersionCommit()))) {
					log.Debugf("trying to infer the identifier to bump based on the commit message of commit '%s'", cc.GetSHA())
					for cmcEntryKey, cmcEntryValue := range commitMessageConventions {
						log.Debugf("evaluating commit '%s' against message convention '%s'", cc.GetSHA(), cmcEntryKey)
						re, err := regexp2.Compile(*cmcEntryValue.GetExpression(), 0)
						if err != nil {
							log.Errorf("cannot compile regular expression '%s': %v", *cmcEntryValue.GetExpression(), err)
						}
						match, err := re.MatchString(cc.GetMessage().GetFullMessage())
						if err != nil {
							log.Errorf("cannot evaluate regular expression '%s' against '%s': %v", *cmcEntryValue.GetExpression(), cc.GetMessage().GetFullMessage(), err)
						}
						if match {
							log.Debugf("commit message convention '%s' matches commit '%s'", cmcEntryKey, cc.GetSHA())
							for bumpExpressionKey, bumpExpressionValue := range *cmcEntryValue.GetBumpExpressions() {
								log.Debugf("matching commit '%s' ('%s') against bump expression '%s' ('%s') of message convention '%s'", cc.GetSHA(), cc.GetMessage().GetFullMessage(), bumpExpressionKey, bumpExpressionValue, cmcEntryKey)
								re, err = regexp2.Compile(bumpExpressionValue, 0)
								if err != nil {
									log.Errorf("cannot compile regular expression '%s': %v", bumpExpressionValue, err)
								}
								match, err = re.MatchString(cc.GetMessage().GetFullMessage())
								if err != nil {
									log.Errorf("cannot evaluate regular expression '%s' against '%s': %v", bumpExpressionValue, cc.GetMessage().GetFullMessage(), err)
								}
								if match {
									log.Debugf("bump expression '%s' of message convention '%s' matches commit '%s', meaning that the '%s' identifier has to be bumped, according to this commit", bumpExpressionKey, cmcEntryKey, cc.GetSHA(), bumpExpressionKey)
									// if we reached this point this is also in the 'prime commit' scope
									primeBumpIdentifiersResult = append(primeBumpIdentifiersResult, bumpExpressionKey)
									primeSignificantCommitsResult = append(primeSignificantCommitsResult, cc)

									if !(releaseScope.HasPreviousVersion() && releaseScope.HasPreviousVersionCommit()) {
										// if the previous version wasn't found yet this is in the 'previous commit' scope
										previousBumpIdentifiersResult = append(previousBumpIdentifiersResult, bumpExpressionKey)
										previousSignificantCommitsResult = append(previousSignificantCommitsResult, cc)
									}
								} else {
									log.Debugf("bump expression '%s' of message convention '%s' doesn't match commit '%s'", bumpExpressionKey, cmcEntryKey, cc.GetSHA())
								}
							}
						} else {
							log.Debugf("commit message convention '%s' doesn't match commit '%s', skipping", cmcEntryKey, cc.GetSHA())
						}
					}
				}
			} else {
				log.Debugf("no commit message convention has been configured, skipping inference of the identifier to bump based on commit messages")
			}
		}

		// stop walking the commit history if we already have the previous and prime versions (and their commits), otherwise keep walking
		return !(releaseScope.HasPreviousVersion() && releaseScope.HasPreviousVersionCommit() && releaseScope.HasPrimeVersion() && releaseScope.HasPrimeVersionCommit())
	})

	log.Debugf("walking the commit history finished. The release scope contains %d commits.", len(releaseScope.GetCommits()))
	if collapsedVersioning != nil && *collapsedVersioning {
		if releaseScope.GetPreviousVersion() == nil {
			if releaseScope.GetPrimeVersion() == nil {
				log.Debugf("after scanning the commit history the previousVersion is '%s' and the primeVersion is '%s'", "null", "null")
			} else {
				log.Debugf("after scanning the commit history the previousVersion is '%s' and the primeVersion is '%s'", "null", *releaseScope.GetPrimeVersion())
			}
		} else {
			if releaseScope.GetPrimeVersion() == nil {
				log.Debugf("after scanning the commit history the previousVersion is '%s' and the primeVersion is '%s'", *releaseScope.GetPreviousVersion(), "null")
			} else {
				log.Debugf("after scanning the commit history the previousVersion is '%s' and the primeVersion is '%s'", *releaseScope.GetPreviousVersion(), *releaseScope.GetPrimeVersion())
			}
		}
		log.Debugf("significant commits (bumping identifiers) since the previousVersion are '%d', while those since the primeVersion are '%d'", len(previousSignificantCommits), len(primeSignificantCommits))
	} else {
		if releaseScope.GetPreviousVersion() == nil {
			log.Debugf("after scanning the commit history the previousVersion is '%s'", "null")
		} else {
			log.Debugf("after scanning the commit history the previousVersion is '%s'", *releaseScope.GetPreviousVersion())
		}
		log.Debugf("significant commits (bumping identifiers) since the previousVersion are '%d'", len(previousSignificantCommits))
	}
	return previousSignificantCommitsResult, previousBumpIdentifiersResult, primeSignificantCommitsResult, primeBumpIdentifiersResult, nil
}

/*
Checks the state object and if it finds some values are missing (after scanning the Git repository) fills
them with defaults.

This method assumes the Git history has already been scanned and the state object already has the information
coming from it.

Arguments are as follows:

- releaseType the release type giving parameters on how to compute the version. It can't be nil

Error is:

- DataAccessError in case the configuration can't be loaded for some reason.
- IllegalPropertyError in case the configuration has some illegal options.
- ReleaseError if the task is unable to complete for reasons due to the release process.
*/
func (c *Infer) fillStateMissingValuesWithDefaults(releaseType *ent.ReleaseType) error {
	if releaseType == nil {
		return &errs.NilPointerError{Message: fmt.Sprintf("the release type cannot be nil")}
	}

	releaseScope, err := c.State().GetReleaseScope()
	if err != nil {
		return err
	}
	if !releaseScope.HasPreviousVersion() || !releaseScope.HasPreviousVersionCommit() {
		initialVersion, err := c.State().GetConfiguration().GetInitialVersion()
		if err != nil {
			return err
		}
		log.Debugf("the commit history had no information about the previousVersion and previousVersionCommit, using default initial value '%s' for the previousVersion", *initialVersion)
		// use the configured initial version as the previous version
		releaseScope.SetPreviousVersion(initialVersion)
		releaseScope.SetPreviousVersionCommit(nil)
	}
	// if we couldn't infer the prime version and its commit, set the state attributes to the configured initial values
	if !releaseScope.HasPrimeVersion() || !releaseScope.HasPrimeVersionCommit() {
		if *releaseType.GetCollapseVersions() {
			initialVersion, err := c.State().GetConfiguration().GetInitialVersion()
			if err != nil {
				return err
			}
			log.Debugf("the commit history had no information about the primeVersion and primeVersionCommit and the release type uses collapsed versioning, using default initial value '%s' for the primeVersion", *initialVersion)
			// use the configured initial version as the prime version
			releaseScope.SetPrimeVersion(initialVersion)
			releaseScope.SetPrimeVersionCommit(nil)
		} else {
			log.Debugf("the commit history had no information about the primeVersion and primeVersionCommit but the release type doesn't use collapsed versioning, using the same values as the previousVersion and previousVersionCommit")
			// use the configured initial version as the prime version
			releaseScope.SetPrimeVersion(releaseScope.GetPreviousVersion())
			releaseScope.SetPrimeVersionCommit(releaseScope.GetPreviousVersionCommit())
		}
	}
	return nil
}

/*
Applies the extra identifiers defined by the releaseType, if any, and returns the new version with the extra identifiers.

Arguments are as follows:

- scheme the versioning scheme in use. It can't be nil or empty
- releaseType the release type giving parameters on how to compute the version. It can't be nil
- version the version to apply the identifiers to. It can't be nil

Error is:

- DataAccessError in case the configuration can't be loaded for some reason.
- IllegalPropertyError in case the configuration has some illegal options.
- GitError in case of unexpected issues when accessing the Git repository.
- ReleaseError if the task is unable to complete for reasons due to the release process.
*/
func (c *Infer) applyExtraIdentifiers(scheme *ver.Scheme, releaseType *ent.ReleaseType, version *ver.Version) (*ver.Version, error) {
	if scheme == nil {
		return nil, &errs.NilPointerError{Message: fmt.Sprintf("the scheme cannot be nil")}
	}
	if releaseType == nil {
		return nil, &errs.NilPointerError{Message: fmt.Sprintf("the release type cannot be nil")}
	}
	if version == nil {
		return nil, &errs.NilPointerError{Message: fmt.Sprintf("the version cannot be nil")}
	}

	res := *version

	log.Debugf("applying '%d' extra identifiers defined by the release type to version '%s'", len(*(*releaseType).GetIdentifiers()), res.String())

	for _, identifier := range *(*releaseType).GetIdentifiers() {
		if (*identifier).GetQualifier() == nil {
			log.Debugf("applying the '%s' extra identifier to version '%s'", "nil", res.String())
		} else {
			log.Debugf("applying the '%s' extra identifier to version '%s'", *(*identifier).GetQualifier(), res.String())
		}
		if (*identifier).GetQualifier() == nil || "" == strings.TrimSpace(*(*identifier).GetQualifier()) {
			return nil, &errs.IllegalPropertyError{Message: fmt.Sprintf("identifiers must define a non blank qualifier")}
		}

		identifierQualifier, err := c.renderTemplate((*identifier).GetQualifier())
		if err != nil {
			return nil, err
		}
		if identifierQualifier == nil || "" == *identifierQualifier {
			return nil, &errs.IllegalPropertyError{Message: fmt.Sprintf("the identifier qualifier must evaluate to a non empty string. Configured value is '%s', rendered string is '%s'", *(*identifier).GetQualifier(), *identifierQualifier)}
		}

		identifierValue, err := c.renderTemplate((*identifier).GetValue())
		if err != nil {
			return nil, err
		}
		if identifierValue == nil {
			log.Debugf("the extra identifier is defined by qualifier='%s' and value='%s', which are resolved to qualifier='%s' and value='%s'", *(*identifier).GetQualifier(), "nil", *identifierQualifier, "nil")
		} else {
			log.Debugf("the extra identifier is defined by qualifier='%s' and value='%s', which are resolved to qualifier='%s' and value='%s'", *(*identifier).GetQualifier(), *(*identifier).GetValue(), *identifierQualifier, *identifierValue)
		}

		// Semver is the only supported scheme so far...
		if ver.SEMVER == *scheme {
			semanticVersion, err := ver.ValueOfSemanticVersion(res.String()) // faster and safer than casting...
			if err != nil {
				return nil, err
			}

			if (*identifier).GetPosition() != nil && ent.PRE_RELEASE == *(*identifier).GetPosition() {
				// the value must be converted to an Integer when using SemVer and the pre-release part
				var identifierValueAsInteger *int

				if !(identifierValue == nil || "" == *identifierValue) {
					i, err := strconv.Atoi(*identifierValue)
					if err != nil {
						return nil, &errs.IllegalPropertyError{Message: fmt.Sprintf("invalid integer value '%s' for identifier '%s'. Semantic versioning requires integer numbers as values for identifiers in the pre release part.", *identifierValue, *(*identifier).GetQualifier())}
					}
					identifierValueAsInteger = &i
				} else {
					identifierValueAsInteger = nil
				}

				if identifierValueAsInteger == nil {
					semanticVersion, err = semanticVersion.SetPrereleaseAttributeWith(*identifierQualifier, nil)
				} else {
					semanticVersion, err = semanticVersion.SetPrereleaseAttributeWith(*identifierQualifier, identifierValueAsInteger)
				}
				if err != nil {
					return nil, err
				}
			} else if (*identifier).GetPosition() == nil || ent.BUILD == *(*identifier).GetPosition() {
				// BUILD is the default if no position is set
				if identifierValue == nil || "" == strings.TrimSpace(*identifierValue) {
					semanticVersion, err = semanticVersion.SetBuildAttributeWith(*identifierQualifier, nil)
				} else {
					semanticVersion, err = semanticVersion.SetBuildAttributeWith(*identifierQualifier, identifierValue)
				}
				if err != nil {
					return nil, err
				}
			} else {
				return nil, &errs.IllegalPropertyError{Message: fmt.Sprintf("illegal identifier position '%s' for identifier '%s'", *(*identifier).GetPosition(), *(*identifier).GetQualifier())}
			}

			res = semanticVersion

			log.Debugf("the version after applying the '%s' extra identifier is '%s'", *(*identifier).GetQualifier(), semanticVersion.String())
		} else {
			return nil, &errs.IllegalPropertyError{Message: fmt.Sprintf("extra identifiers are supported for '%s' scheme only", ver.SEMVER)}
		}
	}

	return &res, nil
}

/*
Computes the new version (if needed) based on the given arguments. The computed version is not stored in the state
object but is just returned by this method, ready to be further mangled. Extra attributes are also applied, if the release type
requires them.

This method assumes the Git history has already been scanned and the state object already has the information
coming from it.

When this method returns also the relevant attributes in the state object are updated, with more detail:
  - the bump is set to the identifier that has been bumped, if any;
  - the releaseScope/significantCommits is defined with the commits within the scope that yield to some
    version identified to be bumped, if any;

Arguments are as follows:

  - scheme the versioning scheme in use. It can't be nil or empty
  - bump the identifier to bump. It may be nil but when it's not this method just bumps this identifier and returns
    the version computed that way, with no further considerations. Set this argument to a non nil value only to override
    the internal logic of determining the identifier to bump, otherwise just pass it nil
  - releaseLenient when true prefixes, even others than the releasePrefix, are tolerated when parsing and comparing
    the prime and previous version
  - releasePrefix the release prefix that has been configured. This is considered when parsing and comparing the prime and previous
    version. It may be nil or empty
  - releaseType the release type giving parameters on how to compute the version. It can't be nil
  - scopeCommits the commits within the release scope. It can't be nil
  - previousVersion the previous version inferred from the Git commit history. It can't be nil so in case no previous version
    has been detected from the commit history the default one must be used
  - significantCommitsSincePreviousVersion the list of commits considered to be significant since the previousVersion. This list is not
    inspected but it's content is just used to set the list of significant commits on the
    release scope in case the release type does not use collapsed
    versioning} or it does but yet the version that is being returned is the one obtained by bumping the previousVersion
  - bumpIdentifierOnPreviousVersion the identifier to be bumped on the previous version. It may be nil in case no identifier
    has to be bumped on the previous version
  - primeVersion the prime version inferred from the Git commit history. It can't be nil so in case no prime version
    has been detected from the commit history the default one must be used. This is ignored when not using collapsed versioning}
  - significantCommitsSincePrimeVersion the list of commits considered to be significant since the prime version. This list is not inspected
    but it's content is just used to set the list of significant commits on the release scope in case the release type uses
    collapsed versioning and the version that is being returned is the one obtained by bumping the prime version
  - bumpIdentifierOnPrimeVersion the identifier to be bumped on the prime version. It may be nil in case no identifier
    has to be bumped on the prime version. This is ignored when the release type is not using collapsed versioning

Error is:

- DataAccessError in case the configuration can't be loaded for some reason.
- IllegalPropertyError in case the configuration has some illegal options.
- GitError in case of unexpected issues when accessing the Git repository.
- ReleaseError if the task is unable to complete for reasons due to the release process.
*/
func (c *Infer) computeVersion(scheme *ver.Scheme, bump *string, releaseLenient *bool, releasePrefix *string, releaseType *ent.ReleaseType, scopeCommits []*gitent.Commit, previousVersion *ver.Version, significantCommitsSincePreviousVersion []gitent.Commit, bumpIdentifierOnPreviousVersion *string, primeVersion *ver.Version, significantCommitsSincePrimeVersion []gitent.Commit, bumpIdentifierOnPrimeVersion *string) (*ver.Version, error) {
	if scheme == nil {
		return nil, &errs.NilPointerError{Message: fmt.Sprintf("the scheme cannot be nil")}
	}
	if releaseType == nil {
		return nil, &errs.NilPointerError{Message: fmt.Sprintf("the release type cannot be nil")}
	}
	if scopeCommits == nil {
		return nil, &errs.NilPointerError{Message: fmt.Sprintf("the scope commits list cannot be nil")}
	}
	if previousVersion == nil {
		return nil, &errs.NilPointerError{Message: fmt.Sprintf("the previous version cannot be nil")}
	}
	if primeVersion == nil {
		return nil, &errs.NilPointerError{Message: fmt.Sprintf("the prime version cannot be nil")}
	}

	if bump == nil {
		if len(scopeCommits) == 0 {
			// there are no new commits in the release scope
			log.Debugf("the release scope does not contain any commit since the previous version, version remains unchanged: '%s'", (*previousVersion).String())
			return previousVersion, nil
		} else {
			// initialize the object to be returned
			var res ver.Version

			// when using collapsed versioning we need to return greatest between:
			// - the primeVersion bumped with the core identifier among all those from significant commits since the primeVersion
			//   (only if we have significant commits since the primeVersion), then bumped with the pre-release identifier
			// - the previousVersion bumped with the pre-release identifier (only if we have significant commits since the previousVersion)
			//   while the core identifiers are never bumped (as it's done by the prime version)
			if *(*releaseType).GetCollapseVersions() {
				if (*releaseType).GetCollapsedVersionQualifier() == nil || "" == strings.TrimSpace(*(*releaseType).GetCollapsedVersionQualifier()) {
					return nil, &errs.ReleaseError{Message: fmt.Sprintf("the releaseType.collapsedVersionQualifier must have a value when using collapsed versioning")}
				}

				collapsedVersionQualifier, err := c.renderTemplate((*releaseType).GetCollapsedVersionQualifier())
				if err != nil {
					return nil, err
				}
				if collapsedVersionQualifier == nil || "" == strings.TrimSpace(*collapsedVersionQualifier) {
					return nil, &errs.ReleaseError{Message: fmt.Sprintf("the releaseType.collapsedVersionQualifier must have a value when using collapsed versioning. The template '%s' has been configured but it yields to '%s' after evaluation", *(*releaseType).GetCollapsedVersionQualifier(), *collapsedVersionQualifier)}
				}

				// compute the prime version
				var primeVersionBumped ver.Version
				if bumpIdentifierOnPrimeVersion == nil || "" == strings.TrimSpace(*bumpIdentifierOnPrimeVersion) {
					// bump only the collapsed identifier on the prime version
					log.Debugf("the release scope does not contain any significant commit since the prime version, only collapsed identifier '%s' is bumped while core identifiers are not bumped on prime version: '%s'", *collapsedVersionQualifier, (*primeVersion).String())
					primeVersionBumped, err = (*primeVersion).BumpVersion(*collapsedVersionQualifier)
					if err != nil {
						return nil, err
					}
					log.Debugf("bumping qualifier '%s' on prime version '%s' yields to '%s'", *collapsedVersionQualifier, (*primeVersion).String(), primeVersionBumped.String())
				} else {
					// bump the two identifiers on the prime version
					log.Debugf("the release scope contains significant commits since the prime version, core identifier '%s' and collapsed identifier '%s' are bumped on prime version: '%s'", *bumpIdentifierOnPrimeVersion, *collapsedVersionQualifier, (*primeVersion).String())
					primeVersionBumped, err = (*primeVersion).BumpVersion(*bumpIdentifierOnPrimeVersion)
					if err != nil {
						return nil, err
					}
					primeVersionBumped, err = primeVersionBumped.BumpVersion(*collapsedVersionQualifier)
					if err != nil {
						return nil, err
					}
					log.Debugf("bumping qualifiers '%s' and '%s' on prime version '%s' yields to '%s'", *bumpIdentifierOnPrimeVersion, *collapsedVersionQualifier, (*primeVersion).String(), primeVersionBumped.String())
				}

				// compute the previous version
				var previousVersionBumped ver.Version
				if bumpIdentifierOnPreviousVersion == nil || "" == strings.TrimSpace(*bumpIdentifierOnPreviousVersion) {
					// do not bump anything on the previous version
					log.Debugf("the release scope does not contain any significant commit since the previous version, identifiers are not bumped on previous version: '%s'", (*previousVersion).String())
					previousVersionBumped = *previousVersion
				} else {
					// bump only the collapsed identifier on the previous version
					log.Debugf("the release scope contains significant commits since the previous version, collapsed identifier '%s' is bumped on previous version: '%s'", *collapsedVersionQualifier, (*previousVersion).String())
					previousVersionBumped, err = (*previousVersion).BumpVersion(*collapsedVersionQualifier)
					if err != nil {
						return nil, err
					}
					log.Debugf("bumping qualifier '%s' on previous version '%s' yields to '%s'", *collapsedVersionQualifier, (*previousVersion).String(), previousVersionBumped.String())
				}

				// now compare the prime and previous version and see which one is greater
				var comparison int
				if *releaseLenient {
					v1 := primeVersionBumped.String()
					v2 := previousVersionBumped.String()
					comparison = ver.CompareWithSanitization(*scheme, &v1, &v2, *releaseLenient)
				} else {
					v1 := primeVersionBumped.String()
					v2 := previousVersionBumped.String()
					comparison = ver.CompareWithPrefix(*scheme, &v1, &v2, releasePrefix)
				}
				if comparison <= 0 {
					res = previousVersionBumped
					err = c.State().SetBump(bumpIdentifierOnPreviousVersion)
					if err != nil {
						return nil, err
					}
					if significantCommitsSincePreviousVersion != nil {
						releaseScope, err := c.State().GetReleaseScope()
						if err != nil {
							return nil, err
						}
						significantCommits := releaseScope.GetSignificantCommits()
						for _, commit := range significantCommitsSincePreviousVersion {
							commitToAppend := commit // avoid duplicate appends of the same item
							significantCommits = append(significantCommits, &commitToAppend)
						}
						releaseScope.SetSignificantCommits(significantCommits)
					}
				} else {
					res = primeVersionBumped
					err = c.State().SetBump(bumpIdentifierOnPrimeVersion)
					if err != nil {
						return nil, err
					}
					if significantCommitsSincePrimeVersion != nil {
						releaseScope, err := c.State().GetReleaseScope()
						if err != nil {
							return nil, err
						}
						significantCommits := releaseScope.GetSignificantCommits()
						for _, commit := range significantCommitsSincePrimeVersion {
							commitToAppend := commit // avoid duplicate appends of the same item
							significantCommits = append(significantCommits, &commitToAppend)
						}
						releaseScope.SetSignificantCommits(significantCommits)
					}
				}
				log.Debugf("the greatest version between '%s' and '%s' is '%s', which is the new (collapsed) version", primeVersionBumped, previousVersionBumped, res)
			} else {
				if bumpIdentifierOnPreviousVersion == nil || "" == strings.TrimSpace(*bumpIdentifierOnPreviousVersion) {
					log.Debugf("the release scope does not contain any significant commit since the previous version, version remains unchanged: '%s'", (*previousVersion).String())
					res = *previousVersion
				} else {
					log.Debugf("bumping component '%s' on version '%s'", *bumpIdentifierOnPreviousVersion, (*previousVersion).String())
					var err error
					res, err = (*previousVersion).BumpVersion(*bumpIdentifierOnPreviousVersion)
					if err != nil {
						return nil, err
					}
					c.State().SetBump(bumpIdentifierOnPreviousVersion)
					if significantCommitsSincePreviousVersion != nil {
						releaseScope, err := c.State().GetReleaseScope()
						if err != nil {
							return nil, err
						}
						significantCommits := releaseScope.GetSignificantCommits()
						for _, commit := range significantCommitsSincePreviousVersion {
							commitToAppend := commit // avoid duplicate appends of the same item
							significantCommits = append(significantCommits, &commitToAppend)
						}
						releaseScope.SetSignificantCommits(significantCommits)
					}
				}
			}

			// apply extra identifiers if there are significant commits
			if (bumpIdentifierOnPreviousVersion != nil && "" != strings.TrimSpace(*bumpIdentifierOnPreviousVersion)) ||
				(*(*releaseType).GetCollapseVersions() && bumpIdentifierOnPrimeVersion != nil && "" != strings.TrimSpace(*bumpIdentifierOnPrimeVersion)) {
				// apply extra identifiers, if any has been configured for the release type
				if (*releaseType).GetIdentifiers() == nil || len(*(*releaseType).GetIdentifiers()) == 0 {
					log.Debugf("the release type does not define any (enabled) extra identifiers so none is applied")
				} else {
					r, err := c.applyExtraIdentifiers(scheme, releaseType, &res)
					if err != nil {
						return nil, err
					}
					res = *r
				}
			}
			return &res, nil
		}
	} else {
		// the bump identifier has been detected by previous runs or overridden by the user
		log.Debugf("bumping component '%s' on version '%s'", *bump, (*previousVersion).String())
		res, err := (*previousVersion).BumpVersion(*bump)
		if err != nil {
			return nil, err
		}
		return &res, nil
	}
}

/*
Checks if the given version complies with the version range. The version range can be expressed as a static
regular expression template or can be computed dynamically from the branch name.

If the staticVersionRangeExpressionTemplate is not nil then it will be used as a static
regular expression. The string may be a template which is first rendered using the current state as the
context and then used as a regular expression to match the given version. Using it as a template
allows to make it dynamic and use state values (like the release prefix, for example).

If the staticVersionRangeExpressionTemplate is nil then the branch is considered.
If also the branch is nil then no check is performed, otherwise the branch name is
used to infer a dynamic regular expression that will be used to match the version.

If the check is required (at least one between staticVersionRangeExpressionTemplate and branch
is not nil) and succeeds returns true, if it's not required (both
staticVersionRangeExpressionTemplate and branch are nil) returns false,
otherwise if it is required and does not succeed a ReleaseError is thrown.

When using dynamic range checks the current branch is parsed trying to be as tolerant as possible, just finding
some pattern like major[.minor[.patch]] anywhere within the string. Each one of these identifiers can be
a fixed number or an x, which acts as a wildcard. For example:
  - 1.x: means that only version with major number 1 are accepted, while the minor and patch numbers can be anything
  - x.2.x: means that any major and patch numbers are allowed, while the minor number can be any valid number
  - rel/v1.2.3: is tolerated but the 'rel/v' is just ignored, while the version number can only be 1.2.3
  - v1.x-abc.123+def.456: tolerates any pre-release and build parts and the 'v' prefix, while the major number must
    be 1 (while the minor and patch can be anything)

When this method returns the version range attribute is set on the current
State instance.

Arguments are as follows:

  - scheme the versioning scheme in use. It can be nil only when the check is not required or when
    it has to be statically performed (in other words only when also branch is nil)
  - version the version to check. It can't be nil
  - staticVersionRangeExpressionTemplate the optional template that, once resolved, is used to check the
    given version. If nil the check is performed dynamically by inferring the version from the name of
    the branch (when branch is not nil) or not performed at all (when branch is also nil)
  - branch the name of the branch used to infer the version range regular expression from. This is ignored
    when staticVersionRangeExpressionTemplate is not nil as static checking has priority over
    dynamic checking. When staticVersionRangeExpressionTemplate is nil, if branch is
    also nil then no check is performed, otherwise the regular expression is inferred from the branch name.

Error is:

- DataAccessError in case the configuration can't be loaded for some reason.
- IllegalPropertyError in case the configuration has some illegal options.
- GitError in case of unexpected issues when accessing the Git repository.
- ReleaseError if the task is unable to complete for reasons due to the release process.
*/
func (c *Infer) checkVersionRange(scheme *ver.Scheme, version *ver.Version, staticVersionRangeExpressionTemplate *string, branch *string) (bool, error) {
	if version == nil {
		return false, &errs.NilPointerError{Message: fmt.Sprintf("version cannot be nil")}
	}

	// determine which regular expression to use, if static or inferred by the branch name
	if staticVersionRangeExpressionTemplate == nil || "" == strings.TrimSpace(*staticVersionRangeExpressionTemplate) {
		if branch == nil || "" == strings.TrimSpace(*branch) {
			log.Debugf("no version range check is required")
			err := c.State().SetVersionRange(nil)
			if err != nil {
				return false, err
			}
		} else {
			if scheme == nil {
				return false, &errs.NilPointerError{Message: fmt.Sprintf("scheme cannot be nil")}
			}
			// infer the expression dynamically from the branch name
			log.Debugf("the version range check is required with the regular expression inferred by the branch name")

			// Semver is the only supported scheme so far...
			if ver.SEMVER == *scheme {
				log.Debugf("scanning the branch name '%s' searching for a version range pattern", *branch)
				re, err := regexp2.Compile(SEMVER_DYNAMIC_VERSION_RANGE_FROM_BRANCH_NAME_REGEX, 0)
				if err != nil {
					return false, &errs.IllegalPropertyError{Message: fmt.Sprintf("cannot compile regular expression '%s'", SEMVER_DYNAMIC_VERSION_RANGE_FROM_BRANCH_NAME_REGEX), Cause: err}
				}
				m, err := re.FindStringMatch(*branch)
				if err != nil {
					return false, &errs.IllegalPropertyError{Message: fmt.Sprintf("regular expression '%s' can't be matched: %v", SEMVER_DYNAMIC_VERSION_RANGE_FROM_BRANCH_NAME_REGEX, err), Cause: err}
				}
				var major *regexp2.Group
				var minor *regexp2.Group
				var patch *regexp2.Group
				if m == nil { // when this is nil it means no match was found
					return false, &errs.IllegalPropertyError{Message: fmt.Sprintf("branch name '%s' doesn't seem to contain a parseable version range", *branch)}
				} else {
					major = m.GroupByName("major")
					minor = m.GroupByName("minor")
					patch = m.GroupByName("patch")
				}

				log.Debugf("building the dynamic version range regular expression with constraints inferred by '%s'", *branch)

				var dynamicVersionRangeExpression strings.Builder
				dynamicVersionRangeExpression.WriteString("^")

				// Where named groups are not defined or wildcarded allow any positive integer in that position,
				// otherwise just bring the number that came from the branch name.
				// Consider that these named groups can only be positive integers or 'x' to be matched by the
				// regular expression above.
				if major == nil || len(major.Captures) == 0 || "" == strings.TrimSpace(major.Captures[0].String()) || strings.EqualFold("x", major.Captures[0].String()) {
					dynamicVersionRangeExpression.WriteString("(0|[1-9]\\d*)")
				} else {
					dynamicVersionRangeExpression.WriteString(major.Captures[0].String())
				}
				dynamicVersionRangeExpression.WriteString("\\.")
				if minor == nil || len(minor.Captures) == 0 || "" == strings.TrimSpace(minor.Captures[0].String()) || strings.EqualFold("x", minor.Captures[0].String()) {
					dynamicVersionRangeExpression.WriteString("(0|[1-9]\\d*)")
				} else {
					dynamicVersionRangeExpression.WriteString(minor.Captures[0].String())
				}
				dynamicVersionRangeExpression.WriteString("\\.")
				if patch == nil || len(patch.Captures) == 0 || "" == strings.TrimSpace(patch.Captures[0].String()) || strings.EqualFold("x", patch.Captures[0].String()) {
					dynamicVersionRangeExpression.WriteString("(0|[1-9]\\d*)")
				} else {
					dynamicVersionRangeExpression.WriteString(patch.Captures[0].String())
				}

				// in order to tolerate any pre-release of build parts, let's finish the expression with a non capturing group
				// that accepts anything after a '-' or '+'
				dynamicVersionRangeExpression.WriteString("(?:(?:-|\\+).*)?$")

				log.Debugf("the dynamic version range regular expression that was built from '%s' is '%s'", *branch, dynamicVersionRangeExpression.String())
				// now we have the dynamically built regular expression
				dynamicVersionRangeExpressionString := dynamicVersionRangeExpression.String()
				c.State().SetVersionRange(&dynamicVersionRangeExpressionString)
			} else {
				return false, &errs.IllegalPropertyError{Message: fmt.Sprintf("version range check is supported for '%s' scheme only", ver.SEMVER.String())}
			}
		}
	} else {
		// use the statically configured expression
		log.Debugf("the version range check is required using a static regular expression")
		versionRangeRegExp, err := c.renderTemplate(staticVersionRangeExpressionTemplate)
		if err != nil {
			return false, err
		}

		log.Debugf("the configured regular expression template used for version range checks is '%s', which evaluates to '%s'", *staticVersionRangeExpressionTemplate, *versionRangeRegExp)

		err = c.State().SetVersionRange(versionRangeRegExp)
		if err != nil {
			return false, err
		}
	}

	if c.State().HasVersionRange() {
		versionRange, err := c.State().GetVersionRange()
		if err != nil {
			return false, err
		}
		log.Debugf("performing version range check against version '%s' using the expression '%s'", (*version).String(), *versionRange)
		re, err := regexp2.Compile(*versionRange, 0)
		if err != nil {
			return false, &errs.IllegalPropertyError{Message: fmt.Sprintf("cannot compile regular expression '%s' (evaluated by template '%s')", *versionRange, *staticVersionRangeExpressionTemplate), Cause: err}
		}
		match, err := re.MatchString((*version).String())
		if err != nil {
			return false, &errs.IllegalPropertyError{Message: fmt.Sprintf("cannot evaluate regular expression '%s' (evaluated by template '%s') against '%s'", *versionRange, *staticVersionRangeExpressionTemplate, (*version).String()), Cause: err}
		}
		if match {
			log.Debugf("version '%s' successfully matches version range pattern '%s'", (*version).String(), *versionRange)
			return true, nil
		} else {
			return false, &errs.ReleaseError{Message: fmt.Sprintf("version '%s' doesn't match version range pattern '%s'", (*version).String(), *versionRange)}
		}
	} else {
		log.Debugf("no version range check is performed")
		return false, nil
	}
}

/*
Checks if the given version is the latest in the repository, according to the scheme.
To run this check the given version is checked against all tags in the repository (ignoring those not
complying with the given scheme) and only if the given version is to be considered newer or equal to any
other version tag true is returned.

Arguments are as follows:

  - scheme the versioning scheme in use
  - version the version to check
  - releaseLenient when true prefixes, even others than the releasePrefix, are tolerated when parsing and comparing versions
  - releasePrefix the release prefix that has been configured. This is considered when parsing and comparing versions. It may be nil or empty

Error is:

- DataAccessError in case the configuration can't be loaded for some reason.
- IllegalPropertyError in case the configuration has some illegal options.
- GitError in case of unexpected issues when accessing the Git repository.
- ReleaseError if the task is unable to complete for reasons due to the release process.
*/
func (c *Infer) checkLatestVersion(scheme ver.Scheme, version string, releaseLenient *bool, releasePrefix *string) (bool, error) {
	log.Debugf("checking if version '%s' is the latest in the repository", version)

	tags, err := (*c.Repository()).GetTags()
	if err != nil {
		return false, err
	}

	for _, tag := range tags {
		tagName := tag.GetName()
		log.Tracef("checking against tag '%s'", tagName)
		var isLegal bool
		if releaseLenient != nil && *releaseLenient {
			isLegal = ver.IsLegalWithLenience(scheme, tagName, *releaseLenient)
		} else {
			isLegal = ver.IsLegalWithPrefix(scheme, tagName, releasePrefix)
		}
		if isLegal {
			log.Tracef("tag '%s' is a legal version according to '%s'", tagName, scheme.String())
			if releaseLenient != nil && *releaseLenient {
				if ver.CompareWithSanitization(scheme, &version, &tagName, *releaseLenient) < 0 {
					log.Debugf("tag '%s' is greater than '%s' according to '%s' so '%s' is not the latest version", tagName, version, scheme.String(), version)
					return false, nil
				} else {
					log.Tracef("tag '%s' is less or equal than '%s' according to '%s' so next tags will be tested (if any)", tagName, version, scheme.String())
				}
			} else {
				if ver.CompareWithPrefix(scheme, &version, &tagName, releasePrefix) < 0 {
					log.Debugf("tag '%s' is greater than '%s' according to '%s' so '%s' is not the latest version", tagName, version, scheme.String(), version)
					return false, nil
				} else {
					log.Tracef("tag '%s' is less or equal than '%s' according to '%s' so next tags will be tested (if any)", tagName, version, scheme.String())
				}
			}
		} else {
			log.Tracef("tag '%s' is not a legal version according to '%s' and will be ignored", tagName, scheme.String())
		}
	}
	log.Debugf("version '%s' is the latest in the repository since no newer version has been found", version)
	return true, nil
}

/*
Reset the attributes store by this command into the internal state object.
This is required before running the command in order to make sure that the new execution is not affected
by a stale status coming from previous runs.

Error is:

- DataAccessError in case the configuration can't be loaded for some reason.
- IllegalPropertyError in case the configuration has some illegal options.
*/
func (c *Infer) clearStateOutputAttributes() error {
	log.Debugf("clearing the state from Infer outputs")
	err := c.State().SetBranch(nil)
	if err != nil {
		return err
	}
	// the bump attribute can only be set (or reset) when the used didn't override the value from the configuration
	configurationBump, err := c.State().GetConfiguration().GetBump()
	if err != nil {
		return err
	}
	if configurationBump == nil {
		err = c.State().SetBump(nil)
		if err != nil {
			return err
		}
	}
	releaseScope, err := c.State().GetReleaseScope()
	if err != nil {
		return err
	}
	releaseScope.SetCommits(make([]*gitent.Commit, 0))
	releaseScope.SetPreviousVersion(nil)
	releaseScope.SetPreviousVersionCommit(nil)
	releaseScope.SetPrimeVersion(nil)
	releaseScope.SetPrimeVersionCommit(nil)
	releaseScope.SetSignificantCommits(make([]*gitent.Commit, 0))
	err = c.State().SetReleaseType(nil)
	if err != nil {
		return err
	}
	// the version attribute can only be set (or reset) when the used didn't override the value from the configuration
	configurationVersion, err := c.State().GetConfiguration().GetVersion()
	if err != nil {
		return err
	}
	if configurationVersion == nil {
		err = c.State().SetVersion(nil)
		if err != nil {
			return err
		}
	}
	err = c.State().SetVersionRange(nil)
	if err != nil {
		return err
	}
	return nil
}

/*
This method stores the state internal attributes used for up-to-date checks so that subsequent invocations
of the IsUpToDate() method can find them and determine if the command is already up to date.

This method is meant to be invoked at the end of a successful Run().

Error is:

- DataAccessError in case the configuration can't be loaded for some reason.
- IllegalPropertyError in case the configuration has some illegal options.
- GitError in case of unexpected issues when accessing the Git repository.
*/
func (c *Infer) storeStatusInternalAttributes() error {
	log.Debugf("storing the Infer command internal attributes to the State")
	configurationBump, err := c.State().GetConfiguration().GetBump()
	if err != nil {
		return err
	}
	err = c.putInternalAttribute(INFER_INTERNAL_INPUT_ATTRIBUTE_CONFIGURED_BUMP, configurationBump)
	if err != nil {
		return err
	}
	configurationInitialVersion, err := c.State().GetConfiguration().GetInitialVersion()
	if err != nil {
		return err
	}
	err = c.putInternalAttribute(INFER_INTERNAL_INPUT_ATTRIBUTE_CONFIGURED_INITIAL_VERSION, configurationInitialVersion)
	if err != nil {
		return err
	}
	var configurationReleaseLenientString *string
	configurationReleaseLenient, err := c.State().GetConfiguration().GetReleaseLenient()
	if err != nil {
		return err
	}
	if configurationReleaseLenient != nil {
		configurationReleaseLenientPlainString := strconv.FormatBool(*configurationReleaseLenient)
		configurationReleaseLenientString = &configurationReleaseLenientPlainString
	}
	err = c.putInternalAttribute(INFER_INTERNAL_INPUT_ATTRIBUTE_CONFIGURED_RELEASE_LENIENT, configurationReleaseLenientString)
	if err != nil {
		return err
	}
	configurationReleasePrefix, err := c.State().GetConfiguration().GetReleasePrefix()
	if err != nil {
		return err
	}
	err = c.putInternalAttribute(INFER_INTERNAL_INPUT_ATTRIBUTE_CONFIGURED_RELEASE_PREFIX, configurationReleasePrefix)
	if err != nil {
		return err
	}
	var configurationSchemeString *string
	configurationScheme, err := c.State().GetConfiguration().GetScheme()
	if err != nil {
		return err
	}
	if configurationScheme != nil {
		configurationSchemePlainString := configurationScheme.String()
		configurationSchemeString = &configurationSchemePlainString
	}
	err = c.putInternalAttribute(INFER_INTERNAL_INPUT_ATTRIBUTE_CONFIGURED_SCHEME, configurationSchemeString)
	if err != nil {
		return err
	}
	configurationVersion, err := c.State().GetConfiguration().GetVersion()
	if err != nil {
		return err
	}
	err = c.putInternalAttribute(INFER_INTERNAL_INPUT_ATTRIBUTE_CONFIGURED_VERSION, configurationVersion)
	if err != nil {
		return err
	}
	currentBranch, err := c.getCurrentBranch()
	if err != nil {
		return err
	}
	err = c.putInternalAttribute(INFER_INTERNAL_INPUT_ATTRIBUTE_REPOSITORY_CURRENT_BRANCH, &currentBranch)
	if err != nil {
		return err
	}
	latestCommit, err := c.getLatestCommit()
	if err != nil {
		return err
	}
	err = c.putInternalAttribute(INFER_INTERNAL_INPUT_ATTRIBUTE_REPOSITORY_LAST_COMMIT, &latestCommit)
	if err != nil {
		return err
	}

	stateVersion, err := c.State().GetVersion()
	if err != nil {
		return err
	}
	err = c.putInternalAttribute(INFER_INTERNAL_INPUT_ATTRIBUTE_STATE_VERSION, stateVersion)
	if err != nil {
		return err
	}
	return nil
}

/*
Returns true if this command is up to date, which means that the internal State would not
change by running the command again. It other words, when this method returns true any
invocation of the Run method is needless and idempotent about the state.

This method uses the quickest method to verify whether the state is up to date or not. This method must not rely on
dependencies and it must always evaluate its own status independently.

As a general rule this method checks if its inputs (i.e. from the configuration) have changed since the last run.

Error is:

- DataAccessError in case the configuration can't be loaded for some reason.
- IllegalPropertyError in case the configuration has some illegal options.
- GitError in case of unexpected issues when accessing the Git repository.
*/
func (c *Infer) IsUpToDate() (bool, error) {
	log.Debugf("checking whether the Infer command is up to date")
	// Never up to date if this command hasn't stored a version yet into the state or the stored version is different than the state version
	stateVersion, err := c.State().GetVersion()
	if err != nil {
		return false, err
	}
	if stateVersion == nil {
		log.Debugf("the Infer command is not up to date because the internal state has no version yet or the state version doesn't match the version previously generated by Infer")
		return false, nil
	}
	isStateVersionUpTodate, err := c.isInternalAttributeUpToDate(INFER_INTERNAL_INPUT_ATTRIBUTE_STATE_VERSION, stateVersion)
	if err != nil {
		return false, err
	}
	if !isStateVersionUpTodate {
		log.Debugf("the Infer command is not up to date because the internal state has no version yet or the state version doesn't match the version previously generated by Infer")
		return false, nil
	}

	// The command is never considered up to date when the repository branch or last commit has changed
	currentBranch, err := c.getCurrentBranch()
	if err != nil {
		return false, err
	}
	isCurrentBranchUpTodate, err := c.isInternalAttributeUpToDate(INFER_INTERNAL_INPUT_ATTRIBUTE_REPOSITORY_CURRENT_BRANCH, &currentBranch)
	if err != nil {
		return false, err
	}
	latestCommit, err := c.getLatestCommit()
	if err != nil {
		return false, err
	}
	isLatestCommitUpTodate, err := c.isInternalAttributeUpToDate(INFER_INTERNAL_INPUT_ATTRIBUTE_REPOSITORY_LAST_COMMIT, &latestCommit)
	if err != nil {
		return false, err
	}
	if !isCurrentBranchUpTodate || !isLatestCommitUpTodate {
		log.Debugf("the Infer command is not up to date because the last commit or the current branch has changed")
		return false, nil
	}
	// Check if configuration parameters have changed
	configurationBump, err := c.State().GetConfiguration().GetBump()
	if err != nil {
		return false, err
	}
	isConfigurationBumpUpTodate, err := c.isInternalAttributeUpToDate(INFER_INTERNAL_INPUT_ATTRIBUTE_CONFIGURED_BUMP, configurationBump)
	if err != nil {
		return false, err
	}
	configurationInitialVersion, err := c.State().GetConfiguration().GetInitialVersion()
	if err != nil {
		return false, err
	}
	isConfigurationInitialVersionUpTodate, err := c.isInternalAttributeUpToDate(INFER_INTERNAL_INPUT_ATTRIBUTE_CONFIGURED_INITIAL_VERSION, configurationInitialVersion)
	if err != nil {
		return false, err
	}
	configurationReleaseLenient, err := c.State().GetConfiguration().GetReleaseLenient()
	if err != nil {
		return false, err
	}
	var configurationReleaseLenientString *string
	if configurationReleaseLenient != nil {
		configurationReleaseLenientPlainString := strconv.FormatBool(*configurationReleaseLenient)
		configurationReleaseLenientString = &configurationReleaseLenientPlainString
	}
	isConfigurationReleaseLenientUpTodate, err := c.isInternalAttributeUpToDate(INFER_INTERNAL_INPUT_ATTRIBUTE_CONFIGURED_RELEASE_LENIENT, configurationReleaseLenientString)
	if err != nil {
		return false, err
	}
	configurationReleasePrefix, err := c.State().GetConfiguration().GetReleasePrefix()
	if err != nil {
		return false, err
	}
	isConfigurationReleasePrefixUpTodate, err := c.isInternalAttributeUpToDate(INFER_INTERNAL_INPUT_ATTRIBUTE_CONFIGURED_RELEASE_PREFIX, configurationReleasePrefix)
	if err != nil {
		return false, err
	}
	configurationScheme, err := c.State().GetConfiguration().GetScheme()
	if err != nil {
		return false, err
	}
	var configurationSchemeString *string
	if configurationScheme != nil {
		configurationSchemePlainString := configurationScheme.String()
		configurationSchemeString = &configurationSchemePlainString
	}
	isConfigurationSchemeUpTodate, err := c.isInternalAttributeUpToDate(INFER_INTERNAL_INPUT_ATTRIBUTE_CONFIGURED_SCHEME, configurationSchemeString)
	if err != nil {
		return false, err
	}
	configurationVersion, err := c.State().GetConfiguration().GetVersion()
	if err != nil {
		return false, err
	}
	isConfigurationVersionUpTodate, err := c.isInternalAttributeUpToDate(INFER_INTERNAL_INPUT_ATTRIBUTE_CONFIGURED_VERSION, configurationVersion)
	if err != nil {
		return false, err
	}
	res := isConfigurationBumpUpTodate && isConfigurationInitialVersionUpTodate && isConfigurationReleaseLenientUpTodate && isConfigurationReleasePrefixUpTodate && isConfigurationSchemeUpTodate && isConfigurationVersionUpTodate
	if res {
		log.Debugf("the Infer command is up to date")
	} else {
		log.Debugf("the Infer command is not up to date because the configuration or the internal state has changed")
	}
	return res, nil
}

/*
Infers all the required informations to produce a new release from the Git repository.

Inputs to this task are:
- the Git repository and the commit history;
- the configuration;

Outputs from this task are all stored in the State object, with more detail:
  - the version is defined with the new version identifier for the new release; if the user has overridden
    the version by configuration that value is simply used and no inference is done; if the version is not overridden
    by configuration and no previous versions can be found in the history the initial version from the
    configuration is used
    the releaseScope/commits is defined with the commits within the scope
  - the releaseScope/significantCommits is defined with the commits within the scope that yield to some
    version identified to be bumped, if any;
  - the releaseScope/previousVersion and releaseScope/previousVersionCommit are defined with the
    tag and SHA-1 of the previous release, if any; if no previous release is found or the version was overridden
    by the user configuration they will be nil;
  - the releaseScope/primeVersion and releaseScope/primeVersionCommit are defined with the
    tag and SHA-1 of the prime release, if any; if no prime release is found or the version was overridden
    by the user configuration they will be nil;
  - the releaseScope/initialCommit is defined with the SHA-1 of the commit right after the
    releaseScope/previousVersionCommit or, when releaseScope/previousVersionCommit can't be
    inferred, the repository root commit SHA-1 is used; if the user overrides the version by configuration
    this value remains nil
  - the releaseType

Error is:

- DataAccessError in case the configuration can't be loaded for some reason.
- IllegalPropertyError in case the configuration has some illegal options.
- GitError in case of unexpected issues when accessing the Git repository.
- ReleaseError if the task is unable to complete for reasons due to the release process.
*/
func (c *Infer) Run() (*stt.State, error) {
	log.Debugf("running the Infer command...")

	err := c.clearStateOutputAttributes()
	if err != nil {
		return nil, err
	}

	releaseType, err := c.resolveReleaseType()
	if err != nil {
		return nil, err
	}
	err = c.State().SetReleaseType(releaseType)
	if err != nil {
		return nil, err
	}

	scheme, err := c.State().GetScheme()
	if err != nil {
		return nil, err
	}
	releaseLenient, err := c.State().GetConfiguration().GetReleaseLenient()
	if err != nil {
		return nil, err
	}
	releasePrefix, err := c.State().GetConfiguration().GetReleasePrefix()
	if err != nil {
		return nil, err
	}
	configurationVersion, err := c.State().GetConfiguration().GetVersion()
	if err != nil {
		return nil, err
	}

	if c.State().HasVersion() && configurationVersion != nil {
		log.Debugf("version overridden by user: '%s'", *configurationVersion)
	} else {
		// The following collections are used to collect the significant commits and the identifiers to be
		// bumped since the prime version or since the previous version.
		// The primeSignificantCommits and primeBumpIdentifiers are only used for collapsed versioning.
		// The ''previous? and 'prime' collections must be kept separated because we only know which
		// identifiers to use only after the versions have been bumped and we know which one is greater.
		// The decision of which ones to use is up to the computeVersion(...) method.
		previousSignificantCommits := []gitent.Commit{}
		primeSignificantCommits := []gitent.Commit{}
		previousBumpIdentifiers := []string{}
		primeBumpIdentifiers := []string{}

		// some state attributes must be set first as they're also used for template rendering afterwards
		currentBranch, err := c.getCurrentBranch()
		if err != nil {
			return nil, err
		}
		log.Debugf("current Git branch is '%s'", currentBranch)
		c.State().SetBranch(&currentBranch)

		// STEP 1: scan the Git repository to collect informations from the commit history
		bump, err := c.State().GetBump()
		if err != nil {
			return nil, err
		}
		var filterTags *string
		filterTags = nil
		if releaseType.GetFilterTags() != nil {
			filterTags, err = c.renderTemplate(releaseType.GetFilterTags())
			if err != nil {
				return nil, err
			}
		}
		commitMessageConventions, err := c.State().GetConfiguration().GetCommitMessageConventions()
		if err != nil {
			return nil, err
		}
		previousSignificantCommits, previousBumpIdentifiers, primeSignificantCommits, primeBumpIdentifiers, err = c.scanRepository(scheme, bump, releaseLenient, releasePrefix, releaseType.GetCollapseVersions(), filterTags, *commitMessageConventions.GetItems(), previousSignificantCommits, previousBumpIdentifiers, primeSignificantCommits, primeBumpIdentifiers)
		if err != nil {
			return nil, err
		}

		// STEP 2: use default values for those attributes that were not found in the Git commit history
		err = c.fillStateMissingValuesWithDefaults(releaseType)
		if err != nil {
			return nil, err
		}

		// STEP 3: compute the new version
		releaseScope, err := c.State().GetReleaseScope()
		if err != nil {
			return nil, err
		}
		var previousVersion ver.Version
		if *releaseLenient {
			previousVersion, err = ver.ValueOfWithSanitization(*scheme, *releaseScope.GetPreviousVersion(), *releaseLenient)
		} else {
			previousVersion, err = ver.ValueOfWithPrefix(*scheme, *releaseScope.GetPreviousVersion(), releasePrefix)
		}
		if err != nil {
			return nil, err
		}
		var primeVersion ver.Version
		if *releaseLenient {
			primeVersion, err = ver.ValueOfWithSanitization(*scheme, *releaseScope.GetPrimeVersion(), *releaseLenient)
		} else {
			primeVersion, err = ver.ValueOfWithPrefix(*scheme, *releaseScope.GetPrimeVersion(), releasePrefix)
		}
		if err != nil {
			return nil, err
		}
		version, err := c.computeVersion(scheme, bump, releaseLenient, releasePrefix, releaseType, releaseScope.GetCommits(), &previousVersion, previousSignificantCommits, ver.MostRelevantIdentifierIn(*scheme, previousBumpIdentifiers), &primeVersion, primeSignificantCommits, ver.MostRelevantIdentifierIn(*scheme, primeBumpIdentifiers))
		if err != nil {
			return nil, err
		}

		log.Debugf("computed version is: '%s'", (*version).String())

		var stringVersion string
		if releasePrefix == nil {
			stringVersion = (*version).String()
		} else {
			stringVersion = *releasePrefix + (*version).String()
		}
		log.Infof("Version: '%s'", stringVersion)

		// STEP 4: perform consistency checks against configured or implicit constraints
		var checkVersionRangeOk bool
		if releaseType.GetVersionRange() == nil || "" == strings.TrimSpace(*releaseType.GetVersionRange()) {
			if releaseType.GetVersionRangeFromBranchName() != nil && *releaseType.GetVersionRangeFromBranchName() == true {
				checkVersionRangeOk, err = c.checkVersionRange(scheme, version, nil, &currentBranch)
				if err != nil {
					return nil, err
				}
			} else {
				checkVersionRangeOk, err = c.checkVersionRange(scheme, version, nil, nil)
				if err != nil {
					return nil, err
				}
			}
		} else {
			if releaseType.GetVersionRangeFromBranchName() != nil && *releaseType.GetVersionRangeFromBranchName() == true {
				checkVersionRangeOk, err = c.checkVersionRange(scheme, version, releaseType.GetVersionRange(), &currentBranch)
				if err != nil {
					return nil, err
				}
			} else {
				checkVersionRangeOk, err = c.checkVersionRange(scheme, version, releaseType.GetVersionRange(), nil)
				if err != nil {
					return nil, err
				}
			}
		}
		if checkVersionRangeOk {
			log.Debugf("version '%s' successfully passed range checks", (*version).String())
		} else {
			log.Debugf("version '%s' did not require version range checks", (*version).String())
		}

		// STEP 5: store values to the state object
		c.State().SetVersion(&stringVersion)
	}
	stringVersion, err := c.State().GetVersion()
	if err != nil {
		return nil, err
	}
	// check if the state version, regardless whether it was inferred or overridden, is the latest
	latestVersion, err := c.checkLatestVersion(*scheme, *stringVersion, releaseLenient, releasePrefix)
	if err != nil {
		return nil, err
	}
	c.State().SetLatestVersion(&latestVersion)

	err = c.storeStatusInternalAttributes()
	if err != nil {
		return nil, err
	}

	return c.State(), nil
}
