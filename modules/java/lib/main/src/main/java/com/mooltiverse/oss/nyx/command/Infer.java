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
package com.mooltiverse.oss.nyx.command;

import static com.mooltiverse.oss.nyx.log.Markers.COMMAND;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mooltiverse.oss.nyx.ReleaseException;
import com.mooltiverse.oss.nyx.data.CommitMessageConvention;
import com.mooltiverse.oss.nyx.data.DataAccessException;
import com.mooltiverse.oss.nyx.data.Identifier;
import com.mooltiverse.oss.nyx.data.IdentifierPosition;
import com.mooltiverse.oss.nyx.data.Tag;
import com.mooltiverse.oss.nyx.data.IllegalPropertyException;
import com.mooltiverse.oss.nyx.data.ReleaseType;
import com.mooltiverse.oss.nyx.git.GitException;
import com.mooltiverse.oss.nyx.git.Repository;
import com.mooltiverse.oss.nyx.state.State;
import com.mooltiverse.oss.nyx.version.Scheme;
import com.mooltiverse.oss.nyx.version.SemanticVersion;
import com.mooltiverse.oss.nyx.version.Version;
import com.mooltiverse.oss.nyx.version.Versions;

/**
 * The Infer command takes care of inferring and computing informations in order to make a new release.
 * 
 * After this task is executed the state object has:<br>
 * - the {@code version} attribute set to the release version
 * 
 * This class is not meant to be used in multi-threaded environments.
 */
public class Infer extends AbstractCommand {
    /**
     * The private logger instance
     */
    private static final Logger logger = LoggerFactory.getLogger(Infer.class);

    /**
     * The name used for the internal state attribute where we store the configured bump.
     */
    private static final String CONFIGURED_BUMP = Infer.class.getSimpleName().concat(".").concat("configured").concat(".").concat("bump");

    /**
     * The name used for the internal state attribute where we store the configured initial version.
     */
    private static final String CONFIGURED_INITIAL_VERSION = Infer.class.getSimpleName().concat(".").concat("configured").concat(".").concat("initialVersion");

    /**
     * The name used for the internal state attribute where we store the configured release lenient.
     */
    private static final String CONFIGURED_RELEASE_LENIENT = Infer.class.getSimpleName().concat(".").concat("configured").concat(".").concat("releaseLenient");

    /**
     * The name used for the internal state attribute where we store the configured release prefix.
     */
    private static final String CONFIGURED_RELEASE_PREFIX = Infer.class.getSimpleName().concat(".").concat("configured").concat(".").concat("releasePrefix");

    /**
     * The name used for the internal state attribute where we store the configured scheme.
     */
    private static final String CONFIGURED_SCHEME = Infer.class.getSimpleName().concat(".").concat("configured").concat(".").concat("scheme");

    /**
     * The name used for the internal state attribute where we store the configured version.
     */
    private static final String CONFIGURED_VERSION = Infer.class.getSimpleName().concat(".").concat("configured").concat(".").concat("version");

    /**
     * The name used for the internal state attribute where we store current branch name.
     */
    private static final String INTERNAL_BRANCH = Infer.class.getSimpleName().concat(".").concat("repository").concat(".").concat("current").concat(".").concat("branch");

    /**
     * The name used for the internal state attribute where we store the SHA-1 of the last
     * commit in the current branch by the time this command was last executed.
     */
    private static final String INTERNAL_LAST_COMMIT = Infer.class.getSimpleName().concat(".").concat("last").concat(".").concat("commit");

    /**
     * This regular expression is used to match a branch name in order to see if it's a wildcard that may represent
     * a valid version range.
     * 
     * The expression uses named groups {@code major}, {@code minor} and {@code patch} for those parts meant to represent
     * their respective semantic version identifiers, while there are other anonymous (non capturing groups) for those parts
     * that are tolerated but ignored.
     * 
     * Each part can be present or not. When it's not present it's assumed that any identifier may appear in that position.
     * When present it may be a number, in which case only those numbers are allowed in the version range check, or it may
     * be an {@code x}, which acts as a wildcard, allowing any number to appear in there.
     * 
     * For example:<br>
     * - 1.x: means that only version with major number 1 are accepted, while the minor and patch numbers can be anything<br>
     * - x.2.x: means that any major and patch numbers are allowed, while the minor number can be any valid number<br>
     * - rel/v1.2.3: is tolerated but the 'rel/v' is just ignored, while the version number can only be 1.2.3<br>
     * - v1.x-abc.123+def.456: tolerates any pre-release and build parts and the 'v' prefix, while the major number must
     *   be 1 (while the minor and patch can be anything)<br>
     */
    private static final String SEMVER_DYNAMIC_VERSION_RANGE_FROM_BRANCH_NAME_REGEX = "^(?:.*?)(?<major>x|[0-9]+)(?:\\.(?<minor>x|[0-9]+)(?:\\.(?<patch>x|[0-9]+))?)?(?:(?:-|\\+).*)?$";

    /**
     * Standard constructor.
     * 
     * @param state the state reference
     * @param repository the repository reference
     * 
     * @throws NullPointerException if a given argument is {@code null}
     */
    public Infer(State state, Repository repository) {
        super(state, repository);
        logger.debug(COMMAND, "New Infer command object");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isUpToDate()
        throws DataAccessException, IllegalPropertyException, GitException {
        logger.debug(COMMAND, "Checking whether the Infer command is up to date");
        // The command is never considered up to date when the repository is not clean
        if (!isRepositoryClean())
            return false;
        // Never up to date if this command hasn't stored a version yet into the state
        if (Objects.isNull(state().getVersion()))
            return false;

        // The command is never considered up to date when the repository branch or last commit has changed
        if ((!isInternalAttributeUpToDate(INTERNAL_BRANCH, getCurrentBranch())) || (!isInternalAttributeUpToDate(INTERNAL_LAST_COMMIT, getLatestCommit())))
            return false;
        // Check if configuration parameters have changed
        return isInternalAttributeUpToDate(CONFIGURED_VERSION, state().getConfiguration().getVersion()) &&
            isInternalAttributeUpToDate(CONFIGURED_BUMP, state().getConfiguration().getBump()) &&
            isInternalAttributeUpToDate(CONFIGURED_SCHEME, state().getConfiguration().getScheme()) &&
            isInternalAttributeUpToDate(CONFIGURED_INITIAL_VERSION, state().getConfiguration().getInitialVersion()) &&
            isInternalAttributeUpToDate(CONFIGURED_RELEASE_PREFIX, state().getConfiguration().getReleasePrefix()) &&
            isInternalAttributeUpToDate(CONFIGURED_RELEASE_LENIENT, state().getConfiguration().getReleaseLenient());
    }

    /**
     * This method stores the state internal attributes used for up-to-date checks so that subsequent invocations
     * of the {@link #isUpToDate()} method can find them and determine if the command is already up to date.
     * 
     * This method is meant to be invoked at the end of a succesful {@link #run()}.
     * 
     * @throws DataAccessException in case the configuration can't be loaded for some reason.
     * @throws IllegalPropertyException in case the configuration has some illegal options.
     * @throws GitException in case of unexpected issues when accessing the Git repository.
     * 
     * @see #isUpToDate()
     * @see State#getInternals()
     */
    private void storeStatusInternalAttributes()
        throws DataAccessException, IllegalPropertyException, GitException {
        logger.debug(COMMAND, "Storing the Infer command internal attributes to the State");
        putInternalAttribute(INTERNAL_BRANCH, getCurrentBranch());
        putInternalAttribute(INTERNAL_LAST_COMMIT, getLatestCommit());
        putInternalAttribute(CONFIGURED_VERSION, state().getConfiguration().getVersion());
        putInternalAttribute(CONFIGURED_BUMP, state().getConfiguration().getBump());
        putInternalAttribute(CONFIGURED_SCHEME, state().getConfiguration().getScheme());
        putInternalAttribute(CONFIGURED_INITIAL_VERSION, state().getConfiguration().getInitialVersion());
        putInternalAttribute(CONFIGURED_RELEASE_PREFIX, state().getConfiguration().getReleasePrefix());
        putInternalAttribute(CONFIGURED_RELEASE_LENIENT, state().getConfiguration().getReleaseLenient());
    }

    /**
     * Infers all the required informations to produce a new release from the Git repository.
     * <br>
     * Inputs to this task are:<br>
     * - the Git repository and the commit history;<br>
     * - the configuration;<br>
     * <br>
     * Outputs from this task are all stored in the State object, with more detail:<br>
     * - the {@code version} is defined with the new version identifier for the new release; if the user has overridden
     *   the version by configuration that value is simply used and no inference is done; if the version is not overridden
     *   by configuration and no previous versions can be found in the history the initial version from the
     *   configuration is used<br>
     * - the {@code releaseScope/commits} is defined with the commits within the scope<br>
     * - the {@code releaseScope/significantCommits} is defined with the commits within the scope that yield to some
     *   version identified to be bumped, if any;<br>
     * - the {@code releaseScope/previousVersion} and {@code releaseScope/previousVersionCommit} are defined with the
     *   tag and SHA-1 of the previous release, if any; if no previous release is found or the version was overridden
     *   by the user configuration they will be {@code null};<br>
     * - the {@code releaseScope/primeVersion} and {@code releaseScope/primeVersionCommit} are defined with the
     *   tag and SHA-1 of the prime release, if any; if no prime release is found or the version was overridden
     *   by the user configuration they will be {@code null};<br>
     * - the {@code releaseScope/initialCommit} is defined with the SHA-1 of the commit right after the
     *   {@code releaseScope/previousVersionCommit} or, when {@code releaseScope/previousVersionCommit} can't be
     *   inferred, the repository root commit SHA-1 is used; if the user overrides the version by configuration
     *   this value remains {@code null}
     * - the {@code releaseType} is defined with the templates resolved<br>
     * 
     * @throws DataAccessException in case the configuration can't be loaded for some reason.
     * @throws IllegalPropertyException in case the configuration has some illegal options.
     * @throws GitException in case of unexpected issues when accessing the Git repository.
     * @throws ReleaseException if the task is unable to complete for reasons due to the release process.
     * 
     * @return the updated reference to the state object. The returned object is the same instance passed in the constructor.
     * 
     * @see #isUpToDate()
     * @see #state()
     */
    @Override
    public State run()
        throws DataAccessException, IllegalPropertyException, GitException, ReleaseException {
        logger.debug(COMMAND, "Running the Infer command...");

        state().setReleaseType(resolveReleaseType());

        if (state().hasVersion() && !Objects.isNull(state().getConfiguration().getVersion()))
            logger.info(COMMAND, "Version overridden by user: {}", state().getConfiguration().getVersion());
        else {
            // some state attributes must be set first as they're also used for template rendering afterwards
            logger.debug(COMMAND, "Current Git branch is {}", repository().getCurrentBranch());
            state().setBranch(repository().getCurrentBranch());

            // these objects are fetched in advance from the state because they may throw an exception
            // that we can't handle in the lambda expression
            final boolean bumpOverride = state().hasBump();
            final Map<String,CommitMessageConvention> commitMessageConventions = state().getConfiguration().getCommitMessageConventions().getItems();
            final boolean releaseLenient = state().getConfiguration().getReleaseLenient().booleanValue();
            final String releasePrefix = state().getConfiguration().getReleasePrefix();
            final Scheme scheme = state().getScheme();
            final ReleaseType releaseType = state().getReleaseType();
            final String filterTagsExpression = renderTemplate(releaseType.getFilterTags());
            final boolean collapseVersions = releaseType.getCollapseVersions();

            // the following sets are used to collect the identifiers to be bumped since the prime version or
            // since the previous version
            // the primeBumpIdentifiers is only used for collapsed versioning
            // these two sets must be kept separated because we only know which identifiers to use only after
            // the versions have been bumped and we know which one is greater
            // these must be used only if the bump was not overridden by configuration
            final Set<String> previousBumpIdentifiers = new HashSet<String>();
            final Set<String> primeBumpIdentifiers = new HashSet<String>();

            // the following maps are temporary storages for the significant commits
            // at this stage we don't know which one will be used yet so we just park them here for later evaluation
            final Map<String,String> previousSignificantCommits = new HashMap<String,String>();
            final Map<String,String> primeSignificantCommits = new HashMap<String,String>();

            logger.debug(COMMAND, "Walking the commit history...");
            repository().walkHistory(null, null, c -> {
                logger.debug(COMMAND, "Stepping by commit {}", c.getSHA());
                logger.debug(COMMAND, "Commit {} has {} tags: {}", c.getSHA(), c.getTags().size(), c.getTags());

                // Inspect the tags in order to determine what kind of commit this is.
                // If this commit has tags that make it the 'previous version commit' then the release scope
                // previousVersion and previousVersionCommit are set and this commit closes the release scope
                // (without being part of it), otherwise this is just another commit that belongs to the scope
                // The primeVersion and primeVersionCommit are also detected: if the release type is using
                // collapsed versioning their search may go beyond (backward) the previousVersion and previousVersionCommit
                // otherwise they are the same.
                // If the commit has multiple valid version tags they are all evaluated and compared to select the greatest
                for (Tag tag: c.getTags()) {
                    if (releaseLenient ? Versions.isLegal(scheme, tag.getName(), releaseLenient) : Versions.isLegal(scheme, tag.getName(), releasePrefix)) {
                        logger.debug(COMMAND, "Evaluating tag {}: tag is a valid version according to the {} scheme and will be passed to the next evaluation steps. The tag is applied to commit {}", tag.getName(), scheme.toString(), c.getSHA());

                        final int previousVersionComparison = releaseLenient ? Versions.compare(scheme, tag.getName(), state().getReleaseScope().getPreviousVersion(), releaseLenient) : Versions.compare(scheme, tag.getName(), state().getReleaseScope().getPreviousVersion(), releasePrefix);
                        if (previousVersionComparison > 0) {
                            logger.debug(COMMAND, "Evaluating tag {}: tag is greater than previously selected previousVersion tag {} and will be passed to the next evaluation steps. The tag is applied to commit {}", tag.getName(), Objects.isNull(state().getReleaseScope().getPreviousVersion()) ? "<none>" : state().getReleaseScope().getPreviousVersion(), c.getSHA());

                            if (Objects.isNull(filterTagsExpression) || filterTagsExpression.isBlank() || Pattern.matches(filterTagsExpression, tag.getName())) {
                                if (Objects.isNull(filterTagsExpression) || filterTagsExpression.isBlank()) {
                                    logger.debug(COMMAND, "Evaluating tag {}: the selected release type does not specify any additional filter for tags (or the tags filter template yields to an empty regular expression after evaluation) so the tag will be used as the previousVersion and the commit {} is used as the previousVersionCommit", tag.getName(), c.getSHA());
                                }
                                else if (Pattern.matches(filterTagsExpression, tag.getName())) {
                                    logger.debug(COMMAND, "Evaluating tag {}: the selected release type specifies an additional filter '{}' (after template evaluation) for tags and the tag succesfully matches so the tag will be used as the previousVersion and the commit {} is used as the previousVersionCommit", tag.getName(), filterTagsExpression, c.getSHA());
                                }

                                state().getReleaseScope().setPreviousVersion(tag.getName());
                                state().getReleaseScope().setPreviousVersionCommit(c.getSHA());
                            }
                            else logger.debug(COMMAND, "Evaluating tag {}: the selected release type specifies an additional filter '{}' (after template evaluation) for tags but the tag doesn't match it so it will be ignored. The tag is applied to commit {}", tag.getName(), filterTagsExpression, c.getSHA());
                        }
                        else logger.debug(COMMAND, "Evaluating tag {}: tag is less than previously selected previousVersion tag {} so it will be ignored. The tag is applied to commit {}", tag.getName(), Objects.isNull(state().getReleaseScope().getPreviousVersion()) ? "<none>" : state().getReleaseScope().getPreviousVersion(), c.getSHA());

                        if (collapseVersions) {
                            logger.debug(COMMAND, "Evaluating tag {}: the selected release type uses collapsed versioning so the tag will be passed to the next evaluation steps to check if it's a valid primeVersion. The tag is applied to commit {}", tag.getName(), c.getSHA());

                            if (releaseLenient ? Versions.isCore(scheme, tag.getName(), releaseLenient) : Versions.isCore(scheme, tag.getName(), releasePrefix)) {
                                logger.debug(COMMAND, "Evaluating tag {}: tag is a valid core version according to the {} scheme and the selected release type uses collapsed versioning so the tag will be passed to the next evaluation steps to check if it's a valid primeVersion. The tag is applied to commit {}", tag.getName(), scheme.toString(), c.getSHA());
                            
                                final int primeVersionComparison = releaseLenient ? Versions.compare(scheme, tag.getName(), state().getReleaseScope().getPrimeVersion(), releaseLenient) : Versions.compare(scheme, tag.getName(), state().getReleaseScope().getPrimeVersion(), releasePrefix);
                                if (primeVersionComparison > 0) {
                                    logger.debug(COMMAND, "Evaluating tag {}: tag is greater than previously selected primeVersion tag {} so it will be used as the primeVersion and the commit {} is used as the primeVersionCommit", tag.getName(), Objects.isNull(state().getReleaseScope().getPrimeVersion()) ? "<none>" : state().getReleaseScope().getPrimeVersion(), c.getSHA());

                                    state().getReleaseScope().setPrimeVersion(tag.getName());
                                    state().getReleaseScope().setPrimeVersionCommit(c.getSHA());

                                    if (!state().getReleaseScope().hasPreviousVersion()) {
                                        logger.debug(COMMAND, "Evaluating tag {}: a primeVersion has been encountered before any valid previousVersion so this tag will also be used as the previousVersion the commit {} as the prreviousVersionCommit", tag.getName(), c.getSHA());

                                        state().getReleaseScope().setPreviousVersion(tag.getName());
                                        state().getReleaseScope().setPreviousVersionCommit(c.getSHA());
                                    }
                                }
                                else logger.debug(COMMAND, "Evaluating tag {}: tag is less than previously selected primeVersion tag {} so it will be ignored for the primeVersion", tag.getName(), Objects.isNull(state().getReleaseScope().getPrimeVersion()) ? "<none>" : state().getReleaseScope().getPrimeVersion());
                            }
                            else logger.debug(COMMAND, "Evaluating tag {}: tag is not a valid core version according to the {} scheme so the tag will be ignored for the primeVersion. The tag is applied to commit {}", tag.getName(), scheme.toString(), c.getSHA());
                        }
                    }
                    else logger.debug(COMMAND, "Evaluating tag {}: tag is not a valid version according to the {} scheme and will be ignored. The tag is applied to commit {}", tag.getName(), scheme.toString(), c.getSHA());
                }

                // If this is a commit within the scope let's add it to the scope and inspect it
                if (!(state().getReleaseScope().hasPreviousVersion() && state().getReleaseScope().hasPreviousVersionCommit())) {
                    logger.debug(COMMAND, "Commit {} has no valid version tags so it's added to the release scope", c.getSHA());
                    state().getReleaseScope().getCommits().add(c.getSHA());
                }

                // if the 'bump' was not overridden by user, evaluate the commit message against the configured conventions to see which identifier must be dumped, if any
                if (!bumpOverride) {
                    if (!Objects.isNull(commitMessageConventions)) {
                        // Let's find the identifier to bump (unless the bump was overridden by user).
                        // We need to consider all commits within the scope and, when using collapsed versioning,
                        // also those between the primeVersionCommit and the finalCommit
                        if ((!(state().getReleaseScope().hasPreviousVersion() && state().getReleaseScope().hasPreviousVersionCommit())) ||
                            (collapseVersions && (!(state().getReleaseScope().hasPrimeVersion() && state().getReleaseScope().hasPrimeVersionCommit())))) {
                            logger.debug(COMMAND, "Trying to infer the identifier to bump based on the commit message of commit {}", c.getSHA());
                            for (Map.Entry<String,CommitMessageConvention> cmcEntry: commitMessageConventions.entrySet()) {
                                logger.debug(COMMAND, "Evaluating commit {} against message convention {}", c.getSHA(), cmcEntry.getKey());                                
                                Matcher messageMatcher = Pattern.compile(cmcEntry.getValue().getExpression()).matcher(c.getMessage().getFullMessage());
                                if (messageMatcher.matches()) {
                                    logger.debug(COMMAND, "Commit message convention {} matches commit {}", cmcEntry.getKey(), c.getSHA());
                                    for (Map.Entry<String,String> bumpExpression: cmcEntry.getValue().getBumpExpressions().entrySet()) {
                                        logger.debug(COMMAND, "Matching commit {} ('{}') against bump expression {} ('{}') of message convention {}", c.getSHA(), c.getMessage().getFullMessage(), bumpExpression.getKey(), bumpExpression.getValue(), cmcEntry.getKey());
                                        Matcher bumpMatcher = Pattern.compile(bumpExpression.getValue()).matcher(c.getMessage().getFullMessage());
                                        if (bumpMatcher.matches()) {
                                            logger.debug(COMMAND, "Bump expression {} ('{}') of message convention {} matches commit {} ('{}'), meaning that the {} identifier has to be bumped, according to this commit", bumpExpression.getKey(), bumpExpression.getValue(), cmcEntry.getKey(), c.getSHA(), c.getMessage().getFullMessage(), bumpExpression.getKey());
                                            // if we reached this point this is also in the 'prime commit' scope
                                            primeBumpIdentifiers.add(bumpExpression.getKey());
                                            primeSignificantCommits.put(c.getSHA(), bumpExpression.getKey());

                                            if (!(state().getReleaseScope().hasPreviousVersion() && state().getReleaseScope().hasPreviousVersionCommit())) {
                                                // if the previous version wasn't found yet this is in the 'previous commit' scope
                                                previousBumpIdentifiers.add(bumpExpression.getKey());
                                                previousSignificantCommits.put(c.getSHA(), bumpExpression.getKey());
                                            }
                                        }
                                        else logger.debug(COMMAND, "Bump expression {} ('{}') of message convention {} doesn't match commit {} ('{}')", bumpExpression.getKey(), bumpExpression.getValue(), cmcEntry.getKey(), c.getSHA(), c.getMessage().getFullMessage());
                                    }
                                }
                                else logger.debug(COMMAND, "Commit message convention {} doesn't match commit {}, skipping", cmcEntry.getKey(), c.getSHA());
                            }
                        }
                    }
                    else logger.debug(COMMAND, "No commit message convention has been configured, skipping inference of the identifier to bump based on commit messages");
                }

                // stop walking the commit history if we already have the previous and prime versions (and their commits), otherwise keep walking
                return !(state().getReleaseScope().hasPreviousVersion() && state().getReleaseScope().hasPreviousVersionCommit() &&
                         state().getReleaseScope().hasPrimeVersion() && state().getReleaseScope().hasPrimeVersionCommit());
            });
            logger.debug(COMMAND, "Walking the commit history finished.");

            // if we couldn't infer the initial version and its commit, set the state attributes to the configured initial values or the prime version (if available)
            if (!state().getReleaseScope().hasPreviousVersion() || !state().getReleaseScope().hasPreviousVersionCommit()) {
                logger.debug(COMMAND, "The commit history had no information about the previousVersion and previousVersionCommit, using default initial value {} for the previousVersion", state().getConfiguration().getInitialVersion());
                // use the configured initial version as the previous version
                state().getReleaseScope().setPreviousVersion(state().getConfiguration().getInitialVersion());
                state().getReleaseScope().setPreviousVersionCommit(null);
            }
            // if we couldn't infer the prime version and its commit, set the state attributes to the configured initial values
            if (!state().getReleaseScope().hasPrimeVersion() || !state().getReleaseScope().hasPrimeVersionCommit()) {
                if (collapseVersions) {
                    logger.debug(COMMAND, "The commit history had no information about the primeVersion and primeVersionCommit and the release type uses collapsed versioning, using default initial value {} for the primeVersion", state().getConfiguration().getInitialVersion());
                    // use the configured initial version as the prime version
                    state().getReleaseScope().setPrimeVersion(state().getConfiguration().getInitialVersion());
                    state().getReleaseScope().setPrimeVersionCommit(null);
                }
                else {
                    logger.debug(COMMAND, "The commit history had no information about the primeVersion and primeVersionCommit but the release type doesn't use collapsed versioning, using the same values as the previousVersion and previousVersionCommit");
                    // use the configured initial version as the prime version
                    state().getReleaseScope().setPrimeVersion(state().getReleaseScope().getPreviousVersion());
                    state().getReleaseScope().setPrimeVersionCommit(state().getReleaseScope().getPreviousVersionCommit());
                }
            }

            // if the user hasn't overridden the bump identifier let's find out which identifier is supposed to be bumped
            // in the previous version scope and in the prime version scope (if used collapsed versioning)
            // for each scope, the identifier to bump is the most significant one among the collected ones
            String previousVersionBumpIdentifier = null;
            String primeVersionBumpIdentifier = null;
            if (!bumpOverride) {
                previousVersionBumpIdentifier = Versions.mostRelevantIdentifier(scheme, previousBumpIdentifiers);
                logger.debug(COMMAND, "The most relevant identifier to bump on the previous version inferred among significant commits is {}", Objects.isNull(previousVersionBumpIdentifier) ? "null" : previousVersionBumpIdentifier);
                
                if (collapseVersions) {
                    primeVersionBumpIdentifier = Versions.mostRelevantIdentifier(scheme, primeBumpIdentifiers);
                    logger.debug(COMMAND, "The most relevant identifier to bump on the prime version inferred among significant commits is {}", Objects.isNull(primeVersionBumpIdentifier) ? "null" : primeVersionBumpIdentifier);
                }
            }

            // parse the previous version
            Version previousVersion = releaseLenient ? Versions.valueOf(scheme, state().getReleaseScope().getPreviousVersion(), releaseLenient) : Versions.valueOf(scheme, state().getReleaseScope().getPreviousVersion(), releasePrefix);

            // finally compute the version
            Version version = null;
            if (state().hasBump()) {
                // the bump identifier has been detected by previous runs or overridden by the user
                logger.debug(COMMAND, "Bumping component {} on version {}", state().getBump(), previousVersion.toString());
                version = previousVersion.bump(state().getBump());
            }
            else {
                // when using collapsed versioning we need to return greatest between:
                // - the primeVersion bumped with the core identifier among all those from significant commits since the primeVersion
                //   (only if we have significant commits since the primeVersion), then bumped with the pre-release identifier
                // - the previousVersion bumped with the pre-release identifier (only if we have significant commits since the previousVersion)
                //   while the core identifiers are never bumped (as it's done by the prime version)
                if (collapseVersions) {
                    if (Objects.isNull(releaseType.getCollapsedVersionQualifier()) || releaseType.getCollapsedVersionQualifier().isBlank())
                        throw new ReleaseException("The releaseType.collapsedVersionQualifier must have a value when using collapsed versioning");

                    String collapsedVersionQualifier = renderTemplate(releaseType.getCollapsedVersionQualifier());
                    if (Objects.isNull(collapsedVersionQualifier) || collapsedVersionQualifier.isBlank())
                        throw new ReleaseException(String.format("The releaseType.collapsedVersionQualifier must have a value when using collapsed versioning. The template '%s' has been configured but it yields to '%s' after evaluation", releaseType.getCollapsedVersionQualifier(), Objects.isNull(collapsedVersionQualifier) ? "null" : collapsedVersionQualifier));

                    // parse the prime version
                    Version primeVersion = releaseLenient ? Versions.valueOf(scheme, state().getReleaseScope().getPrimeVersion(), releaseLenient) : Versions.valueOf(scheme, state().getReleaseScope().getPrimeVersion(), releasePrefix);
                    
                    // compute the prime version
                    Version primeVersionBumped = null;
                    if (Objects.isNull(primeVersionBumpIdentifier) || primeVersionBumpIdentifier.isBlank()) {
                        // bump only the collapsed identifier on the prime version
                        logger.info(COMMAND, "The release scope does not contain any significant commit since the prime version, only collapsed identifier {} is bumped while core identifiers are not bumped on prime version: {}", collapsedVersionQualifier, primeVersion.toString());
                        primeVersionBumped = primeVersion.bump(collapsedVersionQualifier);
                        logger.debug(COMMAND, "Bumping qualifier {} on prime version {} yields to {}", collapsedVersionQualifier, primeVersion.toString(), primeVersionBumped.toString());
                    }
                    else {
                        // bump the two identifiers on the prime version
                        logger.info(COMMAND, "The release scope contains significant commits since the prime version, core identifier {} and collapsed identifier {} are bumped on prime version: {}", primeVersionBumpIdentifier, collapsedVersionQualifier, primeVersion.toString());
                        primeVersionBumped = primeVersion.bump(primeVersionBumpIdentifier).bump(collapsedVersionQualifier);
                        logger.debug(COMMAND, "Bumping qualifiers {} and {} on prime version {} yields to {}", primeVersionBumpIdentifier, collapsedVersionQualifier, primeVersion.toString(), primeVersionBumped.toString());
                    }

                    // compute the previous version
                    Version previousVersionBumped = null;
                    if (Objects.isNull(previousVersionBumpIdentifier) || previousVersionBumpIdentifier.isBlank()) {
                        // do not bump anything on the previous version
                        logger.info(COMMAND, "The release scope does not contain any significant commit since the previous version, identifiers are not bumped on previous version: {}", previousVersion.toString());
                        previousVersionBumped = previousVersion;
                    }
                    else {
                        // bump only the collapsed identifier on the previous version
                        logger.info(COMMAND, "The release scope contains significant commits since the previous version, collapsed identifier {} is bumped on previous version: {}", collapsedVersionQualifier, previousVersion.toString());
                        previousVersionBumped = previousVersion.bump(collapsedVersionQualifier);
                        logger.debug(COMMAND, "Bumping qualifier {} on previous version {} yields to {}", collapsedVersionQualifier, previousVersion.toString(), previousVersionBumped.toString());

                    }

                    // now compare the prime and previous version and see which one is greater
                    int comparison = releaseLenient ? Versions.compare(scheme, primeVersionBumped.toString(), previousVersionBumped.toString(), releaseLenient) : Versions.compare(scheme, primeVersionBumped.toString(), previousVersionBumped.toString(), releasePrefix);
                    if (comparison <= 0) {
                        version = previousVersionBumped;
                        state().setBump(previousVersionBumpIdentifier);
                        state().getReleaseScope().getSignificantCommits().putAll(previousSignificantCommits);
                    }                        
                    else {
                        version = primeVersionBumped;
                        state().setBump(primeVersionBumpIdentifier);
                        state().getReleaseScope().getSignificantCommits().putAll(primeSignificantCommits);
                    }
                    logger.debug(COMMAND, "The greatest version between {} and {} is {}, which is the new (collapsed) version", primeVersionBumped, previousVersionBumped, version);
                }
                else {
                    if (Objects.isNull(previousVersionBumpIdentifier) || previousVersionBumpIdentifier.isBlank()) {
                        logger.info(COMMAND, "The release scope does not contain any significant commit since the previous version, version remains unchanged: {}", previousVersion.toString());
                        version = previousVersion;
                    }
                    else {
                        logger.debug(COMMAND, "Bumping component {} on version {}", previousVersionBumpIdentifier, previousVersion.toString());
                        version = previousVersion.bump(previousVersionBumpIdentifier);
                        state().setBump(previousVersionBumpIdentifier);
                        state().getReleaseScope().getSignificantCommits().putAll(previousSignificantCommits);
                    }
                }
            }

            // apply extra identifiers if there are significant commits
            if ((!Objects.isNull(previousVersionBumpIdentifier) && !previousVersionBumpIdentifier.isEmpty()) ||
                (collapseVersions && !Objects.isNull(primeVersionBumpIdentifier) && !primeVersionBumpIdentifier.isEmpty())) {
                // apply extra identifiers, if any has been configured for the release type
                if (Objects.isNull(releaseType.getIdentifiers()) || Objects.isNull(releaseType.getIdentifiers().getEnabled()) || releaseType.getIdentifiers().getEnabled().isEmpty()) {
                    logger.debug(COMMAND, "The release type does not define any (enabled) extra identifiers so none is applied");
                }
                else if (Objects.isNull(releaseType.getIdentifiers().getItems()) || releaseType.getIdentifiers().getItems().isEmpty())
                    throw new IllegalPropertyException(String.format("The release type has %d enabled extra identifiers (%s) but none is defined", releaseType.getIdentifiers().getEnabled().size(), String.join(", ", releaseType.getIdentifiers().getEnabled())));
                else {
                    logger.debug(COMMAND, "Applying {} extra identifiers defined by the release type to version {}", releaseType.getIdentifiers().getItems().size(), version.toString());

                    for (String identifierName: releaseType.getIdentifiers().getEnabled()) {
                        Identifier identifier = releaseType.getIdentifiers().getItem(identifierName);
                        if (Objects.isNull(identifier))
                            throw new IllegalPropertyException(String.format("The identifier %s is enabled but not defined", identifierName));

                        logger.debug(COMMAND, "Applying the {} extra identifier to version {}", identifierName, version.toString());
                        if (Objects.isNull(identifier.getQualifier()) || identifier.getQualifier().isBlank())
                            throw new IllegalPropertyException(String.format("The identifier %s must define a non blank qualifier", identifierName));
                        
                        String identifierQualifier = renderTemplate(identifier.getQualifier());
                        if (Objects.isNull(identifierQualifier) || identifierQualifier.isBlank())
                            throw new IllegalPropertyException(String.format("The identifier %s must evaluate to a non empty string. Configured value is '%s', rendered string is '%s'", identifierName, identifier.getQualifier(), identifierQualifier));
                        
                        String identifierValue = renderTemplate(identifier.getValue());
                        logger.debug(COMMAND, "The {} extra identifier is defined by qualifier='{}' and value='{}', which are resolved to qualifier='{}' and value='{}'", identifierName, identifier.getQualifier(), identifier.getValue(), identifierQualifier, identifierValue);

                        // Semver is the only supported scheme so far...
                        if (Scheme.SEMVER.equals(scheme)) {
                            SemanticVersion semanticVersion = SemanticVersion.valueOf(version.toString()); // faster and safer than casting...

                            if (IdentifierPosition.PRE_RELEASE.equals(identifier.getPosition())) {
                                // the value must be converted to an Integer when using SemVer and the pre-release part
                                Integer identifierValueAsInteger = null;

                                if (!(Objects.isNull(identifierValue) || identifierValue.isBlank())) {
                                    try {
                                        identifierValueAsInteger = Integer.valueOf(identifierValue);
                                    }
                                    catch (NumberFormatException nfe) {
                                        throw new IllegalPropertyException(String.format("Invalid integer value %s for Identifier %s. Semantic versioning requires integer numbers as values for identifiers in the pre release part.", identifierValue, identifierName), nfe);
                                    }
                                }

                                semanticVersion = semanticVersion.setPrereleaseAttribute(identifierQualifier, Objects.isNull(identifierValueAsInteger) ? null : identifierValueAsInteger);
                            }
                            else if (IdentifierPosition.BUILD.equals(identifier.getPosition()) || Objects.isNull(identifier.getPosition())) {
                                // BUILD is the default if no position is set
                                semanticVersion = semanticVersion.setBuildAttribute(identifierQualifier, (Objects.isNull(identifierValue) || identifierValue.isBlank()) ? null : identifierValue);
                            }
                            else throw new IllegalPropertyException(String.format("Illegal identifier position %s for identifier %s", identifier.getPosition(), identifierName));

                            version = semanticVersion;

                            logger.debug(COMMAND, "The version after applying the {} extra identifier is {}", identifierName, version.toString());
                        }
                        else throw new IllegalPropertyException(String.format("Extra identifiers are supported for %s scheme only", Scheme.SEMVER));
                    }
                }
            }

            // check if the version complies with version constraints, if any
            if (checkVersionRange(scheme, version, (Objects.isNull(releaseType.getVersionRange()) || releaseType.getVersionRange().isBlank()) ? null : releaseType.getVersionRange(), releaseType.getVersionRangeFromBranchName() ? repository().getCurrentBranch() : null))
                logger.debug(COMMAND, "Version {} succesfully passed range checks", version.toString());
            else logger.debug(COMMAND, "Version {} did not require version range checks", version.toString());

            logger.info(COMMAND, "Inferred version is: {}", version.toString());

            // store values to the state object
            state().setVersion(Objects.isNull(releasePrefix) ? version.toString() : releasePrefix.concat(version.toString()));
        }

        storeStatusInternalAttributes();
        return state();
    }

    /**
     * Checks if the given version complies with the version range. The version range can be expressed as a static
     * regular expression template or can be computed dynamically from the branch name.
     * <br>
     * If the {@code staticVersionRangeExpressionTemplate} is not {@code null} then it will be used as a static
     * regular expression. The string may be a template which is first rendered using the current state as the
     * context and then used as a regular expression to match the given {@code version}. Using it as a template
     * allows to make it dynamic and use state values (like the release prefix, for example).
     * <br>
     * If the {@code staticVersionRangeExpressionTemplate} is {@code null} then the {@code branch} is considered.
     * If also the {@code branch} is {@code null} then no check is performed, otherwise the branch name is
     * used to infer a dynamic regular expression that will be used to match the version.
     * <br>
     * If the check is required (at least one between {@code staticVersionRangeExpressionTemplate} and {@code branch}
     * is not {@code null}) and succeeds returns {@code true}, if it's not required (both
     * {@code staticVersionRangeExpressionTemplate} and {@code branch} are {@code null}) returns {@code false},
     * otherwise if it is required and does not succeed a {@link ReleaseException} is thrown.
     * <br>
     * When using dynamic range checks the current branch is parsed trying to be as tolerant as possible, just finding
     * some pattern like {@code major[.minor[.patch]]} anywhere within the string. Each one of these identifiers can be
     * a fixed number or an {@code x}, which acts as a wildcard. For example:<br>
     * - 1.x: means that only version with major number 1 are accepted, while the minor and patch numbers can be anything<br>
     * - x.2.x: means that any major and patch numbers are allowed, while the minor number can be any valid number<br>
     * - rel/v1.2.3: is tolerated but the 'rel/v' is just ignored, while the version number can only be 1.2.3<br>
     * - v1.x-abc.123+def.456: tolerates any pre-release and build parts and the 'v' prefix, while the major number must
     *   be 1 (while the minor and patch can be anything)<br>
     * <br>
     * When this method returns the {@link State#getVersionRange() version range} attribute is set on the current
     * {@link State} instance.
     * 
     * @param scheme the versioning scheme in use. It can be {@code null} only when the check is not required or when
     * it has to be statically performed (in other words only when also {@code branch} is {@code null})
     * @param version the version to check. It can't be {@code null}
     * @param staticVersionRangeExpressionTemplate the optional template that, once resolved, is used to check the
     * given version. If {@code null} the check is performed dynamically by inferring the version from the name of
     * the branch (when {@code branch} is not {@code null}) or not performed at all (when {@code branch} is also
     * {@code null})
     * @param branch the name of the branch used to infer the version range regular expression from. This is ignored
     * when {@code staticVersionRangeExpressionTemplate} is not {@code null} as static checking has priority over
     * dynamic checking. When {@code staticVersionRangeExpressionTemplate} is {@code null}, if {@code branch} is
     * also null then no check is performed, otherwise the regular expression is inferred from the branch name.
     * 
     * @return {@code true} if the check was succesfully performed, {@code false} if no check was performed
     * 
     * @throws DataAccessException in case the configuration can't be loaded for some reason.
     * @throws IllegalPropertyException in case the configuration has some illegal options.
     * @throws GitException in case of unexpected issues when accessing the Git repository.
     * @throws ReleaseException if the task is unable to complete for reasons due to the release process.
     */
    private boolean checkVersionRange(Scheme scheme, Version version, String staticVersionRangeExpressionTemplate, String branch)
        throws DataAccessException, IllegalPropertyException, GitException, ReleaseException {
        Objects.requireNonNull(version);

        // determine which regular expression to use, if static or inferred by the branch name
        if (Objects.isNull(staticVersionRangeExpressionTemplate) || staticVersionRangeExpressionTemplate.isBlank()) {
            if (Objects.isNull(branch) || branch.isBlank()) {
                logger.debug(COMMAND, "No version range check is required");
                state().setVersionRange(null);
            }
            else {
                Objects.requireNonNull(scheme);
                // infer the expression dynamically from the branch name
                logger.debug(COMMAND, "The version range check is required with the regular expression inferred by the branch name");

                // Semver is the only supported scheme so far...
                if (Scheme.SEMVER.equals(scheme)) {
                    logger.debug(COMMAND, "Scanning the branch name {} searching for a version range pattern", branch);

                    Matcher m = Pattern.compile(SEMVER_DYNAMIC_VERSION_RANGE_FROM_BRANCH_NAME_REGEX).matcher(branch);
                    String major = null;
                    String minor = null;
                    String patch = null;
                    try {
                        m.find();
                        major = m.group("major");
                        minor = m.group("minor");
                        patch = m.group("patch");
                    }
                    catch (IllegalStateException | IllegalArgumentException ie) {
                        throw new ReleaseException(String.format("Branch name '%s' doesn't seem to contain a parseable version range", branch));
                    }

                    logger.debug(COMMAND, "Building the dynamic version range regular expression with constraints inferred by {}", branch);

                    StringBuilder dynamicVersionRangeExpression = new StringBuilder("^");

                    // Where named groups are not defined or wildcarded allow any positive integer in that position,
                    // otherwise just bring the number that came from the branch name.
                    // Consider that these named groups can only be positive integers or 'x' to be matched by the
                    // regular expression above.
                    if (Objects.isNull(major) || major.isBlank() || "x".equalsIgnoreCase(major))
                        dynamicVersionRangeExpression.append("(0|[1-9]\\d*)");
                    else dynamicVersionRangeExpression.append(major);
                    dynamicVersionRangeExpression.append("\\.");
                    if (Objects.isNull(minor) || minor.isBlank() || "x".equalsIgnoreCase(minor))
                        dynamicVersionRangeExpression.append("(0|[1-9]\\d*)");
                    else dynamicVersionRangeExpression.append(minor);
                    dynamicVersionRangeExpression.append("\\.");
                    if (Objects.isNull(patch) || patch.isBlank() || "x".equalsIgnoreCase(patch))
                        dynamicVersionRangeExpression.append("(0|[1-9]\\d*)");
                    else dynamicVersionRangeExpression.append(patch);

                    // in order to tolerate any pre-release of build parts, let's finish the expression with a non capturing group
                    // that accepts anything after a '-' or '+'
                    dynamicVersionRangeExpression.append("(?:(?:-|\\+).*)?$");

                    logger.debug(COMMAND, "The dynamic version range regular expression that was built from {} is '{}'", branch, dynamicVersionRangeExpression.toString());
                    // now we have the dynamically built regular expression
                    state().setVersionRange(dynamicVersionRangeExpression.toString());
                }
                else throw new IllegalPropertyException(String.format("Version range check is supported for %s scheme only", Scheme.SEMVER));                
            }
        }
        else {
            // use the statically configured expression
            logger.debug(COMMAND, "The version range check is required using a static regular expression");
            String versionRangeRegExp = renderTemplate(staticVersionRangeExpressionTemplate);

            logger.debug(COMMAND, "The configured regular expression template used for version range checks is '{}', which evaluates to '{}'", staticVersionRangeExpressionTemplate, versionRangeRegExp);

            state().setVersionRange(versionRangeRegExp);
        }

        if (state().hasVersionRange()) {
            logger.debug(COMMAND, "Performing version range check against version {} using the expression '{}'", version.toString(), state().getVersionRange());
            try {
                if (Pattern.matches(state().getVersionRange(), version.toString())) {
                    logger.debug(COMMAND, "Version {} succesfully matches version range pattern {}", version.toString(), state().getVersionRange());
                    return true;
                }
                else {
                    throw new ReleaseException(String.format("Version %s doesn't match version range pattern %s", version.toString(), state().getVersionRange()));
                }
            }
            catch (PatternSyntaxException pse) {
                throw new IllegalPropertyException(String.format("Cannot compile regular expression '%s' (evaluated by template %s) ", state().getVersionRange(), staticVersionRangeExpressionTemplate), pse);
            }
        }
        else {
            logger.debug(COMMAND, "No version range check is performed");
            return false;
        }
    }
}
