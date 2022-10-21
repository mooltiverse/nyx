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

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.Reader;
import java.io.StringWriter;
import java.text.SimpleDateFormat;
import java.time.ZoneOffset;
import java.util.Date;
import java.util.Map;
import java.util.Objects;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mooltiverse.oss.nyx.ReleaseException;
import com.mooltiverse.oss.nyx.entities.Changelog;
import com.mooltiverse.oss.nyx.entities.CommitMessageConvention;
import com.mooltiverse.oss.nyx.entities.IllegalPropertyException;
import com.mooltiverse.oss.nyx.entities.git.Commit;
import com.mooltiverse.oss.nyx.git.GitException;
import com.mooltiverse.oss.nyx.git.Repository;
import com.mooltiverse.oss.nyx.io.DataAccessException;
import com.mooltiverse.oss.nyx.state.State;
import com.mooltiverse.oss.nyx.template.Templates;

/**
 * The Make command takes care of building the release artifacts.
 * 
 * This class is not meant to be used in multi-threaded environments.
 */
public class Make extends AbstractCommand {
    /**
     * The name of the resource to load for the default template. Value: {@value}.
     */
    public static final String DEFAULT_TEMPLATE_RESOURCE_NAME = "changelog.tpl";

    /**
     * The private logger instance
     */
    private static final Logger logger = LoggerFactory.getLogger(Make.class);

    /**
     * The common prefix used for all the internal state attributes managed by this class.
     */
    private static final String INTERNAL_ATTRIBUTE_PREFIX = "make";

    /**
     * The common prefix used for all the internal state attributes managed by this class, representing an input.
     */
    private static final String INTERNAL_INPUT_ATTRIBUTE_PREFIX = INTERNAL_ATTRIBUTE_PREFIX.concat(".").concat("input");

    /**
     * The name used for the internal state attribute where we store the path to the changelog file.
     */
    private static final String INTERNAL_INPUT_ATTRIBUTE_CHANGELOG_FILE = INTERNAL_INPUT_ATTRIBUTE_PREFIX.concat(".").concat("changelog").concat(".").concat("file");

    /**
     * The name used for the internal state attribute where we store current branch name.
     */
    private static final String INTERNAL_INPUT_ATTRIBUTE_REPOSITORY_CURRENT_BRANCH = INTERNAL_INPUT_ATTRIBUTE_PREFIX.concat(".").concat("repository").concat(".").concat("current").concat(".").concat("branch");

    /**
     * The name used for the internal state attribute where we store the SHA-1 of the last
     * commit in the current branch by the time this command was last executed.
     */
    private static final String INTERNAL_INPUT_ATTRIBUTE_REPOSITORY_LAST_COMMIT = INTERNAL_INPUT_ATTRIBUTE_PREFIX.concat(".").concat("repository").concat(".").concat("last").concat(".").concat("commit");

    /**
     * The name used for the internal state attribute where we store the initial commit.
     */
    private static final String INTERNAL_INPUT_ATTRIBUTE_STATE_INITIAL_COMMIT = INTERNAL_INPUT_ATTRIBUTE_PREFIX.concat(".").concat("state").concat(".").concat("initialCommit");

    /**
     * The flag telling if the current version is new.
     */
    private static final String INTERNAL_INPUT_ATTRIBUTE_STATE_NEW_VERSION = INTERNAL_INPUT_ATTRIBUTE_PREFIX.concat(".").concat("state").concat(".").concat("newVersion");

    /**
     * The name used for the internal state attribute where we store the version.
     */
    private static final String INTERNAL_INPUT_ATTRIBUTE_VERSION = INTERNAL_INPUT_ATTRIBUTE_PREFIX.concat(".").concat("version");

    /**
     * Standard constructor.
     * 
     * @param state the state reference
     * @param repository the repository reference
     * 
     * @throws NullPointerException if a given argument is {@code null}
     */
    public Make(State state, Repository repository) {
        super(state, repository);
        logger.debug(COMMAND, "New Make command object");
    }

    /**
     * Returns a reader object that reads the template to be used for rendering.
     * If the configuretion overrides the template then the reader will point to that
     * template otherwise the default template will be returned.
     * 
     * @return a reader object that reads the template to be used for rendering. Never {@code null}.
     * 
     * @throws DataAccessException if an exception occurs when reading the template.
     * @throws IllegalPropertyException in case the configuration has some illegal options.
     */
    private Reader getChangelogTemplateReader()
        throws DataAccessException, IllegalPropertyException {
        if (Objects.isNull(state().getConfiguration().getChangelog().getTemplate()) || state().getConfiguration().getChangelog().getTemplate().isBlank()) {
            logger.debug(COMMAND, "The changelog template has not been overridden by configuration. Loading the default template.");
            try {
                InputStream is = getClass().getClassLoader().getResourceAsStream(DEFAULT_TEMPLATE_RESOURCE_NAME);
                if (Objects.isNull(is))
                    throw new DataAccessException(String.format("Unable to load the default changelog template resource from '%s'", DEFAULT_TEMPLATE_RESOURCE_NAME));
                else return new InputStreamReader(is);
            }
            catch (SecurityException se) {
                throw new DataAccessException(String.format("Unable to load the default changelog template resource from '%s'", DEFAULT_TEMPLATE_RESOURCE_NAME), se);
            }
        }
        else {
            try {
                return new FileReader(state().getConfiguration().getChangelog().getTemplate());
            }
            catch (FileNotFoundException fnfe) {
                throw new DataAccessException(String.format("Unable to load the configured changelog template file from '%s'", state().getConfiguration().getChangelog().getTemplate()), fnfe);
            }
        }
    }

    /**
     * Builds the changelog assets.
     * 
     * @throws DataAccessException in case the configuration can't be loaded for some reason.
     * @throws IllegalPropertyException in case the configuration has some illegal options.
     * @throws GitException in case of unexpected issues when accessing the Git repository.
     * @throws ReleaseException if the task is unable to complete for reasons due to the release process.
     */
    private void buildChangelog()
        throws DataAccessException, IllegalPropertyException, GitException, ReleaseException {
        // The destination path is also used as a flag to enable or disable the changelog generation, so if it's not configured the changelog is not generated
        File changelogFile = getChangelogFile();

        if (Objects.isNull(changelogFile))
            logger.debug(COMMAND, "Changelog has not been configured or it has no destination path. Skipping the changelog generation.");
        else if (!state().getNewVersion())
            logger.debug(COMMAND, "No new version has been inferred so the changelog generation will be skipped.");
        else {
            logger.debug(COMMAND, "Building the changelog data model");

            Changelog changelog = new Changelog();
            state().setChangelog(changelog);

            // As of this version we can just pick the commits from the current release scope but in future versions,
            // when we need to have the 'Unreleased' fictional release plus, optionally, older releases,
            // (as per https://github.com/mooltiverse/nyx/issues/79) we'll need to walk the commit history just like the
            // Infer command does.

            // Create the timestamp string
            Date date = new Date(Long.valueOf(state().getTimestamp()).longValue());
            SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
            dateFormat.setTimeZone(TimeZone.getTimeZone(ZoneOffset.UTC));
            String dateString = dateFormat.format(date);

            // As of now we just have one release: the one being issued
            Changelog.Release release = new Changelog.Release(state().getVersion(), dateString);
            changelog.getReleases().add(release);

            // As of now we can just pick the commits from the current release scope.
            // We just need to distribute the commits among sections, which means filtering and translating
            // the sections if the user has configured them, or just use the commit 'type's as section names
            // if the user didn't map the sections
            for (Commit commit: state().getReleaseScope().getCommits()) {
                // Now we need to infer the commit type by using the commit message conventions
                String commitType = null;
                if (!Objects.isNull(state().getConfiguration().getCommitMessageConventions().getItems())) {
                    logger.debug(COMMAND, "Trying to infer the commit type based on the commit message of commit '{}'", commit.getSHA());
                    for (Map.Entry<String,CommitMessageConvention> cmcEntry: state().getConfiguration().getCommitMessageConventions().getItems().entrySet()) {
                        logger.debug(COMMAND, "Evaluating commit '{}' against message convention '{}'", commit.getSHA(), cmcEntry.getKey());                                
                        Matcher messageMatcher = Pattern.compile(cmcEntry.getValue().getExpression()).matcher(commit.getMessage().getFullMessage());
                        try {
                            if (messageMatcher.find()) {
                                commitType = messageMatcher.group("type");
                                logger.debug(COMMAND, "The type of commit '{}' is '{}'", commit.getSHA(), commitType);
                            }
                        }
                        catch (IllegalArgumentException iae) {
                            throw new IllegalPropertyException(String.format("The regular expression '%s' defined for commit message convention '%s' does not define the 'type' named capturing group", cmcEntry.getValue().getExpression(), cmcEntry.getKey()), iae);
                        }
                        catch (IllegalStateException ise) {
                            throw new ReleaseException(String.format("Cannot infer the commit type for commit '%s' match operation failed for the regular expression '%s' from commit message convention '%s'", commit.getSHA(), cmcEntry.getValue().getExpression(), cmcEntry.getKey()), ise);
                        }
                        if (Objects.isNull(commitType))
                            logger.debug(COMMAND, "The commit type cannot be inferred for commit '{}' using the regular expression '{}' from commit message convention '{}'", commit.getSHA(), cmcEntry.getValue().getExpression(), cmcEntry.getKey());
                    }
                }
                if (Objects.isNull(commitType) || commitType.isBlank())
                    logger.debug(COMMAND, "No commit message convention has been configured or the configured commit message conventions do not allow to infer the 'type' for commit '{}'. The commit will not appear in the changelog.", commit.getSHA());
                else {
                    // If the user has defined some sections mapping we need to map the commit type to those sections,
                    // otherwise the section will be the commit type
                    if (Objects.isNull(state().getConfiguration().getChangelog().getSections()) || state().getConfiguration().getChangelog().getSections().isEmpty()) {
                        logger.debug(COMMAND, "Changelog sections haven't been defined by user. Commit '{}' will appear in section '{}' (same as the commit type)", commit.getSHA(), commitType);
                        release.getSection(commitType, true).getCommits().add(commit);
                    }
                    else {
                        for (Map.Entry<String,String> sectionEntry: state().getConfiguration().getChangelog().getSections().entrySet()) {
                            logger.debug(COMMAND, "Evaluating commit type '{}' against changelog section '{}'", commitType, sectionEntry.getKey());
                            if (Pattern.matches(sectionEntry.getValue(), commitType)) {
                                logger.debug(COMMAND, "Expression '{}' for section '{}' successfully matches type '{}' so commit '{}' will appear under the '{}' section", sectionEntry.getValue(), sectionEntry.getKey(), commitType, commit.getSHA(), sectionEntry.getKey());
                                release.getSection(sectionEntry.getKey(), true).getCommits().add(commit);
                                break;
                            }
                            else {
                                logger.debug(COMMAND, "Expression '{}' for section '{}' does not match type '{}'. Trying with next sections, if any.", sectionEntry.getValue(), sectionEntry.getKey(), commitType);
                                continue;
                            }
                        }
                    }
                }
            }

            logger.debug(COMMAND, "Rendering the changelog");
            if (state().getConfiguration().getDryRun()) {
                logger.info(COMMAND, "Changelog rendering skipped due to dry run");
            }
            else {
                try {
                    StringWriter stringWriter = new StringWriter();
                    Templates.render(getChangelogTemplateReader(), changelog, stringWriter);
                    stringWriter.flush();
                    stringWriter.close();

                    String changelogBuffer = stringWriter.toString();

                    // if substitutions have been defined, let's apply them
                    if (!Objects.isNull(state().getConfiguration().getChangelog().getSubstitutions())) {
                        logger.debug(COMMAND, "Applying configured substitutions to the changelog");
                        for (Map.Entry<String,String> substitutionEntry: state().getConfiguration().getChangelog().getSubstitutions().entrySet()) {
                            Pattern substitutionPattern = Pattern.compile(substitutionEntry.getKey());
                            Matcher substitutionMatcher = substitutionPattern.matcher(changelogBuffer);
                            StringBuffer buffer = new StringBuffer();
                            while (substitutionMatcher.find()) {
                                String substitutionValue = substitutionMatcher.group(1); // by the docs, only the group 1 is considered
                                substitutionMatcher.appendReplacement(buffer, String.format(substitutionEntry.getValue(), substitutionValue, substitutionValue, substitutionValue, substitutionValue, substitutionValue)); // by the docs, the %s placeholder can be used up to 5 times
                            }
                            substitutionMatcher.appendTail(buffer);
                            changelogBuffer = buffer.toString();
                        }
                        logger.debug(COMMAND, "Configured substitutions have been applied to the changelog");
                    }

                    // now write to the actual destination file
                    FileWriter fileWriter = new FileWriter(changelogFile);
                    fileWriter.write(changelogBuffer);
                    fileWriter.flush();
                    fileWriter.close();
                }
                catch (IOException ioe) {
                    throw new DataAccessException(String.format("Unable to render the changelog to file '%s'. Make sure the path to the file exists and can be written.", changelogFile.getAbsolutePath()), ioe);
                }

                logger.debug(COMMAND, "The changelog has been saved to '{}'", changelogFile.getAbsolutePath());
            }
        }
    }

    /**
     * Builds the configured assets.
     * 
     * @throws DataAccessException in case the configuration can't be loaded for some reason.
     * @throws IllegalPropertyException in case the configuration has some illegal options.
     * @throws GitException in case of unexpected issues when accessing the Git repository.
     * @throws ReleaseException if the task is unable to complete for reasons due to the release process.
     */
    private void buildAssets()
        throws DataAccessException, IllegalPropertyException, GitException, ReleaseException {
        // The only asset to build is the changelog
        buildChangelog();
    }

    /**
     * This method stores the state internal attributes used for up-to-date checks so that subsequent invocations
     * of the {@link #isUpToDate()} method can find them and determine if the command is already up to date.
     * 
     * This method is meant to be invoked at the end of a successful {@link #run()}.
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
        logger.debug(COMMAND, "Storing the Make command internal attributes to the State");
        if (!state().getConfiguration().getDryRun()) {
            File changelogFile = getChangelogFile();
            putInternalAttribute(INTERNAL_INPUT_ATTRIBUTE_CHANGELOG_FILE, Objects.isNull(changelogFile) ? "null" : changelogFile.getAbsolutePath());
            putInternalAttribute(INTERNAL_INPUT_ATTRIBUTE_REPOSITORY_CURRENT_BRANCH, getCurrentBranch());
            putInternalAttribute(INTERNAL_INPUT_ATTRIBUTE_REPOSITORY_LAST_COMMIT, getLatestCommit());
            putInternalAttribute(INTERNAL_INPUT_ATTRIBUTE_STATE_INITIAL_COMMIT, state().getReleaseScope().getInitialCommit());
            putInternalAttribute(INTERNAL_INPUT_ATTRIBUTE_STATE_NEW_VERSION, state().getNewVersion());
            putInternalAttribute(INTERNAL_INPUT_ATTRIBUTE_VERSION, state().getVersion());
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isUpToDate()
        throws DataAccessException, IllegalPropertyException, GitException {
        logger.debug(COMMAND, "Checking whether the Make command is up to date");
        // Never up to date if this command hasn't stored a version yet into the state
        if (Objects.isNull(state().getVersion())) {
            logger.debug(COMMAND, "The Make command is not up to date because the internal state has no version yet");
            return false;
        }

        File changelogFile = getChangelogFile();
        if (!Objects.isNull(changelogFile)) {
            // The command is never considered up to date when the configuration requires a changelog file but the state has no such object reference
            if (Objects.isNull(state().getChangelog())) {
                logger.debug(COMMAND, "The Make command is not up to date because a changelog file has been configured ('{}') but the internal state has no changelog yet", changelogFile.getAbsolutePath());
                return false;
            }
            // The command is never considered up to date when the changelog file hasn't been saved yet or it has changed
            if (!isInternalAttributeUpToDate(INTERNAL_INPUT_ATTRIBUTE_CHANGELOG_FILE, changelogFile.getAbsolutePath())) {
                logger.debug(COMMAND, "The Make command is not up to date because a changelog file has been configured ('{}') but the configured path has changed", changelogFile.getAbsolutePath());
                return false;
            }
            if (!changelogFile.exists()) {
                logger.debug(COMMAND, "The Make command is not up to date because a changelog file has been configured ('{}') but the file does not exist", changelogFile.getAbsolutePath());
                return false;
            }
        }

        // The command is never considered up to date when the repository branch or last commit has changed
        if ((!isInternalAttributeUpToDate(INTERNAL_INPUT_ATTRIBUTE_REPOSITORY_CURRENT_BRANCH, getCurrentBranch())) || (!isInternalAttributeUpToDate(INTERNAL_INPUT_ATTRIBUTE_REPOSITORY_LAST_COMMIT, getLatestCommit())) || (!isInternalAttributeUpToDate(INTERNAL_INPUT_ATTRIBUTE_STATE_INITIAL_COMMIT, state().getReleaseScope().getInitialCommit()))) {
            logger.debug(COMMAND, "The Make command is not up to date because the range of commits or the current branch has changed");
            return false;
        }

        // Check if configuration parameters have changed
        boolean res = isInternalAttributeUpToDate(INTERNAL_INPUT_ATTRIBUTE_VERSION, state().getVersion()) &&
            isInternalAttributeUpToDate(INTERNAL_INPUT_ATTRIBUTE_STATE_NEW_VERSION, state().getNewVersion());
        if (res) {
            logger.debug(COMMAND, "The Make command is up to date");
        }
        else {
            logger.debug(COMMAND, "The Make command is not up to date because the configuration or the internal state has changed");
        }
        return res;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public State run()
        throws DataAccessException, IllegalPropertyException, GitException, ReleaseException {
        logger.debug(COMMAND, "Running the Make command...");

        buildAssets();

        storeStatusInternalAttributes();
        return state();
    }
}