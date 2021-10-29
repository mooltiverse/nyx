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

import java.io.IOException;

import java.util.Objects;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mooltiverse.oss.nyx.ReleaseException;
import com.mooltiverse.oss.nyx.data.DataAccessException;
import com.mooltiverse.oss.nyx.data.IllegalPropertyException;
import com.mooltiverse.oss.nyx.data.ReleaseType;
import com.mooltiverse.oss.nyx.data.WorkspaceStatus;
import com.mooltiverse.oss.nyx.git.GitException;
import com.mooltiverse.oss.nyx.git.Repository;
import com.mooltiverse.oss.nyx.state.State;
import com.mooltiverse.oss.nyx.template.Templates;

/**
 * The common superclass for Nyx commands.
 * 
 * This class is not meant to be used in multi-threaded environments.
 * 
 * All implementing classes must have a public constructor that accept a {@link State} and a {@link Repository} parameter.
 */
public abstract class AbstractCommand implements Command {
    /**
     * The private logger instance
     */
    private static final Logger logger = LoggerFactory.getLogger(AbstractCommand.class);

    /**
     * The private instance of the Git repository.
     */
    private final Repository repository;

    /**
     * The private instance of the state.
     */
    private final State state;

    /**
     * Standard constructor.
     * 
     * @param state the state reference
     * @param repository the repository reference
     * 
     * @throws NullPointerException if a given argument is {@code null}
     */
    protected AbstractCommand(State state, Repository repository) {
        super();
        Objects.requireNonNull(state, "The State object cannot be null");
        Objects.requireNonNull(repository, "The Repository object cannot be null");
        this.state = state;
        this.repository = repository;
        logger.debug(COMMAND, "New command object");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public final State state() {
        return state;
    }

    /**
     * Returns the repository object.
     * 
     * @return the repository object.
     */
    public final Repository repository() {
        return repository;
    }

    /**
     * Returns the name of the current branch or a commit SHA-1 if the repository is in the detached head state.
     * 
     * @return the name of the current branch or a commit SHA-1 if the repository is in the detached head state.
     * 
     * @throws GitException in case of unexpected issues when accessing the Git repository.
     * 
     * @see #repository()
     */
    protected String getCurrentBranch()
        throws GitException {
        try {
            return repository().getCurrentBranch();
        }
        catch (GitException ge) {
            throw new GitException(ge);
        }
    }

    /**
     * Returns the SHA-1 identifier of the last commit in the current branch.
     * 
     * @return the SHA-1 identifier of the last commit in the current branch or {@code code} if the repository has no commits yet.
     * 
     * @throws GitException in case of unexpected issues when accessing the Git repository.
     * 
     * @see #repository()
     */
    protected String getLatestCommit()
        throws GitException {
        try {
            return repository().getLatestCommit();
        }
        catch (GitException ge) {
            throw new GitException(ge);
        }
    }

    /**
     * Returns {@code true} if the repository is in a clean state (no uncommitted changes).
     * 
     * @return {@code true} if the repository is in a clean state (no uncommitted changes).
     * 
     * @throws GitException in case of unexpected issues when accessing the Git repository.
     * 
     * @see #repository()
     */
    protected boolean isRepositoryClean()
        throws GitException {
        try {
            return repository().isClean();
        }
        catch (GitException ge) {
            throw new GitException(ge);
        }
    }

    /**
     * Returns {@code true} if the internal attributes map contains an attribute with the given name and its value
     * equals the given expected value. The comparison is always performed using the {@link Object#toString()} method
     * of the expected value.
     * 
     * @param attributeName the name of the attribute to check. It can't be {@code null}
     * @param expectedValue the expected value of the attribute. It may be {@code null}
     * 
     * @return {@code true} if the internal attributes map contains an attribute with the given name and its value
     * equals the given expected value.
     */
    protected boolean isInternalAttributeUpToDate(String attributeName, Object expectedValue) {
        return !Objects.isNull(state().getInternals().get(attributeName)) && state().getInternals().get(attributeName).equals(Objects.isNull(expectedValue) ? "null" : expectedValue.toString());
    }

    /**
     * Retrieves the attribute with the given name from the internal attributes map. The returned value is always
     * the {@link Object#toString()} representation of the stored value.
     * 
     * @param attributeName the name of the attribute to store. It can't be {@code null}
     * 
     * @return the value of the attribute, if available, otherwise {@code null}
     */
    protected String getInternalAttribute(String attributeName) {
        return state().getInternals().get(attributeName);
    }

    /**
     * Stores the attribute with the given name to the internal attributes map. The stored value is always
     * performed the {@link Object#toString()} of the given value.
     * 
     * @param attributeName the name of the attribute to store. It can't be {@code null}
     * @param attributeValue the value of the attribute. It may be {@code null}
     */
    protected void putInternalAttribute(String attributeName, Object attributeValue) {
        state().getInternals().put(attributeName, Objects.isNull(attributeValue) ? "null" : attributeValue.toString());
    }

    /**
     * Renders the given template using the internal {@link State} object as the context.
     * 
     * @param template the string template to render.
     * 
     * @return the rendered template. It is {@code null} if the given template is {@code null}.
     * 
     * @throws IllegalPropertyException in case the given template can't be rendered.
     */
    protected String renderTemplate(String template)
        throws IllegalPropertyException {
        if (Objects.isNull(template))
            return null;
        else if (template.isBlank())
            return template;
        else try {
            return Templates.render(template, state());
        }
        catch (IOException ioe) {
            throw new IllegalPropertyException(String.format("Template '%s' cannot be rendered using the current state", template));
        }
    }

    /**
     * Renders the given template using the internal {@link State} object as the context, returning
     * the boolean value, according to {@link Templates#toBoolean(String)}.
     * 
     * @param template the string template to render.
     * 
     * @return the rendered template as boolean. It is {@code false} if the given template is {@code null}.
     * 
     * @throws IllegalPropertyException in case the given template can't be rendered.
     */
    protected Boolean renderTemplateAsBoolean(String template)
        throws IllegalPropertyException {
        return Templates.toBoolean(renderTemplate(template));
    }

    /**
     * Renders the given template using the internal {@link State} object as the context, returning
     * the integer value, according to {@link Templates#toInteger(String)}.
     * 
     * @param template the string template to render.
     * 
     * @return the rendered template as integer. It is {@code 0} if the given template is {@code null}.
     * 
     * @throws IllegalPropertyException in case the given template can't be rendered.
     */
    protected Integer renderTemplateAsInteger(String template)
        throws IllegalPropertyException {
        return Templates.toInteger(renderTemplate(template));
    }

    /**
     * Selects the right release type among those configured based on their matching attributes.
     * 
     * @return the resolved release type. Never {@code null}.
     * 
     * @throws DataAccessException in case the configuration can't be loaded for some reason.
     * @throws IllegalPropertyException in case the configuration has some illegal options.
     * @throws GitException in case of unexpected issues when accessing the Git repository.
     * @throws ReleaseException if the task is unable to complete for reasons due to the release process.
     */
    protected ReleaseType resolveReleaseType()
        throws DataAccessException, IllegalPropertyException, GitException, ReleaseException {

        if (Objects.isNull(state().getConfiguration().getReleaseTypes()) || Objects.isNull(state().getConfiguration().getReleaseTypes().getEnabled()) || state().getConfiguration().getReleaseTypes().getEnabled().isEmpty())
            throw new ReleaseException("No release types have been configured. Please configure them using the releaseTypes option.");

        logger.debug(COMMAND, "Resolving the release type among enabled ones: '{}'", String.join(", ", state().getConfiguration().getReleaseTypes().getEnabled()));
        for (String releaseTypeName: state().getConfiguration().getReleaseTypes().getEnabled()) {
            logger.debug(COMMAND, "Evaluating release type: '{}'", releaseTypeName);
            ReleaseType releaseType = state().getConfiguration().getReleaseTypes().getItem(releaseTypeName);

            if (Objects.isNull(releaseType))
                throw new ReleaseException(String.format("Release type '%s' is configured among enabled ones but is not configured", releaseTypeName));
            
            // evaluate the matching criteria: branch name
            if (Objects.isNull(releaseType.getMatchBranches()) || releaseType.getMatchBranches().isBlank())
                logger.debug(COMMAND, "Release type '{}' does not specify any branch name requirement", releaseTypeName);
            else {
                String matchBranchesRendered = renderTemplate(releaseType.getMatchBranches());
                if (Objects.isNull(matchBranchesRendered) || matchBranchesRendered.isBlank())
                    logger.debug(COMMAND, "Release type '{}' specifies a match branches template '{}' that evaluates to an empty regular expression", releaseTypeName, releaseType.getMatchBranches());
                else {
                    logger.debug(COMMAND, "Release type '{}' specifies a match branches template '{}' that evaluates to regular expression: '{}'", releaseTypeName, releaseType.getMatchBranches(), matchBranchesRendered);
                    try {
                        if (Pattern.matches(matchBranchesRendered, getCurrentBranch()))
                            logger.debug(COMMAND, "Current branch '{}' succesfully matched by release type '{}' matchBranches regular expression '{}'", getCurrentBranch(), releaseTypeName, matchBranchesRendered);
                        else {
                            logger.debug(COMMAND, "Current branch '{}' not matched by release type '{}' matchBranches regular expression '{}'. Skipping release type '{}'", getCurrentBranch(), releaseTypeName, matchBranchesRendered, releaseTypeName);
                            continue;
                        }
                    }
                    catch (PatternSyntaxException pse) {
                        throw new IllegalPropertyException(String.format("Release type '%s' has a malformed matchBranches regular expression: '%s' (was '%s' before rendering the template rendering)", releaseTypeName, matchBranchesRendered, releaseType.getMatchBranches()), pse);
                    }
                }
            }

            // evaluate the matching criteria: environment variables
            if (Objects.isNull(releaseType.getMatchEnvironmentVariables()) || releaseType.getMatchEnvironmentVariables().isEmpty())
                logger.debug(COMMAND, "Release type '{}' does not specify any environment variable requirement", releaseTypeName);
            else {
                boolean mismatch = false;
                for (String varName: releaseType.getMatchEnvironmentVariables().keySet()) {
                    logger.debug(COMMAND, "Evaluating environment variable '{}' as required by release type '{}'", varName, releaseTypeName);

                    String varValue = System.getenv(varName);

                    if (Objects.isNull(varValue)) {
                        logger.debug(COMMAND, "Environment variable '{}' is required by release type '{}' but is not defined in the current environment. Skipping release type '{}'", varName, releaseTypeName, releaseTypeName);
                        mismatch = true;
                        continue;
                    }

                    String varVarueRegExp = releaseType.getMatchEnvironmentVariables().get(varName);
                    try {
                        if (Objects.isNull(varVarueRegExp) || varVarueRegExp.isBlank() || Pattern.matches(varVarueRegExp, varValue))
                            logger.debug(COMMAND, "Environment variable '{}' value succesfully matched by release type '{}' regular expression '{}'", varName, releaseTypeName, varVarueRegExp);
                        else {
                            logger.debug(COMMAND, "Environment variable '{}' value not matched by release type '{}' regular expression '{}'", varName, releaseTypeName, varVarueRegExp);
                            mismatch = true;
                            continue;
                        }
                    }
                    catch (PatternSyntaxException pse) {
                        throw new IllegalPropertyException(String.format("Release type '%s' has a malformed environment variable regular expression '%s' to match for environment variable '%s'", releaseTypeName, varVarueRegExp, varName), pse);
                    }
                }

                if (mismatch) {
                    logger.debug(COMMAND, "Environment variables not matched by release type '{}'", releaseTypeName);
                    continue;
                }
            }

            // evaluate the matching criteria: workspace status
            if (Objects.isNull(releaseType.getMatchWorkspaceStatus()))
                logger.debug(COMMAND, "Release type '{}' does not specify any workspace status requirement", releaseTypeName);
            else {
                if ((WorkspaceStatus.CLEAN.equals(releaseType.getMatchWorkspaceStatus()) && isRepositoryClean()) || (WorkspaceStatus.DIRTY.equals(releaseType.getMatchWorkspaceStatus()) && (!isRepositoryClean())))
                    logger.debug(COMMAND, "Current repository status '{}' succesfully matched by release type '{}' matchWorkspaceStatus filter '{}'", isRepositoryClean() ? "CLEAN" : "DIRTY", releaseTypeName, releaseType.getMatchWorkspaceStatus().toString());
                else {
                    logger.debug(COMMAND, "Current repository status '{}' not matched by release type '{}' matchWorkspaceStatus filter '{}'. Skipping release type '{}'", isRepositoryClean() ? "CLEAN" : "DIRTY", releaseTypeName, releaseType.getMatchWorkspaceStatus().toString(), releaseTypeName);
                    continue;
                }
            }
            
            // if we reached this point the release type matches all of the filters so it can be returned
            logger.debug(COMMAND, "Release type '{}' has been selected", releaseTypeName);
            return releaseType;
        }
        throw new IllegalPropertyException("No suitable release types have been configured or none of the configured release types matches the current environment");
    }
}