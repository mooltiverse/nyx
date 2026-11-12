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
package com.mooltiverse.oss.nyx.git.tools;

import static java.nio.charset.StandardCharsets.UTF_8;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.TreeMap;
import java.util.concurrent.Callable;

import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.lib.CommitBuilder;
import org.eclipse.jgit.lib.Constants;
import org.eclipse.jgit.lib.FileMode;
import org.eclipse.jgit.lib.ObjectId;
import org.eclipse.jgit.lib.ObjectInserter;
import org.eclipse.jgit.lib.PersonIdent;
import org.eclipse.jgit.lib.Ref;
import org.eclipse.jgit.lib.RefUpdate;
import org.eclipse.jgit.lib.Repository;
import org.eclipse.jgit.lib.TreeFormatter;
import org.eclipse.jgit.storage.file.FileRepositoryBuilder;
import org.eclipse.jgit.transport.CredentialsProvider;
import org.eclipse.jgit.transport.PushResult;
import org.eclipse.jgit.transport.RefSpec;
import org.eclipse.jgit.transport.RemoteRefUpdate;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mooltiverse.oss.nyx.services.GitHostedRepository;

/**
 * A pool of long-lived hosted repositories for integration tests, used instead of creating (and deleting) a new
 * repository for every test.
 * <br>
 * Hosting services apply rate limits on content creation, and repository creation in particular, which a test suite
 * creating one repository per test exceeds. Pooled repositories are created once and then reused across test runs.
 * <br>
 * Isolation between tests is preserved by resetting a repository every time it's acquired (not when it's released, so a
 * test that crashed or was interrupted can't affect the next one) and then verifying the result:
 * <ol>
 *   <li>the service specific contents that survive the deletion of tags and branches (i.e. releases and packages)
 *   are deleted and the branch protections that would prevent the reset are removed (see
 *   {@link #beforePush(Object)})</li>
 *   <li>the {@value #DEFAULT_BRANCH} branch is force pushed to a deterministic reference commit (see
 *   {@link #referenceCommitID()})</li>
 *   <li>the {@value #DEFAULT_BRANCH} branch is set as the default branch and the repository visibility is set as
 *   requested (see {@link #afterPush(Object, boolean)})</li>
 *   <li>all other branches and all tags are deleted</li>
 *   <li>the remote state is verified, retrying for a while to cope with eventual consistency: the only remote refs
 *   must be the {@value #DEFAULT_BRANCH} branch pointing to the reference commit and the service specific state must
 *   be clean (see {@link #inspect(Object, boolean)})</li>
 * </ol>
 * If the verification fails the repository is deleted and created again, which is the only case a repository is created
 * after the first time.
 * <br>
 * Repository names are scoped by environment (see {@link #defaultEnvironment()}), visibility and slot, so that
 * concurrent runs on different environments (i.e. CI jobs and local runs) never share a repository. Tests within the
 * same environment run sequentially so no locking is needed. A test needing more than one repository with the same
 * visibility at the same time uses different slots.
 * <br>
 * Tests using pooled repositories must not change the repository settings (other than the contents and releases).
 *
 * @param <H> the service specific type representing a remote repository
 * @param <R> the type of the repositories returned to tests
 */
public abstract class HostedRepositoryPool<H, R extends GitHostedRepository> {
    /**
     * The common prefix of all pooled repository names.
     */
    public static final String NAME_PREFIX = "nyx-test-pool-java-";

    /**
     * The name of the only branch in a clean pooled repository, which is also the default branch.
     */
    public static final String DEFAULT_BRANCH = "main";

    /**
     * The message of the reference commit, the same used by hosting services when creating initialized repositories.
     */
    static final String REFERENCE_COMMIT_MESSAGE = "Initial commit\n";

    /**
     * The contents of the only file in the reference commit.
     */
    private static final String REFERENCE_FILE_CONTENT = "# Nyx test repository\n\nThis repository is reset at every use by Nyx integration tests. Do not use it for anything else.\n";

    /**
     * The name of the only file in the reference commit.
     */
    private static final String REFERENCE_FILE_NAME = "README.md";

    /**
     * The author and committer of the reference commit. The timestamp is fixed so the commit ID is deterministic.
     */
    private static final PersonIdent REFERENCE_IDENTITY = new PersonIdent("Nyx Test", "nyx-test@users.noreply.github.com", Instant.parse("2020-01-01T00:00:00Z"), ZoneOffset.UTC);

    /**
     * The description used for pooled repositories.
     */
    protected static final String REPOSITORY_DESCRIPTION = "Nyx integration tests pooled repository. Do not use it for anything else.";

    /**
     * How many times an operation subject to eventual consistency is attempted before giving up.
     */
    private static final int ATTEMPTS = 10;

    /**
     * The delay between attempts, in milliseconds.
     */
    private static final long ATTEMPT_DELAY = 1500L;

    /**
     * The private logger instance.
     */
    private static final Logger logger = LoggerFactory.getLogger(HostedRepositoryPool.class);

    /**
     * The environment this pool is scoped to.
     */
    private final String environment;

    /**
     * Builds a pool scoped to the given environment.
     *
     * @param environment the environment the pool is scoped to. It can't be {@code null} or blank. It's sanitized
     * so it can be used in repository names.
     */
    protected HostedRepositoryPool(String environment) {
        super();
        Objects.requireNonNull(environment, "The environment can't be null");
        this.environment = sanitize(environment);
        if (this.environment.isEmpty())
            throw new IllegalArgumentException(String.format("The environment '%s' yields an empty name", environment));
    }

    /**
     * Returns the environment pools are scoped to by default:
     * <ul>
     *   <li>on GitHub Actions: {@code ci-<runner OS>-<Java feature version>}, as these are the dimensions of the CI
     *   job matrix</li>
     *   <li>elsewhere: {@code local-<user name>}</li>
     * </ul>
     *
     * @return the environment pools are scoped to by default
     */
    public static String defaultEnvironment() {
        if (Boolean.parseBoolean(System.getenv("GITHUB_ACTIONS")))
            return sanitize(String.format("ci-%s-%d", Objects.toString(System.getenv("RUNNER_OS"), "unknown"), Runtime.version().feature()));
        else return sanitize("local-".concat(Objects.toString(System.getProperty("user.name"), "unknown")));
    }

    /**
     * Returns the {@link #defaultEnvironment() default environment} with the given scope appended, so that different
     * groups of tests (i.e. integration and functional tests) never share a repository.
     * 
     * @param scope the scope (i.e. {@code functional}). It can't be {@code null}.
     * 
     * @return the default environment with the given scope appended
     */
    public static String scopedEnvironment(String scope) {
        Objects.requireNonNull(scope, "The scope can't be null");
        return sanitize(defaultEnvironment().concat("-").concat(scope));
    }

    /**
     * Returns the given value lowercased, with every character not allowed in repository names replaced by a dash
     * and with no leading or trailing dashes.
     *
     * @param value the value to sanitize
     *
     * @return the sanitized value
     */
    public static String sanitize(String value) {
        return value.toLowerCase(Locale.ROOT).replaceAll("[^a-z0-9-]+", "-").replaceAll("^-+|-+$", "");
    }

    /**
     * Returns the environment this pool is scoped to.
     *
     * @return the environment this pool is scoped to
     */
    public String getEnvironment() {
        return environment;
    }

    /**
     * Returns the name of the public repository for the given slot.
     *
     * @param slot the slot, starting from 0
     *
     * @return the name of the repository for the given slot
     */
    public String repositoryName(int slot) {
        return repositoryName(slot, false);
    }

    /**
     * Returns the name of the repository for the given slot and visibility.
     *
     * @param slot the slot, starting from 0
     * @param restricted {@code true} for private repositories, {@code false} for public ones
     *
     * @return the name of the repository for the given slot and visibility
     */
    public String repositoryName(int slot, boolean restricted) {
        if (slot < 0)
            throw new IllegalArgumentException(String.format("The slot can't be negative: %d", slot));
        return String.format("%s%s-%s%d", NAME_PREFIX, environment, restricted ? "private-" : "", slot);
    }

    /**
     * Returns the ID of the reference commit a clean repository has as the only commit. The commit is built from
     * constant contents and metadata, so its ID never changes.
     *
     * @return the ID of the reference commit
     */
    public static ObjectId referenceCommitID() {
        try (ObjectInserter.Formatter formatter = new ObjectInserter.Formatter()) {
            return insertReferenceCommit(formatter);
        }
    }

    /**
     * Inserts the reference commit and the objects it refers to using the given inserter.
     * When the inserter is a {@link ObjectInserter.Formatter} nothing is stored and only the ID is computed.
     *
     * @param inserter the object inserter
     *
     * @return the ID of the reference commit
     */
    private static ObjectId insertReferenceCommit(ObjectInserter inserter) {
        ObjectId blob = inserter.idFor(Constants.OBJ_BLOB, REFERENCE_FILE_CONTENT.getBytes(UTF_8));
        TreeFormatter tree = new TreeFormatter();
        tree.append(REFERENCE_FILE_NAME, FileMode.REGULAR_FILE, blob);
        CommitBuilder commit = new CommitBuilder();
        commit.setTreeId(inserter.idFor(tree));
        commit.setAuthor(REFERENCE_IDENTITY);
        commit.setCommitter(REFERENCE_IDENTITY);
        commit.setMessage(REFERENCE_COMMIT_MESSAGE);
        commit.setEncoding(UTF_8);
        try {
            if (inserter instanceof ObjectInserter.Formatter)
                return inserter.idFor(Constants.OBJ_COMMIT, commit.build());
            inserter.insert(Constants.OBJ_BLOB, REFERENCE_FILE_CONTENT.getBytes(UTF_8));
            inserter.insert(tree);
            ObjectId id = inserter.insert(commit);
            inserter.flush();
            return id;
        }
        catch (IOException ioe) {
            throw new IllegalStateException("Unable to build the reference commit", ioe);
        }
    }

    /**
     * Returns the public repository for the given slot, after resetting it to a clean state and verifying the result.
     *
     * @param slot the slot, starting from 0. Tests using more than one repository at the same time must use
     * different slots.
     *
     * @return the clean repository
     *
     * @throws Exception if the repository can't be retrieved, created or reset
     *
     * @see #acquire(int, boolean)
     */
    public R acquire(int slot)
        throws Exception {
        return acquire(slot, false);
    }

    /**
     * Returns the repository for the given slot and visibility, after resetting it to a clean state and verifying
     * the result. The repository is created if it doesn't exist or if it can't be brought to a clean state.
     *
     * @param slot the slot, starting from 0. Tests using more than one repository with the same visibility at the
     * same time must use different slots.
     * @param restricted {@code true} for a private repository, {@code false} for a public one
     *
     * @return the clean repository
     *
     * @throws Exception if the repository can't be retrieved, created or reset
     */
    public R acquire(int slot, boolean restricted)
        throws Exception {
        String name = repositoryName(slot, restricted);
        H repository = find(name);
        if (Objects.isNull(repository)) {
            logger.info("The pooled repository '{}' does not exist yet and will be created", name);
            repository = createAndWait(name, restricted);
        }

        reset(repository, restricted);
        H clean = awaitClean(name, restricted);
        if (Objects.isNull(clean)) {
            // the reset didn't work, maybe due to some setting or state the reset doesn't handle, so start over
            logger.warn("The pooled repository '{}' could not be reset and will be deleted and created again. Problems: {}", name, inspectByName(name, restricted));
            delete(repository);
            repository = createAndWait(name, restricted);
            reset(repository, restricted);
            clean = awaitClean(name, restricted);
            if (Objects.isNull(clean))
                throw new IllegalStateException(String.format("The pooled repository '%s' is not clean even after being created again. Problems: %s", name, inspectByName(name, restricted)));
        }
        return wrap(clean);
    }

    /**
     * Returns the list of differences between the current state of the public repository for the given slot and a
     * clean repository, without changing anything.
     *
     * @param slot the slot, starting from 0
     *
     * @return the list of differences, empty if the repository is clean
     *
     * @throws Exception if the repository can't be inspected
     */
    public List<String> inspect(int slot)
        throws Exception {
        return inspect(slot, false);
    }

    /**
     * Returns the list of differences between the current state of the repository for the given slot and visibility
     * and a clean repository, without changing anything.
     *
     * @param slot the slot, starting from 0
     * @param restricted {@code true} for a private repository, {@code false} for a public one
     *
     * @return the list of differences, empty if the repository is clean
     *
     * @throws Exception if the repository can't be inspected
     */
    public List<String> inspect(int slot, boolean restricted)
        throws Exception {
        return inspectByName(repositoryName(slot, restricted), restricted);
    }

    /**
     * Returns the list of differences between the current state of the repository with the given name and a clean
     * repository.
     *
     * @param name the repository name
     * @param restricted {@code true} if the repository is expected to be private, {@code false} if public
     *
     * @return the list of differences, empty if the repository is clean
     *
     * @throws Exception if the repository can't be inspected
     */
    private List<String> inspectByName(String name, boolean restricted)
        throws Exception {
        H repository = find(name);
        if (Objects.isNull(repository))
            return List.<String>of(String.format("The repository '%s' does not exist", name));

        List<String> problems = new ArrayList<String>();
        Map<String,ObjectId> refs = listRemoteRefs(httpURL(repository));
        Map<String,ObjectId> expectedRefs = Map.<String,ObjectId>of(Constants.R_HEADS.concat(DEFAULT_BRANCH), referenceCommitID());
        if (!expectedRefs.equals(refs))
            problems.add(String.format("Unexpected refs: %s, expected: %s", refs, expectedRefs));
        problems.addAll(inspect(repository, restricted));
        return problems;
    }

    /**
     * Inspects the repository with the given name until it's clean or the attempts are exhausted, to cope with
     * eventual consistency.
     *
     * @param name the repository name
     * @param restricted {@code true} if the repository is expected to be private, {@code false} if public
     *
     * @return the up to date repository if it's clean, {@code null} otherwise
     *
     * @throws Exception if the repository can't be inspected
     */
    private H awaitClean(String name, boolean restricted)
        throws Exception {
        for (int attempt = 1; attempt <= ATTEMPTS; attempt++) {
            if (inspectByName(name, restricted).isEmpty())
                // fetch the repository again as its attributes may have changed
                return find(name);
            Thread.sleep(ATTEMPT_DELAY);
        }
        return null;
    }

    /**
     * Resets the given repository to a clean state. See the class description for details.
     *
     * @param repository the repository
     * @param restricted {@code true} if the repository must be private, {@code false} if public
     *
     * @throws Exception if the repository can't be reset
     */
    private void reset(H repository, boolean restricted)
        throws Exception {
        beforePush(repository);

        String url = httpURL(repository);
        File directory = Files.createTempDirectory("nyx-test-pool-").toFile();
        try (Repository local = new FileRepositoryBuilder().setGitDir(directory).setBare().build()) {
            local.create(true);
            ObjectId reference = insertReferenceCommit(local.newObjectInserter());
            String defaultRef = Constants.R_HEADS.concat(DEFAULT_BRANCH);
            RefUpdate refUpdate = local.updateRef(defaultRef);
            refUpdate.setNewObjectId(reference);
            refUpdate.forceUpdate();

            // push the default branch first, as services refuse to delete the current default branch, which may be
            // a different one until it's changed by afterPush()
            retry(() -> push(local, url, List.<RefSpec>of(new RefSpec("+".concat(defaultRef).concat(":").concat(defaultRef)))));

            afterPush(repository, restricted);

            List<RefSpec> deletions = new ArrayList<RefSpec>();
            for (String ref: listRemoteRefs(url).keySet()) {
                if (!defaultRef.equals(ref))
                    deletions.add(new RefSpec(":".concat(ref)));
            }
            if (!deletions.isEmpty()) {
                logger.debug("Deleting refs {} from the pooled repository '{}'", deletions, url);
                retry(() -> push(local, url, deletions));
            }
        }
        finally {
            deleteRecursively(directory);
        }
    }

    /**
     * Pushes the given ref specs from the given local repository to the given URL and fails if any of them is rejected.
     *
     * @param local the local repository
     * @param url the remote repository URL
     * @param refSpecs the ref specs to push
     *
     * @return {@code null}
     *
     * @throws Exception if the push fails or any ref update is rejected
     */
    private Void push(Repository local, String url, Collection<RefSpec> refSpecs)
        throws Exception {
        Iterable<PushResult> results = Git.wrap(local).push().setRemote(url).setRefSpecs(new ArrayList<RefSpec>(refSpecs)).setForce(true).setCredentialsProvider(credentials()).call();
        for (PushResult result: results) {
            for (RemoteRefUpdate update: result.getRemoteUpdates()) {
                if (!RemoteRefUpdate.Status.OK.equals(update.getStatus()) && !RemoteRefUpdate.Status.UP_TO_DATE.equals(update.getStatus()) && !RemoteRefUpdate.Status.NON_EXISTING.equals(update.getStatus()))
                    throw new IOException(String.format("Updating '%s' on '%s' failed with status %s: %s", update.getRemoteName(), url, update.getStatus(), update.getMessage()));
            }
        }
        return null;
    }

    /**
     * Returns the branches and tags of the remote repository at the given URL.
     *
     * @param url the remote repository URL
     *
     * @return the map of ref names and the IDs they point to, sorted by name
     *
     * @throws Exception if the remote repository can't be read
     */
    private Map<String,ObjectId> listRemoteRefs(String url)
        throws Exception {
        Map<String,ObjectId> refs = new TreeMap<String,ObjectId>();
        for (Ref ref: Git.lsRemoteRepository().setRemote(url).setHeads(true).setTags(true).setCredentialsProvider(credentials()).call()) {
            if (!ref.getName().endsWith("^{}"))
                refs.put(ref.getName(), ref.getObjectId());
        }
        return refs;
    }

    /**
     * Creates the repository with the given name and waits until it's available.
     *
     * @param name the repository name
     * @param restricted {@code true} for a private repository, {@code false} for a public one
     *
     * @return the new repository
     *
     * @throws Exception if the repository can't be created
     */
    private H createAndWait(String name, boolean restricted)
        throws Exception {
        // creation is not retried, as failures are often due to rate limits, which retrying would make worse
        create(name, restricted);
        return retry(() -> Objects.requireNonNull(find(name), String.format("The repository '%s' is not available yet", name)));
    }

    /**
     * Runs the given operation until it succeeds or the attempts are exhausted, to cope with eventual consistency
     * (i.e. on newly created repositories).
     *
     * @param <T> the type of the operation result
     * @param operation the operation
     *
     * @return the operation result
     *
     * @throws Exception the exception thrown by the last attempt
     */
    protected static <T> T retry(Callable<T> operation)
        throws Exception {
        Exception last = null;
        for (int attempt = 1; attempt <= ATTEMPTS; attempt++) {
            try {
                return operation.call();
            }
            catch (Exception e) {
                last = e;
                logger.debug("Attempt {} of {} failed: {}", attempt, ATTEMPTS, e.getMessage());
                Thread.sleep(ATTEMPT_DELAY);
            }
        }
        throw last;
    }

    /**
     * Deletes the given file or directory with all its contents, ignoring failures.
     *
     * @param file the file or directory to delete
     */
    private static void deleteRecursively(File file) {
        File[] children = file.listFiles();
        if (!Objects.isNull(children)) {
            for (File child: children)
                deleteRecursively(child);
        }
        file.delete();
    }

    /**
     * Returns the repository with the given name owned by the authenticated user, or {@code null} if it doesn't exist.
     *
     * @param name the repository name
     *
     * @return the repository, or {@code null} if it doesn't exist
     *
     * @throws Exception if the repository can't be retrieved
     */
    protected abstract H find(String name)
        throws Exception;

    /**
     * Creates an empty repository with the given name, owned by the authenticated user.
     *
     * @param name the repository name
     * @param restricted {@code true} for a private repository, {@code false} for a public one
     *
     * @throws Exception if the repository can't be created
     */
    protected abstract void create(String name, boolean restricted)
        throws Exception;

    /**
     * Deletes the given repository.
     *
     * @param repository the repository
     *
     * @throws Exception if the repository can't be deleted
     */
    protected abstract void delete(H repository)
        throws Exception;

    /**
     * Returns the HTTP URL of the given repository, used for Git operations.
     *
     * @param repository the repository
     *
     * @return the HTTP URL of the given repository
     */
    protected abstract String httpURL(H repository);

    /**
     * Returns the credentials for Git operations.
     *
     * @return the credentials for Git operations
     */
    protected abstract CredentialsProvider credentials();

    /**
     * Prepares the given repository for the reset, before the default branch is force pushed: deletes the service
     * specific contents that survive the deletion of tags and branches (i.e. releases and packages) and removes the
     * protections that would prevent the force push or the deletion of refs.
     *
     * @param repository the repository
     *
     * @throws Exception if the repository can't be prepared
     */
    protected abstract void beforePush(H repository)
        throws Exception;

    /**
     * Completes the reset of the given repository after the default branch has been pushed: makes sure the
     * {@value #DEFAULT_BRANCH} branch is the default one and the repository visibility is the expected one.
     *
     * @param repository the repository
     * @param restricted {@code true} if the repository must be private, {@code false} if public
     *
     * @throws Exception if the repository can't be updated
     */
    protected abstract void afterPush(H repository, boolean restricted)
        throws Exception;

    /**
     * Returns the list of differences between the service specific state of the given repository (i.e. default
     * branch, visibility, releases, packages) and a clean repository. Git refs are inspected separately.
     *
     * @param repository the repository
     * @param restricted {@code true} if the repository is expected to be private, {@code false} if public
     *
     * @return the list of differences, empty if the repository is clean
     *
     * @throws Exception if the repository can't be inspected
     */
    protected abstract List<String> inspect(H repository, boolean restricted)
        throws Exception;

    /**
     * Returns the object returned to tests for the given repository.
     *
     * @param repository the repository
     *
     * @return the object returned to tests for the given repository
     *
     * @throws Exception if the object can't be built
     */
    protected abstract R wrap(H repository)
        throws Exception;
}
