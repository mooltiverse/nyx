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
package com.mooltiverse.oss.nyx.gradle;

import static org.junit.jupiter.api.Assertions.*;

import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.StringWriter;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.gradle.testkit.runner.BuildResult;

import com.mooltiverse.oss.nyx.entities.Provider;
import com.mooltiverse.oss.nyx.services.GitHostingService;
import com.mooltiverse.oss.nyx.services.GitHostedRepository;
import com.mooltiverse.oss.nyx.services.Release;
import com.mooltiverse.oss.nyx.services.ReleaseService;
import com.mooltiverse.oss.nyx.services.ServiceFactory;
import com.mooltiverse.oss.nyx.services.User;
import com.mooltiverse.oss.nyx.services.UserService;
import com.mooltiverse.oss.nyx.git.tools.Scenario;
import com.mooltiverse.oss.nyx.git.tools.Script;
import com.mooltiverse.oss.nyx.git.util.RandomUtil;
import com.mooltiverse.oss.nyx.services.github.GitHub;
import com.mooltiverse.oss.nyx.services.gitlab.GitLab;

/**
 * The test suite:<br>
 * <br>
 * - defines a fixture of input data and parameters for the test
 * - defines a fixture of checks to run after Nyx execution, in the form of assertions
 * - runs the test, according to the above fixture
 * - performs the checks, according to the above fixture
 */
public class TestSuite {
	/**
     * The private logger instance.
     */
    private static final Logger logger = LoggerFactory.getLogger(TestSuite.class);

	/**
     * The test suite short descriptive name
     */
    public String name = null;

	/**
     * The Gradle tasks to run
     */
	public String[] tasks = null;

	/**
     * The Gradle version to test against. If null the default version will be used
     */
	public String gradleVersion = null;

	/**
	 * The configuration arguments to pass on the command line (without the Nyx command)
	 */
	public String[] args = null;

	/**
	 * The map of environment variables to use when running Nyx and returns the suite. Each entry key is the variable
	 * name, while the value is the variable value.
	 */
	public Map<String,String> env = null;

	/**
     * The map of files to create in the repository directory when running Nyx. Keys are file names
     * (relative to the Git repository), while values are file contents
     */
    public Map<String, String> files = null;

	/**
     * The checks to run against file contents after running Nyx. Keys are file names, values are strings
	 * or regular expressions to match within the file.
	 * Checks will be tried using values as plain strings first and only if the plain
	 * string can't be found in the file it will be used as a regular expression. The check will fail only if
	 * both tries fail. If the file does not exist or it doesn't match the given content the assertion will fail.
     */
	public Map<String, Set<String>> fileContentChecks = null;

	/**
     * The name of an optional hosting service ('github' or 'gitlab') to use to create a remote repository first,
	 * then clone and apply the scenario on. This is also used to test release publishing.
	 * This is different (and doesn't clash with) than the remote repository configured with RemoteRepoName.
	 * Please note that enabling hosting services will slightly slow down tests.
	 * Also note that if the hosting service requires some credentials, they must be passed by some configuration means.
     */
	public String hostingRepoService = null;

	/**
     * The checks to run against the hosted repository releases after running Nyx. The tags herein contained must all be
	 * present in the hosting service as release names.
	 * The slice contains tags that must correspond to release names on the hosting service
	 * (which must be configured using HostingRepoService.
     */
	public Set<String> hostedReleaseTags = null;

	/**
     * The checks to run against repository tags after running Nyx. The tags herein contained must all be
	 * present in the repository.
     */
	public Set<String> repositoryTags = null;

	/**
     * The checks to run against remote repository tags after running Nyx. The tags herein contained must all be
	 * present in the remote repository (which must be configured).
	 * Please note that in order for this to work the remote repository RemoteRepoName must be configured
	 * or assertions will fail.
     */
	public Set<String> remoteRepositoryTags = null;

	/**
     * The name of an optional bare remote repository to create on the local file system and configured as a remote
	 * against the main repository to test that 'push' operations reflect changes in the remote as well.
	 * An example to pass here is 'replica'. You should not use 'origin' here to avoid name clashes with remote hosting
	 * services that may also be used for test.
	 * This is different (and doesn't clash with) than the hosted repository configured with HostingRepoService.
     */
	public String remoteRepoName = null;

	/**
     * The test scenario, used to set up the Git repository to a well known state
     */
	public Scenario scenario = null;

	/**
	 * Returns a string description of the test suite.
	 */
	public String toString() {
		return "Gradle "+(Objects.isNull(gradleVersion) ? "" : gradleVersion)+" Plugin "+(Objects.isNull(name) ? "(anonymous)" : "("+name+")"+" (JVM "+System.getProperty("java.specification.version")+")");
	}

	/**
	 * Tests the suite using the given command and returns the error.
	 *
	 * This method sets up the environment, runs Nyx and performs the checks.
	 *
	 * @param context the execution context to use to run Nyx
	 * 
	 *  @throws Exception in case of any issues
	 */
	public void Test(GradleExecutionContext context)
		throws Exception {
		// set up the hosted service references, if configured
		GitHostingService gitHostingService = null;
		ReleaseService gitHostingReleaseService = null;
		UserService gitHostingUserService = null;
		GitHostedRepository gitHostedRepository = null;
		Provider provider = null;
		if (!Objects.isNull(hostingRepoService)) {
			String randomID = RandomUtil.randomAlphabeticString(5);
			provider = Provider.valueOf(hostingRepoService.toUpperCase());
			Map<String,String> hostingServiceOptions = new HashMap<String,String>();
			switch (provider)
			{
				case GITHUB: {
					// the 'gitHubTestUserToken' environment variable is set by the build script
					assertNotNull(System.getProperty("gitHubTestUserToken"), "A GitHub authentication token must be passed to this test as an environment variable but it was not set");
					hostingServiceOptions.put(GitHub.AUTHENTICATION_TOKEN_OPTION_NAME, System.getProperty("gitHubTestUserToken"));
					break;
				}
				case GITLAB: {
					// the 'gitLabTestUserToken' environment variable is set by the build script
					assertNotNull(System.getProperty("gitLabTestUserToken"), "A GitLab authentication token must be passed to this test as an environment variable but it was not set");
					hostingServiceOptions.put(GitLab.AUTHENTICATION_TOKEN_OPTION_NAME, System.getProperty("gitLabTestUserToken"));
					break;
				}
				default: throw new RuntimeException("unknown provider");
			}

			gitHostingService = ServiceFactory.gitHostingServiceInstance(provider, hostingServiceOptions);
			gitHostingUserService = ServiceFactory.userServiceInstance(provider, hostingServiceOptions);
			gitHostingReleaseService = ServiceFactory.releaseServiceInstance(provider, hostingServiceOptions);
			gitHostedRepository = gitHostingService.createGitRepository(randomID, "Test repository "+randomID, false, true);

			// if we clone too quickly next calls may fail
			Thread.sleep(4000);
		}
	
		// set up the Git repository
		Script script = null;
		logger.debug("Setting up the Git repository scenario");
		if (Objects.isNull(gitHostingService)) {
			// in case no hosting service is used, just prepare the repository locally
			script = scenario.realize();
			logger.debug("Git repository scenario created in: '{}'", script.getWorkingDirectory());
		}
		else {
			// in case an hosting service is used, create the local repository by applying the scenarion on a clone of the hosted repository
			switch (provider)
			{
				case GITHUB:
					// the 'gitHubTestUserToken' environment variable is set by the build script
					assertNotNull(System.getProperty("gitHubTestUserToken"), "A GitHub authentication token must be passed to this test as an environment variable but it was not set");
					script = scenario.applyOnClone(gitHostedRepository.getHTTPURL(), System.getProperty("gitHubTestUserToken"), "");
					break;
				case GITLAB:
					// the 'gitLabTestUserToken' environment variable is set by the build script
					assertNotNull(System.getProperty("gitLabTestUserToken"), "A GitLab authentication token must be passed to this test as an environment variable but it was not set");
					script = scenario.applyOnClone(gitHostedRepository.getHTTPURL(), "PRIVATE-TOKEN", System.getProperty("gitLabTestUserToken"));
					break;
				default: throw new RuntimeException("unknown provider");
			}
			logger.debug("Git repository scenario created in: '{}' on clone from '{}'", script.getWorkingDirectory(), gitHostedRepository.getHTTPURL());
		}
		script.getWorkingDirectory().deleteOnExit(); // clean up

		// set up the remote repository, if configured
		Script remoteScript = null;
		if (!Objects.isNull(remoteRepoName)) {
			remoteScript = Scenario.BARE.realize(true);
			script.addRemote(remoteScript.getGitDirectory(), remoteRepoName);
			remoteScript.getGitDirectory().deleteOnExit();
		}

		// Create the files in the repository
		logger.debug("Creating files in repository");
		if (!Objects.isNull(files) && !files.isEmpty()) {
			for (Map.Entry<String,String> filesEntry: files.entrySet()) {
				FileWriter output = null;
				try {
					output = new FileWriter(new File(script.getWorkingDirectory(), filesEntry.getKey()));
					output.write(filesEntry.getValue());
					output.flush();
				}
				finally {
					if (output != null) {
						output.close();
					}
				}
			}
		}

		// Prepare the command to run
		logger.debug("Setting up the command to run");
		String[] cmdArgs = args;
		if (!Objects.isNull(tasks)) {
			for (String task: tasks) {
				cmdArgs = Arrays.copyOf(cmdArgs, cmdArgs.length + 1);
				cmdArgs[cmdArgs.length - 1] = task;
			}
		}
		GradleCommand cmd = context.getCommand(script.getWorkingDirectory(), gradleVersion, env, cmdArgs);

		// Run the command
		logger.debug("running Gradle");
		logger.debug("   in directory              : '{}'", script.getWorkingDirectory());
		logger.debug("   with arguments ", Objects.isNull(cmdArgs) ? "" : String.join(" ", cmdArgs));
		logger.debug("   with '{}' environment variables", Objects.isNull(env) ? 0 : env.size());
		//logger.trace("   with '{}' environment variables: '{}'", Objects.isNull(env) ? 0 : env.size(), env) // keep this at the trace level as it may expose the token values
		BuildResult result = cmd.run();
		logger.debug("command executed without errors");
		logger.debug("command output is: *** START ***");
		logger.debug(result.getOutput());
		logger.debug("command output is: ***  END  ***");
		// also log to stdout as Gradle mutes the logger
		System.out.println("command executed without errors");
		System.out.println("command output is: *** START ***");
		System.out.println(result.getOutput());
		System.out.println("command output is: ***  END  ***");
		System.out.flush();

		// Run the checks on file contents
		logger.debug("Running file content checks, if any");
		if (!Objects.isNull(fileContentChecks) && !fileContentChecks.isEmpty()) {
			for (Map.Entry<String, Set<String>> fileContentChecksEntry: fileContentChecks.entrySet()) {
				File file = new File(script.getWorkingDirectory(), fileContentChecksEntry.getKey());
				assertTrue(file.exists(), String.format("file '%s' does not exist after running Nyx", file.getAbsolutePath()));
				StringWriter writer = new StringWriter();
				FileReader reader = new FileReader(file);
				reader.transferTo(writer);
				reader.close();
				String fileContent = writer.toString();
				for (String fileContentItemToCheck : fileContentChecksEntry.getValue()) {
					// try first if the file contains the plain string
					boolean plainStringContained = fileContent.contains(fileContentItemToCheck);
					boolean regexMatched = false;
					if (plainStringContained) {
						logger.debug("File '{}' contains plain string '{}'", file.getAbsolutePath(), fileContentItemToCheck);
					}
					else {
						// if it's not contained as a plain string try matching it as a regular expression
						logger.debug("File '{}' does not contain plain string '{}', now trying to interpret the string as a regular expression and matching it", file.getAbsolutePath(), fileContentItemToCheck);
						regexMatched = fileContent.matches(fileContentItemToCheck);
					}
					assertTrue(plainStringContained || regexMatched, String.format("File '%s' was expected to contain plain string '%s' or match it as a regular expression but it doesn't", file.getAbsolutePath(), fileContentItemToCheck));
					if (!(plainStringContained || regexMatched)) {
						System.out.println(String.format("File '%s' content is:", file.getAbsolutePath()));
						System.out.println(fileContent);
						System.out.flush();
					}
				}
			}
		}

		// Run the checks on the Git repository
		logger.debug("Running tag repository checks");
		if (!Objects.isNull(repositoryTags) && !repositoryTags.isEmpty()) {
			for (String tagToCheck: repositoryTags) {
				assertTrue(script.getTags().keySet().contains(tagToCheck), String.format("Repository was supposed to contain tag '%s' but it only contains tags '%s'", tagToCheck, String.join(",", script.getTags().keySet())));
			}
		}

		// Run the checks on the remote Git repository on the local machine
		logger.debug("Running tag remote repository checks");
		if (!Objects.isNull(remoteRepositoryTags) && !remoteRepositoryTags.isEmpty()) {
			assertNotNull(remoteScript, "Checks on the remote repository tags have been defined but the remote repository hasn't been configured");
			for (String tagToCheck: remoteRepositoryTags) {
				assertTrue(remoteScript.getTags().keySet().contains(tagToCheck), String.format("Remote repository '%s' was supposed to contain tag '%s' but it only contains tags '%s'", remoteRepoName, tagToCheck, String.join(",", remoteScript.getTags().keySet())));
			}
		}

		// Run the checks on the hosting Git service
		logger.debug("running hosting release checks");
		// It's unlikely that we actually test these because the 'services' configuration, required for this to work,
		// need the hosted repository name and owned in advance (in the configuration), but since the repository is
		// created dynamically, this will unlikely be tested
		if (!Objects.isNull(hostedReleaseTags) && !hostedReleaseTags.isEmpty()) {
			// if we read too quickly we often get a 404 from the server so let's wait a short while
			Thread.sleep(2000);

			assertNotNull(gitHostingReleaseService, "Checks on the hosting service releases have been defined but the hosting service hasn't been configured");
			for (String releaseToCheck: hostedReleaseTags) {
				User user = gitHostingUserService.getAuthenticatedUser();
				Release release = gitHostingReleaseService.getReleaseByTag(user.getUserName(), gitHostedRepository.getName(), releaseToCheck);
				assertNotNull(release, String.format("Hosting service '%s' (project '%s') was supposed to contain release '%s' but it doesn't", provider, gitHostedRepository.getHTTPURL(), releaseToCheck));
			}
		}

		logger.debug("test complete");

		// clean up
		if (!Objects.isNull(gitHostedRepository)) {
			switch (provider)
			{
				case GITHUB: {
					gitHostingService.deleteGitRepository(gitHostedRepository.getName());
					break;
				}
				case GITLAB: {
					gitHostingService.deleteGitRepository(gitHostedRepository.getID());
					break;
				}
				default: throw new RuntimeException("unknown provider");
			}
		}
	}
}
