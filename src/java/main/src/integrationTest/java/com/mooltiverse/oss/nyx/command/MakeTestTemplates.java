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

import static org.junit.jupiter.api.Assertions.*;
import static com.mooltiverse.oss.nyx.configuration.presets.CommitMessageConventions.CONVENTIONAL_COMMITS;
import static com.mooltiverse.oss.nyx.configuration.presets.CommitMessageConventions.CONVENTIONAL_COMMITS_FOR_MERGE;

import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.StringWriter;
import java.nio.file.Files;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.TestTemplate;
import org.junit.jupiter.api.extension.ExtendWith;

import com.mooltiverse.oss.nyx.command.template.Baseline;
import com.mooltiverse.oss.nyx.command.template.CommandInvocationContextProvider;
import com.mooltiverse.oss.nyx.command.template.CommandProxy;
import com.mooltiverse.oss.nyx.command.template.CommandSelector;
import com.mooltiverse.oss.nyx.command.template.StandaloneCommandProxy;
import com.mooltiverse.oss.nyx.configuration.SimpleConfigurationLayer;
import com.mooltiverse.oss.nyx.entities.CommitMessageConvention;
import com.mooltiverse.oss.nyx.entities.CommitMessageConventions;
import com.mooltiverse.oss.nyx.entities.Substitution;
import com.mooltiverse.oss.nyx.entities.Substitutions;
import com.mooltiverse.oss.nyx.entities.Changelog.Release;
import com.mooltiverse.oss.nyx.git.GitException;
import com.mooltiverse.oss.nyx.git.tools.Scenario;
import com.mooltiverse.oss.nyx.git.tools.Script;

@DisplayName("Make")
public class MakeTestTemplates {
    /**
     * Reads the contents of the given file and returns its content as a string.
     * 
     * @param file the file to read
     * 
     * @return the file content
     * 
     * @throws Exception in case of any issue
     */
    private static String readFile(File file)
        throws Exception {
        StringWriter buffer = new StringWriter();
        FileReader reader = new FileReader(file);
        reader.transferTo(buffer);
        reader.close();
        return buffer.toString();
    }

    /**
     * Writes the given contents to the given file
     * 
     * @param file the file to write
     * @param content the content to write
     * 
     * @return the file content
     * 
     * @throws Exception in case of any issue
     */
    private static void writeFile(File file, String content)
        throws Exception {
        FileWriter writer = new FileWriter(file);
        writer.write(content);
        writer.flush();
        writer.close();
    }

    @Nested
    @DisplayName("Make constructor")
    @ExtendWith(CommandInvocationContextProvider.class)
    public static class ConstructorTests {
        /**
         * Test that the given class has the required 2 arguments constructor and that it doesn't fail as long as it
         * has non null parameters
         */
        @TestTemplate
        @DisplayName("Make()")
        @Baseline(Scenario.FROM_SCRATCH)
        void constructorTest(@CommandSelector(Commands.MAKE) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            assertNotNull(command);
        }
    }

    @Nested
    @DisplayName("Make state")
    @ExtendWith(CommandInvocationContextProvider.class)
    public static class StateTests {
        /**
         * Check that the state() method never returns a {@code null} object
         */
        @TestTemplate
        @DisplayName("Make.state()")
        @Baseline(Scenario.FROM_SCRATCH)
        void stateTest(@CommandSelector(Commands.MAKE) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            assertNotNull(command.state());
        }
    }

    @Nested
    @DisplayName("Make isUpToDate")
    @ExtendWith(CommandInvocationContextProvider.class)
    public static class UpToDateTests {
        /**
         * Check that the isUpToDate() returns {@code false} when the command instance is just created and {@code true} after one execution in a repository
         * with at least one commit and in a clean state
         */
        @TestTemplate
        @DisplayName("Make.isUpToDate()")
        @Baseline(Scenario.FROM_SCRATCH)
        void isUpToDateTest(@CommandSelector(Commands.MAKE) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            assertFalse(command.isUpToDate());

            // running in an empty repository, with no commits, throws an exception
            assertThrows(GitException.class, () -> command.run());
            assertFalse(command.isUpToDate());

            // add some commits to the repository and after one run the task should be up to date
            script.andCommitWithTag("111.122.133");
            command.run();

            // when the command is executed standalone, Infer is not executed so isUpToDate() will always return false
            if (command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME))
                assertFalse(command.isUpToDate());
            else assertTrue(command.isUpToDate()); 

            // and running again with no changes must still be up to date
            command.run();
            // when the command is executed standalone, Infer is not executed so isUpToDate() will always return false
            if (command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME))
                assertFalse(command.isUpToDate());
            else assertTrue(command.isUpToDate()); 
        }

        /**
         * Check that the isUpToDate() returns {@code false} when the changelog destination file is configured but
         * the file is missing
         */
        @TestTemplate
        @DisplayName("Make.isUpToDate() == false when changelog file is configured but missing")
        @Baseline(Scenario.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS)
        void isUpToDateTestWithMissingChangelogFile(@CommandSelector(Commands.MAKE) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            // first create the temporary directory and the abstract destination file
            File destinationDir = Files.createTempDirectory("nyx-test-make-test-").toFile();
            destinationDir.deleteOnExit();
            File changelogFile = new File(destinationDir, "CHANGELOG.md");

            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.getChangelog().setPath(changelogFile.getAbsolutePath());
            // add the conventional commits convention
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("conventionalCommits"),
                    Map.<String,CommitMessageConvention>of("conventionalCommits", CONVENTIONAL_COMMITS))
            );
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);

            assertFalse(command.isUpToDate());
            assertFalse(changelogFile.exists());

            command.run();

            // when the command is executed standalone, Infer is not executed so isUpToDate() will always return false
            if (command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME))
                assertFalse(command.isUpToDate());
            else {
                assertTrue(changelogFile.exists());

                assertTrue(command.isUpToDate());

                // now delete the file and make sure it's no longer up to date
                changelogFile.delete();
                assertFalse(changelogFile.exists());

                assertFalse(command.isUpToDate());
            }
        }
    }

    @Nested
    @DisplayName("Make idempotency")
    @ExtendWith(CommandInvocationContextProvider.class)
    public static class IdempotencyTests {
        /**
         * Check that multiple runs yield to the same result with a commit message convention configured
         */
        @TestTemplate
        @DisplayName("Make idempotency with conventional commit message convention")
        @Baseline(Scenario.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS)
        void idempotencyWithCommitMessageConvention(@CommandSelector(Commands.MAKE) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            // first create the temporary directory and the abstract destination file
            File destinationDir = Files.createTempDirectory("nyx-test-make-test-").toFile();
            destinationDir.deleteOnExit();
            File changelogFile = new File(destinationDir, "CHANGELOG.md");

            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.getChangelog().setPath(changelogFile.getAbsolutePath());
            // add the conventional commits convention
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("conventionalCommits"),
                    Map.<String,CommitMessageConvention>of("conventionalCommits", CONVENTIONAL_COMMITS))
            );
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);

            assertFalse(changelogFile.exists());

            // run a first time
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertTrue(changelogFile.exists());

                // collect its state values
                List<Release> changelogReleases = List.<Release>copyOf(command.state().getChangelog().getReleases());
                Boolean newVersion = command.state().getNewVersion();
                String version = command.state().getVersion();
                String changelogFileContent = readFile(changelogFile);

                // run again and check that all values are still the same
                assertTrue(command.isUpToDate());
                command.run();

                assertTrue(changelogFile.exists());
                assertEquals(changelogReleases, command.state().getChangelog().getReleases());
                assertEquals(newVersion, command.state().getNewVersion());
                assertEquals(version, command.state().getVersion());
                assertEquals(changelogFileContent, readFile(changelogFile));

                // now delete the file and make sure it's no longer up to date
                changelogFile.delete();
                assertFalse(changelogFile.exists());
                changelogFileContent = null;

                // add some commits to the repository and after one run the task should be up to date
                script.andCommitWithTag("111.122.133");
                assertFalse(command.isUpToDate());
                command.run();
                assertTrue(command.isUpToDate());

                // check that some values have changed
                assertNull(command.state().getChangelog()); // the internal changelog is now null because there is no new version and the changelog hasn't been recreated
                assertNotEquals(newVersion, command.state().getNewVersion());
                assertNotEquals(version, command.state().getVersion());
                assertFalse(changelogFile.exists());
                
                // collect state values again
                changelogReleases = null;
                newVersion = command.state().getNewVersion();
                version = command.state().getVersion();

                // run again and make sure values didn't change
                assertTrue(command.isUpToDate());
                command.run();
                assertTrue(command.isUpToDate());

                assertNull(command.state().getChangelog()); // the internal changelog is now null because there is no new version
                assertEquals(newVersion, command.state().getNewVersion());
                assertEquals(version, command.state().getVersion());
                assertFalse(changelogFile.exists());

                // once more, also considering that its still up to date
                assertTrue(command.isUpToDate());
                command.run();

                assertNull(command.state().getChangelog()); // the internal changelog is now null because there is no new version
                assertEquals(newVersion, command.state().getNewVersion());
                assertEquals(version, command.state().getVersion());
                assertFalse(changelogFile.exists());
            }
        }

        /**
         * Check that multiple runs yield to the same result without a commit message convention configured
         */
        @TestTemplate
        @DisplayName("Make idempotency without commit message convention")
        @Baseline(Scenario.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS)
        void idempotencyWithoutCommitMessageConvention(@CommandSelector(Commands.MAKE) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            // first create the temporary directory and the abstract destination file
            File destinationDir = Files.createTempDirectory("nyx-test-make-test-").toFile();
            destinationDir.deleteOnExit();
            File changelogFile = new File(destinationDir, "CHANGELOG.md");

            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.getChangelog().setPath(changelogFile.getAbsolutePath());
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);

            assertFalse(changelogFile.exists());

            // run a first time
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertFalse(changelogFile.exists()); // with no convention no version is generated and no changelog is written

                // collect its state values
                Boolean newVersion = command.state().getNewVersion();
                String version = command.state().getVersion();

                // run again and check that all values are still the same
                assertTrue(command.isUpToDate());
                command.run();

                assertFalse(changelogFile.exists());
                assertNull(command.state().getChangelog());
                assertEquals(newVersion, command.state().getNewVersion());
                assertEquals(version, command.state().getVersion());

                // add some commits to the repository and after one run the task should be up to date
                script.andCommitWithTag("111.122.133");
                assertFalse(command.isUpToDate());
                command.run();
                assertTrue(command.isUpToDate());

                // check that some values have changed
                assertNull(command.state().getChangelog()); // the internal changelog is still null because there is no new version and the changelog hasn't been recreated
                assertEquals(newVersion, command.state().getNewVersion());
                assertNotEquals(version, command.state().getVersion());
                assertFalse(changelogFile.exists());
                
                // collect state values again
                version = command.state().getVersion();

                // run again and make sure values didn't change
                assertTrue(command.isUpToDate());
                command.run();
                assertTrue(command.isUpToDate());

                assertNull(command.state().getChangelog()); // the internal changelog is now null because there is no new version
                assertEquals(newVersion, command.state().getNewVersion());
                assertEquals(version, command.state().getVersion());
                assertFalse(changelogFile.exists());

                // once more, also considering that its still up to date
                assertTrue(command.isUpToDate());
                command.run();

                assertNull(command.state().getChangelog()); // the internal changelog is now null because there is no new version
                assertEquals(newVersion, command.state().getNewVersion());
                assertEquals(version, command.state().getVersion());
                assertFalse(changelogFile.exists());
            }
        }

        /**
         * Check that multiple runs yield to the same result with a commit message convention configured
         */
        @TestTemplate
        @DisplayName("Make idempotency in dirty repository")
        @Baseline(Scenario.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS)
        void idempotencyInDirtyRepository(@CommandSelector(Commands.MAKE) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            // first create the temporary directory and the abstract destination file
            File destinationDir = Files.createTempDirectory("nyx-test-make-test-").toFile();
            destinationDir.deleteOnExit();
            File changelogFile = new File(destinationDir, "CHANGELOG.md");

            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.getChangelog().setPath(changelogFile.getAbsolutePath());
            // add the conventional commits convention
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("conventionalCommits"),
                    Map.<String,CommitMessageConvention>of("conventionalCommits", CONVENTIONAL_COMMITS))
            );
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);

            assertFalse(changelogFile.exists());

            // run a first time
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertTrue(changelogFile.exists());

                // collect its state values
                List<Release> changelogReleases = List.<Release>copyOf(command.state().getChangelog().getReleases());
                Boolean newVersion = command.state().getNewVersion();
                String version = command.state().getVersion();
                String changelogFileContent = readFile(changelogFile);

                // add some uncommitted changes
                script.updateAllWorkbenchFiles();

                // run again and check that all values are still the same
                assertTrue(command.isUpToDate());
                command.run();

                assertTrue(changelogFile.exists());
                assertEquals(changelogReleases, command.state().getChangelog().getReleases());
                assertEquals(newVersion, command.state().getNewVersion());
                assertEquals(version, command.state().getVersion());
                assertEquals(changelogFileContent, readFile(changelogFile));

                // now delete the file and make sure it's no longer up to date
                changelogFile.delete();
                assertFalse(changelogFile.exists());
                changelogFileContent = null;

                // add some commits to the repository and after one run the task should be up to date
                script.andCommitWithTag("111.122.133");
                assertFalse(command.isUpToDate());
                command.run();
                assertTrue(command.isUpToDate());

                // check that some values have changed
                assertNull(command.state().getChangelog()); // the internal changelog is now null because there is no new version and the changelog hasn't been recreated
                assertNotEquals(newVersion, command.state().getNewVersion());
                assertNotEquals(version, command.state().getVersion());
                assertFalse(changelogFile.exists());
                
                // collect state values again
                changelogReleases = null;
                newVersion = command.state().getNewVersion();
                version = command.state().getVersion();

                // run again and make sure values didn't change
                assertTrue(command.isUpToDate());
                command.run();
                assertTrue(command.isUpToDate());

                assertNull(command.state().getChangelog()); // the internal changelog is now null because there is no new version
                assertEquals(newVersion, command.state().getNewVersion());
                assertEquals(version, command.state().getVersion());
                assertFalse(changelogFile.exists());

                // once more, also considering that its still up to date
                assertTrue(command.isUpToDate());
                command.run();

                assertNull(command.state().getChangelog()); // the internal changelog is now null because there is no new version
                assertEquals(newVersion, command.state().getNewVersion());
                assertEquals(version, command.state().getVersion());
                assertFalse(changelogFile.exists());
            }
        }
    }

    @Nested
    @DisplayName("Make run")
    @ExtendWith(CommandInvocationContextProvider.class)
    public static class RunTests {
        @TestTemplate
        @DisplayName("Make.run() with no destination file > yield to no changelog because there is no destination file")
        @Baseline(Scenario.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS)
        void runTestWithNoDestinationFile(@CommandSelector(Commands.MAKE) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the conventional commits convention
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("conventionalCommits"),
                    Map.<String,CommitMessageConvention>of("conventionalCommits", CONVENTIONAL_COMMITS))
            );
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertNull(command.state().getChangelog());
            }
        }

        @TestTemplate
        @DisplayName("Make.run() with no commit message convention > yield to no changelog because no new version is generated")
        @Baseline(Scenario.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS)
        void runTestWithNoCommitConvention(@CommandSelector(Commands.MAKE) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            // first create the temporary directory and the abstract destination file
            File destinationDir = Files.createTempDirectory("nyx-test-make-test-").toFile();
            destinationDir.deleteOnExit();
            File changelogFile = new File(destinationDir, "CHANGELOG.md");

            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.getChangelog().setPath(changelogFile.getAbsolutePath());
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);

            assertFalse(changelogFile.exists());

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertFalse(changelogFile.exists());

                assertNull(command.state().getChangelog());
            }
        }

        @TestTemplate
        @DisplayName("Make.run() with conventional commit message convention and without sections > yield to changelog where sections are commit types")
        @Baseline(Scenario.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS)
        void runTestWithConventionalCommitsConventionAndWithoutSections(@CommandSelector(Commands.MAKE) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            // first create the temporary directory and the abstract destination file
            File destinationDir = Files.createTempDirectory("nyx-test-make-test-").toFile();
            destinationDir.deleteOnExit();
            File changelogFile = new File(destinationDir, "CHANGELOG.md");

            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.getChangelog().setPath(changelogFile.getAbsolutePath());
            // add the conventional commits convention
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("conventionalCommits"),
                    Map.<String,CommitMessageConvention>of("conventionalCommits", CONVENTIONAL_COMMITS))
            );
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);

            assertFalse(changelogFile.exists());

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertTrue(changelogFile.exists());

                // print the file to standard output for inspection purpose
                System.out.println("------- CHANGELOG -------");
                System.out.println("Loading from: "+changelogFile.getAbsolutePath());
                System.out.println("-----------------------------------------");
                System.out.println(readFile(changelogFile));
                System.out.println("-----------------------------------------");
                System.out.flush();

                // test the data model
                assertEquals(1, command.state().getChangelog().getReleases().size());
                assertEquals("0.1.0", command.state().getChangelog().getReleases().get(0).getName());
                assertEquals(2, command.state().getChangelog().getReleases().get(0).getSections().size());
                assertEquals("feat", command.state().getChangelog().getReleases().get(0).getSections().get(0).getName());
                assertEquals(1, command.state().getChangelog().getReleases().get(0).getSections().get(0).getCommits().size());
                assertEquals("fix", command.state().getChangelog().getReleases().get(0).getSections().get(1).getName());
                assertEquals(1, command.state().getChangelog().getReleases().get(0).getSections().get(1).getCommits().size());

                // test the rendered file
                String fileContent = readFile(changelogFile);
                assertTrue(fileContent.startsWith("# Changelog"));  // title header check
                assertTrue(fileContent.contains("## 0.1.0 "));      // release header check
                assertTrue(fileContent.contains("### feat"));       // section header check
                assertTrue(fileContent.contains("] feat: Untagged commit #2 (")); // partial line check
                assertTrue(fileContent.contains("### fix"));        // section header check
                assertTrue(fileContent.contains("] fix: Untagged commit #1 ("));  // partial line check
            }
        }

        @TestTemplate
        @DisplayName("Make.run() with conventional commit message convention and with custom sections > yield to changelog where sections have been remapped")
        @Baseline(Scenario.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS)
        void runTestWithConventionalCommitsConventionAndWithCustomSections(@CommandSelector(Commands.MAKE) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            // first create the temporary directory and the abstract destination file
            File destinationDir = Files.createTempDirectory("nyx-test-make-test-").toFile();
            destinationDir.deleteOnExit();
            File changelogFile = new File(destinationDir, "CHANGELOG.md");

            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.getChangelog().setPath(changelogFile.getAbsolutePath());
            // add the sections to be remapped
            configurationLayerMock.getChangelog().setSections(Map.<String,String>of(
                "Added", "^feat$",
                "Fixed", "^fix$"
            ));
            // add the conventional commits convention
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("conventionalCommits"),
                    Map.<String,CommitMessageConvention>of("conventionalCommits", CONVENTIONAL_COMMITS))
            );
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);

            assertFalse(changelogFile.exists());

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertTrue(changelogFile.exists());

                // print the file to standard output for inspection purpose
                System.out.println("------- CHANGELOG -------");
                System.out.println("Loading from: "+changelogFile.getAbsolutePath());
                System.out.println("-----------------------------------------");
                System.out.println(readFile(changelogFile));
                System.out.println("-----------------------------------------");
                System.out.flush();

                // test the data model
                assertEquals(1, command.state().getChangelog().getReleases().size());
                assertEquals("0.1.0", command.state().getChangelog().getReleases().get(0).getName());
                assertEquals(2, command.state().getChangelog().getReleases().get(0).getSections().size());
                assertEquals("Added", command.state().getChangelog().getReleases().get(0).getSections().get(0).getName());
                assertEquals(1, command.state().getChangelog().getReleases().get(0).getSections().get(0).getCommits().size());
                assertEquals("Fixed", command.state().getChangelog().getReleases().get(0).getSections().get(1).getName());
                assertEquals(1, command.state().getChangelog().getReleases().get(0).getSections().get(1).getCommits().size());

                // test the rendered file
                String fileContent = readFile(changelogFile);
                assertTrue(fileContent.startsWith("# Changelog"));  // title header check
                assertTrue(fileContent.contains("## 0.1.0 "));      // release header check
                assertTrue(fileContent.contains("### Added"));       // section header check
                assertTrue(fileContent.contains("] feat: Untagged commit #2 (")); // partial line check
                assertTrue(fileContent.contains("### Fixed"));        // section header check
                assertTrue(fileContent.contains("] fix: Untagged commit #1 ("));  // partial line check
            }
        }

        @TestTemplate
        @DisplayName("Make.run() with conventional commit message convention and with custom sections and substitutions > yield to changelog where sections have been remapped and substitutions applied")
        @Baseline(Scenario.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS)
        void runTestWithConventionalCommitsConventionAndWithCustomSectionsAndSubstitutions(@CommandSelector(Commands.MAKE) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            // first create the temporary directory and the abstract destination file
            File destinationDir = Files.createTempDirectory("nyx-test-make-test-").toFile();
            destinationDir.deleteOnExit();
            File changelogFile = new File(destinationDir, "CHANGELOG.md");

            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.getChangelog().setPath(changelogFile.getAbsolutePath());
            // add the sections to be remapped
            configurationLayerMock.getChangelog().setSections(Map.<String,String>of(
                "Added", "^feat$",
                "Fixed", "^fix$"
            ));
            // add the substitution rules to replace issue IDs with links
            configurationLayerMock.getChangelog().setSubstitutions(Map.<String,String>of(
                "(?m)#([0-9]+)(?s)", "[#%s](https://example.com/issues/%s)"
            ));
            // add the conventional commits convention
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("conventionalCommits"),
                    Map.<String,CommitMessageConvention>of("conventionalCommits", CONVENTIONAL_COMMITS))
            );
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);

            assertFalse(changelogFile.exists());

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertTrue(changelogFile.exists());

                // print the file to standard output for inspection purpose
                System.out.println("------- CHANGELOG -------");
                System.out.println("Loading from: "+changelogFile.getAbsolutePath());
                System.out.println("-----------------------------------------");
                System.out.println(readFile(changelogFile));
                System.out.println("-----------------------------------------");
                System.out.flush();

                // test the data model
                assertEquals(1, command.state().getChangelog().getReleases().size());
                assertEquals("0.1.0", command.state().getChangelog().getReleases().get(0).getName());
                assertEquals(2, command.state().getChangelog().getReleases().get(0).getSections().size());
                assertEquals("Added", command.state().getChangelog().getReleases().get(0).getSections().get(0).getName());
                assertEquals(1, command.state().getChangelog().getReleases().get(0).getSections().get(0).getCommits().size());
                assertEquals("Fixed", command.state().getChangelog().getReleases().get(0).getSections().get(1).getName());
                assertEquals(1, command.state().getChangelog().getReleases().get(0).getSections().get(1).getCommits().size());

                // test the rendered file
                String fileContent = readFile(changelogFile);
                assertTrue(fileContent.startsWith("# Changelog"));  // title header check
                assertTrue(fileContent.contains("## 0.1.0 "));      // release header check
                assertTrue(fileContent.contains("### Added"));       // section header check
                assertFalse(fileContent.contains(" #2 ")); // partial line check
                assertTrue(fileContent.contains("[#2](https://example.com/issues/2)")); // partial line check
                assertTrue(fileContent.contains("### Fixed"));        // section header check
                assertFalse(fileContent.contains(" #1 ")); // partial line check
                assertTrue(fileContent.contains("[#1](https://example.com/issues/1)"));  // partial line check
            }
        }

        @TestTemplate
        @DisplayName("Make.run() with custom template from a local file > yield to custom changelog")
        @Baseline(Scenario.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS)
        void runTestWithCustomTemplateFromLocalFile(@CommandSelector(Commands.MAKE) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            // first create the temporary directory and the abstract destination file
            File destinationDir = Files.createTempDirectory("nyx-test-make-test-").toFile();
            destinationDir.deleteOnExit();
            // create the custom template, with simple strings used as markers
            File templateFile = new File(destinationDir, "template.tpl");
            writeFile(templateFile, "# This is a custom changelog\n            {{#releases}}\n            ## {{name}} ({{date}})\n            {{/releases}}\n");
            File changelogFile = new File(destinationDir, "CHANGELOG.md");

            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.getChangelog().setPath(changelogFile.getAbsolutePath());
            configurationLayerMock.getChangelog().setTemplate(templateFile.getAbsolutePath());
            // add the conventional commits convention
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("conventionalCommits"),
                    Map.<String,CommitMessageConvention>of("conventionalCommits", CONVENTIONAL_COMMITS))
            );
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);

            assertFalse(changelogFile.exists());

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertTrue(changelogFile.exists());

                // print the file to standard output for inspection purpose
                System.out.println("------- CHANGELOG -------");
                System.out.println("Loading from: "+changelogFile.getAbsolutePath());
                System.out.println("-----------------------------------------");
                System.out.println(readFile(changelogFile));
                System.out.println("-----------------------------------------");
                System.out.flush();

                // test the data model
                assertEquals(1, command.state().getChangelog().getReleases().size());
                assertEquals("0.1.0", command.state().getChangelog().getReleases().get(0).getName());
                assertEquals(2, command.state().getChangelog().getReleases().get(0).getSections().size());
                assertEquals("feat", command.state().getChangelog().getReleases().get(0).getSections().get(0).getName());
                assertEquals(1, command.state().getChangelog().getReleases().get(0).getSections().get(0).getCommits().size());
                assertEquals("fix", command.state().getChangelog().getReleases().get(0).getSections().get(1).getName());
                assertEquals(1, command.state().getChangelog().getReleases().get(0).getSections().get(1).getCommits().size());

                // test the rendered file
                String fileContent = readFile(changelogFile);
                assertTrue(fileContent.startsWith("# This is a custom changelog"));  // title header check
                assertTrue(fileContent.contains("## 0.1.0 "));      // release header check
            }
        }

        @TestTemplate
        @DisplayName("Make.run() with custom template from a remote URL > yield to custom changelog")
        @Baseline(Scenario.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS)
        void runTestWithCustomTemplateFromURL(@CommandSelector(Commands.MAKE) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            // first create the temporary directory and the abstract destination file
            File destinationDir = Files.createTempDirectory("nyx-test-make-test-").toFile();
            destinationDir.deleteOnExit();
            File changelogFile = new File(destinationDir, "CHANGELOG.md");

            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.getChangelog().setPath(changelogFile.getAbsolutePath());
            // add the substitution rules to replace issue IDs with links
            configurationLayerMock.getChangelog().setSubstitutions(Map.<String,String>of(
                "(?m)#([0-9]+)(?s)", "[#%s](https://example.com/issues/%s)"
            ));
            // load the preconfigured from the Nyx repository, getting the raw content, just to have it loaded from a remote URL
            configurationLayerMock.getChangelog().setTemplate("https://raw.githubusercontent.com/mooltiverse/nyx/main/src/go/nyx/command/template/changelog.tpl");
            // add the conventional commits convention
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("conventionalCommits"),
                    Map.<String,CommitMessageConvention>of("conventionalCommits", CONVENTIONAL_COMMITS))
            );
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);

            assertFalse(changelogFile.exists());

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertTrue(changelogFile.exists());

                // print the file to standard output for inspection purpose
                System.out.println("------- CHANGELOG -------");
                System.out.println("Loading from: "+changelogFile.getAbsolutePath());
                System.out.println("-----------------------------------------");
                System.out.println(readFile(changelogFile));
                System.out.println("-----------------------------------------");
                System.out.flush();

                // test the data model
                assertEquals(1, command.state().getChangelog().getReleases().size());
                assertEquals("0.1.0", command.state().getChangelog().getReleases().get(0).getName());
                assertEquals(2, command.state().getChangelog().getReleases().get(0).getSections().size());
                assertEquals("feat", command.state().getChangelog().getReleases().get(0).getSections().get(0).getName());
                assertEquals(1, command.state().getChangelog().getReleases().get(0).getSections().get(0).getCommits().size());
                assertEquals("fix", command.state().getChangelog().getReleases().get(0).getSections().get(1).getName());
                assertEquals(1, command.state().getChangelog().getReleases().get(0).getSections().get(1).getCommits().size());

                // test the rendered file
                String fileContent = readFile(changelogFile);
                assertTrue(fileContent.startsWith("# Changelog"));  // title header check
                assertTrue(fileContent.contains("## 0.1.0 "));      // release header check
            }
        }

        @TestTemplate
        @DisplayName("Make.run() with existing changelog file and append=head > yield to new changelog with new content inserted on top of previous content")
        @Baseline(Scenario.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS)
        void runTestWithExistingFileAndAppendingToHead(@CommandSelector(Commands.MAKE) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            // first create the temporary directory and the abstract destination file
            File destinationDir = Files.createTempDirectory("nyx-test-make-test-").toFile();
            destinationDir.deleteOnExit();

            // create the custom template, with simple strings used as markers
            File templateFile = new File(destinationDir, "template.tpl");
            // this template only writes static content, which is easier to match after the changelog has been generated
            writeFile(templateFile, "NEW CHANGELOG CONTENT\n");
            File changelogFile = new File(destinationDir, "CHANGELOG.md");

            // create a changelog with some existing content so we can test where content is appended
            writeFile(changelogFile, "PREVIOUS CHANGELOG CONTENT\n");

            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.getChangelog().setAppend("head");
            configurationLayerMock.getChangelog().setPath(changelogFile.getAbsolutePath());
            configurationLayerMock.getChangelog().setTemplate(templateFile.getAbsolutePath());
            // add the conventional commits convention
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("conventionalCommits"),
                    Map.<String,CommitMessageConvention>of("conventionalCommits", CONVENTIONAL_COMMITS))
            );
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);

            assertTrue(changelogFile.exists());

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertTrue(changelogFile.exists());

                // print the file to standard output for inspection purpose
                System.out.println("------- CHANGELOG -------");
                System.out.println("Loading from: "+changelogFile.getAbsolutePath());
                System.out.println("-----------------------------------------");
                System.out.println(readFile(changelogFile));
                System.out.println("-----------------------------------------");
                System.out.flush();

                // test the rendered file
                String fileContent = readFile(changelogFile).replaceAll("\\r", ""); // remove \r to make test work on windows too
                assertEquals("NEW CHANGELOG CONTENT\nPREVIOUS CHANGELOG CONTENT\n", fileContent);

                // run again and make sure values didn't change
                command.run();

                // test the rendered file again
                fileContent = readFile(changelogFile).replaceAll("\\r", ""); // remove \r to make test work on windows too
                assertEquals("NEW CHANGELOG CONTENT\nPREVIOUS CHANGELOG CONTENT\n", fileContent);
            }
        }

        @TestTemplate
        @DisplayName("Make.run() with existing changelog file and append=tail > yield to new changelog with new content inserted after previous content")
        @Baseline(Scenario.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS)
        void runTestWithExistingFileAndAppendingToTail(@CommandSelector(Commands.MAKE) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            // first create the temporary directory and the abstract destination file
            File destinationDir = Files.createTempDirectory("nyx-test-make-test-").toFile();
            destinationDir.deleteOnExit();

            // create the custom template, with simple strings used as markers
            File templateFile = new File(destinationDir, "template.tpl");
            // this template only writes static content, which is easier to match after the changelog has been generated
            writeFile(templateFile, "NEW CHANGELOG CONTENT\n");
            File changelogFile = new File(destinationDir, "CHANGELOG.md");

            // create a changelog with some existing content so we can test where content is appended
            writeFile(changelogFile, "PREVIOUS CHANGELOG CONTENT\n");

            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.getChangelog().setAppend("tail");
            configurationLayerMock.getChangelog().setPath(changelogFile.getAbsolutePath());
            configurationLayerMock.getChangelog().setTemplate(templateFile.getAbsolutePath());
            // add the conventional commits convention
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("conventionalCommits"),
                    Map.<String,CommitMessageConvention>of("conventionalCommits", CONVENTIONAL_COMMITS))
            );
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);

            assertTrue(changelogFile.exists());

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertTrue(changelogFile.exists());

                // print the file to standard output for inspection purpose
                System.out.println("------- CHANGELOG -------");
                System.out.println("Loading from: "+changelogFile.getAbsolutePath());
                System.out.println("-----------------------------------------");
                System.out.println(readFile(changelogFile));
                System.out.println("-----------------------------------------");
                System.out.flush();

                // test the rendered file
                String fileContent = readFile(changelogFile).replaceAll("\\r", "");  // remove \r to make test work on windows too
                assertEquals("PREVIOUS CHANGELOG CONTENT\nNEW CHANGELOG CONTENT\n", fileContent);

                // run again and make sure values didn't change
                command.run();

                // test the rendered file again
                fileContent = readFile(changelogFile).replaceAll("\\r", ""); // remove \r to make test work on windows too
                assertEquals("PREVIOUS CHANGELOG CONTENT\nNEW CHANGELOG CONTENT\n", fileContent);
            }
        }

        @TestTemplate
        @DisplayName("Make.run() with substitutions using the 'cargo_version' preset")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runTestWithSubstitutionsUsingCargoVersionPreset(@CommandSelector(Commands.MAKE) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            // first create the temporary directory and a subdirectory
            File destinationDir = script.getWorkingDirectory();
            //Files.createTempDirectory("nyx-test-make-test-").toFile();
            File destinationSubDir = new File(destinationDir, "sub");
            destinationSubDir.mkdirs();
            destinationSubDir.deleteOnExit();
            destinationDir.deleteOnExit();

            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.setSubstitutions(
                new Substitutions(
                    List.<String>of("cargo_version"),
                    Map.<String,Substitution>of("cargo_version", com.mooltiverse.oss.nyx.configuration.presets.Substitutions.CARGO_VERSION)
                )
            );
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);

            File cargoFileInRootDirectory = new File(destinationDir, "Cargo.toml");
            File cargoFileInSubDirectory = new File(destinationSubDir, "Cargo.toml");
            File otherFileInRootDirectory = new File(destinationDir, "other.txt");
            File otherFileInSubDirectory = new File(destinationSubDir, "other.txt");
            List<File> files = List.<File>of(
                cargoFileInRootDirectory,
                cargoFileInSubDirectory,
                otherFileInRootDirectory,
                otherFileInSubDirectory
            );

            FileWriter writer = new FileWriter(cargoFileInRootDirectory);
            writer.write("[package]\n");
            writer.write("name = \"hello_world\" # the name of the package\n");
            writer.write("version = \"91.92.93\" # the current version, obeying semver\n");
            writer.write("authors = [\"Alice <a@example.com>\", \"Bob <b@example.com>\"]\n");
            writer.flush();
            writer.close();

            writer = new FileWriter(cargoFileInSubDirectory);
            writer.write("[package]\n");
            writer.write("name = \"hello_world\" # the name of the package\n");
            writer.write("   version   =     \"91.92.93\" # the current version, obeying semver\n");
            writer.write("authors = [\"Alice <a@example.com>\", \"Bob <b@example.com>\"]\n");
            writer.flush();
            writer.close();

            writer = new FileWriter(otherFileInRootDirectory);
            writer.write("Some text file not to be touched by the command");
            writer.flush();
            writer.close();

            writer = new FileWriter(otherFileInSubDirectory);
            writer.write("Some text file not to be touched by the command");
            writer.flush();
            writer.close();

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                // print the files to standard output for inspection purpose
                for (File f: files) {
                    System.out.println("--------------------- "+f.getAbsolutePath()+" ---------------------");
                    System.out.println(readFile(f));
                    System.out.println("-----------------------------------------");
                    System.out.flush();
                }

                // test the rendered files
                String fileContent = readFile(cargoFileInRootDirectory);
                assertFalse(fileContent.contains("91.92.93"));
                assertTrue(fileContent.contains("version = \"0.1.0\""));

                fileContent = readFile(cargoFileInSubDirectory);
                assertFalse(fileContent.contains("91.92.93"));
                assertTrue(fileContent.contains("version = \"0.1.0\""));

                fileContent = readFile(otherFileInRootDirectory);
                assertEquals("Some text file not to be touched by the command", fileContent);

                fileContent = readFile(otherFileInSubDirectory);
                assertEquals("Some text file not to be touched by the command", fileContent);
            }
        }

        @TestTemplate
        @DisplayName("Make.run() with substitutions using the 'composer_version' preset")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runTestWithSubstitutionsUsingComposerVersionPreset(@CommandSelector(Commands.MAKE) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            // first create the temporary directory and a subdirectory
            File destinationDir = script.getWorkingDirectory();
            //Files.createTempDirectory("nyx-test-make-test-").toFile();
            File destinationSubDir = new File(destinationDir, "sub");
            destinationSubDir.mkdirs();
            destinationSubDir.deleteOnExit();
            destinationDir.deleteOnExit();

            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.setSubstitutions(
                new Substitutions(
                    List.<String>of("composer_version"),
                    Map.<String,Substitution>of("composer_version", com.mooltiverse.oss.nyx.configuration.presets.Substitutions.COMPOSER_VERSION)
                )
            );
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);

            File composerFileInRootDirectory = new File(destinationDir, "composer.json");
            File composerFileInSubDirectory = new File(destinationSubDir, "composer.json");
            File otherFileInRootDirectory = new File(destinationDir, "other.txt");
            File otherFileInSubDirectory = new File(destinationSubDir, "other.txt");
            List<File> files = List.<File>of(
                composerFileInRootDirectory,
                composerFileInSubDirectory,
                otherFileInRootDirectory,
                otherFileInSubDirectory
            );

            FileWriter writer = new FileWriter(composerFileInRootDirectory);
            writer.write("{\n");
            writer.write("    \"version\": \"91.92.93\"\n");
            writer.write("}\n");
            writer.flush();
            writer.close();

            writer = new FileWriter(composerFileInSubDirectory);
            writer.write("{\n");
            writer.write("    \"version\"  :      \"91.92.93\"\n");
            writer.write("}\n");
            writer.flush();
            writer.close();

            writer = new FileWriter(otherFileInRootDirectory);
            writer.write("Some text file not to be touched by the command");
            writer.flush();
            writer.close();

            writer = new FileWriter(otherFileInSubDirectory);
            writer.write("Some text file not to be touched by the command");
            writer.flush();
            writer.close();

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                // print the files to standard output for inspection purpose
                for (File f: files) {
                    System.out.println("--------------------- "+f.getAbsolutePath()+" ---------------------");
                    System.out.println(readFile(f));
                    System.out.println("-----------------------------------------");
                    System.out.flush();
                }

                // test the rendered files
                String fileContent = readFile(composerFileInRootDirectory);
                assertFalse(fileContent.contains("91.92.93"));
                assertTrue(fileContent.contains("\"version\": \"0.1.0\""));

                fileContent = readFile(composerFileInSubDirectory);
                assertFalse(fileContent.contains("91.92.93"));
                assertTrue(fileContent.contains("\"version\": \"0.1.0\""));

                fileContent = readFile(otherFileInRootDirectory);
                assertEquals("Some text file not to be touched by the command", fileContent);

                fileContent = readFile(otherFileInSubDirectory);
                assertEquals("Some text file not to be touched by the command", fileContent);
            }
        }

        @TestTemplate
        @DisplayName("Make.run() with substitutions using the 'dart_version' preset")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runTestWithSubstitutionsUsingDartVersionPreset(@CommandSelector(Commands.MAKE) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            // first create the temporary directory and a subdirectory
            File destinationDir = script.getWorkingDirectory();
            //Files.createTempDirectory("nyx-test-make-test-").toFile();
            File destinationSubDir = new File(destinationDir, "sub");
            destinationSubDir.mkdirs();
            destinationSubDir.deleteOnExit();
            destinationDir.deleteOnExit();

            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.setSubstitutions(
                new Substitutions(
                    List.<String>of("dart_version"),
                    Map.<String,Substitution>of("dart_version", com.mooltiverse.oss.nyx.configuration.presets.Substitutions.DART_VERSION)
                )
            );
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);

            File dartFileInRootDirectory = new File(destinationDir, "pubspec.yaml");
            File dartFileInSubDirectory = new File(destinationSubDir, "pubspec.yaml");
            File otherFileInRootDirectory = new File(destinationDir, "other.txt");
            File otherFileInSubDirectory = new File(destinationSubDir, "other.txt");
            List<File> files = List.<File>of(
                dartFileInRootDirectory,
                dartFileInSubDirectory,
                otherFileInRootDirectory,
                otherFileInSubDirectory
            );

            FileWriter writer = new FileWriter(dartFileInRootDirectory);
            writer.write("name: newtify\n");
            writer.write("description: >-\n");
            writer.write("    Blah blah\n");
            writer.write("version: 91.92.93\n");
            writer.write("homepage: https://example-pet-store.com/newtify\n");
            writer.flush();
            writer.close();

            writer = new FileWriter(dartFileInSubDirectory);
            writer.write("name: newtify\n");
            writer.write("description: >-\n");
            writer.write("    Blah blah\n");
            writer.write("version: 91.92.93\n");
            writer.write("homepage: https://example-pet-store.com/newtify\n");
            writer.flush();
            writer.close();

            writer = new FileWriter(otherFileInRootDirectory);
            writer.write("Some text file not to be touched by the command");
            writer.flush();
            writer.close();

            writer = new FileWriter(otherFileInSubDirectory);
            writer.write("Some text file not to be touched by the command");
            writer.flush();
            writer.close();

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                // print the files to standard output for inspection purpose
                for (File f: files) {
                    System.out.println("--------------------- "+f.getAbsolutePath()+" ---------------------");
                    System.out.println(readFile(f));
                    System.out.println("-----------------------------------------");
                    System.out.flush();
                }

                // test the rendered files
                String fileContent = readFile(dartFileInRootDirectory);
                assertFalse(fileContent.contains("91.92.93"));
                assertTrue(fileContent.contains("version: \"0.1.0\""));

                fileContent = readFile(dartFileInSubDirectory);
                assertFalse(fileContent.contains("91.92.93"));
                assertTrue(fileContent.contains("version: \"0.1.0\""));

                fileContent = readFile(otherFileInRootDirectory);
                assertEquals("Some text file not to be touched by the command", fileContent);

                fileContent = readFile(otherFileInSubDirectory);
                assertEquals("Some text file not to be touched by the command", fileContent);
            }
        }

        @TestTemplate
        @DisplayName("Make.run() with substitutions using the 'elixir_version' preset")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runTestWithSubstitutionsUsingElixirVersionPreset(@CommandSelector(Commands.MAKE) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            // first create the temporary directory and a subdirectory
            File destinationDir = script.getWorkingDirectory();
            //Files.createTempDirectory("nyx-test-make-test-").toFile();
            File destinationSubDir = new File(destinationDir, "sub");
            destinationSubDir.mkdirs();
            destinationSubDir.deleteOnExit();
            destinationDir.deleteOnExit();

            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.setSubstitutions(
                new Substitutions(
                    List.<String>of("elixir_version"),
                    Map.<String,Substitution>of("elixir_version", com.mooltiverse.oss.nyx.configuration.presets.Substitutions.ELIXIR_VERSION)
                )
            );
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);

            File elixirFileInRootDirectory = new File(destinationDir, "mix.exs");
            File elixirFileInSubDirectory = new File(destinationSubDir, "mix.exs");
            File otherFileInRootDirectory = new File(destinationDir, "other.txt");
            File otherFileInSubDirectory = new File(destinationSubDir, "other.txt");
            List<File> files = List.<File>of(
                elixirFileInRootDirectory,
                elixirFileInSubDirectory,
                otherFileInRootDirectory,
                otherFileInSubDirectory
            );

            FileWriter writer = new FileWriter(elixirFileInRootDirectory);
            writer.write("defmodule KV.MixProject do\n");
            writer.write("  use Mix.Project\n");
            writer.write("  def project do\n");
            writer.write("    [\n");
            writer.write("      app: :kv,\n");
            writer.write("      version: \"91.92.93\",\n");
            writer.write("      elixir: \"~> 1.11\",\n");
            writer.write("    ]\n");
            writer.write("  end\n");
            writer.flush();
            writer.close();

            writer = new FileWriter(elixirFileInSubDirectory);
            writer.write("defmodule KV.MixProject do\n");
            writer.write("  use Mix.Project\n");
            writer.write("  def project do\n");
            writer.write("    [\n");
            writer.write("      app: :kv,\n");
            writer.write("      version   :     \"91.92.93\"   ,\n");
            writer.write("      elixir: \"~> 1.11\",\n");
            writer.write("    ]\n");
            writer.write("  end\n");
            writer.flush();
            writer.close();

            writer = new FileWriter(otherFileInRootDirectory);
            writer.write("Some text file not to be touched by the command");
            writer.flush();
            writer.close();

            writer = new FileWriter(otherFileInSubDirectory);
            writer.write("Some text file not to be touched by the command");
            writer.flush();
            writer.close();

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                // print the files to standard output for inspection purpose
                for (File f: files) {
                    System.out.println("--------------------- "+f.getAbsolutePath()+" ---------------------");
                    System.out.println(readFile(f));
                    System.out.println("-----------------------------------------");
                    System.out.flush();
                }

                // test the rendered files
                String fileContent = readFile(elixirFileInRootDirectory);
                assertFalse(fileContent.contains("91.92.93"));
                assertTrue(fileContent.contains("version: \"0.1.0\""));

                fileContent = readFile(elixirFileInSubDirectory);
                assertFalse(fileContent.contains("91.92.93"));
                assertTrue(fileContent.contains("version: \"0.1.0\""));

                fileContent = readFile(otherFileInRootDirectory);
                assertEquals("Some text file not to be touched by the command", fileContent);

                fileContent = readFile(otherFileInSubDirectory);
                assertEquals("Some text file not to be touched by the command", fileContent);
            }
        }

        @TestTemplate
        @DisplayName("Make.run() with substitutions using the 'expo_version' preset")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runTestWithSubstitutionsUsingExpoVersionPreset(@CommandSelector(Commands.MAKE) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            // first create the temporary directory and a subdirectory
            File destinationDir = script.getWorkingDirectory();
            //Files.createTempDirectory("nyx-test-make-test-").toFile();
            File destinationSubDir = new File(destinationDir, "sub");
            destinationSubDir.mkdirs();
            destinationSubDir.deleteOnExit();
            destinationDir.deleteOnExit();

            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.setSubstitutions(
                new Substitutions(
                    List.<String>of("expo_version"),
                    Map.<String,Substitution>of("expo_version", com.mooltiverse.oss.nyx.configuration.presets.Substitutions.EXPO_VERSION)
                )
            );
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);

            File expoFileInRootDirectory = new File(destinationDir, "app.json");
            File expoFileInSubDirectory = new File(destinationSubDir, "app.config.json");
            File otherFileInRootDirectory = new File(destinationDir, "other.txt");
            File otherFileInSubDirectory = new File(destinationSubDir, "other.txt");
            List<File> files = List.<File>of(
                expoFileInRootDirectory,
                expoFileInSubDirectory,
                otherFileInRootDirectory,
                otherFileInSubDirectory
            );

            FileWriter writer = new FileWriter(expoFileInRootDirectory);
            writer.write("{\n");
            writer.write("    \"expo\": {\n");
            writer.write("        \"name\": \"appname\",\n");
            writer.write("        \"slug\": \"appslug\",\n");
            writer.write("        \"version\": \"91.92.93\",\n");
            writer.write("        \"orientation\": \"portrait\",\n");
            writer.write("    }\n");
            writer.write("}\n");
            writer.flush();
            writer.close();

            writer = new FileWriter(expoFileInSubDirectory);
            writer.write("{\n");
            writer.write("    \"expo\": {\n");
            writer.write("        \"name\": \"appname\",\n");
            writer.write("        \"slug\": \"appslug\",\n");
            writer.write("        \"version\"   :     \"91.92.93\",\n");
            writer.write("        \"orientation\": \"portrait\",\n");
            writer.write("    }\n");
            writer.write("}\n");
            writer.flush();
            writer.close();

            writer = new FileWriter(otherFileInRootDirectory);
            writer.write("Some text file not to be touched by the command");
            writer.flush();
            writer.close();

            writer = new FileWriter(otherFileInSubDirectory);
            writer.write("Some text file not to be touched by the command");
            writer.flush();
            writer.close();

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                // print the files to standard output for inspection purpose
                for (File f: files) {
                    System.out.println("--------------------- "+f.getAbsolutePath()+" ---------------------");
                    System.out.println(readFile(f));
                    System.out.println("-----------------------------------------");
                    System.out.flush();
                }

                // test the rendered files
                String fileContent = readFile(expoFileInRootDirectory);
                assertFalse(fileContent.contains("91.92.93"));
                assertTrue(fileContent.contains("\"version\": \"0.1.0\""));

                fileContent = readFile(expoFileInRootDirectory);
                assertFalse(fileContent.contains("91.92.93"));
                assertTrue(fileContent.contains("\"version\": \"0.1.0\""));

                fileContent = readFile(otherFileInRootDirectory);
                assertEquals("Some text file not to be touched by the command", fileContent);

                fileContent = readFile(otherFileInSubDirectory);
                assertEquals("Some text file not to be touched by the command", fileContent);
            }
        }

        @TestTemplate
        @DisplayName("Make.run() with substitutions using the 'helm_version' preset")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runTestWithSubstitutionsUsingHelmVersionPreset(@CommandSelector(Commands.MAKE) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            // first create the temporary directory and a subdirectory
            File destinationDir = script.getWorkingDirectory();
            //Files.createTempDirectory("nyx-test-make-test-").toFile();
            File destinationSubDir = new File(destinationDir, "sub");
            destinationSubDir.mkdirs();
            destinationSubDir.deleteOnExit();
            destinationDir.deleteOnExit();

            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.setSubstitutions(
                new Substitutions(
                    List.<String>of("helm_version"),
                    Map.<String,Substitution>of("helm_version", com.mooltiverse.oss.nyx.configuration.presets.Substitutions.HELM_VERSION)
                )
            );
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);

            File helmFileInRootDirectory = new File(destinationDir, "Chart.yaml");
            File helmFileInSubDirectory = new File(destinationSubDir, "Chart.yaml");
            File otherFileInRootDirectory = new File(destinationDir, "other.txt");
            File otherFileInSubDirectory = new File(destinationSubDir, "other.txt");
            List<File> files = List.<File>of(
                helmFileInRootDirectory,
                helmFileInSubDirectory,
                otherFileInRootDirectory,
                otherFileInSubDirectory
            );

            FileWriter writer = new FileWriter(helmFileInRootDirectory);
            writer.write("apiVersion: The chart API version\n");
            writer.write("name: The name of the chart (required)\n");
            writer.write("version: \"91.92.93\"\n");
            writer.write("kubeVersion: A SemVer range of compatible Kubernetes versions (optional)\n");
            writer.write("description: A single-sentence description of this project (optional)\n");
            writer.flush();
            writer.close();

            writer = new FileWriter(helmFileInSubDirectory);
            writer.write("apiVersion: The chart API version\n");
            writer.write("name: The name of the chart (required)\n");
            writer.write("version: \"91.92.93\"\n");
            writer.write("kubeVersion: A SemVer range of compatible Kubernetes versions (optional)\n");
            writer.write("description: A single-sentence description of this project (optional)\n");
            writer.flush();
            writer.close();

            writer = new FileWriter(otherFileInRootDirectory);
            writer.write("Some text file not to be touched by the command");
            writer.flush();
            writer.close();

            writer = new FileWriter(otherFileInSubDirectory);
            writer.write("Some text file not to be touched by the command");
            writer.flush();
            writer.close();

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                // print the files to standard output for inspection purpose
                for (File f: files) {
                    System.out.println("--------------------- "+f.getAbsolutePath()+" ---------------------");
                    System.out.println(readFile(f));
                    System.out.println("-----------------------------------------");
                    System.out.flush();
                }

                // test the rendered files
                String fileContent = readFile(helmFileInRootDirectory);
                assertFalse(fileContent.contains("91.92.93"));
                assertTrue(fileContent.contains("version: \"0.1.0\""));

                fileContent = readFile(helmFileInSubDirectory);
                assertFalse(fileContent.contains("91.92.93"));
                assertTrue(fileContent.contains("version: \"0.1.0\""));

                fileContent = readFile(otherFileInRootDirectory);
                assertEquals("Some text file not to be touched by the command", fileContent);

                fileContent = readFile(otherFileInSubDirectory);
                assertEquals("Some text file not to be touched by the command", fileContent);
            }
        }

        @TestTemplate
        @DisplayName("Make.run() with substitutions using the 'node_version' preset")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runTestWithSubstitutionsUsingNodeVersionPreset(@CommandSelector(Commands.MAKE) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            // first create the temporary directory and a subdirectory
            File destinationDir = script.getWorkingDirectory();
            //Files.createTempDirectory("nyx-test-make-test-").toFile();
            File destinationSubDir = new File(destinationDir, "sub");
            destinationSubDir.mkdirs();
            destinationSubDir.deleteOnExit();
            destinationDir.deleteOnExit();

            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.setSubstitutions(
                new Substitutions(
                    List.<String>of("node_version"),
                    Map.<String,Substitution>of("node_version", com.mooltiverse.oss.nyx.configuration.presets.Substitutions.NODE_VERSION)
                )
            );
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);

            File nodeFileInRootDirectory = new File(destinationDir, "package.json");
            File nodeFileInSubDirectory = new File(destinationSubDir, "package.json");
            File otherFileInRootDirectory = new File(destinationDir, "other.txt");
            File otherFileInSubDirectory = new File(destinationSubDir, "other.txt");
            List<File> files = List.<File>of(
                nodeFileInRootDirectory,
                nodeFileInSubDirectory,
                otherFileInRootDirectory,
                otherFileInSubDirectory
            );

            FileWriter writer = new FileWriter(nodeFileInRootDirectory);
            writer.write("{\n");
            writer.write("    \"name\": \"foo\",\n");
            writer.write("    \"version\": \"91.92.93\",\n");
            writer.write("    \"description\": \"A packaged foo fooer for fooing foos\",\n");
            writer.write("}\n");
            writer.flush();
            writer.close();

            writer = new FileWriter(nodeFileInSubDirectory);
            writer.write("{\n");
            writer.write("    \"name\": \"foo\",\n");
            writer.write("    \"version\"   :     \"91.92.93\",\n");
            writer.write("    \"description\": \"A packaged foo fooer for fooing foos\",\n");
            writer.write("}\n");
            writer.flush();
            writer.close();

            writer = new FileWriter(otherFileInRootDirectory);
            writer.write("Some text file not to be touched by the command");
            writer.flush();
            writer.close();

            writer = new FileWriter(otherFileInSubDirectory);
            writer.write("Some text file not to be touched by the command");
            writer.flush();
            writer.close();

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                // print the files to standard output for inspection purpose
                for (File f: files) {
                    System.out.println("--------------------- "+f.getAbsolutePath()+" ---------------------");
                    System.out.println(readFile(f));
                    System.out.println("-----------------------------------------");
                    System.out.flush();
                }

                // test the rendered files
                String fileContent = readFile(nodeFileInRootDirectory);
                assertFalse(fileContent.contains("91.92.93"));
                assertTrue(fileContent.contains("\"version\": \"0.1.0\""));

                fileContent = readFile(nodeFileInSubDirectory);
                assertFalse(fileContent.contains("91.92.93"));
                assertTrue(fileContent.contains("\"version\": \"0.1.0\""));

                fileContent = readFile(otherFileInRootDirectory);
                assertEquals("Some text file not to be touched by the command", fileContent);

                fileContent = readFile(otherFileInSubDirectory);
                assertEquals("Some text file not to be touched by the command", fileContent);
            }
        }

        @TestTemplate
        @DisplayName("Make.run() with substitutions using the 'text_version' preset")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runTestWithSubstitutionsUsingTextVersionPreset(@CommandSelector(Commands.MAKE) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            // first create the temporary directory and a subdirectory
            File destinationDir = script.getWorkingDirectory();
            //Files.createTempDirectory("nyx-test-make-test-").toFile();
            File destinationSubDir = new File(destinationDir, "sub");
            destinationSubDir.mkdirs();
            destinationSubDir.deleteOnExit();
            destinationDir.deleteOnExit();

            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.setSubstitutions(
                new Substitutions(
                    List.<String>of("text_version"),
                    Map.<String,Substitution>of("text_version", com.mooltiverse.oss.nyx.configuration.presets.Substitutions.TEXT_VERSION)
                )
            );
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);

            File textFileInRootDirectory = new File(destinationDir, "version.txt");
            File textFileInSubDirectory = new File(destinationSubDir, "version.txt");
            File otherFileInRootDirectory = new File(destinationDir, "other.txt");
            File otherFileInSubDirectory = new File(destinationSubDir, "other.txt");
            List<File> files = List.<File>of(
                textFileInRootDirectory,
                textFileInSubDirectory,
                otherFileInRootDirectory,
                otherFileInSubDirectory
            );

            FileWriter writer = new FileWriter(textFileInRootDirectory);
            writer.write("91.92.93");
            writer.flush();
            writer.close();

            writer = new FileWriter(textFileInSubDirectory);
            writer.write("91.92.93");
            writer.flush();
            writer.close();

            writer = new FileWriter(otherFileInRootDirectory);
            writer.write("Some text file not to be touched by the command");
            writer.flush();
            writer.close();

            writer = new FileWriter(otherFileInSubDirectory);
            writer.write("Some text file not to be touched by the command");
            writer.flush();
            writer.close();

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                // print the files to standard output for inspection purpose
                for (File f: files) {
                    System.out.println("--------------------- "+f.getAbsolutePath()+" ---------------------");
                    System.out.println(readFile(f));
                    System.out.println("-----------------------------------------");
                    System.out.flush();
                }

                // test the rendered files
                String fileContent = readFile(textFileInRootDirectory);
                assertEquals("0.1.0", fileContent);

                fileContent = readFile(textFileInSubDirectory);
                assertEquals("0.1.0", fileContent);

                fileContent = readFile(otherFileInRootDirectory);
                assertEquals("Some text file not to be touched by the command", fileContent);

                fileContent = readFile(otherFileInSubDirectory);
                assertEquals("Some text file not to be touched by the command", fileContent);
            }
        }

        @TestTemplate
        @DisplayName("Make.run() with conventional commit for merge message convention and without sections > yield to changelog where sections are commit types")
        @Baseline(Scenario.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS_FOR_MERGE)
        void runTestWithConventionalCommitsForMergeConventionAndWithoutSections(@CommandSelector(Commands.MAKE) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            // first create the temporary directory and the abstract destination file
            File destinationDir = Files.createTempDirectory("nyx-test-make-test-").toFile();
            destinationDir.deleteOnExit();
            File changelogFile = new File(destinationDir, "CHANGELOG.md");

            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.getChangelog().setPath(changelogFile.getAbsolutePath());
            // add the conventional commits for merge convention
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("conventionalCommitsForMerge"),
                    Map.<String,CommitMessageConvention>of("conventionalCommitsForMerge", CONVENTIONAL_COMMITS_FOR_MERGE))
            );
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);

            assertFalse(changelogFile.exists());

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertTrue(changelogFile.exists());

                // print the file to standard output for inspection purpose
                System.out.println("------- CHANGELOG -------");
                System.out.println("Loading from: "+changelogFile.getAbsolutePath());
                System.out.println("-----------------------------------------");
                System.out.println(readFile(changelogFile));
                System.out.println("-----------------------------------------");
                System.out.flush();

                // test the data model
                assertEquals(1, command.state().getChangelog().getReleases().size());
                assertEquals("1.0.0", command.state().getChangelog().getReleases().get(0).getName());
                assertEquals(2, command.state().getChangelog().getReleases().get(0).getSections().size());
                assertEquals("fix", command.state().getChangelog().getReleases().get(0).getSections().get(0).getName());
                assertEquals(1, command.state().getChangelog().getReleases().get(0).getSections().get(0).getCommits().size());
                assertEquals("feat", command.state().getChangelog().getReleases().get(0).getSections().get(1).getName());
                assertEquals(1, command.state().getChangelog().getReleases().get(0).getSections().get(1).getCommits().size());

                // test the rendered file
                String fileContent = readFile(changelogFile);
                assertTrue(fileContent.startsWith("# Changelog"));  // title header check
                assertTrue(fileContent.contains("## 1.0.0 "));      // release header check
                assertTrue(fileContent.contains("### feat"));       // section header check
                assertTrue(fileContent.contains("### fix"));        // section header check
                assertTrue(fileContent.contains("] Alpha ("));      // partial line check
            }
        }
    }
}