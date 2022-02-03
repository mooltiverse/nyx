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
import com.mooltiverse.oss.nyx.git.GitException;
import com.mooltiverse.oss.nyx.git.Scenario;
import com.mooltiverse.oss.nyx.git.Script;

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
        void constructorTest(@CommandSelector(Commands.MAKE) CommandProxy command)
            throws Exception {
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
        void stateTest(@CommandSelector(Commands.MAKE) CommandProxy command)
            throws Exception {
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
            // first create the temporary directory and the abstract destination file
            File destinationDir = Files.createTempDirectory("nyx-test-make-test-").toFile();
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
    @DisplayName("Make run")
    @ExtendWith(CommandInvocationContextProvider.class)
    public static class RunTests {
        @TestTemplate
        @DisplayName("Make.run() with no destination file > yield to no changelog because there is no destination file")
        @Baseline(Scenario.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS)
        void runTestWithNoDestinationFile(@CommandSelector(Commands.MAKE) CommandProxy command)
            throws Exception {
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
        void runTestWithNoCommitConvention(@CommandSelector(Commands.MAKE) CommandProxy command)
            throws Exception {
            // first create the temporary directory and the abstract destination file
            File destinationDir = Files.createTempDirectory("nyx-test-make-test-").toFile();
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
        void runTestWithConventionalCommitsConventionAndWithoutSections(@CommandSelector(Commands.MAKE) CommandProxy command)
            throws Exception {
            // first create the temporary directory and the abstract destination file
            File destinationDir = Files.createTempDirectory("nyx-test-make-test-").toFile();
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
        void runTestWithConventionalCommitsConventionAndWithCustomSections(@CommandSelector(Commands.MAKE) CommandProxy command)
            throws Exception {
            // first create the temporary directory and the abstract destination file
            File destinationDir = Files.createTempDirectory("nyx-test-make-test-").toFile();
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
        void runTestWithConventionalCommitsConventionAndWithCustomSectionsAndSubstitutions(@CommandSelector(Commands.MAKE) CommandProxy command)
            throws Exception {
            // first create the temporary directory and the abstract destination file
            File destinationDir = Files.createTempDirectory("nyx-test-make-test-").toFile();
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
        @DisplayName("Make.run() with custom template > yield to custom changelog")
        @Baseline(Scenario.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS)
        void runTestWithCustomTemplate(@CommandSelector(Commands.MAKE) CommandProxy command)
            throws Exception {
            // first create the temporary directory and the abstract destination file
            File destinationDir = Files.createTempDirectory("nyx-test-make-test-").toFile();
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
    }
}