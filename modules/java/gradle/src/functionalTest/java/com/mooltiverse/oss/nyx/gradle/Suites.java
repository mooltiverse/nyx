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

import java.util.ArrayList;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Stream;

import org.junit.jupiter.params.provider.Arguments;

import com.mooltiverse.oss.nyx.git.tools.Scenario;

/**
 * The test suite:<br>
 * <br>
 * - defines a fixture of input data and parameters for the test
 * - defines a fixture of checks to run after Nyx execution, in the form of assertions
 * - runs the test, according to the above fixture
 * - performs the checks, according to the above fixture
 */
public class Suites {
    private static final String newLine = System.getProperty("line.separator");

    /**
     * Returns array of Gradle versions to succesfully test against.
     * 
     * The list is taken from https://gradle.org/releases/.
     * 
     * @param quickTestsOnly if {@code true} the result will contain a minimum set of significant versions,
     * otherwise all suites are returned.
     * 
     * @return a new array of test suites
     */
    public static String[] wellKnownWorkingGradleVersions(boolean quickTestsOnly) {
        String[] versions = new String[] {
            // Versions that are known to work
            "7.6", "7.5.1", "7.5", "7.4.2", "7.4.1", "7.4", "7.3.3", "7.3.2", "7.3.1", "7.3", "7.2", "7.1.1", "7.1", "7.0.2", "7.0.1", "7.0",

            // - version "6.5" has a bug (https://github.com/gradle/gradle/issues/13367) that prevents us to test, fixed in "6.5.1"
            "6.9.3", "6.9.2", "6.9.1", "6.9", "6.8.3", "6.8.2", "6.8.1", "6.8", "6.7.1", "6.7", "6.6.1", "6.6", "6.5.1", /*"6.5",*/ "6.4.1", "6.4", "6.3", "6.2.2", "6.2.1", "6.2", "6.1.1", "6.1", "6.0.1", "6.0",

            /* Gradle versions prior than 6.0 fails to test with an exception like:
                    > Could not find method services() for arguments [build_4o3mdmvy94ykemibox706yopu$_run_closure1$_closure2@18c3fdb5] on object of type com.mooltiverse.oss.nyx.gradle.NyxExtension.

            This means it has a different method for setting nested blocks into the extension object.
            If support for these versions is strongly needed we may find a workaround but it's worthless so far. */
            //"5.6.4", "5.6.3", "5.6.2", "5.6.1", "5.6", "5.5.1", "5.5"
            
            /* Gradle versions prior than 5.5 do not support ObjectFactory.domainObjectContainer​(Class<T> elementType), indeed introduced in version 5.5,
            which is used for example in NyxExtension.
            If support for these versions is strongly needed we may find a workaround but it's worthless so far. */
            //"5.4.1", "5.4", "5.3.1", "5.3", "5.2.1", "5.2"
            
            /* Gradle versions from 4.9 to 5.1.1 fail to test with an exception like:
                    > Could not create an instance of type com.mooltiverse.oss.nyx.gradle.NyxExtension_Decorated.
                        > Could not find any public constructor for class com.mooltiverse.oss.nyx.gradle.NyxExtension_Decorated which accepts parameters [].

            This has to deal with the injection of the ObjectFactory in constructors (i.e. in the NyxExtension) and is solved by adding another
            constructor with no parameters, which in turn implies another workaround to get an ObjectFactory.
            If support for these versions is strongly needed we may find a workaround but it's worthless so far. */
            //"5.1.1", "5.1", "5.0", "4.10.3", "4.10.2", "4.10.1", "4.10", "4.9"
            
            /* Gradle versions prior than 4.9 do not support Conviguration Avoidance API (https://docs.gradle.org/current/userguide/task_configuration_avoidance.html)*/
            //"4.8.1", "4.8"

            /* Gradle version 4.7 fails to test with an exception like:
                    Could not create service of type ScriptPluginFactory using BuildScopeServices.createScriptPluginFactory().
                    > Could not create service of type PluginResolutionStrategyInternal using BuildScopeServices.createPluginResolutionStrategy().

            We're not investigating any further unless some user needs support for that version, which is quite outdated*/
            //"4.7",
            
            /* Gradle versions prior between 2.6 and 4.6 fail to test with an exception like:
                    org.gradle.api.GradleException: Unable to start the daemon process.
                    ...
                    Could not create service of type DaemonContext using DaemonServices.createDaemonContext().

            We're not investigating any further unless some user needs support for those versions, which are quite outdated*/
            //"4.7", "4.6", "4.5.1", "4.5", "4.4.1", "4.4", "4.3.1", "4.3", "4.2.1", "4.2", "4.1", "4.0.2", "4.0.1", "4.0", 
            //"3.5.1", "3.5", "3.4.1", "3.4", "3.3", "3.2.1", "3.2", "3.1", "3.0",
            //"2.14.1", "2.14", "2.13", "2.12", "2.11", "2.10", "2.9", "2.8", "2.7", "2.6"
            
            /* Gradle versions prior than 2.6 are not supported by Testkit
            See: https://docs.gradle.org/current/userguide/test_kit.html#sub:test-kit-compatibility*/
            //"2.5", "2.4", "2.3", "2.2.1", "2.2", "2.1", "2.0",
            //"1.12", "1.11", "1.10", "1.9", "1.8", "1.7", "1.6", "1.5", "1.4", "1.3", "1.2", "1.1", "1.0",
            //"0.9.2", "0.9.1", "0.9", "0.8", "0.7",
        };

        // if quick tests are requested, just return the latest version (the first in the array)
        return quickTestsOnly ? new String[]{versions[0]} : versions;
    }

    /**
     * Returns a new array of test suites
     * 
     * @param quickTestsOnly if {@code true} the result will contain a minimum set of significant suites,
     * otherwise all suites are returned.
     * 
     * @return a new array of test suites
     */
    public static TestSuite[] wellKnownFunctionalTestSuites(boolean quickTestsOnly) {
        TestSuite[] wellKnownFunctionalTestSuitesQuick =  new TestSuite[]{
            // This suite runs Nyx Infer using a common configuration (using the regular Groovy Gradle plugin)
            new TestSuite(){{
                name        = "Infer [Groovy Regular Plugin]";
                args        = new String[]{"--info", "--stacktrace"}; // these are command line arguments for Gradle, not the Nyx plugin
                tasks       = new String[]{"nyxInfer"};
                scenario    = Scenario.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS;
                env         = Map.<String,String>of(
                                "NYX_PRESET", "extended"
                );
                files       = Map.<String,String>of(
                    // the .gitignore file, required to avoid conflicts due to locked files (i.e. 'The process cannot access the file because another process has locked a portion of the file') etc
                    ".gitignore", ".gitignore".concat(newLine)
                        .concat(".gradle").concat(newLine)
                        .concat("build.gradle").concat(newLine)
                        .concat("settings.gradle").concat(newLine)
                        .concat(".nyx-state.yml").concat(newLine),
                    // the Gradle build script
                    "build.gradle", "".concat(newLine)
                        .concat("plugins {").concat(newLine)
                        .concat("    id 'com.mooltiverse.oss.nyx'").concat(newLine)
                        .concat("}").concat(newLine)
                        .concat(newLine)
                        .concat("nyx {").concat(newLine)
                        .concat("    dryRun = false").concat(newLine)
                        .concat("    resume = false").concat(newLine)
                        .concat("    stateFile = '.nyx-state.yml'").concat(newLine)
                        .concat("    verbosity = \"DEBUG\"").concat(newLine)
                        .concat("}").concat(newLine),
                    // the Gradle settings file
                    "settings.gradle", "".concat(newLine)
                        .concat("rootProject.name = 'nyx-gradle-plugin-test'").concat(newLine),
                    // the .nyx-shared.yaml is the standard shared configuration file
                    ".nyx-shared.yaml", "---".concat(newLine)
                        .concat("commitMessageConventions:").concat(newLine)
                        .concat("  enabled:").concat(newLine)
                        .concat("    - conventionalCommits").concat(newLine)
                        .concat("  items:").concat(newLine)
                        .concat("    conventionalCommits:").concat(newLine)
                        .concat("      expression: \"(?m)^(?<type>[a-zA-Z0-9_]+)(!)?(\\\\((?<scope>[a-z ]+)\\\\))?:( (?<title>.+))$(?s).*\"").concat(newLine)
                        .concat("      bumpExpressions:").concat(newLine)
                        .concat("        major: \"(?s)(?m)^[a-zA-Z0-9_]+(!|.*^(BREAKING( |-)CHANGE: )).*\"").concat(newLine)
                        .concat("        minor: \"(?s)(?m)^feat(?!!|.*^(BREAKING( |-)CHANGE: )).*\"").concat(newLine)
                        .concat("        patch: \"(?s)(?m)^fix(?!!|.*^(BREAKING( |-)CHANGE: )).*\"").concat(newLine)
                        .concat("initialVersion: \"1.0.0\"").concat(newLine)
                        .concat("releaseLenient: true").concat(newLine)
                        .concat("releasePrefix: v").concat(newLine)
                        .concat("scheme: SEMVER").concat(newLine),
                    // the .nyx.json is the standard configuration file
				    ".nyx.json", "{".concat(newLine)
                        .concat("    \"releaseTypes\": {").concat(newLine)
                        .concat("        \"items\": {").concat(newLine)
                        .concat("            \"mainline\": {").concat(newLine)
                        .concat("                \"collapseVersions\":false,").concat(newLine)
                        .concat("                \"filterTags\":\"^({{configuration.releasePrefix}})?([0-9]\\\\d*)\\\\.([0-9]\\\\d*)\\\\.([0-9]\\\\d*)$\",").concat(newLine)
                        .concat("                \"gitCommit\":\"true\",").concat(newLine)
                        .concat("                \"gitCommitMessage\":\"Release version {{version}}\",").concat(newLine)
                        .concat("                \"gitPush\":\"false\",").concat(newLine)
                        .concat("                \"gitTag\":\"true\",").concat(newLine)
                        .concat("                \"gitTagMessage\":\"Tag version {{version}}\",").concat(newLine)
                        .concat("                \"matchBranches\":\"^(master|main)$\",").concat(newLine)
                        .concat("                \"publish\":\"true\"").concat(newLine)
                        .concat("            }").concat(newLine)
                        .concat("        }").concat(newLine)
                        .concat("    }").concat(newLine)
                        .concat("}")
                );
                remoteRepoName  = null;
                hostingRepoService = null;
                fileContentChecks = Map.<String,Set<String>>of(
                    ".nyx-state.yml", Set.<String>of(
                        "bump: \"minor\"",
                        "initialVersion: \"1.0.0\"",
                        "preset: \"extended\"",
                        "releaseLenient: true",
                        "releasePrefix: \"v\"",
                        "newVersion: true",
                        "newRelease: true",
                        "previousVersion: \"0.0.4\"",
                        "primeVersion: \"0.0.4\"",
                        "version: \"v0.1.0\""
                    )
                );
                repositoryTags = Set.<String>of(); // no new tag is created by Infer
                remoteRepositoryTags = Set.<String>of(); // not using remotes for this suite
                hostedReleaseTags = Set.<String>of(); // not using hosting services for this suite
            }},

            // This suite runs Nyx Infer using a common configuration (using the settings Groovy Gradle plugin)
            new TestSuite(){{
                name        = "Infer [Groovy Settings Plugin]";
                args        = new String[]{"--info", "--stacktrace"}; // these are command line arguments for Gradle, not the Nyx plugin
                tasks       = new String[]{"nyxInfer"};
                scenario    = Scenario.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS;
                env         = Map.<String,String>of(
                                "NYX_PRESET", "extended"
                );
                files       = Map.<String,String>of(
                    // the .gitignore file, required to avoid conflicts due to locked files (i.e. 'The process cannot access the file because another process has locked a portion of the file') etc
                    ".gitignore", ".gitignore".concat(newLine)
                        .concat(".gradle").concat(newLine)
                        .concat("build.gradle").concat(newLine)
                        .concat("settings.gradle").concat(newLine)
                        .concat(".nyx-state.yml").concat(newLine),
                    // the Gradle build script
                    "build.gradle", "".concat(newLine)
                        .concat("task dummy() {").concat(newLine)
                        .concat("}").concat(newLine),
                    // the Gradle settings file
                    "settings.gradle", "".concat(newLine)
                        .concat("plugins {").concat(newLine)
                        .concat("    id 'com.mooltiverse.oss.nyx'").concat(newLine)
                        .concat("}").concat(newLine)
                        .concat(newLine)
                        .concat("rootProject.name = 'nyx-gradle-plugin-test'").concat(newLine)
                        .concat(newLine)
                        .concat("nyx {").concat(newLine)
                        .concat("    dryRun = false").concat(newLine)
                        .concat("    resume = false").concat(newLine)
                        .concat("    stateFile = '.nyx-state.yml'").concat(newLine)
                        .concat("    verbosity = \"DEBUG\"").concat(newLine)
                        .concat("}").concat(newLine),
                    // the .nyx-shared.yaml is the standard shared configuration file
                    ".nyx-shared.yaml", "---".concat(newLine)
                        .concat("commitMessageConventions:").concat(newLine)
                        .concat("  enabled:").concat(newLine)
                        .concat("    - conventionalCommits").concat(newLine)
                        .concat("  items:").concat(newLine)
                        .concat("    conventionalCommits:").concat(newLine)
                        .concat("      expression: \"(?m)^(?<type>[a-zA-Z0-9_]+)(!)?(\\\\((?<scope>[a-z ]+)\\\\))?:( (?<title>.+))$(?s).*\"").concat(newLine)
                        .concat("      bumpExpressions:").concat(newLine)
                        .concat("        major: \"(?s)(?m)^[a-zA-Z0-9_]+(!|.*^(BREAKING( |-)CHANGE: )).*\"").concat(newLine)
                        .concat("        minor: \"(?s)(?m)^feat(?!!|.*^(BREAKING( |-)CHANGE: )).*\"").concat(newLine)
                        .concat("        patch: \"(?s)(?m)^fix(?!!|.*^(BREAKING( |-)CHANGE: )).*\"").concat(newLine)
                        .concat("initialVersion: \"1.0.0\"").concat(newLine)
                        .concat("releaseLenient: true").concat(newLine)
                        .concat("releasePrefix: v").concat(newLine)
                        .concat("scheme: SEMVER").concat(newLine),
                    // the .nyx.json is the standard configuration file
				    ".nyx.json", "{".concat(newLine)
                        .concat("    \"releaseTypes\": {").concat(newLine)
                        .concat("        \"items\": {").concat(newLine)
                        .concat("            \"mainline\": {").concat(newLine)
                        .concat("                \"collapseVersions\":false,").concat(newLine)
                        .concat("                \"filterTags\":\"^({{configuration.releasePrefix}})?([0-9]\\\\d*)\\\\.([0-9]\\\\d*)\\\\.([0-9]\\\\d*)$\",").concat(newLine)
                        .concat("                \"gitCommit\":\"true\",").concat(newLine)
                        .concat("                \"gitCommitMessage\":\"Release version {{version}}\",").concat(newLine)
                        .concat("                \"gitPush\":\"false\",").concat(newLine)
                        .concat("                \"gitTag\":\"true\",").concat(newLine)
                        .concat("                \"gitTagMessage\":\"Tag version {{version}}\",").concat(newLine)
                        .concat("                \"matchBranches\":\"^(master|main)$\",").concat(newLine)
                        .concat("                \"publish\":\"true\"").concat(newLine)
                        .concat("            }").concat(newLine)
                        .concat("        }").concat(newLine)
                        .concat("    }").concat(newLine)
                        .concat("}")
                );
                remoteRepoName  = null;
                hostingRepoService = null;
                fileContentChecks = Map.<String,Set<String>>of(
                    ".nyx-state.yml", Set.<String>of(
                        "bump: \"minor\"",
                        "initialVersion: \"1.0.0\"",
                        "preset: \"extended\"",
                        "releaseLenient: true",
                        "releasePrefix: \"v\"",
                        "newVersion: true",
                        "newRelease: true",
                        "previousVersion: \"0.0.4\"",
                        "primeVersion: \"0.0.4\"",
                        "version: \"v0.1.0\""
                    )
                );
                repositoryTags = Set.<String>of(); // no new tag is created by Infer
                remoteRepositoryTags = Set.<String>of(); // not using remotes for this suite
                hostedReleaseTags = Set.<String>of(); // not using hosting services for this suite
            }},

            // This suite runs Nyx Infer using a common configuration (using the settings Groovy Gradle plugin)
            // The version is inferred in the early stage even if Nyx's tasks are not invoked explicitly
            new TestSuite(){{
                name        = "Early Infer [Groovy Settings Plugin]";
                args        = new String[]{"--info", "--stacktrace"}; // these are command line arguments for Gradle, not the Nyx plugin
                tasks       = new String[]{"dummy"};
                scenario    = Scenario.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS;
                env         = Map.<String,String>of(
                                "NYX_PRESET", "extended"
                );
                files       = Map.<String,String>of(
                    // the .gitignore file, required to avoid conflicts due to locked files (i.e. 'The process cannot access the file because another process has locked a portion of the file') etc
                    ".gitignore", ".gitignore".concat(newLine)
                        .concat(".gradle").concat(newLine)
                        .concat("build.gradle").concat(newLine)
                        .concat("settings.gradle").concat(newLine)
                        .concat(".nyx-state.yml").concat(newLine),
                    // the Gradle build script
                    "build.gradle", "".concat(newLine)
                        .concat("task dummy() {").concat(newLine)
                        .concat("}").concat(newLine),
                    // the Gradle settings file
                    "settings.gradle", "".concat(newLine)
                        .concat("plugins {").concat(newLine)
                        .concat("    id 'com.mooltiverse.oss.nyx'").concat(newLine)
                        .concat("}").concat(newLine)
                        .concat(newLine)
                        .concat("rootProject.name = 'nyx-gradle-plugin-test'").concat(newLine)
                        .concat(newLine)
                        .concat("nyx {").concat(newLine)
                        .concat("    dryRun = false").concat(newLine)
                        .concat("    resume = false").concat(newLine)
                        .concat("    stateFile = '.nyx-state.yml'").concat(newLine)
                        .concat("    verbosity = \"DEBUG\"").concat(newLine)
                        .concat("}").concat(newLine),
                    // the .nyx-shared.yaml is the standard shared configuration file
                    ".nyx-shared.yaml", "---".concat(newLine)
                        .concat("commitMessageConventions:").concat(newLine)
                        .concat("  enabled:").concat(newLine)
                        .concat("    - conventionalCommits").concat(newLine)
                        .concat("  items:").concat(newLine)
                        .concat("    conventionalCommits:").concat(newLine)
                        .concat("      expression: \"(?m)^(?<type>[a-zA-Z0-9_]+)(!)?(\\\\((?<scope>[a-z ]+)\\\\))?:( (?<title>.+))$(?s).*\"").concat(newLine)
                        .concat("      bumpExpressions:").concat(newLine)
                        .concat("        major: \"(?s)(?m)^[a-zA-Z0-9_]+(!|.*^(BREAKING( |-)CHANGE: )).*\"").concat(newLine)
                        .concat("        minor: \"(?s)(?m)^feat(?!!|.*^(BREAKING( |-)CHANGE: )).*\"").concat(newLine)
                        .concat("        patch: \"(?s)(?m)^fix(?!!|.*^(BREAKING( |-)CHANGE: )).*\"").concat(newLine)
                        .concat("initialVersion: \"1.0.0\"").concat(newLine)
                        .concat("releaseLenient: true").concat(newLine)
                        .concat("releasePrefix: v").concat(newLine)
                        .concat("scheme: SEMVER").concat(newLine),
                    // the .nyx.json is the standard configuration file
				    ".nyx.json", "{".concat(newLine)
                        .concat("    \"releaseTypes\": {").concat(newLine)
                        .concat("        \"items\": {").concat(newLine)
                        .concat("            \"mainline\": {").concat(newLine)
                        .concat("                \"collapseVersions\":false,").concat(newLine)
                        .concat("                \"filterTags\":\"^({{configuration.releasePrefix}})?([0-9]\\\\d*)\\\\.([0-9]\\\\d*)\\\\.([0-9]\\\\d*)$\",").concat(newLine)
                        .concat("                \"gitCommit\":\"true\",").concat(newLine)
                        .concat("                \"gitCommitMessage\":\"Release version {{version}}\",").concat(newLine)
                        .concat("                \"gitPush\":\"false\",").concat(newLine)
                        .concat("                \"gitTag\":\"true\",").concat(newLine)
                        .concat("                \"gitTagMessage\":\"Tag version {{version}}\",").concat(newLine)
                        .concat("                \"matchBranches\":\"^(master|main)$\",").concat(newLine)
                        .concat("                \"publish\":\"true\"").concat(newLine)
                        .concat("            }").concat(newLine)
                        .concat("        }").concat(newLine)
                        .concat("    }").concat(newLine)
                        .concat("}")
                );
                remoteRepoName  = null;
                hostingRepoService = null;
                fileContentChecks = Map.<String,Set<String>>of(
                    ".nyx-state.yml", Set.<String>of(
                        "bump: \"minor\"",
                        "initialVersion: \"1.0.0\"",
                        "preset: \"extended\"",
                        "releaseLenient: true",
                        "releasePrefix: \"v\"",
                        "newVersion: true",
                        "newRelease: true",
                        "previousVersion: \"0.0.4\"",
                        "primeVersion: \"0.0.4\"",
                        "version: \"v0.1.0\""
                    )
                );
                repositoryTags = Set.<String>of(); // no new tag is created by Infer
                remoteRepositoryTags = Set.<String>of(); // not using remotes for this suite
                hostedReleaseTags = Set.<String>of(); // not using hosting services for this suite
            }},

            // This suite runs Nyx Infer using a common configuration (using the regular Kotlin Gradle plugin)
            // TODO: make this test use Kotlin files (convert build.gradle and settings.gradle to Kotlin)
            //       and run it along with the others.
            //       There's an issue on this: https://github.com/mooltiverse/nyx/issues/98
            /*new TestSuite(){{
                name        = "Infer [Kotlin Regular Plugin]";
                args        = new String[]{"--info", "--stacktrace"}; // these are command line arguments for Gradle, not the Nyx plugin
                tasks       = new String[]{"nyxInfer"};
                scenario    = Scenario.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS;
                env         = Map.<String,String>of(
                                "NYX_PRESET", "extended"
                );
                files       = Map.<String,String>of(
                    // the .gitignore file, required to avoid conflicts due to locked files (i.e. 'The process cannot access the file because another process has locked a portion of the file') etc
                    ".gitignore", ".gitignore".concat(newLine)
                        .concat(".gradle").concat(newLine)
                        .concat("build.gradle").concat(newLine)
                        .concat("settings.gradle").concat(newLine)
                        .concat(".nyx-state.yml").concat(newLine),
                    // the Gradle build script
                    "build.gradle", "".concat(newLine)
                        .concat("plugins {").concat(newLine)
                        .concat("    id 'com.mooltiverse.oss.nyx'").concat(newLine)
                        .concat("}").concat(newLine)
                        .concat(newLine)
                        .concat("nyx {").concat(newLine)
                        .concat("    dryRun = false").concat(newLine)
                        .concat("    resume = false").concat(newLine)
                        .concat("    stateFile = '.nyx-state.yml'").concat(newLine)
                        .concat("    verbosity = \"DEBUG\"").concat(newLine)
                        .concat("}").concat(newLine),
                    // the Gradle settings file
                    "settings.gradle", "".concat(newLine)
                        .concat("rootProject.name = 'nyx-gradle-plugin-test'").concat(newLine),
                    // the .nyx-shared.yaml is the standard shared configuration file
                    ".nyx-shared.yaml", "---".concat(newLine)
                        .concat("commitMessageConventions:").concat(newLine)
                        .concat("  enabled:").concat(newLine)
                        .concat("    - conventionalCommits").concat(newLine)
                        .concat("  items:").concat(newLine)
                        .concat("    conventionalCommits:").concat(newLine)
                        .concat("      expression: \"(?m)^(?<type>[a-zA-Z0-9_]+)(!)?(\\\\((?<scope>[a-z ]+)\\\\))?:( (?<title>.+))$(?s).*\"").concat(newLine)
                        .concat("      bumpExpressions:").concat(newLine)
                        .concat("        major: \"(?s)(?m)^[a-zA-Z0-9_]+(!|.*^(BREAKING( |-)CHANGE: )).*\"").concat(newLine)
                        .concat("        minor: \"(?s)(?m)^feat(?!!|.*^(BREAKING( |-)CHANGE: )).*\"").concat(newLine)
                        .concat("        patch: \"(?s)(?m)^fix(?!!|.*^(BREAKING( |-)CHANGE: )).*\"").concat(newLine)
                        .concat("initialVersion: \"1.0.0\"").concat(newLine)
                        .concat("releaseLenient: true").concat(newLine)
                        .concat("releasePrefix: v").concat(newLine)
                        .concat("scheme: SEMVER").concat(newLine),
                    // the .nyx.json is the standard configuration file
				    ".nyx.json", "{".concat(newLine)
                        .concat("    \"releaseTypes\": {").concat(newLine)
                        .concat("        \"items\": {").concat(newLine)
                        .concat("            \"mainline\": {").concat(newLine)
                        .concat("                \"collapseVersions\":false,").concat(newLine)
                        .concat("                \"filterTags\":\"^({{configuration.releasePrefix}})?([0-9]\\\\d*)\\\\.([0-9]\\\\d*)\\\\.([0-9]\\\\d*)$\",").concat(newLine)
                        .concat("                \"gitCommit\":\"true\",").concat(newLine)
                        .concat("                \"gitCommitMessage\":\"Release version {{version}}\",").concat(newLine)
                        .concat("                \"gitPush\":\"false\",").concat(newLine)
                        .concat("                \"gitTag\":\"true\",").concat(newLine)
                        .concat("                \"gitTagMessage\":\"Tag version {{version}}\",").concat(newLine)
                        .concat("                \"matchBranches\":\"^(master|main)$\",").concat(newLine)
                        .concat("                \"publish\":\"true\"").concat(newLine)
                        .concat("            }").concat(newLine)
                        .concat("        }").concat(newLine)
                        .concat("    }").concat(newLine)
                        .concat("}")
                );
                remoteRepoName  = null;
                hostingRepoService = null;
                fileContentChecks = Map.<String,Set<String>>of(
                    ".nyx-state.yml", Set.<String>of(
                        "bump: \"minor\"",
                        "initialVersion: \"1.0.0\"",
                        "preset: \"extended\"",
                        "releaseLenient: true",
                        "releasePrefix: \"v\"",
                        "newVersion: true",
                        "newRelease: true",
                        "previousVersion: \"0.0.4\"",
                        "primeVersion: \"0.0.4\"",
                        "version: \"v0.1.0\""
                    )
                );
                repositoryTags = Set.<String>of(); // no new tag is created by Infer
                remoteRepositoryTags = Set.<String>of(); // not using remotes for this suite
                hostedReleaseTags = Set.<String>of(); // not using hosting services for this suite
            }},*/

            // This suite runs Nyx Infer using a common configuration (using the settings Kotlin Gradle plugin)
            // TODO: make this test use Kotlin files (convert build.gradle and settings.gradle to Kotlin)
            //       and run it along with the others.
            //       There's an issue on this: https://github.com/mooltiverse/nyx/issues/98
            /*new TestSuite(){{
                name        = "Infer [Kotlin Settings Plugin]";
                args        = new String[]{"--info", "--stacktrace"}; // these are command line arguments for Gradle, not the Nyx plugin
                tasks       = new String[]{"nyxInfer"};
                scenario    = Scenario.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS;
                env         = Map.<String,String>of(
                                "NYX_PRESET", "extended"
                );
                files       = Map.<String,String>of(
                    // the .gitignore file, required to avoid conflicts due to locked files (i.e. 'The process cannot access the file because another process has locked a portion of the file') etc
                    ".gitignore", ".gitignore".concat(newLine)
                        .concat(".gradle").concat(newLine)
                        .concat("build.gradle").concat(newLine)
                        .concat("settings.gradle").concat(newLine)
                        .concat(".nyx-state.yml").concat(newLine),
                    // the Gradle build script
                    "build.gradle", "".concat(newLine)
                        .concat("task dummy() {").concat(newLine)
                        .concat("}").concat(newLine),
                    // the Gradle settings file
                    "settings.gradle", "".concat(newLine)
                        .concat("plugins {").concat(newLine)
                        .concat("    id 'com.mooltiverse.oss.nyx'").concat(newLine)
                        .concat("}").concat(newLine)
                        .concat(newLine)
                        .concat("rootProject.name = 'nyx-gradle-plugin-test'").concat(newLine)
                        .concat(newLine)
                        .concat("nyx {").concat(newLine)
                        .concat("    dryRun = false").concat(newLine)
                        .concat("    resume = false").concat(newLine)
                        .concat("    stateFile = '.nyx-state.yml'").concat(newLine)
                        .concat("    verbosity = \"DEBUG\"").concat(newLine)
                        .concat("}").concat(newLine),
                    // the .nyx-shared.yaml is the standard shared configuration file
                    ".nyx-shared.yaml", "---".concat(newLine)
                        .concat("commitMessageConventions:").concat(newLine)
                        .concat("  enabled:").concat(newLine)
                        .concat("    - conventionalCommits").concat(newLine)
                        .concat("  items:").concat(newLine)
                        .concat("    conventionalCommits:").concat(newLine)
                        .concat("      expression: \"(?m)^(?<type>[a-zA-Z0-9_]+)(!)?(\\\\((?<scope>[a-z ]+)\\\\))?:( (?<title>.+))$(?s).*\"").concat(newLine)
                        .concat("      bumpExpressions:").concat(newLine)
                        .concat("        major: \"(?s)(?m)^[a-zA-Z0-9_]+(!|.*^(BREAKING( |-)CHANGE: )).*\"").concat(newLine)
                        .concat("        minor: \"(?s)(?m)^feat(?!!|.*^(BREAKING( |-)CHANGE: )).*\"").concat(newLine)
                        .concat("        patch: \"(?s)(?m)^fix(?!!|.*^(BREAKING( |-)CHANGE: )).*\"").concat(newLine)
                        .concat("initialVersion: \"1.0.0\"").concat(newLine)
                        .concat("releaseLenient: true").concat(newLine)
                        .concat("releasePrefix: v").concat(newLine)
                        .concat("scheme: SEMVER").concat(newLine),
                    // the .nyx.json is the standard configuration file
				    ".nyx.json", "{".concat(newLine)
                        .concat("    \"releaseTypes\": {").concat(newLine)
                        .concat("        \"items\": {").concat(newLine)
                        .concat("            \"mainline\": {").concat(newLine)
                        .concat("                \"collapseVersions\":false,").concat(newLine)
                        .concat("                \"filterTags\":\"^({{configuration.releasePrefix}})?([0-9]\\\\d*)\\\\.([0-9]\\\\d*)\\\\.([0-9]\\\\d*)$\",").concat(newLine)
                        .concat("                \"gitCommit\":\"true\",").concat(newLine)
                        .concat("                \"gitCommitMessage\":\"Release version {{version}}\",").concat(newLine)
                        .concat("                \"gitPush\":\"false\",").concat(newLine)
                        .concat("                \"gitTag\":\"true\",").concat(newLine)
                        .concat("                \"gitTagMessage\":\"Tag version {{version}}\",").concat(newLine)
                        .concat("                \"matchBranches\":\"^(master|main)$\",").concat(newLine)
                        .concat("                \"publish\":\"true\"").concat(newLine)
                        .concat("            }").concat(newLine)
                        .concat("        }").concat(newLine)
                        .concat("    }").concat(newLine)
                        .concat("}")
                );
                remoteRepoName  = null;
                hostingRepoService = null;
                fileContentChecks = Map.<String,Set<String>>of(
                    ".nyx-state.yml", Set.<String>of(
                        "bump: \"minor\"",
                        "initialVersion: \"1.0.0\"",
                        "preset: \"extended\"",
                        "releaseLenient: true",
                        "releasePrefix: \"v\"",
                        "newVersion: true",
                        "newRelease: true",
                        "previousVersion: \"0.0.4\"",
                        "primeVersion: \"0.0.4\"",
                        "version: \"v0.1.0\""
                    )
                );
                repositoryTags = Set.<String>of(); // no new tag is created by Infer
                remoteRepositoryTags = Set.<String>of(); // not using remotes for this suite
                hostedReleaseTags = Set.<String>of(); // not using hosting services for this suite
            }},*/

            // This suite runs Nyx Infer using a common configuration (using the settings Kotlin Gradle plugin)
            // The version is inferred in the early stage even if Nyx's tasks are not invoked explicitly
            // TODO: make this test use Kotlin files (convert build.gradle and settings.gradle to Kotlin)
            //       and run it along with the others.
            //       There's an issue on this: https://github.com/mooltiverse/nyx/issues/98
            /*new TestSuite(){{
                name        = "Early Infer [Kotlin Settings Plugin]";
                args        = new String[]{"--info", "--stacktrace"}; // these are command line arguments for Gradle, not the Nyx plugin
                tasks       = new String[]{"dummy"};
                scenario    = Scenario.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS;
                env         = Map.<String,String>of(
                                "NYX_PRESET", "extended"
                );
                files       = Map.<String,String>of(
                    // the .gitignore file, required to avoid conflicts due to locked files (i.e. 'The process cannot access the file because another process has locked a portion of the file') etc
                    ".gitignore", ".gitignore".concat(newLine)
                        .concat(".gradle").concat(newLine)
                        .concat("build.gradle").concat(newLine)
                        .concat("settings.gradle").concat(newLine)
                        .concat(".nyx-state.yml").concat(newLine),
                    // the Gradle build script
                    "build.gradle", "".concat(newLine)
                        .concat("task dummy() {").concat(newLine)
                        .concat("}").concat(newLine),
                    // the Gradle settings file
                    "settings.gradle", "".concat(newLine)
                        .concat("plugins {").concat(newLine)
                        .concat("    id 'com.mooltiverse.oss.nyx'").concat(newLine)
                        .concat("}").concat(newLine)
                        .concat(newLine)
                        .concat("rootProject.name = 'nyx-gradle-plugin-test'").concat(newLine)
                        .concat(newLine)
                        .concat("nyx {").concat(newLine)
                        .concat("    dryRun = false").concat(newLine)
                        .concat("    resume = false").concat(newLine)
                        .concat("    stateFile = '.nyx-state.yml'").concat(newLine)
                        .concat("    verbosity = \"DEBUG\"").concat(newLine)
                        .concat("}").concat(newLine),
                    // the .nyx-shared.yaml is the standard shared configuration file
                    ".nyx-shared.yaml", "---".concat(newLine)
                        .concat("commitMessageConventions:").concat(newLine)
                        .concat("  enabled:").concat(newLine)
                        .concat("    - conventionalCommits").concat(newLine)
                        .concat("  items:").concat(newLine)
                        .concat("    conventionalCommits:").concat(newLine)
                        .concat("      expression: \"(?m)^(?<type>[a-zA-Z0-9_]+)(!)?(\\\\((?<scope>[a-z ]+)\\\\))?:( (?<title>.+))$(?s).*\"").concat(newLine)
                        .concat("      bumpExpressions:").concat(newLine)
                        .concat("        major: \"(?s)(?m)^[a-zA-Z0-9_]+(!|.*^(BREAKING( |-)CHANGE: )).*\"").concat(newLine)
                        .concat("        minor: \"(?s)(?m)^feat(?!!|.*^(BREAKING( |-)CHANGE: )).*\"").concat(newLine)
                        .concat("        patch: \"(?s)(?m)^fix(?!!|.*^(BREAKING( |-)CHANGE: )).*\"").concat(newLine)
                        .concat("initialVersion: \"1.0.0\"").concat(newLine)
                        .concat("releaseLenient: true").concat(newLine)
                        .concat("releasePrefix: v").concat(newLine)
                        .concat("scheme: SEMVER").concat(newLine),
                    // the .nyx.json is the standard configuration file
				    ".nyx.json", "{".concat(newLine)
                        .concat("    \"releaseTypes\": {").concat(newLine)
                        .concat("        \"items\": {").concat(newLine)
                        .concat("            \"mainline\": {").concat(newLine)
                        .concat("                \"collapseVersions\":false,").concat(newLine)
                        .concat("                \"filterTags\":\"^({{configuration.releasePrefix}})?([0-9]\\\\d*)\\\\.([0-9]\\\\d*)\\\\.([0-9]\\\\d*)$\",").concat(newLine)
                        .concat("                \"gitCommit\":\"true\",").concat(newLine)
                        .concat("                \"gitCommitMessage\":\"Release version {{version}}\",").concat(newLine)
                        .concat("                \"gitPush\":\"false\",").concat(newLine)
                        .concat("                \"gitTag\":\"true\",").concat(newLine)
                        .concat("                \"gitTagMessage\":\"Tag version {{version}}\",").concat(newLine)
                        .concat("                \"matchBranches\":\"^(master|main)$\",").concat(newLine)
                        .concat("                \"publish\":\"true\"").concat(newLine)
                        .concat("            }").concat(newLine)
                        .concat("        }").concat(newLine)
                        .concat("    }").concat(newLine)
                        .concat("}")
                );
                remoteRepoName  = null;
                hostingRepoService = null;
                fileContentChecks = Map.<String,Set<String>>of(
                    ".nyx-state.yml", Set.<String>of(
                        "bump: \"minor\"",
                        "initialVersion: \"1.0.0\"",
                        "preset: \"extended\"",
                        "releaseLenient: true",
                        "releasePrefix: \"v\"",
                        "newVersion: true",
                        "newRelease: true",
                        "previousVersion: \"0.0.4\"",
                        "primeVersion: \"0.0.4\"",
                        "version: \"v0.1.0\""
                    )
                );
                repositoryTags = Set.<String>of(); // no new tag is created by Infer
                remoteRepositoryTags = Set.<String>of(); // not using remotes for this suite
                hostedReleaseTags = Set.<String>of(); // not using hosting services for this suite
            }},*/

            // This suite runs Nyx Infer using a common configuration (using the regular Groovy Gradle plugin)
            new TestSuite(){{
                name        = "Common [Groovy Regular Plugin]";
                args        = new String[]{"--info", "--stacktrace"}; // these are command line arguments for Gradle, not the Nyx plugin
                tasks       = new String[]{"nyxPublish"};
                scenario    = Scenario.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS;
                env         = Map.<String,String>of(
                                "NYX_PRESET", "extended"
                );
                files       = Map.<String,String>of(
                    // the .gitignore file, required to avoid conflicts due to locked files (i.e. 'The process cannot access the file because another process has locked a portion of the file') etc
                    ".gitignore", ".gitignore".concat(newLine)
                        .concat(".gradle").concat(newLine)
                        .concat("build.gradle").concat(newLine)
                        .concat("settings.gradle").concat(newLine)
                        .concat(".nyx-state.yml").concat(newLine),
                    // the Gradle build script
                    "build.gradle", "".concat(newLine)
                        .concat("plugins {").concat(newLine)
                        .concat("    id 'com.mooltiverse.oss.nyx'").concat(newLine)
                        .concat("}").concat(newLine)
                        .concat(newLine)
                        .concat("nyx {").concat(newLine)
                        .concat("    dryRun = false").concat(newLine)
                        .concat("    resume = false").concat(newLine)
                        .concat("    stateFile = '.nyx-state.yml'").concat(newLine)
                        .concat("    verbosity = \"DEBUG\"").concat(newLine)
                        .concat("}").concat(newLine),
                    // the Gradle settings file
                    "settings.gradle", "".concat(newLine)
                        .concat("rootProject.name = 'nyx-gradle-plugin-test'").concat(newLine),
                    // the .nyx-shared.yaml is the standard shared configuration file
                    ".nyx-shared.yaml", "---".concat(newLine)
                        .concat("commitMessageConventions:").concat(newLine)
                        .concat("  enabled:").concat(newLine)
                        .concat("    - conventionalCommits").concat(newLine)
                        .concat("  items:").concat(newLine)
                        .concat("    conventionalCommits:").concat(newLine)
                        .concat("      expression: \"(?m)^(?<type>[a-zA-Z0-9_]+)(!)?(\\\\((?<scope>[a-z ]+)\\\\))?:( (?<title>.+))$(?s).*\"").concat(newLine)
                        .concat("      bumpExpressions:").concat(newLine)
                        .concat("        major: \"(?s)(?m)^[a-zA-Z0-9_]+(!|.*^(BREAKING( |-)CHANGE: )).*\"").concat(newLine)
                        .concat("        minor: \"(?s)(?m)^feat(?!!|.*^(BREAKING( |-)CHANGE: )).*\"").concat(newLine)
                        .concat("        patch: \"(?s)(?m)^fix(?!!|.*^(BREAKING( |-)CHANGE: )).*\"").concat(newLine)
                        .concat("initialVersion: \"1.0.0\"").concat(newLine)
                        .concat("releaseLenient: true").concat(newLine)
                        .concat("releasePrefix: v").concat(newLine)
                        .concat("scheme: SEMVER").concat(newLine),
                    // the .nyx.json is the standard configuration file
				    ".nyx.json", "{".concat(newLine)
                        .concat("    \"changelog\":{").concat(newLine)
                        .concat("        \"path\":\"CHANGELOG.md\",").concat(newLine)
                        .concat("        \"sections\":{").concat(newLine)
                        .concat("            \"Added\":\"^feat$\",").concat(newLine)
                        .concat("            \"Fixed\":\"^fix$\"").concat(newLine)
                        .concat("        },").concat(newLine)
                        .concat("        \"substitutions\":{").concat(newLine)
                        .concat("            \"(?m)#([0-9]+)(?s)\": \"[#%s](https://example.com/issues/%s)\"").concat(newLine)
                        .concat("        }").concat(newLine)
                        .concat("    },").concat(newLine)
                        .concat("    \"releaseTypes\": {").concat(newLine)
                        .concat("        \"items\": {").concat(newLine)
                        .concat("            \"mainline\": {").concat(newLine)
                        .concat("                \"collapseVersions\":false,").concat(newLine)
                        .concat("                \"filterTags\":\"^({{configuration.releasePrefix}})?([0-9]\\\\d*)\\\\.([0-9]\\\\d*)\\\\.([0-9]\\\\d*)$\",").concat(newLine)
                        .concat("                \"gitCommit\":\"true\",").concat(newLine)
                        .concat("                \"gitCommitMessage\":\"Release version {{version}}\",").concat(newLine)
                        .concat("                \"gitPush\":\"false\",").concat(newLine)
                        .concat("                \"gitTag\":\"true\",").concat(newLine)
                        .concat("                \"gitTagMessage\":\"Tag version {{version}}\",").concat(newLine)
                        .concat("                \"matchBranches\":\"^(master|main)$\",").concat(newLine)
                        .concat("                \"publish\":\"true\"").concat(newLine)
                        .concat("            }").concat(newLine)
                        .concat("        }").concat(newLine)
                        .concat("    }").concat(newLine)
                        .concat("}")
                );
                remoteRepoName  = null;
                hostingRepoService = null;
                fileContentChecks = Map.<String,Set<String>>of(
                    ".nyx-state.yml", Set.<String>of(
                        "bump: \"minor\"",
                        "initialVersion: \"1.0.0\"",
                        "preset: \"extended\"",
                        "releaseLenient: true",
                        "releasePrefix: \"v\"",
                        "newVersion: true",
                        "newRelease: true",
                        "previousVersion: \"0.0.4\"",
                        "primeVersion: \"0.0.4\"",
                        "version: \"v0.1.0\""
                    ),
                    "CHANGELOG.md", Set.<String>of(
                        "## v0.1.0",
                        "### Added",
                        "* [] feat: Untagged commit [#2](https://example.com/issues/2)",
                        "### Fixed",
                        "* [] fix: Untagged commit [#1](https://example.com/issues/1)"
                    )
                );
                repositoryTags = Set.<String>of("v0.1.0");
                remoteRepositoryTags = Set.<String>of(); // not using remotes for this suite
                hostedReleaseTags = Set.<String>of(); // not using hosting services for this suite
            }},

            // This suite runs Nyx Infer using a common configuration (using the settings Groovy Gradle plugin)
            new TestSuite(){{
                name        = "Common [Groovy Settings Plugin]";
                args        = new String[]{"--info", "--stacktrace"}; // these are command line arguments for Gradle, not the Nyx plugin
                tasks       = new String[]{"nyxPublish"};
                scenario    = Scenario.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS;
                env         = Map.<String,String>of(
                                "NYX_PRESET", "extended"
                );
                files       = Map.<String,String>of(
                    // the .gitignore file, required to avoid conflicts due to locked files (i.e. 'The process cannot access the file because another process has locked a portion of the file') etc
                    ".gitignore", ".gitignore".concat(newLine)
                        .concat(".gradle").concat(newLine)
                        .concat("build.gradle").concat(newLine)
                        .concat("settings.gradle").concat(newLine)
                        .concat(".nyx-state.yml").concat(newLine),
                    // the Gradle build script
                    "build.gradle", "".concat(newLine)
                        .concat("task dummy() {").concat(newLine)
                        .concat("}").concat(newLine),
                    // the Gradle settings file
                    "settings.gradle", "".concat(newLine)
                        .concat("plugins {").concat(newLine)
                        .concat("    id 'com.mooltiverse.oss.nyx'").concat(newLine)
                        .concat("}").concat(newLine)
                        .concat(newLine)
                        .concat("rootProject.name = 'nyx-gradle-plugin-test'").concat(newLine)
                        .concat(newLine)
                        .concat("nyx {").concat(newLine)
                        .concat("    dryRun = false").concat(newLine)
                        .concat("    resume = false").concat(newLine)
                        .concat("    stateFile = '.nyx-state.yml'").concat(newLine)
                        .concat("    verbosity = \"DEBUG\"").concat(newLine)
                        .concat("}").concat(newLine),
                    // the .nyx-shared.yaml is the standard shared configuration file
                    ".nyx-shared.yaml", "---".concat(newLine)
                        .concat("commitMessageConventions:").concat(newLine)
                        .concat("  enabled:").concat(newLine)
                        .concat("    - conventionalCommits").concat(newLine)
                        .concat("  items:").concat(newLine)
                        .concat("    conventionalCommits:").concat(newLine)
                        .concat("      expression: \"(?m)^(?<type>[a-zA-Z0-9_]+)(!)?(\\\\((?<scope>[a-z ]+)\\\\))?:( (?<title>.+))$(?s).*\"").concat(newLine)
                        .concat("      bumpExpressions:").concat(newLine)
                        .concat("        major: \"(?s)(?m)^[a-zA-Z0-9_]+(!|.*^(BREAKING( |-)CHANGE: )).*\"").concat(newLine)
                        .concat("        minor: \"(?s)(?m)^feat(?!!|.*^(BREAKING( |-)CHANGE: )).*\"").concat(newLine)
                        .concat("        patch: \"(?s)(?m)^fix(?!!|.*^(BREAKING( |-)CHANGE: )).*\"").concat(newLine)
                        .concat("initialVersion: \"1.0.0\"").concat(newLine)
                        .concat("releaseLenient: true").concat(newLine)
                        .concat("releasePrefix: v").concat(newLine)
                        .concat("scheme: SEMVER").concat(newLine),
                    // the .nyx.json is the standard configuration file
				    ".nyx.json", "{".concat(newLine)
                        .concat("    \"changelog\":{").concat(newLine)
                        .concat("        \"path\":\"CHANGELOG.md\",").concat(newLine)
                        .concat("        \"sections\":{").concat(newLine)
                        .concat("            \"Added\":\"^feat$\",").concat(newLine)
                        .concat("            \"Fixed\":\"^fix$\"").concat(newLine)
                        .concat("        },").concat(newLine)
                        .concat("        \"substitutions\":{").concat(newLine)
                        .concat("            \"(?m)#([0-9]+)(?s)\": \"[#%s](https://example.com/issues/%s)\"").concat(newLine)
                        .concat("        }").concat(newLine)
                        .concat("    },").concat(newLine)
                        .concat("    \"releaseTypes\": {").concat(newLine)
                        .concat("        \"items\": {").concat(newLine)
                        .concat("            \"mainline\": {").concat(newLine)
                        .concat("                \"collapseVersions\":false,").concat(newLine)
                        .concat("                \"filterTags\":\"^({{configuration.releasePrefix}})?([0-9]\\\\d*)\\\\.([0-9]\\\\d*)\\\\.([0-9]\\\\d*)$\",").concat(newLine)
                        .concat("                \"gitCommit\":\"true\",").concat(newLine)
                        .concat("                \"gitCommitMessage\":\"Release version {{version}}\",").concat(newLine)
                        .concat("                \"gitPush\":\"false\",").concat(newLine)
                        .concat("                \"gitTag\":\"true\",").concat(newLine)
                        .concat("                \"gitTagMessage\":\"Tag version {{version}}\",").concat(newLine)
                        .concat("                \"matchBranches\":\"^(master|main)$\",").concat(newLine)
                        .concat("                \"publish\":\"true\"").concat(newLine)
                        .concat("            }").concat(newLine)
                        .concat("        }").concat(newLine)
                        .concat("    }").concat(newLine)
                        .concat("}")
                );
                remoteRepoName  = null;
                hostingRepoService = null;
                fileContentChecks = Map.<String,Set<String>>of(
                    ".nyx-state.yml", Set.<String>of(
                        "bump: \"minor\"",
                        "initialVersion: \"1.0.0\"",
                        "preset: \"extended\"",
                        "releaseLenient: true",
                        "releasePrefix: \"v\"",
                        "newVersion: true",
                        "newRelease: true",
                        "previousVersion: \"0.0.4\"",
                        "primeVersion: \"0.0.4\"",
                        "version: \"v0.1.0\""
                    ),
                    "CHANGELOG.md", Set.<String>of(
                        "## v0.1.0",
                        "### Added",
                        "* [] feat: Untagged commit [#2](https://example.com/issues/2)",
                        "### Fixed",
                        "* [] fix: Untagged commit [#1](https://example.com/issues/1)"
                    )
                );
                repositoryTags = Set.<String>of("v0.1.0");
                remoteRepositoryTags = Set.<String>of(); // not using remotes for this suite
                hostedReleaseTags = Set.<String>of(); // not using hosting services for this suite
            }},

            // This suite runs Nyx Infer using a common configuration with also a remote repository (locally stored) (using the regular Groovy Gradle plugin)
            new TestSuite(){{
                name        = "Remote [Groovy Regular Plugin]";
                args        = new String[]{"--info", "--stacktrace"}; // these are command line arguments for Gradle, not the Nyx plugin
                tasks       = new String[]{"nyxPublish"};
                scenario    = Scenario.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS;
                env         = Map.<String,String>of(
                                "NYX_PRESET", "extended"
                );
                files       = Map.<String,String>of(
                    // the .gitignore file, required to avoid conflicts due to locked files (i.e. 'The process cannot access the file because another process has locked a portion of the file') etc
                    ".gitignore", ".gitignore".concat(newLine)
                        .concat(".gradle").concat(newLine)
                        .concat("build.gradle").concat(newLine)
                        .concat("settings.gradle").concat(newLine)
                        .concat(".nyx-state.yml").concat(newLine),
                    // the Gradle build script
                    "build.gradle", "".concat(newLine)
                        .concat("plugins {").concat(newLine)
                        .concat("    id 'com.mooltiverse.oss.nyx'").concat(newLine)
                        .concat("}").concat(newLine)
                        .concat(newLine)
                        .concat("nyx {").concat(newLine)
                        .concat("    dryRun = false").concat(newLine)
                        .concat("    resume = false").concat(newLine)
                        .concat("    stateFile = '.nyx-state.yml'").concat(newLine)
                        .concat("    verbosity = \"DEBUG\"").concat(newLine)
                        .concat("}").concat(newLine),
                    // the Gradle settings file
                    "settings.gradle", "".concat(newLine)
                        .concat("rootProject.name = 'nyx-gradle-plugin-test'").concat(newLine),
                    // the .nyx-shared.yaml is the standard shared configuration file
                    ".nyx-shared.yaml", "---".concat(newLine)
                        .concat("commitMessageConventions:").concat(newLine)
                        .concat("  enabled:").concat(newLine)
                        .concat("    - conventionalCommits").concat(newLine)
                        .concat("  items:").concat(newLine)
                        .concat("    conventionalCommits:").concat(newLine)
                        .concat("      expression: \"(?m)^(?<type>[a-zA-Z0-9_]+)(!)?(\\\\((?<scope>[a-z ]+)\\\\))?:( (?<title>.+))$(?s).*\"").concat(newLine)
                        .concat("      bumpExpressions:").concat(newLine)
                        .concat("        major: \"(?s)(?m)^[a-zA-Z0-9_]+(!|.*^(BREAKING( |-)CHANGE: )).*\"").concat(newLine)
                        .concat("        minor: \"(?s)(?m)^feat(?!!|.*^(BREAKING( |-)CHANGE: )).*\"").concat(newLine)
                        .concat("        patch: \"(?s)(?m)^fix(?!!|.*^(BREAKING( |-)CHANGE: )).*\"").concat(newLine)
                        .concat("initialVersion: \"1.0.0\"").concat(newLine)
                        .concat("releaseLenient: true").concat(newLine)
                        .concat("releasePrefix: v").concat(newLine)
                        .concat("scheme: SEMVER").concat(newLine),
                    // the .nyx.json is the standard configuration file
				    ".nyx.json", "{".concat(newLine)
                        .concat("    \"changelog\":{").concat(newLine)
                        .concat("        \"path\":\"CHANGELOG.md\",").concat(newLine)
                        .concat("        \"sections\":{").concat(newLine)
                        .concat("            \"Added\":\"^feat$\",").concat(newLine)
                        .concat("            \"Fixed\":\"^fix$\"").concat(newLine)
                        .concat("        },").concat(newLine)
                        .concat("        \"substitutions\":{").concat(newLine)
                        .concat("            \"(?m)#([0-9]+)(?s)\": \"[#%s](https://example.com/issues/%s)\"").concat(newLine)
                        .concat("        }").concat(newLine)
                        .concat("    },").concat(newLine)
                        .concat("    \"releaseTypes\": {").concat(newLine)
                        .concat("        \"remoteRepositories\":[").concat(newLine)
                        .concat("            \"replica\"").concat(newLine)
                        .concat("        ],").concat(newLine)
                        .concat("        \"items\": {").concat(newLine)
                        .concat("            \"mainline\": {").concat(newLine)
                        .concat("                \"collapseVersions\":false,").concat(newLine)
                        .concat("                \"filterTags\":\"^({{configuration.releasePrefix}})?([0-9]\\\\d*)\\\\.([0-9]\\\\d*)\\\\.([0-9]\\\\d*)$\",").concat(newLine)
                        .concat("                \"gitCommit\":\"true\",").concat(newLine)
                        .concat("                \"gitCommitMessage\":\"Release version {{version}}\",").concat(newLine)
                        .concat("                \"gitPush\":\"true\",").concat(newLine)
                        .concat("                \"gitTag\":\"true\",").concat(newLine)
                        .concat("                \"gitTagMessage\":\"Tag version {{version}}\",").concat(newLine)
                        .concat("                \"matchBranches\":\"^(master|main)$\",").concat(newLine)
                        .concat("                \"publish\":\"true\"").concat(newLine)
                        .concat("            }").concat(newLine)
                        .concat("        }").concat(newLine)
                        .concat("    }").concat(newLine)
                        .concat("}")
                );
                remoteRepoName  = "replica";
                hostingRepoService = null;
                fileContentChecks = Map.<String,Set<String>>of(
                    ".nyx-state.yml", Set.<String>of(
                        "bump: \"minor\"",
                        "initialVersion: \"1.0.0\"",
                        "preset: \"extended\"",
                        "releaseLenient: true",
                        "releasePrefix: \"v\"",
                        "newVersion: true",
                        "newRelease: true",
                        "previousVersion: \"0.0.4\"",
                        "primeVersion: \"0.0.4\"",
                        "version: \"v0.1.0\""
                    ),
                    "CHANGELOG.md", Set.<String>of(
                        "## v0.1.0",
                        "### Added",
                        "* [] feat: Untagged commit [#2](https://example.com/issues/2)",
                        "### Fixed",
                        "* [] fix: Untagged commit [#1](https://example.com/issues/1)"
                    )
                );
                repositoryTags = Set.<String>of("v0.1.0");
                remoteRepositoryTags = Set.<String>of("v0.1.0");
                hostedReleaseTags = Set.<String>of(); // not using hosting services for this suite
            }},

            // This suite runs Nyx Infer using a common configuration with also a remote repository (locally stored) (using the settings Groovy Gradle plugin)
            new TestSuite(){{
                name        = "Remote [Groovy Settings Plugin]";
                args        = new String[]{"--info", "--stacktrace"}; // these are command line arguments for Gradle, not the Nyx plugin
                tasks       = new String[]{"nyxPublish"};
                scenario    = Scenario.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS;
                env         = Map.<String,String>of(
                                "NYX_PRESET", "extended"
                );
                files       = Map.<String,String>of(
                    // the .gitignore file, required to avoid conflicts due to locked files (i.e. 'The process cannot access the file because another process has locked a portion of the file') etc
                    ".gitignore", ".gitignore".concat(newLine)
                        .concat(".gradle").concat(newLine)
                        .concat("build.gradle").concat(newLine)
                        .concat("settings.gradle").concat(newLine)
                        .concat(".nyx-state.yml").concat(newLine),
                    // the Gradle build script
                    "build.gradle", "".concat(newLine)
                        .concat("task dummy() {").concat(newLine)
                        .concat("}").concat(newLine),
                    // the Gradle settings file
                    "settings.gradle", "".concat(newLine)
                        .concat("plugins {").concat(newLine)
                        .concat("    id 'com.mooltiverse.oss.nyx'").concat(newLine)
                        .concat("}").concat(newLine)
                        .concat(newLine)
                        .concat("rootProject.name = 'nyx-gradle-plugin-test'").concat(newLine)
                        .concat(newLine)
                        .concat("nyx {").concat(newLine)
                        .concat("    dryRun = false").concat(newLine)
                        .concat("    resume = false").concat(newLine)
                        .concat("    stateFile = '.nyx-state.yml'").concat(newLine)
                        .concat("    verbosity = \"DEBUG\"").concat(newLine)
                        .concat("}").concat(newLine),
                    // the .nyx-shared.yaml is the standard shared configuration file
                    ".nyx-shared.yaml", "---".concat(newLine)
                        .concat("commitMessageConventions:").concat(newLine)
                        .concat("  enabled:").concat(newLine)
                        .concat("    - conventionalCommits").concat(newLine)
                        .concat("  items:").concat(newLine)
                        .concat("    conventionalCommits:").concat(newLine)
                        .concat("      expression: \"(?m)^(?<type>[a-zA-Z0-9_]+)(!)?(\\\\((?<scope>[a-z ]+)\\\\))?:( (?<title>.+))$(?s).*\"").concat(newLine)
                        .concat("      bumpExpressions:").concat(newLine)
                        .concat("        major: \"(?s)(?m)^[a-zA-Z0-9_]+(!|.*^(BREAKING( |-)CHANGE: )).*\"").concat(newLine)
                        .concat("        minor: \"(?s)(?m)^feat(?!!|.*^(BREAKING( |-)CHANGE: )).*\"").concat(newLine)
                        .concat("        patch: \"(?s)(?m)^fix(?!!|.*^(BREAKING( |-)CHANGE: )).*\"").concat(newLine)
                        .concat("initialVersion: \"1.0.0\"").concat(newLine)
                        .concat("releaseLenient: true").concat(newLine)
                        .concat("releasePrefix: v").concat(newLine)
                        .concat("scheme: SEMVER").concat(newLine),
                    // the .nyx.json is the standard configuration file
				    ".nyx.json", "{".concat(newLine)
                        .concat("    \"changelog\":{").concat(newLine)
                        .concat("        \"path\":\"CHANGELOG.md\",").concat(newLine)
                        .concat("        \"sections\":{").concat(newLine)
                        .concat("            \"Added\":\"^feat$\",").concat(newLine)
                        .concat("            \"Fixed\":\"^fix$\"").concat(newLine)
                        .concat("        },").concat(newLine)
                        .concat("        \"substitutions\":{").concat(newLine)
                        .concat("            \"(?m)#([0-9]+)(?s)\": \"[#%s](https://example.com/issues/%s)\"").concat(newLine)
                        .concat("        }").concat(newLine)
                        .concat("    },").concat(newLine)
                        .concat("    \"releaseTypes\": {").concat(newLine)
                        .concat("        \"remoteRepositories\":[").concat(newLine)
                        .concat("            \"replica\"").concat(newLine)
                        .concat("        ],").concat(newLine)
                        .concat("        \"items\": {").concat(newLine)
                        .concat("            \"mainline\": {").concat(newLine)
                        .concat("                \"collapseVersions\":false,").concat(newLine)
                        .concat("                \"filterTags\":\"^({{configuration.releasePrefix}})?([0-9]\\\\d*)\\\\.([0-9]\\\\d*)\\\\.([0-9]\\\\d*)$\",").concat(newLine)
                        .concat("                \"gitCommit\":\"true\",").concat(newLine)
                        .concat("                \"gitCommitMessage\":\"Release version {{version}}\",").concat(newLine)
                        .concat("                \"gitPush\":\"true\",").concat(newLine)
                        .concat("                \"gitTag\":\"true\",").concat(newLine)
                        .concat("                \"gitTagMessage\":\"Tag version {{version}}\",").concat(newLine)
                        .concat("                \"matchBranches\":\"^(master|main)$\",").concat(newLine)
                        .concat("                \"publish\":\"true\"").concat(newLine)
                        .concat("            }").concat(newLine)
                        .concat("        }").concat(newLine)
                        .concat("    }").concat(newLine)
                        .concat("}")
                );
                remoteRepoName  = "replica";
                hostingRepoService = null;
                fileContentChecks = Map.<String,Set<String>>of(
                    ".nyx-state.yml", Set.<String>of(
                        "bump: \"minor\"",
                        "initialVersion: \"1.0.0\"",
                        "preset: \"extended\"",
                        "releaseLenient: true",
                        "releasePrefix: \"v\"",
                        "newVersion: true",
                        "newRelease: true",
                        "previousVersion: \"0.0.4\"",
                        "primeVersion: \"0.0.4\"",
                        "version: \"v0.1.0\""
                    ),
                    "CHANGELOG.md", Set.<String>of(
                        "## v0.1.0",
                        "### Added",
                        "* [] feat: Untagged commit [#2](https://example.com/issues/2)",
                        "### Fixed",
                        "* [] fix: Untagged commit [#1](https://example.com/issues/1)"
                    )
                );
                repositoryTags = Set.<String>of("v0.1.0");
                remoteRepositoryTags = Set.<String>of("v0.1.0");
                hostedReleaseTags = Set.<String>of(); // not using hosting services for this suite
            }},
        };

        TestSuite[] wellKnownFunctionalTestSuitesNonQuick =  new TestSuite[]{
            // This suite runs Nyx Infer using a common configuration with also a remote repository (locally stored) (using the regular Groovy Gradle plugin)
            new TestSuite(){{
                name        = "Custom [Groovy Regular Plugin]";
                args        = new String[]{"--info", "--stacktrace"}; // these are command line arguments for Gradle, not the Nyx plugin
                tasks       = new String[]{"nyxPublish"};
                scenario    = Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED;
                env         = Map.<String,String>of(
                                "NYX_PRESET", "extended"
                );
                files       = Map.<String,String>of(
                    // the .gitignore file, required to avoid conflicts due to locked files (i.e. 'The process cannot access the file because another process has locked a portion of the file') etc
                    ".gitignore", ".gitignore".concat(newLine)
                        .concat(".gradle").concat(newLine)
                        .concat("build.gradle").concat(newLine)
                        .concat("settings.gradle").concat(newLine)
                        .concat(".nyx-state.yml").concat(newLine),
                    // the Gradle build script
                    "build.gradle", "".concat(newLine)
                        .concat("plugins {").concat(newLine)
                        .concat("    id 'com.mooltiverse.oss.nyx'").concat(newLine)
                        .concat("}").concat(newLine)
                        .concat(newLine)
                        .concat("nyx {").concat(newLine)
                        .concat("    dryRun = false").concat(newLine)
                        .concat("    resume = false").concat(newLine)
                        .concat("    stateFile = '.nyx-state.yml'").concat(newLine)
                        .concat("    verbosity = \"DEBUG\"").concat(newLine)
                        .concat("}").concat(newLine),
                    // the Gradle settings file
                    "settings.gradle", "".concat(newLine)
                        .concat("rootProject.name = 'nyx-gradle-plugin-test'").concat(newLine),
                    // the .nyx-shared.yaml is the standard shared configuration file
                    ".nyx-shared.yaml", "---".concat(newLine)
                        .concat("commitMessageConventions:").concat(newLine)
                        .concat("  enabled:").concat(newLine)
                        .concat("    - testConvention").concat(newLine)
                        .concat("  items:").concat(newLine)
                        .concat("    testConvention:").concat(newLine)
                        .concat("      expression: \".*\"").concat(newLine)
                        .concat("      bumpExpressions:").concat(newLine)
                        .concat("        patch: \".*\"").concat(newLine),
                    // the .nyx.json is the standard configuration file
				    ".nyx.json", "{".concat(newLine)
                        .concat("    \"changelog\":{").concat(newLine)
                        .concat("        \"path\":\"CHANGELOG.md\",").concat(newLine)
                        .concat("        \"sections\":{").concat(newLine)
                        .concat("            \"Added\":\"^feat$\",").concat(newLine)
                        .concat("            \"Fixed\":\"^fix$\"").concat(newLine)
                        .concat("        },").concat(newLine)
                        .concat("        \"substitutions\":{").concat(newLine)
                        .concat("            \"(?m)#([0-9]+)(?s)\": \"[#%s](https://example.com/issues/%s)\"").concat(newLine)
                        .concat("        }").concat(newLine)
                        .concat("    },").concat(newLine)
                        .concat("    \"releaseTypes\": {").concat(newLine)
                        .concat("        \"remoteRepositories\":[").concat(newLine)
                        .concat("            \"replica\"").concat(newLine)
                        .concat("        ],").concat(newLine)
                        .concat("        \"items\": {").concat(newLine)
                        .concat("            \"mainline\": {").concat(newLine)
                        .concat("                \"collapseVersions\":false,").concat(newLine)
                        .concat("                \"filterTags\":\"^({{configuration.releasePrefix}})?([0-9]\\\\d*)\\\\.([0-9]\\\\d*)\\\\.([0-9]\\\\d*)$\",").concat(newLine)
                        .concat("                \"gitCommit\":\"true\",").concat(newLine)
                        .concat("                \"gitCommitMessage\":\"Release version {{version}}\",").concat(newLine)
                        .concat("                \"gitPush\":\"true\",").concat(newLine)
                        .concat("                \"gitTag\":\"true\",").concat(newLine)
                        .concat("                \"gitTagMessage\":\"Tag version {{version}}\",").concat(newLine)
                        .concat("                \"matchBranches\":\"^(master|main)$\",").concat(newLine)
                        .concat("                \"publish\":\"true\"").concat(newLine)
                        .concat("            }").concat(newLine)
                        .concat("        }").concat(newLine)
                        .concat("    }").concat(newLine)
                        .concat("}")
                );
                remoteRepoName  = "replica";
                hostingRepoService = null;
                fileContentChecks = Map.<String,Set<String>>of(
                    ".nyx-state.yml", Set.<String>of(
                        "branch: \"master\"",
                        "bump: \"patch\"",
                        "initialVersion: \"0.1.0\"",
                        "preset: \"extended\"",
                        "newVersion: true",
                        "newRelease: true",
                        "previousVersion: \"0.0.5\"",
                        "primeVersion: \"0.0.5\"",
                        "version: \"0.0.6\""
                    ),
                    "CHANGELOG.md", Set.<String>of(
                        "## 0.0.6",
					    "No changes."
                    )
                );
                repositoryTags = Set.<String>of("0.0.6");
                remoteRepositoryTags = Set.<String>of("0.0.6");
                hostedReleaseTags = Set.<String>of(); // not using hosting services for this suite
            }},

            // This suite runs Nyx Infer using a common configuration with also a remote repository (locally stored) (using the settings Groovy Gradle plugin)
            new TestSuite(){{
                name        = "Custom [Groovy Settings Plugin]";
                args        = new String[]{"--info", "--stacktrace"}; // these are command line arguments for Gradle, not the Nyx plugin
                tasks       = new String[]{"nyxPublish"};
                scenario    = Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED;
                env         = Map.<String,String>of(
                                "NYX_PRESET", "extended"
                );
                files       = Map.<String,String>of(
                    // the .gitignore file, required to avoid conflicts due to locked files (i.e. 'The process cannot access the file because another process has locked a portion of the file') etc
                    ".gitignore", ".gitignore".concat(newLine)
                        .concat(".gradle").concat(newLine)
                        .concat("build.gradle").concat(newLine)
                        .concat("settings.gradle").concat(newLine)
                        .concat(".nyx-state.yml").concat(newLine),
                    // the Gradle build script
                    "build.gradle", "".concat(newLine)
                        .concat("task dummy() {").concat(newLine)
                        .concat("}").concat(newLine),
                    // the Gradle settings file
                    "settings.gradle", "".concat(newLine)
                        .concat("plugins {").concat(newLine)
                        .concat("    id 'com.mooltiverse.oss.nyx'").concat(newLine)
                        .concat("}").concat(newLine)
                        .concat(newLine)
                        .concat("rootProject.name = 'nyx-gradle-plugin-test'").concat(newLine)
                        .concat(newLine)
                        .concat("nyx {").concat(newLine)
                        .concat("    dryRun = false").concat(newLine)
                        .concat("    resume = false").concat(newLine)
                        .concat("    stateFile = '.nyx-state.yml'").concat(newLine)
                        .concat("    verbosity = \"DEBUG\"").concat(newLine)
                        .concat("}").concat(newLine),
                    // the .nyx-shared.yaml is the standard shared configuration file
                    ".nyx-shared.yaml", "---".concat(newLine)
                        .concat("commitMessageConventions:").concat(newLine)
                        .concat("  enabled:").concat(newLine)
                        .concat("    - testConvention").concat(newLine)
                        .concat("  items:").concat(newLine)
                        .concat("    testConvention:").concat(newLine)
                        .concat("      expression: \".*\"").concat(newLine)
                        .concat("      bumpExpressions:").concat(newLine)
                        .concat("        patch: \".*\"").concat(newLine),
                    // the .nyx.json is the standard configuration file
				    ".nyx.json", "{".concat(newLine)
                        .concat("    \"changelog\":{").concat(newLine)
                        .concat("        \"path\":\"CHANGELOG.md\",").concat(newLine)
                        .concat("        \"sections\":{").concat(newLine)
                        .concat("            \"Added\":\"^feat$\",").concat(newLine)
                        .concat("            \"Fixed\":\"^fix$\"").concat(newLine)
                        .concat("        },").concat(newLine)
                        .concat("        \"substitutions\":{").concat(newLine)
                        .concat("            \"(?m)#([0-9]+)(?s)\": \"[#%s](https://example.com/issues/%s)\"").concat(newLine)
                        .concat("        }").concat(newLine)
                        .concat("    },").concat(newLine)
                        .concat("    \"releaseTypes\": {").concat(newLine)
                        .concat("        \"remoteRepositories\":[").concat(newLine)
                        .concat("            \"replica\"").concat(newLine)
                        .concat("        ],").concat(newLine)
                        .concat("        \"items\": {").concat(newLine)
                        .concat("            \"mainline\": {").concat(newLine)
                        .concat("                \"collapseVersions\":false,").concat(newLine)
                        .concat("                \"filterTags\":\"^({{configuration.releasePrefix}})?([0-9]\\\\d*)\\\\.([0-9]\\\\d*)\\\\.([0-9]\\\\d*)$\",").concat(newLine)
                        .concat("                \"gitCommit\":\"true\",").concat(newLine)
                        .concat("                \"gitCommitMessage\":\"Release version {{version}}\",").concat(newLine)
                        .concat("                \"gitPush\":\"true\",").concat(newLine)
                        .concat("                \"gitTag\":\"true\",").concat(newLine)
                        .concat("                \"gitTagMessage\":\"Tag version {{version}}\",").concat(newLine)
                        .concat("                \"matchBranches\":\"^(master|main)$\",").concat(newLine)
                        .concat("                \"publish\":\"true\"").concat(newLine)
                        .concat("            }").concat(newLine)
                        .concat("        }").concat(newLine)
                        .concat("    }").concat(newLine)
                        .concat("}")
                );
                remoteRepoName  = "replica";
                hostingRepoService = null;
                fileContentChecks = Map.<String,Set<String>>of(
                    ".nyx-state.yml", Set.<String>of(
                        "branch: \"master\"",
                        "bump: \"patch\"",
                        "initialVersion: \"0.1.0\"",
                        "preset: \"extended\"",
                        "newVersion: true",
                        "newRelease: true",
                        "previousVersion: \"0.0.5\"",
                        "primeVersion: \"0.0.5\"",
                        "version: \"0.0.6\""
                    ),
                    "CHANGELOG.md", Set.<String>of(
                        "## 0.0.6",
					    "No changes."
                    )
                );
                repositoryTags = Set.<String>of("0.0.6");
                remoteRepositoryTags = Set.<String>of("0.0.6");
                hostedReleaseTags = Set.<String>of(); // not using hosting services for this suite
            }},

            // This suite runs Nyx Infer using a common configuration with also GitHub as the hosting service (using the settings Groovy Gradle plugin)
            new TestSuite(){{
                name        = "GitHub [Groovy Regular Plugin]";
                args        = new String[]{"--info", "--stacktrace"}; // these are command line arguments for Gradle, not the Nyx plugin
                tasks       = new String[]{"nyxPublish"};
                scenario    = Scenario.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS;
                env         = Map.<String,String>of(
                                "GH_TOKEN",                             System.getProperty("gitHubTestUserToken"), // The 'gitHubTestUserToken' variable is set in Gradle build scripts
                                "NYX_PRESET",                       "extended",
                                "NYX_GIT_REMOTES_origin_USER",      "{{#environmentVariable}}GH_TOKEN{{/environmentVariable}}",
				                "NYX_GIT_REMOTES_origin_PASSWORD",  ""
                );
                files       = Map.<String,String>of(
                    // the .gitignore file, required to avoid conflicts due to locked files (i.e. 'The process cannot access the file because another process has locked a portion of the file') etc
                    ".gitignore", ".gitignore".concat(newLine)
                        .concat(".gradle").concat(newLine)
                        .concat("build.gradle").concat(newLine)
                        .concat("settings.gradle").concat(newLine)
                        .concat(".nyx-state.yml").concat(newLine),
                    // the Gradle build script
                    "build.gradle", "".concat(newLine)
                        .concat("plugins {").concat(newLine)
                        .concat("    id 'com.mooltiverse.oss.nyx'").concat(newLine)
                        .concat("}").concat(newLine)
                        .concat(newLine)
                        .concat("nyx {").concat(newLine)
                        .concat("    dryRun = false").concat(newLine)
                        .concat("    resume = false").concat(newLine)
                        .concat("    stateFile = '.nyx-state.yml'").concat(newLine)
                        .concat("    verbosity = \"DEBUG\"").concat(newLine)
                        .concat("}").concat(newLine),
                    // the Gradle settings file
                    "settings.gradle", "".concat(newLine)
                        .concat("rootProject.name = 'nyx-gradle-plugin-test'").concat(newLine),
                    // the .nyx-shared.yaml is the standard shared configuration file
                    ".nyx-shared.yaml", "---".concat(newLine)
                        .concat("commitMessageConventions:").concat(newLine)
                        .concat("  enabled:").concat(newLine)
                        .concat("    - conventionalCommits").concat(newLine)
                        .concat("  items:").concat(newLine)
                        .concat("    conventionalCommits:").concat(newLine)
                        .concat("      expression: \"(?m)^(?<type>[a-zA-Z0-9_]+)(!)?(\\\\((?<scope>[a-z ]+)\\\\))?:( (?<title>.+))$(?s).*\"").concat(newLine)
                        .concat("      bumpExpressions:").concat(newLine)
                        .concat("        major: \"(?s)(?m)^[a-zA-Z0-9_]+(!|.*^(BREAKING( |-)CHANGE: )).*\"").concat(newLine)
                        .concat("        minor: \"(?s)(?m)^feat(?!!|.*^(BREAKING( |-)CHANGE: )).*\"").concat(newLine)
                        .concat("        patch: \"(?s)(?m)^fix(?!!|.*^(BREAKING( |-)CHANGE: )).*\"").concat(newLine)
                        .concat("initialVersion: \"1.0.0\"").concat(newLine)
                        .concat("releaseLenient: true").concat(newLine)
                        .concat("releasePrefix: v").concat(newLine)
                        .concat("scheme: SEMVER").concat(newLine),
                    // the .nyx.json is the standard configuration file
				    ".nyx.json", "{".concat(newLine)
                        .concat("    \"changelog\":{").concat(newLine)
                        .concat("        \"path\":\"CHANGELOG.md\",").concat(newLine)
                        .concat("        \"sections\":{").concat(newLine)
                        .concat("            \"Added\":\"^feat$\",").concat(newLine)
                        .concat("            \"Fixed\":\"^fix$\"").concat(newLine)
                        .concat("        },").concat(newLine)
                        .concat("        \"substitutions\":{").concat(newLine)
                        .concat("            \"(?m)#([0-9]+)(?s)\": \"[#%s](https://example.com/issues/%s)\"").concat(newLine)
                        .concat("        }").concat(newLine)
                        .concat("    },").concat(newLine)
                        .concat("    \"releaseAssets\": {").concat(newLine)
                        .concat("        \"asset1\":{").concat(newLine)
                        .concat("            \"fileName\":\"CHANGELOG.md\",").concat(newLine)
                        .concat("            \"description\":\"CHANGELOG\",").concat(newLine)
                        .concat("            \"type\":\"text/plain\",").concat(newLine)
                        .concat("            \"path\":\"CHANGELOG.md\"").concat(newLine)
                        .concat("        }").concat(newLine)
                        .concat("    },").concat(newLine)
                        .concat("    \"releaseTypes\": {").concat(newLine)
                        .concat("        \"remoteRepositories\":[").concat(newLine)
                        .concat("            \"origin\"").concat(newLine)
                        .concat("        ],").concat(newLine)
                        .concat("        \"items\": {").concat(newLine)
                        .concat("            \"mainline\": {").concat(newLine)
                        .concat("                \"collapseVersions\":false,").concat(newLine)
                        .concat("                \"filterTags\":\"^({{configuration.releasePrefix}})?([0-9]\\\\d*)\\\\.([0-9]\\\\d*)\\\\.([0-9]\\\\d*)$\",").concat(newLine)
                        .concat("                \"gitCommit\":\"true\",").concat(newLine)
                        .concat("                \"gitCommitMessage\":\"Release version {{version}}\",").concat(newLine)
                        .concat("                \"gitPush\":\"false\",").concat(newLine)
                        .concat("                \"gitTag\":\"true\",").concat(newLine)
                        .concat("                \"gitTagMessage\":\"Tag version {{version}}\",").concat(newLine)
                        .concat("                \"matchBranches\":\"^(master|main)$\",").concat(newLine)
                        .concat("                \"publish\":\"true\"").concat(newLine)
                        .concat("            }").concat(newLine)
                        .concat("        }").concat(newLine)
                        .concat("    }").concat(newLine)
                        .concat("}")
                );
                remoteRepoName  = null; // use 'origin', from the cloned repository from the hosting service
                hostingRepoService = "github";
                fileContentChecks = Map.<String,Set<String>>of(
                    ".nyx-state.yml", Set.<String>of(
                        "bump: \"minor\"",
                        "initialVersion: \"1.0.0\"",
                        "preset: \"extended\"",
                        "releaseLenient: true",
                        "releasePrefix: \"v\"",
                        "newVersion: true",
                        "newRelease: true",
                        "previousVersion: \"0.0.4\"",
                        "primeVersion: \"0.0.4\"",
                        "version: \"v0.1.0\""
                    ),
                    "CHANGELOG.md", Set.<String>of(
                        "## v0.1.0",
                        "### Added",
                        "* [] feat: Untagged commit [#2](https://example.com/issues/2)",
                        "### Fixed",
                        "* [] fix: Untagged commit [#1](https://example.com/issues/1)"
                    )
                );
                repositoryTags = Set.<String>of("v0.1.0");
                remoteRepositoryTags = Set.<String>of(); // // here we don't use the locally managed remote repository
                hostedReleaseTags = Set.<String>of(); // not using hosting services for this suite
            }},

            // This suite runs Nyx Infer using a common configuration with also GitHub as the hosting service (using the settings Groovy Gradle plugin)
            new TestSuite(){{
                name        = "GitHub [Groovy Settings Plugin]";
                args        = new String[]{"--info", "--stacktrace"}; // these are command line arguments for Gradle, not the Nyx plugin
                tasks       = new String[]{"nyxPublish"};
                scenario    = Scenario.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS;
                env         = Map.<String,String>of(
                                "GH_TOKEN",                             System.getProperty("gitHubTestUserToken"), // The 'gitHubTestUserToken' variable is set in Gradle build scripts
                                "NYX_PRESET",                       "extended",
                                "NYX_GIT_REMOTES_origin_USER",      "{{#environmentVariable}}GH_TOKEN{{/environmentVariable}}",
				                "NYX_GIT_REMOTES_origin_PASSWORD",  ""
                );
                files       = Map.<String,String>of(
                    // the .gitignore file, required to avoid conflicts due to locked files (i.e. 'The process cannot access the file because another process has locked a portion of the file') etc
                    ".gitignore", ".gitignore".concat(newLine)
                        .concat(".gradle").concat(newLine)
                        .concat("build.gradle").concat(newLine)
                        .concat("settings.gradle").concat(newLine)
                        .concat(".nyx-state.yml").concat(newLine),
                    // the Gradle build script
                    "build.gradle", "".concat(newLine)
                        .concat("task dummy() {").concat(newLine)
                        .concat("}").concat(newLine),
                    // the Gradle settings file
                    "settings.gradle", "".concat(newLine)
                        .concat("plugins {").concat(newLine)
                        .concat("    id 'com.mooltiverse.oss.nyx'").concat(newLine)
                        .concat("}").concat(newLine)
                        .concat(newLine)
                        .concat("rootProject.name = 'nyx-gradle-plugin-test'").concat(newLine)
                        .concat(newLine)
                        .concat("nyx {").concat(newLine)
                        .concat("    dryRun = false").concat(newLine)
                        .concat("    resume = false").concat(newLine)
                        .concat("    stateFile = '.nyx-state.yml'").concat(newLine)
                        .concat("    verbosity = \"DEBUG\"").concat(newLine)
                        .concat("}").concat(newLine),
                    // the .nyx-shared.yaml is the standard shared configuration file
                    ".nyx-shared.yaml", "---".concat(newLine)
                        .concat("commitMessageConventions:").concat(newLine)
                        .concat("  enabled:").concat(newLine)
                        .concat("    - conventionalCommits").concat(newLine)
                        .concat("  items:").concat(newLine)
                        .concat("    conventionalCommits:").concat(newLine)
                        .concat("      expression: \"(?m)^(?<type>[a-zA-Z0-9_]+)(!)?(\\\\((?<scope>[a-z ]+)\\\\))?:( (?<title>.+))$(?s).*\"").concat(newLine)
                        .concat("      bumpExpressions:").concat(newLine)
                        .concat("        major: \"(?s)(?m)^[a-zA-Z0-9_]+(!|.*^(BREAKING( |-)CHANGE: )).*\"").concat(newLine)
                        .concat("        minor: \"(?s)(?m)^feat(?!!|.*^(BREAKING( |-)CHANGE: )).*\"").concat(newLine)
                        .concat("        patch: \"(?s)(?m)^fix(?!!|.*^(BREAKING( |-)CHANGE: )).*\"").concat(newLine)
                        .concat("initialVersion: \"1.0.0\"").concat(newLine)
                        .concat("releaseLenient: true").concat(newLine)
                        .concat("releasePrefix: v").concat(newLine)
                        .concat("scheme: SEMVER").concat(newLine),
                    // the .nyx.json is the standard configuration file
				    ".nyx.json", "{".concat(newLine)
                        .concat("    \"changelog\":{").concat(newLine)
                        .concat("        \"path\":\"CHANGELOG.md\",").concat(newLine)
                        .concat("        \"sections\":{").concat(newLine)
                        .concat("            \"Added\":\"^feat$\",").concat(newLine)
                        .concat("            \"Fixed\":\"^fix$\"").concat(newLine)
                        .concat("        },").concat(newLine)
                        .concat("        \"substitutions\":{").concat(newLine)
                        .concat("            \"(?m)#([0-9]+)(?s)\": \"[#%s](https://example.com/issues/%s)\"").concat(newLine)
                        .concat("        }").concat(newLine)
                        .concat("    },").concat(newLine)
                        .concat("    \"releaseAssets\": {").concat(newLine)
                        .concat("        \"asset1\":{").concat(newLine)
                        .concat("            \"fileName\":\"CHANGELOG.md\",").concat(newLine)
                        .concat("            \"description\":\"CHANGELOG\",").concat(newLine)
                        .concat("            \"type\":\"text/plain\",").concat(newLine)
                        .concat("            \"path\":\"CHANGELOG.md\"").concat(newLine)
                        .concat("        }").concat(newLine)
                        .concat("    },").concat(newLine)
                        .concat("    \"releaseTypes\": {").concat(newLine)
                        .concat("        \"remoteRepositories\":[").concat(newLine)
                        .concat("            \"origin\"").concat(newLine)
                        .concat("        ],").concat(newLine)
                        .concat("        \"items\": {").concat(newLine)
                        .concat("            \"mainline\": {").concat(newLine)
                        .concat("                \"collapseVersions\":false,").concat(newLine)
                        .concat("                \"filterTags\":\"^({{configuration.releasePrefix}})?([0-9]\\\\d*)\\\\.([0-9]\\\\d*)\\\\.([0-9]\\\\d*)$\",").concat(newLine)
                        .concat("                \"gitCommit\":\"true\",").concat(newLine)
                        .concat("                \"gitCommitMessage\":\"Release version {{version}}\",").concat(newLine)
                        .concat("                \"gitPush\":\"false\",").concat(newLine)
                        .concat("                \"gitTag\":\"true\",").concat(newLine)
                        .concat("                \"gitTagMessage\":\"Tag version {{version}}\",").concat(newLine)
                        .concat("                \"matchBranches\":\"^(master|main)$\",").concat(newLine)
                        .concat("                \"publish\":\"true\"").concat(newLine)
                        .concat("            }").concat(newLine)
                        .concat("        }").concat(newLine)
                        .concat("    }").concat(newLine)
                        .concat("}")
                );
                remoteRepoName  = null; // use 'origin', from the cloned repository from the hosting service
                hostingRepoService = "github";
                fileContentChecks = Map.<String,Set<String>>of(
                    ".nyx-state.yml", Set.<String>of(
                        "bump: \"minor\"",
                        "initialVersion: \"1.0.0\"",
                        "preset: \"extended\"",
                        "releaseLenient: true",
                        "releasePrefix: \"v\"",
                        "newVersion: true",
                        "newRelease: true",
                        "previousVersion: \"0.0.4\"",
                        "primeVersion: \"0.0.4\"",
                        "version: \"v0.1.0\""
                    ),
                    "CHANGELOG.md", Set.<String>of(
                        "## v0.1.0",
                        "### Added",
                        "* [] feat: Untagged commit [#2](https://example.com/issues/2)",
                        "### Fixed",
                        "* [] fix: Untagged commit [#1](https://example.com/issues/1)"
                    )
                );
                repositoryTags = Set.<String>of("v0.1.0");
                remoteRepositoryTags = Set.<String>of(); // // here we don't use the locally managed remote repository
                hostedReleaseTags = Set.<String>of(); // not using hosting services for this suite
            }},

            // This suite runs Nyx Infer using a common configuration with also GitLab as the hosting service (using the settings Groovy Gradle plugin)
            new TestSuite(){{
                name        = "GitLab [Groovy Regular Plugin]";
                args        = new String[]{"--info", "--stacktrace"}; // these are command line arguments for Gradle, not the Nyx plugin
                tasks       = new String[]{"nyxPublish"};
                scenario    = Scenario.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS;
                env         = Map.<String,String>of(
                                "GL_TOKEN",                             System.getProperty("gitLabTestUserToken"), // The 'gitLabTestUserToken' variable is set in Gradle build scripts
                                "NYX_PRESET",                       "extended",
                                "NYX_GIT_REMOTES_origin_USER",      "PRIVATE-TOKEN",
				                "NYX_GIT_REMOTES_origin_PASSWORD",  "{{#environmentVariable}}GL_TOKEN{{/environmentVariable}}"
                );
                files       = Map.<String,String>of(
                    // the .gitignore file, required to avoid conflicts due to locked files (i.e. 'The process cannot access the file because another process has locked a portion of the file') etc
                    ".gitignore", ".gitignore".concat(newLine)
                        .concat(".gradle").concat(newLine)
                        .concat("build.gradle").concat(newLine)
                        .concat("settings.gradle").concat(newLine)
                        .concat(".nyx-state.yml").concat(newLine),
                    // the Gradle build script
                    "build.gradle", "".concat(newLine)
                        .concat("plugins {").concat(newLine)
                        .concat("    id 'com.mooltiverse.oss.nyx'").concat(newLine)
                        .concat("}").concat(newLine)
                        .concat(newLine)
                        .concat("nyx {").concat(newLine)
                        .concat("    dryRun = false").concat(newLine)
                        .concat("    resume = false").concat(newLine)
                        .concat("    stateFile = '.nyx-state.yml'").concat(newLine)
                        .concat("    verbosity = \"DEBUG\"").concat(newLine)
                        .concat("}").concat(newLine),
                    // the Gradle settings file
                    "settings.gradle", "".concat(newLine)
                        .concat("rootProject.name = 'nyx-gradle-plugin-test'").concat(newLine),
                    // the .nyx-shared.yaml is the standard shared configuration file
                    ".nyx-shared.yaml", "---".concat(newLine)
                        .concat("commitMessageConventions:").concat(newLine)
                        .concat("  enabled:").concat(newLine)
                        .concat("    - conventionalCommits").concat(newLine)
                        .concat("  items:").concat(newLine)
                        .concat("    conventionalCommits:").concat(newLine)
                        .concat("      expression: \"(?m)^(?<type>[a-zA-Z0-9_]+)(!)?(\\\\((?<scope>[a-z ]+)\\\\))?:( (?<title>.+))$(?s).*\"").concat(newLine)
                        .concat("      bumpExpressions:").concat(newLine)
                        .concat("        major: \"(?s)(?m)^[a-zA-Z0-9_]+(!|.*^(BREAKING( |-)CHANGE: )).*\"").concat(newLine)
                        .concat("        minor: \"(?s)(?m)^feat(?!!|.*^(BREAKING( |-)CHANGE: )).*\"").concat(newLine)
                        .concat("        patch: \"(?s)(?m)^fix(?!!|.*^(BREAKING( |-)CHANGE: )).*\"").concat(newLine)
                        .concat("initialVersion: \"1.0.0\"").concat(newLine)
                        .concat("releaseLenient: true").concat(newLine)
                        .concat("releasePrefix: v").concat(newLine)
                        .concat("scheme: SEMVER").concat(newLine),
                    // the .nyx.json is the standard configuration file
				    ".nyx.json", "{".concat(newLine)
                        .concat("    \"changelog\":{").concat(newLine)
                        .concat("        \"path\":\"CHANGELOG.md\",").concat(newLine)
                        .concat("        \"sections\":{").concat(newLine)
                        .concat("            \"Added\":\"^feat$\",").concat(newLine)
                        .concat("            \"Fixed\":\"^fix$\"").concat(newLine)
                        .concat("        },").concat(newLine)
                        .concat("        \"substitutions\":{").concat(newLine)
                        .concat("            \"(?m)#([0-9]+)(?s)\": \"[#%s](https://example.com/issues/%s)\"").concat(newLine)
                        .concat("        }").concat(newLine)
                        .concat("    },").concat(newLine)
                        .concat("    \"releaseAssets\": {").concat(newLine)
                        .concat("        \"asset1\":{").concat(newLine)
                        .concat("            \"fileName\":\"CHANGELOG.md\",").concat(newLine)
                        .concat("            \"description\":\"CHANGELOG\",").concat(newLine)
                        .concat("            \"type\":\"text/plain\",").concat(newLine)
                        .concat("            \"path\":\"CHANGELOG.md\"").concat(newLine)
                        .concat("        }").concat(newLine)
                        .concat("    },").concat(newLine)
                        .concat("    \"releaseTypes\": {").concat(newLine)
                        .concat("        \"remoteRepositories\":[").concat(newLine)
                        .concat("            \"origin\"").concat(newLine)
                        .concat("        ],").concat(newLine)
                        .concat("        \"items\": {").concat(newLine)
                        .concat("            \"mainline\": {").concat(newLine)
                        .concat("                \"collapseVersions\":false,").concat(newLine)
                        .concat("                \"filterTags\":\"^({{configuration.releasePrefix}})?([0-9]\\\\d*)\\\\.([0-9]\\\\d*)\\\\.([0-9]\\\\d*)$\",").concat(newLine)
                        .concat("                \"gitCommit\":\"true\",").concat(newLine)
                        .concat("                \"gitCommitMessage\":\"Release version {{version}}\",").concat(newLine)
                        .concat("                \"gitPush\":\"false\",").concat(newLine)
                        .concat("                \"gitTag\":\"true\",").concat(newLine)
                        .concat("                \"gitTagMessage\":\"Tag version {{version}}\",").concat(newLine)
                        .concat("                \"matchBranches\":\"^(master|main)$\",").concat(newLine)
                        .concat("                \"publish\":\"true\"").concat(newLine)
                        .concat("            }").concat(newLine)
                        .concat("        }").concat(newLine)
                        .concat("    }").concat(newLine)
                        .concat("}")
                );
                remoteRepoName  = null; // use 'origin', from the cloned repository from the hosting service
                hostingRepoService = "gitlab";
                fileContentChecks = Map.<String,Set<String>>of(
                    ".nyx-state.yml", Set.<String>of(
                        "bump: \"minor\"",
                        "initialVersion: \"1.0.0\"",
                        "preset: \"extended\"",
                        "releaseLenient: true",
                        "releasePrefix: \"v\"",
                        "newVersion: true",
                        "newRelease: true",
                        "previousVersion: \"0.0.4\"",
                        "primeVersion: \"0.0.4\"",
                        "version: \"v0.1.0\""
                    ),
                    "CHANGELOG.md", Set.<String>of(
                        "## v0.1.0",
                        "### Added",
                        "* [] feat: Untagged commit [#2](https://example.com/issues/2)",
                        "### Fixed",
                        "* [] fix: Untagged commit [#1](https://example.com/issues/1)"
                    )
                );
                repositoryTags = Set.<String>of("v0.1.0");
                remoteRepositoryTags = Set.<String>of(); // // here we don't use the locally managed remote repository
                hostedReleaseTags = Set.<String>of(); // not using hosting services for this suite
            }},

            // This suite runs Nyx Infer using a common configuration with also GitLab as the hosting service (using the settings Groovy Gradle plugin)
            new TestSuite(){{
                name        = "GitLab [Groovy Settings Plugin]";
                args        = new String[]{"--info", "--stacktrace"}; // these are command line arguments for Gradle, not the Nyx plugin
                tasks       = new String[]{"nyxPublish"};
                scenario    = Scenario.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS;
                env         = Map.<String,String>of(
                                "GL_TOKEN",                             System.getProperty("gitLabTestUserToken"), // The 'gitLabTestUserToken' variable is set in Gradle build scripts
                                "NYX_PRESET",                       "extended",
                                "NYX_GIT_REMOTES_origin_USER",      "PRIVATE-TOKEN",
				                "NYX_GIT_REMOTES_origin_PASSWORD",  "{{#environmentVariable}}GL_TOKEN{{/environmentVariable}}"
                );
                files       = Map.<String,String>of(
                    // the .gitignore file, required to avoid conflicts due to locked files (i.e. 'The process cannot access the file because another process has locked a portion of the file') etc
                    ".gitignore", ".gitignore".concat(newLine)
                        .concat(".gradle").concat(newLine)
                        .concat("build.gradle").concat(newLine)
                        .concat("settings.gradle").concat(newLine)
                        .concat(".nyx-state.yml").concat(newLine),
                    // the Gradle build script
                    "build.gradle", "".concat(newLine)
                        .concat("task dummy() {").concat(newLine)
                        .concat("}").concat(newLine),
                    // the Gradle settings file
                    "settings.gradle", "".concat(newLine)
                        .concat("plugins {").concat(newLine)
                        .concat("    id 'com.mooltiverse.oss.nyx'").concat(newLine)
                        .concat("}").concat(newLine)
                        .concat(newLine)
                        .concat("rootProject.name = 'nyx-gradle-plugin-test'").concat(newLine)
                        .concat(newLine)
                        .concat("nyx {").concat(newLine)
                        .concat("    dryRun = false").concat(newLine)
                        .concat("    resume = false").concat(newLine)
                        .concat("    stateFile = '.nyx-state.yml'").concat(newLine)
                        .concat("    verbosity = \"DEBUG\"").concat(newLine)
                        .concat("}").concat(newLine),
                    // the .nyx-shared.yaml is the standard shared configuration file
                    ".nyx-shared.yaml", "---".concat(newLine)
                        .concat("commitMessageConventions:").concat(newLine)
                        .concat("  enabled:").concat(newLine)
                        .concat("    - conventionalCommits").concat(newLine)
                        .concat("  items:").concat(newLine)
                        .concat("    conventionalCommits:").concat(newLine)
                        .concat("      expression: \"(?m)^(?<type>[a-zA-Z0-9_]+)(!)?(\\\\((?<scope>[a-z ]+)\\\\))?:( (?<title>.+))$(?s).*\"").concat(newLine)
                        .concat("      bumpExpressions:").concat(newLine)
                        .concat("        major: \"(?s)(?m)^[a-zA-Z0-9_]+(!|.*^(BREAKING( |-)CHANGE: )).*\"").concat(newLine)
                        .concat("        minor: \"(?s)(?m)^feat(?!!|.*^(BREAKING( |-)CHANGE: )).*\"").concat(newLine)
                        .concat("        patch: \"(?s)(?m)^fix(?!!|.*^(BREAKING( |-)CHANGE: )).*\"").concat(newLine)
                        .concat("initialVersion: \"1.0.0\"").concat(newLine)
                        .concat("releaseLenient: true").concat(newLine)
                        .concat("releasePrefix: v").concat(newLine)
                        .concat("scheme: SEMVER").concat(newLine),
                    // the .nyx.json is the standard configuration file
				    ".nyx.json", "{".concat(newLine)
                        .concat("    \"changelog\":{").concat(newLine)
                        .concat("        \"path\":\"CHANGELOG.md\",").concat(newLine)
                        .concat("        \"sections\":{").concat(newLine)
                        .concat("            \"Added\":\"^feat$\",").concat(newLine)
                        .concat("            \"Fixed\":\"^fix$\"").concat(newLine)
                        .concat("        },").concat(newLine)
                        .concat("        \"substitutions\":{").concat(newLine)
                        .concat("            \"(?m)#([0-9]+)(?s)\": \"[#%s](https://example.com/issues/%s)\"").concat(newLine)
                        .concat("        }").concat(newLine)
                        .concat("    },").concat(newLine)
                        .concat("    \"releaseAssets\": {").concat(newLine)
                        .concat("        \"asset1\":{").concat(newLine)
                        .concat("            \"fileName\":\"CHANGELOG.md\",").concat(newLine)
                        .concat("            \"description\":\"CHANGELOG\",").concat(newLine)
                        .concat("            \"type\":\"text/plain\",").concat(newLine)
                        .concat("            \"path\":\"CHANGELOG.md\"").concat(newLine)
                        .concat("        }").concat(newLine)
                        .concat("    },").concat(newLine)
                        .concat("    \"releaseTypes\": {").concat(newLine)
                        .concat("        \"remoteRepositories\":[").concat(newLine)
                        .concat("            \"origin\"").concat(newLine)
                        .concat("        ],").concat(newLine)
                        .concat("        \"items\": {").concat(newLine)
                        .concat("            \"mainline\": {").concat(newLine)
                        .concat("                \"collapseVersions\":false,").concat(newLine)
                        .concat("                \"filterTags\":\"^({{configuration.releasePrefix}})?([0-9]\\\\d*)\\\\.([0-9]\\\\d*)\\\\.([0-9]\\\\d*)$\",").concat(newLine)
                        .concat("                \"gitCommit\":\"true\",").concat(newLine)
                        .concat("                \"gitCommitMessage\":\"Release version {{version}}\",").concat(newLine)
                        .concat("                \"gitPush\":\"false\",").concat(newLine)
                        .concat("                \"gitTag\":\"true\",").concat(newLine)
                        .concat("                \"gitTagMessage\":\"Tag version {{version}}\",").concat(newLine)
                        .concat("                \"matchBranches\":\"^(master|main)$\",").concat(newLine)
                        .concat("                \"publish\":\"true\"").concat(newLine)
                        .concat("            }").concat(newLine)
                        .concat("        }").concat(newLine)
                        .concat("    }").concat(newLine)
                        .concat("}")
                );
                remoteRepoName  = null; // use 'origin', from the cloned repository from the hosting service
                hostingRepoService = "gitlab";
                fileContentChecks = Map.<String,Set<String>>of(
                    ".nyx-state.yml", Set.<String>of(
                        "bump: \"minor\"",
                        "initialVersion: \"1.0.0\"",
                        "preset: \"extended\"",
                        "releaseLenient: true",
                        "releasePrefix: \"v\"",
                        "newVersion: true",
                        "newRelease: true",
                        "previousVersion: \"0.0.4\"",
                        "primeVersion: \"0.0.4\"",
                        "version: \"v0.1.0\""
                    ),
                    "CHANGELOG.md", Set.<String>of(
                        "## v0.1.0",
                        "### Added",
                        "* [] feat: Untagged commit [#2](https://example.com/issues/2)",
                        "### Fixed",
                        "* [] fix: Untagged commit [#1](https://example.com/issues/1)"
                    )
                );
                repositoryTags = Set.<String>of("v0.1.0");
                remoteRepositoryTags = Set.<String>of(); // // here we don't use the locally managed remote repository
                hostedReleaseTags = Set.<String>of(); // not using hosting services for this suite
            }},
        };

        if (quickTestsOnly) 
            return wellKnownFunctionalTestSuitesQuick;
        else {
            TestSuite[] res = new TestSuite[wellKnownFunctionalTestSuitesQuick.length + wellKnownFunctionalTestSuitesNonQuick.length];
            System.arraycopy(wellKnownFunctionalTestSuitesQuick, 0, res, 0, wellKnownFunctionalTestSuitesQuick.length);
            System.arraycopy(wellKnownFunctionalTestSuitesNonQuick, 0, res, wellKnownFunctionalTestSuitesQuick.length, wellKnownFunctionalTestSuitesNonQuick.length);
            return res;
        }
    }

    /**
     * A {@link MethodSource} method that returns valid structured data to test gradle tasks.
     * 
     * Each returned argument has a test suite.
     *
     * @return a stream of arguments representing test suites
     */
    public static Stream<Arguments> wellKnownTestSuites() {
        boolean quickTests = Boolean.valueOf(System.getProperty("quickTests"));

        // raise early and clear exceptions in case credentials are not available for testing
        if (!quickTests && Objects.isNull(System.getProperty("gitHubTestUserToken")))
            throw new NullPointerException("The 'gitHubTestUserToken' system property was not set but is required to run the tests");
        if (!quickTests && Objects.isNull(System.getProperty("gitLabTestUserToken")))
            throw new NullPointerException("The 'gitLabTestUserToken' system property was not set but is required to run the tests");

        ArrayList<Arguments> arguments = new ArrayList<Arguments>();
        for (String gradleversion: wellKnownWorkingGradleVersions(quickTests)) {
            for (TestSuite suite : wellKnownFunctionalTestSuites(quickTests)) {
                suite.gradleVersion = gradleversion;
                arguments.add(Arguments.of(suite));
            }
        }
        return arguments.stream();
    }
}
