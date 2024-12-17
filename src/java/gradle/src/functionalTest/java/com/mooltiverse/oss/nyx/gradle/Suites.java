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
import java.util.Collections;
import java.util.List;
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
     * Returns array of Gradle versions to succesfully test against. The returned array also takes into account
     * the JVM version so it returns only Gradle versions compatible thìith the JVM.
     * 
     * The list is taken from https://gradle.org/releases/.
     * 
     * @param quickTestsOnly if {@code true} the result will contain a minimum set of significant versions,
     * otherwise all suites are returned.
     * 
     * @return a new array of test suites
     */
    public static String[] wellKnownWorkingGradleVersions(boolean quickTestsOnly) {
        // Get the current Java version from the system properties
        String javaVersionString = System.getProperty("java.specification.version");
        if (Objects.isNull(javaVersionString) || javaVersionString.isBlank())
            throw new java.lang.RuntimeException(String.format("Unable to inspect the current Java version. The system property '%s' valueis '%s'", "java.specification.version", javaVersionString));
        int javaVersion = 0;
        try {
            javaVersion = Integer.parseInt(javaVersionString);
        } catch (NumberFormatException nfe) {
            throw new java.lang.RuntimeException(String.format("Unable to inspect the current Java version. The system property '%s' value is '%s' and acnnot be parsed to a valid integer", "java.specification.version", javaVersionString), nfe);
        }
        if (javaVersion < 11)
            throw new java.lang.RuntimeException(String.format("Java version '%d' is not supported by the Gradle versions supported by Nyx", javaVersion));

        // Now, with the runtime Java version we can determine the Gradle versions we can test, according to https://docs.gradle.org/current/userguide/compatibility.html
        // These values are important because if we test a Gradle version (by simulating it with TestKit) that doesn't support a certan Java version (because it's too
        // new compared to gradle, usually because it was released after a given Gradle version), we get errors like like 'Unsupported class file major version XX'.
        // The following are modelled respecting the above compatibility matrix.

        // We use two separate collections here, one with the versions to test even when 'quick' tests are selected (the minimum set of significant versions),
        // and the other with versions to be tested when we run extensive tests (not just the 'quick' ones).
        List<String> quickTestVersions = new ArrayList<String>();
        List<String> extensiveTestVersions = new ArrayList<String>();

        // Java versions >= 15 are recommended
        // Gradle versions from 7.0 on are recommended
        if (javaVersion <= 20) {
            // the latest version is always among the 'quick' tests
/*            quickTestVersions.add("8.11.1");
            extensiveTestVersions.add("8.11");
            extensiveTestVersions.add("8.10.2");
            extensiveTestVersions.add("8.10.1");
            extensiveTestVersions.add("8.10");
            extensiveTestVersions.add("8.9");
            extensiveTestVersions.add("8.8");
            extensiveTestVersions.add("8.7");
            extensiveTestVersions.add("8.6");
            extensiveTestVersions.add("8.5");
            extensiveTestVersions.add("8.4");
            extensiveTestVersions.add("8.3");
*/
        }
        if (javaVersion <= 19) {
            // the latest version is always among the 'quick' tests
/*            
            quickTestVersions.add("8.2.1");
            extensiveTestVersions.add("8.2");
            extensiveTestVersions.add("8.1.1");
            extensiveTestVersions.add("8.1");
            extensiveTestVersions.add("8.0.2");
            extensiveTestVersions.add("8.0.1");
            extensiveTestVersions.add("8.0");

            // the previous major version is in the 'quick' tests
            quickTestVersions.add("7.6.4");
            extensiveTestVersions.add("7.6.3");
            extensiveTestVersions.add("7.6.2");
            extensiveTestVersions.add("7.6.1");
            extensiveTestVersions.add("7.6");
*/
quickTestVersions.add("8.2.1");
quickTestVersions.add("8.2");
quickTestVersions.add("8.1.1");
quickTestVersions.add("8.1");
quickTestVersions.add("8.0.2");
extensiveTestVersions.add("8.0.1");
quickTestVersions.add("8.0");

quickTestVersions.add("7.6.4");
quickTestVersions.add("7.6.3");
quickTestVersions.add("7.6.2");
quickTestVersions.add("7.6.1");
quickTestVersions.add("7.6");
        }
        if (javaVersion <= 18) {
            // Gradle versions between 7.0.x and 7.5.x have a bug that prevents using some jars
            // packaged with JDK version < 19. For examples ee https://github.com/gradle/gradle/issues/24390
            /*
            extensiveTestVersions.add("7.5.1");
            extensiveTestVersions.add("7.5");
            */
        }
        if (javaVersion <= 17) {
            // Gradle versions between 7.0.x and 7.5.x have a bug that prevents using some jars
            // packaged with JDK version < 19. For examples ee https://github.com/gradle/gradle/issues/24390
            /*
            extensiveTestVersions.add("7.4.2");
            extensiveTestVersions.add("7.4.1");
            extensiveTestVersions.add("7.4");
            extensiveTestVersions.add("7.3.3");
            extensiveTestVersions.add("7.3.2");
            extensiveTestVersions.add("7.3.1");
            extensiveTestVersions.add("7.3");
            */
        }
        if (javaVersion <= 16) {
/*
            extensiveTestVersions.add("7.2");
            extensiveTestVersions.add("7.1.1");
            extensiveTestVersions.add("7.1");
            extensiveTestVersions.add("7.0.2");
            extensiveTestVersions.add("7.0.1");
            extensiveTestVersions.add("7.0");
            */
quickTestVersions.add("7.2");
quickTestVersions.add("7.1.1");
quickTestVersions.add("7.1");
quickTestVersions.add("7.0.2");
quickTestVersions.add("7.0.1");
quickTestVersions.add("7.0");
        }
        // Gradle versions between 6.0 and 7.0 (excluded) are not recommended but still supported for backward compatibility,
        // with exceptions as per https://github.com/mooltiverse/nyx/issues/153
        if (javaVersion <= 15) {
            // these versions are not supported due to the ASM version it uses, see https://github.com/mooltiverse/nyx/issues/153
            /*
            extensiveTestVersions.add("6.9.4");
            extensiveTestVersions.add("6.9.3");
            extensiveTestVersions.add("6.9.2");
            extensiveTestVersions.add("6.9.1");
            extensiveTestVersions.add("6.9");
            extensiveTestVersions.add("6.8.3");
            extensiveTestVersions.add("6.8.2");
            extensiveTestVersions.add("6.8.1");
            extensiveTestVersions.add("6.8");
            extensiveTestVersions.add("6.7.1");
            quickTestVersions.add("6.7"); // 6.7 is the minimum supported version so we keep it in the 'quick' tests
            */
        }
        // Java versions between 11 and 14 are not recommended but still supported for backward compatibility
        if (javaVersion <= 14) {
            // these versions are not supported due to the ASM version it uses, see https://github.com/mooltiverse/nyx/issues/153
            /*
            extensiveTestVersions.add("6.6.1");
            extensiveTestVersions.add("6.6");
            extensiveTestVersions.add("6.5.1");
            //extensiveTestVersions.add("6.5"); // - this version has a bug (https://github.com/gradle/gradle/issues/13367) that prevents us to test, fixed in "6.5.1"
            */
/*
            extensiveTestVersions.add("6.4.1");
            extensiveTestVersions.add("6.4");
            extensiveTestVersions.add("6.3");
            */
            quickTestVersions.add("6.4.1");
            quickTestVersions.add("6.4");
            quickTestVersions.add("6.3");
        }
        if (javaVersion <= 13) {
/*
            extensiveTestVersions.add("6.2.2");
            extensiveTestVersions.add("6.2.1");
            extensiveTestVersions.add("6.2");
            extensiveTestVersions.add("6.1.1");
            extensiveTestVersions.add("6.1");
            extensiveTestVersions.add("6.0.1");
            extensiveTestVersions.add("6.0");
            */
            quickTestVersions.add("6.2.2");
            quickTestVersions.add("6.2.1");
            quickTestVersions.add("6.2");
            quickTestVersions.add("6.1.1");
            quickTestVersions.add("6.1");
            quickTestVersions.add("6.0.1");
            quickTestVersions.add("6.0");
        }
        /* Gradle versions prior than 6.0 fails to test with an exception like:
                > Could not find method services() for arguments [build_4o3mdmvy94ykemibox706yopu$_run_closure1$_closure2@18c3fdb5] on object of type com.mooltiverse.oss.nyx.gradle.NyxExtension.
           This means it has a different method for setting nested blocks into the extension object.
           If support for these versions is strongly needed we may find a workaround but it's worthless so far.

           Gradle versions prior than 5.5 do not support ObjectFactory.domainObjectContainer​(Class<T> elementType), indeed introduced in version 5.5,
           which is used in NyxExtension.*/
        /*if (javaVersion <= 12) {
            extensiveTestVersions.add("5.6.4");
            extensiveTestVersions.add("5.6.3");
            extensiveTestVersions.add("5.6.2");
            extensiveTestVersions.add("5.6.1");
            extensiveTestVersions.add("5.6");
            extensiveTestVersions.add("5.5.1");
            extensiveTestVersions.add("5.5");
            extensiveTestVersions.add("5.4.1");
            extensiveTestVersions.add("5.4");
        }*/
        /*if (javaVersion <= 11) {
            extensiveTestVersions.add("5.3.1");
            extensiveTestVersions.add("5.3");
            extensiveTestVersions.add("5.2.1");
            extensiveTestVersions.add("5.2");
            extensiveTestVersions.add("5.1.1");
            extensiveTestVersions.add("5.1");
            extensiveTestVersions.add("5.0");
        }*/
        /* Gradle versions from 4.9 to 5.1.1 fail to test with an exception like:
                > Could not create an instance of type com.mooltiverse.oss.nyx.gradle.NyxExtension_Decorated.
                    > Could not find any public constructor for class com.mooltiverse.oss.nyx.gradle.NyxExtension_Decorated which accepts parameters [].

           This has to deal with the injection of the ObjectFactory in constructors (i.e. in the NyxExtension) and is solved by adding another
           constructor with no parameters, which in turn implies another workaround to get an ObjectFactory.

           Gradle versions prior than 4.9 do not support Conviguration Avoidance API (https://docs.gradle.org/current/userguide/task_configuration_avoidance.html)

           Gradle version 4.7 fails to test with an exception like:
                Could not create service of type ScriptPluginFactory using BuildScopeServices.createScriptPluginFactory().
                > Could not create service of type PluginResolutionStrategyInternal using BuildScopeServices.createPluginResolutionStrategy().
           
           */
        /*if (javaVersion <= 10) {
            extensiveTestVersions.add("4.10.3");
            extensiveTestVersions.add("4.10.2");
            extensiveTestVersions.add("4.10.1");
            extensiveTestVersions.add("4.10");
            extensiveTestVersions.add("4.9");
            extensiveTestVersions.add("4.8.1");
            extensiveTestVersions.add("4.8");
            extensiveTestVersions.add("4.7");
        }*/
        /* Gradle versions prior between 2.6 and 4.6 fail to test with an exception like:
                org.gradle.api.GradleException: Unable to start the daemon process.
                ...
                Could not create service of type DaemonContext using DaemonServices.createDaemonContext().*/
        /*if (javaVersion <= 9) {
            extensiveTestVersions.add("4.6");
            extensiveTestVersions.add("4.5.1");
            extensiveTestVersions.add("4.5");
            extensiveTestVersions.add("4.4.1");
            extensiveTestVersions.add("4.4");
            extensiveTestVersions.add("4.3.1");
            extensiveTestVersions.add("4.3");
        }*/
        /* Gradle versions prior than 2.6 are not supported by Testkit
                See: https://docs.gradle.org/current/userguide/test_kit.html#sub:test-kit-compatibility*/
        /*if (javaVersion <= 8) {
            extensiveTestVersions.add("4.2.1");
            extensiveTestVersions.add("4.2");
            extensiveTestVersions.add("4.1");
            extensiveTestVersions.add("4.0.2");
            extensiveTestVersions.add("4.0.1");
            extensiveTestVersions.add("4.0");
            extensiveTestVersions.add("3.5.1");
            extensiveTestVersions.add("3.5");
            extensiveTestVersions.add("3.4.1");
            extensiveTestVersions.add("3.4");
            extensiveTestVersions.add("3.3");
            extensiveTestVersions.add("3.2.1");
            extensiveTestVersions.add("3.2");
            extensiveTestVersions.add("3.1");
            extensiveTestVersions.add("3.0");
            extensiveTestVersions.add("2.14.1");
            extensiveTestVersions.add("2.14");
            extensiveTestVersions.add("2.13");
            extensiveTestVersions.add("2.12");
            extensiveTestVersions.add("2.11");
            extensiveTestVersions.add("2.10");
            extensiveTestVersions.add("2.9");
            extensiveTestVersions.add("2.8");
            extensiveTestVersions.add("2.7");
            extensiveTestVersions.add("2.6");
            extensiveTestVersions.add("2.5");
            extensiveTestVersions.add("2.4");
            extensiveTestVersions.add("2.3");
            extensiveTestVersions.add("2.2.1");
            extensiveTestVersions.add("2.2");
            extensiveTestVersions.add("2.1");
            extensiveTestVersions.add("2.0");
        }*/

        List<String> allSupportedVersions = new ArrayList<String>(quickTestVersions);
        allSupportedVersions.addAll(extensiveTestVersions);
        Collections.sort(allSupportedVersions, Collections.reverseOrder());

        System.out.println("********************************************************************************************************");System.out.flush();
        System.out.println("ATTENTION: JVM and Gradle versions");System.out.flush();
        System.out.println("The current JVM version is "+javaVersionString+" and the list of Gradle versions supported by Testkit");System.out.flush();
        System.out.println("for this version has been narrowed to compatible ones, according to the Gradle compatibility matrix at:");System.out.flush();
        System.out.println("    https://docs.gradle.org/current/userguide/compatibility.html");System.out.flush();
        System.out.println("Namely, the list of supported Gradle versions for this VM is:");System.out.flush();
        System.out.println("    "+String.join(", ", allSupportedVersions));System.out.flush();
        System.out.println("Where the ones always tested, even when the 'quick' tests only are executed, are:");System.out.flush();
        System.out.println("    "+String.join(", ", quickTestVersions));System.out.flush();
        System.out.println("and those tested only for extended tests are:");System.out.flush();
        System.out.println("    "+String.join(", ", extensiveTestVersions));System.out.flush();
        System.out.println("Running these tests with a lower JVM version extends the set of Gradle versions compatible for testing");System.out.flush();
        System.out.println("so if you're trying to test against a version not listed here you may just need to run on an older JVM,");System.out.flush();
        System.out.println("as long as it's supported (11 or above).");System.out.flush();
        System.out.println("********************************************************************************************************");System.out.flush();

        // if quick tests are requested, just return the smallest significant versions, otherwise return the union between the two lists
        return quickTestsOnly ? quickTestVersions.toArray(new String[0]) : allSupportedVersions.toArray(new String[0]);
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
                        .concat(".nyx-summary.txt").concat(newLine)
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
                        .concat("    summaryFile = '.nyx-summary.txt'").concat(newLine)
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
                        .concat("      expression: \"(?m)^(?<type>[a-zA-Z0-9_]+)(\\\\((?<scope>[a-z ]+)\\\\))?(!)?:( (?<title>.+))$(?s).*\"").concat(newLine)
                        .concat("      bumpExpressions:").concat(newLine)
                        // note we use single quotes in the rows below to avoid escaping all of the YAML special characters
                        .concat("        major: '(?s)(?m)^[a-zA-Z0-9_]+(\\([a-z ]+\\))?(!: .*|.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
                        .concat("        minor: '(?s)(?m)^feat(!{0})(\\([a-z ]+\\))?: (?!.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
                        .concat("        patch: '(?s)(?m)^fix(!{0})(\\([a-z ]+\\))?: (?!.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
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
                        "coreVersion: true",
                        "newVersion: true",
                        "newRelease: true",
                        "previousVersion: \"0.0.4\"",
                        "primeVersion: \"0.0.4\"",
                        "version: \"v0.1.0\""
                    ),
                    ".nyx-summary.txt", Set.<String>of(
                        "bump             = minor",
                        "core version     = true",
                        "new release      = true",
                        "new version      = true",
                        "scheme           = SEMVER",
                        "timestamp        = ", // we don't need to test for the timestamp value here
                        "current version  = v0.1.0",
                        "previous version = 0.0.4",
                        "prime version    = 0.0.4"
                    )
                );
                repositoryTags = Set.<String>of(); // no new tag is created by Infer
                remoteRepositoryTags = Set.<String>of(); // not using remotes for this suite
                hostedReleaseTags = Set.<String>of(); // not using hosting services for this suite
            }},

            // This suite runs Nyx Infer using a common configuration (using the regular Kotlin Gradle plugin)
            new TestSuite(){{
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
                        .concat("build.gradle.kts").concat(newLine)
                        .concat("settings.gradle.kts").concat(newLine)
                        .concat(".nyx-summary.txt").concat(newLine)
                        .concat(".nyx-state.yml").concat(newLine),
                    // the Gradle build script
                    "build.gradle.kts", "".concat(newLine)
                        .concat("plugins {").concat(newLine)
                        .concat("    id(\"com.mooltiverse.oss.nyx\")").concat(newLine)
                        .concat("}").concat(newLine)
                        .concat(newLine)
                        .concat("nyx {").concat(newLine)
                        .concat("    dryRun.set(false)").concat(newLine)
                        .concat("    resume.set(false)").concat(newLine)
                        .concat("    summaryFile.set(\".nyx-summary.txt\")").concat(newLine)
                        .concat("    stateFile.set(\".nyx-state.yml\")").concat(newLine)
                        .concat("    verbosity.set(\"DEBUG\")").concat(newLine)
                        .concat("}").concat(newLine),
                    // the Gradle settings file
                    "settings.gradle.kts", "".concat(newLine)
                        .concat("rootProject.name = \"nyx-gradle-plugin-test\"").concat(newLine),
                    // the .nyx-shared.yaml is the standard shared configuration file
                    ".nyx-shared.yaml", "---".concat(newLine)
                        .concat("commitMessageConventions:").concat(newLine)
                        .concat("  enabled:").concat(newLine)
                        .concat("    - conventionalCommits").concat(newLine)
                        .concat("  items:").concat(newLine)
                        .concat("    conventionalCommits:").concat(newLine)
                        .concat("      expression: \"(?m)^(?<type>[a-zA-Z0-9_]+)(\\\\((?<scope>[a-z ]+)\\\\))?(!)?:( (?<title>.+))$(?s).*\"").concat(newLine)
                        .concat("      bumpExpressions:").concat(newLine)
                        // note we use single quotes in the rows below to avoid escaping all of the YAML special characters
                        .concat("        major: '(?s)(?m)^[a-zA-Z0-9_]+(\\([a-z ]+\\))?(!: .*|.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
                        .concat("        minor: '(?s)(?m)^feat(!{0})(\\([a-z ]+\\))?: (?!.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
                        .concat("        patch: '(?s)(?m)^fix(!{0})(\\([a-z ]+\\))?: (?!.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
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
                    ),
                    ".nyx-summary.txt", Set.<String>of(
                        "bump             = minor",
                        "core version     = true",
                        "new release      = true",
                        "new version      = true",
                        "scheme           = SEMVER",
                        "timestamp        = ", // we don't need to test for the timestamp value here
                        "current version  = v0.1.0",
                        "previous version = 0.0.4",
                        "prime version    = 0.0.4"
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
                        .concat(".nyx-summary.txt").concat(newLine)
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
                        .concat("    summaryFile = '.nyx-summary.txt'").concat(newLine)
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
                        .concat("      expression: \"(?m)^(?<type>[a-zA-Z0-9_]+)(\\\\((?<scope>[a-z ]+)\\\\))?(!)?:( (?<title>.+))$(?s).*\"").concat(newLine)
                        .concat("      bumpExpressions:").concat(newLine)
                        // note we use single quotes in the rows below to avoid escaping all of the YAML special characters
                        .concat("        major: '(?s)(?m)^[a-zA-Z0-9_]+(\\([a-z ]+\\))?(!: .*|.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
                        .concat("        minor: '(?s)(?m)^feat(!{0})(\\([a-z ]+\\))?: (?!.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
                        .concat("        patch: '(?s)(?m)^fix(!{0})(\\([a-z ]+\\))?: (?!.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
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
                        "coreVersion: true",
                        "newVersion: true",
                        "newRelease: true",
                        "previousVersion: \"0.0.4\"",
                        "primeVersion: \"0.0.4\"",
                        "version: \"v0.1.0\""
                    ),
                    ".nyx-summary.txt", Set.<String>of(
                        "bump             = minor",
                        "core version     = true",
                        "new release      = true",
                        "new version      = true",
                        "scheme           = SEMVER",
                        "timestamp        = ", // we don't need to test for the timestamp value here
                        "current version  = v0.1.0",
                        "previous version = 0.0.4",
                        "prime version    = 0.0.4"
                    )
                );
                repositoryTags = Set.<String>of(); // no new tag is created by Infer
                remoteRepositoryTags = Set.<String>of(); // not using remotes for this suite
                hostedReleaseTags = Set.<String>of(); // not using hosting services for this suite
            }},

            // This suite runs Nyx Infer using a common configuration (using the settings Kotlin Gradle plugin)
            new TestSuite(){{
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
                        .concat("build.gradle.kts").concat(newLine)
                        .concat("settings.gradle.kts").concat(newLine)
                        .concat(".nyx-summary.txt").concat(newLine)
                        .concat(".nyx-state.yml").concat(newLine),
                    // the Gradle build script
                    "build.gradle.kts", "".concat(newLine)
                        .concat("tasks.register(\"dummy\") {").concat(newLine)
                        .concat("}").concat(newLine),
                    // the Gradle settings file
                    "settings.gradle.kts", "".concat(newLine)
                        .concat("plugins {").concat(newLine)
                        .concat("    id(\"com.mooltiverse.oss.nyx\")").concat(newLine)
                        .concat("}").concat(newLine)
                        .concat(newLine)
                        .concat("rootProject.name = \"nyx-gradle-plugin-test\"").concat(newLine)
                        .concat(newLine)
                        .concat("configure<com.mooltiverse.oss.nyx.gradle.NyxExtension> {").concat(newLine)
                        .concat("    dryRun.set(false)").concat(newLine)
                        .concat("    resume.set(false)").concat(newLine)
                        .concat("    summaryFile.set(\".nyx-summary.txt\")").concat(newLine)
                        .concat("    stateFile.set(\".nyx-state.yml\")").concat(newLine)
                        .concat("    verbosity.set(\"DEBUG\")").concat(newLine)
                        .concat("}").concat(newLine),
                    // the .nyx-shared.yaml is the standard shared configuration file
                    ".nyx-shared.yaml", "---".concat(newLine)
                        .concat("commitMessageConventions:").concat(newLine)
                        .concat("  enabled:").concat(newLine)
                        .concat("    - conventionalCommits").concat(newLine)
                        .concat("  items:").concat(newLine)
                        .concat("    conventionalCommits:").concat(newLine)
                        .concat("      expression: \"(?m)^(?<type>[a-zA-Z0-9_]+)(\\\\((?<scope>[a-z ]+)\\\\))?(!)?:( (?<title>.+))$(?s).*\"").concat(newLine)
                        .concat("      bumpExpressions:").concat(newLine)
                        // note we use single quotes in the rows below to avoid escaping all of the YAML special characters
                        .concat("        major: '(?s)(?m)^[a-zA-Z0-9_]+(\\([a-z ]+\\))?(!: .*|.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
                        .concat("        minor: '(?s)(?m)^feat(!{0})(\\([a-z ]+\\))?: (?!.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
                        .concat("        patch: '(?s)(?m)^fix(!{0})(\\([a-z ]+\\))?: (?!.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
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
                    ),
                    ".nyx-summary.txt", Set.<String>of(
                        "bump             = minor",
                        "core version     = true",
                        "new release      = true",
                        "new version      = true",
                        "scheme           = SEMVER",
                        "timestamp        = ", // we don't need to test for the timestamp value here
                        "current version  = v0.1.0",
                        "previous version = 0.0.4",
                        "prime version    = 0.0.4"
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
                        .concat(".nyx-summary.txt").concat(newLine)
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
                        .concat("    summaryFile = '.nyx-summary.txt'").concat(newLine)
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
                        .concat("      expression: \"(?m)^(?<type>[a-zA-Z0-9_]+)(\\\\((?<scope>[a-z ]+)\\\\))?(!)?:( (?<title>.+))$(?s).*\"").concat(newLine)
                        .concat("      bumpExpressions:").concat(newLine)
                        // note we use single quotes in the rows below to avoid escaping all of the YAML special characters
                        .concat("        major: '(?s)(?m)^[a-zA-Z0-9_]+(\\([a-z ]+\\))?(!: .*|.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
                        .concat("        minor: '(?s)(?m)^feat(!{0})(\\([a-z ]+\\))?: (?!.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
                        .concat("        patch: '(?s)(?m)^fix(!{0})(\\([a-z ]+\\))?: (?!.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
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
                        "coreVersion: true",
                        "newVersion: true",
                        "newRelease: true",
                        "previousVersion: \"0.0.4\"",
                        "primeVersion: \"0.0.4\"",
                        "version: \"v0.1.0\""
                    ),
                    ".nyx-summary.txt", Set.<String>of(
                        "bump             = minor",
                        "core version     = true",
                        "new release      = true",
                        "new version      = true",
                        "scheme           = SEMVER",
                        "timestamp        = ", // we don't need to test for the timestamp value here
                        "current version  = v0.1.0",
                        "previous version = 0.0.4",
                        "prime version    = 0.0.4"
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
                        .concat(".nyx-summary.txt").concat(newLine)
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
                        .concat("    summaryFile = '.nyx-summary.txt'").concat(newLine)
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
                        .concat("      expression: \"(?m)^(?<type>[a-zA-Z0-9_]+)(\\\\((?<scope>[a-z ]+)\\\\))?(!)?:( (?<title>.+))$(?s).*\"").concat(newLine)
                        .concat("      bumpExpressions:").concat(newLine)
                        // note we use single quotes in the rows below to avoid escaping all of the YAML special characters
                        .concat("        major: '(?s)(?m)^[a-zA-Z0-9_]+(\\([a-z ]+\\))?(!: .*|.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
                        .concat("        minor: '(?s)(?m)^feat(!{0})(\\([a-z ]+\\))?: (?!.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
                        .concat("        patch: '(?s)(?m)^fix(!{0})(\\([a-z ]+\\))?: (?!.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
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
                        "coreVersion: true",
                        "newVersion: true",
                        "newRelease: true",
                        "previousVersion: \"0.0.4\"",
                        "primeVersion: \"0.0.4\"",
                        "version: \"v0.1.0\""
                    ),
                    ".nyx-summary.txt", Set.<String>of(
                        "bump             = minor",
                        "core version     = true",
                        "new release      = true",
                        "new version      = true",
                        "scheme           = SEMVER",
                        "timestamp        = ", // we don't need to test for the timestamp value here
                        "current version  = v0.1.0",
                        "previous version = 0.0.4",
                        "prime version    = 0.0.4"
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
                        .concat(".nyx-summary.txt").concat(newLine)
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
                        .concat("    summaryFile = '.nyx-summary.txt'").concat(newLine)
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
                        .concat("      expression: \"(?m)^(?<type>[a-zA-Z0-9_]+)(\\\\((?<scope>[a-z ]+)\\\\))?(!)?:( (?<title>.+))$(?s).*\"").concat(newLine)
                        .concat("      bumpExpressions:").concat(newLine)
                        // note we use single quotes in the rows below to avoid escaping all of the YAML special characters
                        .concat("        major: '(?s)(?m)^[a-zA-Z0-9_]+(\\([a-z ]+\\))?(!: .*|.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
                        .concat("        minor: '(?s)(?m)^feat(!{0})(\\([a-z ]+\\))?: (?!.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
                        .concat("        patch: '(?s)(?m)^fix(!{0})(\\([a-z ]+\\))?: (?!.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
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
                        "coreVersion: true",
                        "newVersion: true",
                        "newRelease: true",
                        "previousVersion: \"0.0.4\"",
                        "primeVersion: \"0.0.4\"",
                        "version: \"v0.1.0\""
                    ),
                    ".nyx-summary.txt", Set.<String>of(
                        "bump             = minor",
                        "core version     = true",
                        "new release      = true",
                        "new version      = true",
                        "scheme           = SEMVER",
                        "timestamp        = ", // we don't need to test for the timestamp value here
                        "current version  = v0.1.0",
                        "previous version = 0.0.4",
                        "prime version    = 0.0.4"
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
                        .concat(".nyx-summary.txt").concat(newLine)
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
                        .concat("    summaryFile = '.nyx-summary.txt'").concat(newLine)
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
                        .concat("      expression: \"(?m)^(?<type>[a-zA-Z0-9_]+)(\\\\((?<scope>[a-z ]+)\\\\))?(!)?:( (?<title>.+))$(?s).*\"").concat(newLine)
                        .concat("      bumpExpressions:").concat(newLine)
                        // note we use single quotes in the rows below to avoid escaping all of the YAML special characters
                        .concat("        major: '(?s)(?m)^[a-zA-Z0-9_]+(\\([a-z ]+\\))?(!: .*|.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
                        .concat("        minor: '(?s)(?m)^feat(!{0})(\\([a-z ]+\\))?: (?!.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
                        .concat("        patch: '(?s)(?m)^fix(!{0})(\\([a-z ]+\\))?: (?!.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
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
                        "coreVersion: true",
                        "newVersion: true",
                        "newRelease: true",
                        "previousVersion: \"0.0.4\"",
                        "primeVersion: \"0.0.4\"",
                        "version: \"v0.1.0\""
                    ),
                    ".nyx-summary.txt", Set.<String>of(
                        "bump             = minor",
                        "core version     = true",
                        "new release      = true",
                        "new version      = true",
                        "scheme           = SEMVER",
                        "timestamp        = ", // we don't need to test for the timestamp value here
                        "current version  = v0.1.0",
                        "previous version = 0.0.4",
                        "prime version    = 0.0.4"
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
                        .concat(".nyx-summary.txt").concat(newLine)
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
                        .concat("    summaryFile = '.nyx-summary.txt'").concat(newLine)
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
                        .concat("      expression: \"(?m)^(?<type>[a-zA-Z0-9_]+)(\\\\((?<scope>[a-z ]+)\\\\))?(!)?:( (?<title>.+))$(?s).*\"").concat(newLine)
                        .concat("      bumpExpressions:").concat(newLine)
                        // note we use single quotes in the rows below to avoid escaping all of the YAML special characters
                        .concat("        major: '(?s)(?m)^[a-zA-Z0-9_]+(\\([a-z ]+\\))?(!: .*|.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
                        .concat("        minor: '(?s)(?m)^feat(!{0})(\\([a-z ]+\\))?: (?!.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
                        .concat("        patch: '(?s)(?m)^fix(!{0})(\\([a-z ]+\\))?: (?!.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
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
                        .concat("                \"gitTagNames\":[\"{{version}}\", \"{{versionMajorNumber}}.{{versionMinorNumber}}\", \"stable\", \"latest\"],").concat(newLine)
                        .concat("                \"matchBranches\":\"^(master|main)$\",").concat(newLine)
                        .concat("                \"publish\":\"true\"").concat(newLine)
                        .concat("            }").concat(newLine)
                        .concat("        }").concat(newLine)
                        .concat("    },").concat(newLine)
                        .concat("    \"substitutions\":{").concat(newLine)
                        .concat("        \"enabled\":[ \"node_version\", \"text_version\" ]").concat(newLine)
                        .concat("    }").concat(newLine)
                        .concat("}"),
                    // the version.txt file, used to test for substitutions, initialized with a fake version number to be replaced by Make
                    "version.txt", "91.92.93"
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
                        "coreVersion: true",
                        "newVersion: true",
                        "newRelease: true",
                        "previousVersion: \"0.0.4\"",
                        "primeVersion: \"0.0.4\"",
                        "version: \"v0.1.0\""
                    ),
                    ".nyx-summary.txt", Set.<String>of(
                        "bump             = minor",
                        "core version     = true",
                        "new release      = true",
                        "new version      = true",
                        "scheme           = SEMVER",
                        "timestamp        = ", // we don't need to test for the timestamp value here
                        "current version  = v0.1.0",
                        "previous version = 0.0.4",
                        "prime version    = 0.0.4"
                    ),
                    "CHANGELOG.md", Set.<String>of(
                        "## v0.1.0",
                        "### Added",
                        "\\* \\[[a-z0-9]{5}\\] feat: Untagged commit \\[#2\\]\\(https:\\/\\/example\\.com\\/issues\\/2\\)",
                        "### Fixed",
                        "\\* \\[[a-z0-9]{5}\\] fix: Untagged commit \\[#1\\]\\(https:\\/\\/example\\.com\\/issues\\/1\\)"
                    ),
                    "version.txt", Set.<String>of(
                        "v0.1.0"
                    )
                );
                repositoryTags = Set.<String>of("v0.1.0", "0.1", "stable", "latest"); // also test for tag aliases set with gitTagNames
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
                        .concat(".nyx-summary.txt").concat(newLine)
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
                        .concat("    summaryFile = '.nyx-summary.txt'").concat(newLine)
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
                        .concat("      expression: \"(?m)^(?<type>[a-zA-Z0-9_]+)(\\\\((?<scope>[a-z ]+)\\\\))?(!)?:( (?<title>.+))$(?s).*\"").concat(newLine)
                        .concat("      bumpExpressions:").concat(newLine)
                        // note we use single quotes in the rows below to avoid escaping all of the YAML special characters
                        .concat("        major: '(?s)(?m)^[a-zA-Z0-9_]+(\\([a-z ]+\\))?(!: .*|.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
                        .concat("        minor: '(?s)(?m)^feat(!{0})(\\([a-z ]+\\))?: (?!.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
                        .concat("        patch: '(?s)(?m)^fix(!{0})(\\([a-z ]+\\))?: (?!.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
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
                        .concat("                \"gitTagNames\":[\"{{version}}\", \"{{versionMajorNumber}}.{{versionMinorNumber}}\", \"stable\", \"latest\"],").concat(newLine)
                        .concat("                \"matchBranches\":\"^(master|main)$\",").concat(newLine)
                        .concat("                \"publish\":\"true\"").concat(newLine)
                        .concat("            }").concat(newLine)
                        .concat("        }").concat(newLine)
                        .concat("    },").concat(newLine)
                        .concat("    \"substitutions\":{").concat(newLine)
                        .concat("        \"enabled\":[ \"node_version\", \"text_version\" ]").concat(newLine)
                        .concat("    }").concat(newLine)
                        .concat("}"),
                    // the version.txt file, used to test for substitutions, initialized with a fake version number to be replaced by Make
                    "version.txt", "91.92.93"
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
                        "coreVersion: true",
                        "newVersion: true",
                        "newRelease: true",
                        "previousVersion: \"0.0.4\"",
                        "primeVersion: \"0.0.4\"",
                        "version: \"v0.1.0\""
                    ),
                    ".nyx-summary.txt", Set.<String>of(
                        "bump             = minor",
                        "core version     = true",
                        "new release      = true",
                        "new version      = true",
                        "scheme           = SEMVER",
                        "timestamp        = ", // we don't need to test for the timestamp value here
                        "current version  = v0.1.0",
                        "previous version = 0.0.4",
                        "prime version    = 0.0.4"
                    ),
                    "CHANGELOG.md", Set.<String>of(
                        "## v0.1.0",
                        "### Added",
                        "\\* \\[[a-z0-9]{5}\\] feat: Untagged commit \\[#2\\]\\(https:\\/\\/example\\.com\\/issues\\/2\\)",
                        "### Fixed",
                        "\\* \\[[a-z0-9]{5}\\] fix: Untagged commit \\[#1\\]\\(https:\\/\\/example\\.com\\/issues\\/1\\)"
                    ),
                    "version.txt", Set.<String>of(
                        "v0.1.0"
                    )
                );
                repositoryTags = Set.<String>of("v0.1.0", "0.1", "stable", "latest"); // also test for tag aliases set with gitTagNames
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
                        .concat(".nyx-summary.txt").concat(newLine)
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
                        .concat("    summaryFile = '.nyx-summary.txt'").concat(newLine)
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
                        .concat("      expression: \"(?m)^(?<type>[a-zA-Z0-9_]+)(\\\\((?<scope>[a-z ]+)\\\\))?(!)?:( (?<title>.+))$(?s).*\"").concat(newLine)
                        .concat("      bumpExpressions:").concat(newLine)
                        // note we use single quotes in the rows below to avoid escaping all of the YAML special characters
                        .concat("        major: '(?s)(?m)^[a-zA-Z0-9_]+(\\([a-z ]+\\))?(!: .*|.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
                        .concat("        minor: '(?s)(?m)^feat(!{0})(\\([a-z ]+\\))?: (?!.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
                        .concat("        patch: '(?s)(?m)^fix(!{0})(\\([a-z ]+\\))?: (?!.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
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
                        .concat("    },").concat(newLine)
                        .concat("    \"substitutions\":{").concat(newLine)
                        .concat("        \"enabled\":[ \"node_version\", \"text_version\" ]").concat(newLine)
                        .concat("    }").concat(newLine)
                        .concat("}"),
                    // the version.txt file, used to test for substitutions, initialized with a fake version number to be replaced by Make
                    "version.txt", "91.92.93"
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
                        "coreVersion: true",
                        "newVersion: true",
                        "newRelease: true",
                        "previousVersion: \"0.0.4\"",
                        "primeVersion: \"0.0.4\"",
                        "version: \"v0.1.0\""
                    ),
                    ".nyx-summary.txt", Set.<String>of(
                        "bump             = minor",
                        "core version     = true",
                        "new release      = true",
                        "new version      = true",
                        "scheme           = SEMVER",
                        "timestamp        = ", // we don't need to test for the timestamp value here
                        "current version  = v0.1.0",
                        "previous version = 0.0.4",
                        "prime version    = 0.0.4"
                    ),
                    "CHANGELOG.md", Set.<String>of(
                        "## v0.1.0",
                        "### Added",
                        "\\* \\[[a-z0-9]{5}\\] feat: Untagged commit \\[#2\\]\\(https:\\/\\/example\\.com\\/issues\\/2\\)",
                        "### Fixed",
                        "\\* \\[[a-z0-9]{5}\\] fix: Untagged commit \\[#1\\]\\(https:\\/\\/example\\.com\\/issues\\/1\\)"
                    ),
                    "version.txt", Set.<String>of(
                        "v0.1.0"
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
                        .concat(".nyx-summary.txt").concat(newLine)
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
                        .concat("    summaryFile = '.nyx-summary.txt'").concat(newLine)
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
                        .concat("      expression: \"(?m)^(?<type>[a-zA-Z0-9_]+)(\\\\((?<scope>[a-z ]+)\\\\))?(!)?:( (?<title>.+))$(?s).*\"").concat(newLine)
                        .concat("      bumpExpressions:").concat(newLine)
                        // note we use single quotes in the rows below to avoid escaping all of the YAML special characters
                        .concat("        major: '(?s)(?m)^[a-zA-Z0-9_]+(\\([a-z ]+\\))?(!: .*|.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
                        .concat("        minor: '(?s)(?m)^feat(!{0})(\\([a-z ]+\\))?: (?!.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
                        .concat("        patch: '(?s)(?m)^fix(!{0})(\\([a-z ]+\\))?: (?!.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
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
                        .concat("    },").concat(newLine)
                        .concat("    \"substitutions\":{").concat(newLine)
                        .concat("        \"enabled\":[ \"node_version\", \"text_version\" ]").concat(newLine)
                        .concat("    }").concat(newLine)
                        .concat("}"),
                    // the version.txt file, used to test for substitutions, initialized with a fake version number to be replaced by Make
                    "version.txt", "91.92.93"
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
                        "coreVersion: true",
                        "newVersion: true",
                        "newRelease: true",
                        "previousVersion: \"0.0.4\"",
                        "primeVersion: \"0.0.4\"",
                        "version: \"v0.1.0\""
                    ),
                    ".nyx-summary.txt", Set.<String>of(
                        "bump             = minor",
                        "core version     = true",
                        "new release      = true",
                        "new version      = true",
                        "scheme           = SEMVER",
                        "timestamp        = ", // we don't need to test for the timestamp value here
                        "current version  = v0.1.0",
                        "previous version = 0.0.4",
                        "prime version    = 0.0.4"
                    ),
                    "CHANGELOG.md", Set.<String>of(
                        "## v0.1.0",
                        "### Added",
                        "\\* \\[[a-z0-9]{5}\\] feat: Untagged commit \\[#2\\]\\(https:\\/\\/example\\.com\\/issues\\/2\\)",
                        "### Fixed",
                        "\\* \\[[a-z0-9]{5}\\] fix: Untagged commit \\[#1\\]\\(https:\\/\\/example\\.com\\/issues\\/1\\)"
                    ),
                    "version.txt", Set.<String>of(
                        "v0.1.0"
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
                        .concat(".nyx-summary.txt").concat(newLine)
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
                        .concat("    summaryFile = '.nyx-summary.txt'").concat(newLine)
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
                        .concat("    },").concat(newLine)
                        .concat("    \"substitutions\":{").concat(newLine)
                        .concat("        \"enabled\":[ \"node_version\", \"text_version\" ]").concat(newLine)
                        .concat("    }").concat(newLine)
                        .concat("}"),
                    // the version.txt file, used to test for substitutions, initialized with a fake version number to be replaced by Make
                    "version.txt", "91.92.93"
                );
                remoteRepoName  = "replica";
                hostingRepoService = null;
                fileContentChecks = Map.<String,Set<String>>of(
                    ".nyx-state.yml", Set.<String>of(
                        "branch: \"master\"",
                        "bump: \"patch\"",
                        "initialVersion: \"0.1.0\"",
                        "preset: \"extended\"",
                        "coreVersion: true",
                        "newVersion: true",
                        "newRelease: true",
                        "previousVersion: \"0.0.5\"",
                        "primeVersion: \"0.0.5\"",
                        "version: \"0.0.6\""
                    ),
                    ".nyx-summary.txt", Set.<String>of(
                        "bump             = patch",
                        "core version     = true",
                        "new release      = true",
                        "new version      = true",
                        "scheme           = SEMVER",
                        "timestamp        = ", // we don't need to test for the timestamp value here
                        "current version  = 0.0.6",
                        "previous version = 0.0.5",
                        "prime version    = 0.0.5"
                    ),
                    "CHANGELOG.md", Set.<String>of(
                        "## 0.0.6",
					    "No changes."
                    ),
                    "version.txt", Set.<String>of(
                        "0.0.6"
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
                        .concat(".nyx-summary.txt").concat(newLine)
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
                        .concat("    summaryFile = '.nyx-summary.txt'").concat(newLine)
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
                        .concat("    },").concat(newLine)
                        .concat("    \"substitutions\":{").concat(newLine)
                        .concat("        \"enabled\":[ \"node_version\", \"text_version\" ]").concat(newLine)
                        .concat("    }").concat(newLine)
                        .concat("}"),
                    // the version.txt file, used to test for substitutions, initialized with a fake version number to be replaced by Make
                    "version.txt", "91.92.93"
                );
                remoteRepoName  = "replica";
                hostingRepoService = null;
                fileContentChecks = Map.<String,Set<String>>of(
                    ".nyx-state.yml", Set.<String>of(
                        "branch: \"master\"",
                        "bump: \"patch\"",
                        "initialVersion: \"0.1.0\"",
                        "preset: \"extended\"",
                        "coreVersion: true",
                        "newVersion: true",
                        "newRelease: true",
                        "previousVersion: \"0.0.5\"",
                        "primeVersion: \"0.0.5\"",
                        "version: \"0.0.6\""
                    ),
                    ".nyx-summary.txt", Set.<String>of(
                        "bump             = patch",
                        "core version     = true",
                        "new release      = true",
                        "new version      = true",
                        "scheme           = SEMVER",
                        "timestamp        = ", // we don't need to test for the timestamp value here
                        "current version  = 0.0.6",
                        "previous version = 0.0.5",
                        "prime version    = 0.0.5"
                    ),
                    "CHANGELOG.md", Set.<String>of(
                        "## 0.0.6",
					    "No changes."
                    ),
                    "version.txt", Set.<String>of(
                        "0.0.6"
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
                        .concat(".nyx-summary.txt").concat(newLine)
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
                        .concat("    summaryFile = '.nyx-summary.txt'").concat(newLine)
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
                        .concat("      expression: \"(?m)^(?<type>[a-zA-Z0-9_]+)(\\\\((?<scope>[a-z ]+)\\\\))?(!)?:( (?<title>.+))$(?s).*\"").concat(newLine)
                        .concat("      bumpExpressions:").concat(newLine)
                        // note we use single quotes in the rows below to avoid escaping all of the YAML special characters
                        .concat("        major: '(?s)(?m)^[a-zA-Z0-9_]+(\\([a-z ]+\\))?(!: .*|.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
                        .concat("        minor: '(?s)(?m)^feat(!{0})(\\([a-z ]+\\))?: (?!.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
                        .concat("        patch: '(?s)(?m)^fix(!{0})(\\([a-z ]+\\))?: (?!.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
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
                        .concat("                \"publish\":\"true\",").concat(newLine)
                        .concat("                \"releaseName\":\"Stable {{version}}\"").concat(newLine)
                        .concat("            }").concat(newLine)
                        .concat("        }").concat(newLine)
                        .concat("    },").concat(newLine)
                        .concat("    \"substitutions\":{").concat(newLine)
                        .concat("        \"enabled\":[ \"node_version\", \"text_version\" ]").concat(newLine)
                        .concat("    }").concat(newLine)
                        .concat("}"),
                    // the version.txt file, used to test for substitutions, initialized with a fake version number to be replaced by Make
                    "version.txt", "91.92.93"
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
                        "coreVersion: true",
                        "newVersion: true",
                        "newRelease: true",
                        "previousVersion: \"0.0.4\"",
                        "primeVersion: \"0.0.4\"",
                        "version: \"v0.1.0\""
                    ),
                    ".nyx-summary.txt", Set.<String>of(
                        "bump             = minor",
                        "core version     = true",
                        "new release      = true",
                        "new version      = true",
                        "scheme           = SEMVER",
                        "timestamp        = ", // we don't need to test for the timestamp value here
                        "current version  = v0.1.0",
                        "previous version = 0.0.4",
                        "prime version    = 0.0.4"
                    ),
                    "CHANGELOG.md", Set.<String>of(
                        "## v0.1.0",
                        "### Added",
                        "\\* \\[[a-z0-9]{5}\\] feat: Untagged commit \\[#2\\]\\(https:\\/\\/example\\.com\\/issues\\/2\\)",
                        "### Fixed",
                        "\\* \\[[a-z0-9]{5}\\] fix: Untagged commit \\[#1\\]\\(https:\\/\\/example\\.com\\/issues\\/1\\)"
                    ),
                    "version.txt", Set.<String>of(
                        "v0.1.0"
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
                        .concat(".nyx-summary.txt").concat(newLine)
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
                        .concat("    summaryFile = '.nyx-summary.txt'").concat(newLine)
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
                        .concat("      expression: \"(?m)^(?<type>[a-zA-Z0-9_]+)(\\\\((?<scope>[a-z ]+)\\\\))?(!)?:( (?<title>.+))$(?s).*\"").concat(newLine)
                        .concat("      bumpExpressions:").concat(newLine)
                        // note we use single quotes in the rows below to avoid escaping all of the YAML special characters
                        .concat("        major: '(?s)(?m)^[a-zA-Z0-9_]+(\\([a-z ]+\\))?(!: .*|.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
                        .concat("        minor: '(?s)(?m)^feat(!{0})(\\([a-z ]+\\))?: (?!.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
                        .concat("        patch: '(?s)(?m)^fix(!{0})(\\([a-z ]+\\))?: (?!.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
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
                        .concat("                \"publish\":\"true\",").concat(newLine)
                        .concat("                \"releaseName\":\"Stable {{version}}\"").concat(newLine)
                        .concat("            }").concat(newLine)
                        .concat("        }").concat(newLine)
                        .concat("    },").concat(newLine)
                        .concat("    \"substitutions\":{").concat(newLine)
                        .concat("        \"enabled\":[ \"node_version\", \"text_version\" ]").concat(newLine)
                        .concat("    }").concat(newLine)
                        .concat("}"),
                    // the version.txt file, used to test for substitutions, initialized with a fake version number to be replaced by Make
                    "version.txt", "91.92.93"
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
                        "coreVersion: true",
                        "newVersion: true",
                        "newRelease: true",
                        "previousVersion: \"0.0.4\"",
                        "primeVersion: \"0.0.4\"",
                        "version: \"v0.1.0\""
                    ),
                    ".nyx-summary.txt", Set.<String>of(
                        "bump             = minor",
                        "core version     = true",
                        "new release      = true",
                        "new version      = true",
                        "scheme           = SEMVER",
                        "timestamp        = ", // we don't need to test for the timestamp value here
                        "current version  = v0.1.0",
                        "previous version = 0.0.4",
                        "prime version    = 0.0.4"
                    ),
                    "CHANGELOG.md", Set.<String>of(
                        "## v0.1.0",
                        "### Added",
                        "\\* \\[[a-z0-9]{5}\\] feat: Untagged commit \\[#2\\]\\(https:\\/\\/example\\.com\\/issues\\/2\\)",
                        "### Fixed",
                        "\\* \\[[a-z0-9]{5}\\] fix: Untagged commit \\[#1\\]\\(https:\\/\\/example\\.com\\/issues\\/1\\)"
                    ),
                    "version.txt", Set.<String>of(
                        "v0.1.0"
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
                        .concat(".nyx-summary.txt").concat(newLine)
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
                        .concat("    summaryFile = '.nyx-summary.txt'").concat(newLine)
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
                        .concat("      expression: \"(?m)^(?<type>[a-zA-Z0-9_]+)(\\\\((?<scope>[a-z ]+)\\\\))?(!)?:( (?<title>.+))$(?s).*\"").concat(newLine)
                        .concat("      bumpExpressions:").concat(newLine)
                        // note we use single quotes in the rows below to avoid escaping all of the YAML special characters
                        .concat("        major: '(?s)(?m)^[a-zA-Z0-9_]+(\\([a-z ]+\\))?(!: .*|.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
                        .concat("        minor: '(?s)(?m)^feat(!{0})(\\([a-z ]+\\))?: (?!.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
                        .concat("        patch: '(?s)(?m)^fix(!{0})(\\([a-z ]+\\))?: (?!.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
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
                        .concat("                \"publish\":\"true\",").concat(newLine)
                        .concat("                \"releaseName\":\"Stable {{version}}\"").concat(newLine)
                        .concat("            }").concat(newLine)
                        .concat("        }").concat(newLine)
                        .concat("    },").concat(newLine)
                        .concat("    \"substitutions\":{").concat(newLine)
                        .concat("        \"enabled\":[ \"node_version\", \"text_version\" ]").concat(newLine)
                        .concat("    }").concat(newLine)
                        .concat("}"),
                    // the version.txt file, used to test for substitutions, initialized with a fake version number to be replaced by Make
                    "version.txt", "91.92.93"
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
                        "coreVersion: true",
                        "newVersion: true",
                        "newRelease: true",
                        "previousVersion: \"0.0.4\"",
                        "primeVersion: \"0.0.4\"",
                        "version: \"v0.1.0\""
                    ),
                    ".nyx-summary.txt", Set.<String>of(
                        "bump             = minor",
                        "core version     = true",
                        "new release      = true",
                        "new version      = true",
                        "scheme           = SEMVER",
                        "timestamp        = ", // we don't need to test for the timestamp value here
                        "current version  = v0.1.0",
                        "previous version = 0.0.4",
                        "prime version    = 0.0.4"
                    ),
                    "CHANGELOG.md", Set.<String>of(
                        "## v0.1.0",
                        "### Added",
                        "\\* \\[[a-z0-9]{5}\\] feat: Untagged commit \\[#2\\]\\(https:\\/\\/example\\.com\\/issues\\/2\\)",
                        "### Fixed",
                        "\\* \\[[a-z0-9]{5}\\] fix: Untagged commit \\[#1\\]\\(https:\\/\\/example\\.com\\/issues\\/1\\)"
                    ),
                    "version.txt", Set.<String>of(
                        "v0.1.0"
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
                        .concat(".nyx-summary.txt").concat(newLine)
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
                        .concat("    summaryFile = '.nyx-summary.txt'").concat(newLine)
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
                        .concat("      expression: \"(?m)^(?<type>[a-zA-Z0-9_]+)(\\\\((?<scope>[a-z ]+)\\\\))?(!)?:( (?<title>.+))$(?s).*\"").concat(newLine)
                        .concat("      bumpExpressions:").concat(newLine)
                        // note we use single quotes in the rows below to avoid escaping all of the YAML special characters
                        .concat("        major: '(?s)(?m)^[a-zA-Z0-9_]+(\\([a-z ]+\\))?(!: .*|.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
                        .concat("        minor: '(?s)(?m)^feat(!{0})(\\([a-z ]+\\))?: (?!.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
                        .concat("        patch: '(?s)(?m)^fix(!{0})(\\([a-z ]+\\))?: (?!.*^(BREAKING( |-)CHANGE: )).*'").concat(newLine)
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
                        .concat("                \"publish\":\"true\",").concat(newLine)
                        .concat("                \"releaseName\":\"Stable {{version}}\"").concat(newLine)
                        .concat("            }").concat(newLine)
                        .concat("        }").concat(newLine)
                        .concat("    },").concat(newLine)
                        .concat("    \"substitutions\":{").concat(newLine)
                        .concat("        \"enabled\":[ \"node_version\", \"text_version\" ]").concat(newLine)
                        .concat("    }").concat(newLine)
                        .concat("}"),
                    // the version.txt file, used to test for substitutions, initialized with a fake version number to be replaced by Make
                    "version.txt", "91.92.93"
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
                        "coreVersion: true",
                        "newVersion: true",
                        "newRelease: true",
                        "previousVersion: \"0.0.4\"",
                        "primeVersion: \"0.0.4\"",
                        "version: \"v0.1.0\""
                    ),
                    ".nyx-summary.txt", Set.<String>of(
                        "bump             = minor",
                        "core version     = true",
                        "new release      = true",
                        "new version      = true",
                        "scheme           = SEMVER",
                        "timestamp        = ", // we don't need to test for the timestamp value here
                        "current version  = v0.1.0",
                        "previous version = 0.0.4",
                        "prime version    = 0.0.4"
                    ),
                    "CHANGELOG.md", Set.<String>of(
                        "## v0.1.0",
                        "### Added",
                        "\\* \\[[a-z0-9]{5}\\] feat: Untagged commit \\[#2\\]\\(https:\\/\\/example\\.com\\/issues\\/2\\)",
                        "### Fixed",
                        "\\* \\[[a-z0-9]{5}\\] fix: Untagged commit \\[#1\\]\\(https:\\/\\/example\\.com\\/issues\\/1\\)"
                    ),
                    "version.txt", Set.<String>of(
                        "v0.1.0"
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
