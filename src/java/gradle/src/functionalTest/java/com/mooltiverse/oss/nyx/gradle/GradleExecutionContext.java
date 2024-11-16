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

import java.io.File;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.mooltiverse.oss.nyx.ExecutionContext;

/**
 * This execution context allows running Nyx as a Gradle plugin.
 * 
 * This object uses TestKit to run gradle.
 */
public class GradleExecutionContext implements ExecutionContext {
    /**
     * Default constructor.
     */
    public GradleExecutionContext() {
        super();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<GradleCommand> getTestCommands(File repoDir, Map<String,String> env, String[] args)
        throws Exception {
        return getTestCommands(repoDir, null, env, args);
    }

    /**
     * Returns the command objects used to run the test.
     * 
     * @param repoDir the directory containing the Git repository
     * @param gradleVersion the Gradle version to test against. If {@code null} the default version will be used
     * @param env the optional map of environment variables to pass to Gradle. It may be {@code null}
     * @param args the optional array of command line arguments to pass to Gradle. It may be {@code null}
     * 
     * @return the command instance
     * 
     * @throws any exception that may be thrown when instantiating the command
     */
    public List<GradleCommand> getTestCommands(File repoDir, String gradleVersion, Map<String,String> env, String[] args)
        throws Exception {
        GradleCommand res = new GradleCommand();
        if (!Objects.isNull(gradleVersion)) {
            res.gradleRunner.withGradleVersion(gradleVersion);
        }
        res.gradleRunner.withEnvironment(env);
        if (!Objects.isNull(args)) {
            res.gradleRunner.withArguments(args);
        }
        res.gradleRunner.withProjectDir(repoDir);
        res.gradleRunner.withPluginClasspath(); // withPluginClasspath() puts the Nyx plugin into the classpath
        res.gradleRunner.forwardOutput();
        res.gradleRunner.withDebug(false); // enable debug if needed
        
        return List.<GradleCommand>of(res);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<GradleCommand> getPreTestCommands(File repoDir, Map<String,String> env, String[] args)
        throws Exception {
        return List.<GradleCommand>of();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<GradleCommand> getPostTestCommands(File repoDir, Map<String,String> env, String[] args)
        throws Exception {
        return List.<GradleCommand>of();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<GradleCommand> getCleanUpCommands(File repoDir, Map<String,String> env, String[] args)
        throws Exception {
        return List.<GradleCommand>of();
    }
}
