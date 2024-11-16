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

import org.gradle.testkit.runner.BuildResult;
import org.gradle.testkit.runner.GradleRunner;

import com.mooltiverse.oss.nyx.Command;

/**
 * This command allows running Nyx as a Gradle plugin.
 * 
 * This object uses TestKit to run gradle.
 */
public class GradleCommand implements Command {
    /**
     * The Gradle runner instance.
     */
    public GradleRunner gradleRunner = null;

    /**
     * Default constructor
     */
    public GradleCommand() {
        gradleRunner = GradleRunner.create();
    }

    /**
     * Runs the command and returns its output
     * 
     * @return the outcome of the execution
     */
    @Override
    public BuildResult run()
        throws Exception {
        return gradleRunner.build();
    }
}
