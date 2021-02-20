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
import java.nio.file.Files;

import java.util.Objects;

import org.gradle.api.Project;
import org.gradle.testfixtures.ProjectBuilder;

import org.junit.jupiter.api.DisplayName;

/**
 * Abstract utility methods and tests for all tests in the plugin package
 */
@DisplayName("AbstractTests")
public class AbstractTests {
    /**
     * Instantiates a new project to be used for tests.
     * 
     * @param projectDir the project directory. If <code>null</code> a new temporary directory is created
     * @param applyNyxPlugin if <code>true</code> the Nyx plugin will be applied to the project
     * 
     * @return the test project
     * 
     * @throws Exception in case of any issue
     */
    static Project newTestProject(File projectDir, boolean applyNyxPlugin)
        throws Exception {
        if (Objects.isNull(projectDir)) {
            projectDir = Files.createTempDirectory(null).toFile();

            // let the VM delete the directories on exit
            projectDir.deleteOnExit(); 
        }

        // create the project
        Project project = ProjectBuilder.builder().withProjectDir(projectDir).build();

        if (applyNyxPlugin) {
            // apply the plugin
            project.getPluginManager().apply(NyxPlugin.ID);
        }

        return project;
    }
        
}
