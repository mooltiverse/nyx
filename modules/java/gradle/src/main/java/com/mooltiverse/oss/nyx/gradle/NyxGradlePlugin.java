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

import org.gradle.api.Plugin;
import org.gradle.api.Project;

/**
 * The main plugin class.
 */
public class NyxGradlePlugin implements Plugin<Project> {
    /**
     * The name of the extension object for this plugin.
     */
    public static final String EXTENSION_NAME = "nyx";

    /**
     * Applies the plugin to the given project.
     * 
     * @param project the project the plugin is applied to
     * 
     * @see org.gradle.api.Plugin#apply(Object)
     */
    public void apply(Project project) {
      // Create the project extension
      project.getExtensions().create(EXTENSION_NAME, NyxGradlePluginExtension.class);

      //project.getTasks().create("helloNyx", HelloNyxTask.class);
    }
}
