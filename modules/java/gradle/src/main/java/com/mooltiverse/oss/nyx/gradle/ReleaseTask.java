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

import javax.inject.Inject;

/**
 * The Release <a href="https://docs.gradle.org/current/userguide/more_about_tasks.html#sec:lifecycle_tasks">lifecycle task</a>.
 * 
 * This task does not provide any concrete action but is used to group together all the release-related tasks so they can be
 * invoked with just this one simple task.
 */
public class ReleaseTask extends LifecycleTask {
    /**
     * The description of the task. This also appears in Gradle help.
     */
    public static final String DESCRIPTION = "Runs all the release tasks";

    /**
     * The name of the task. This is the name of the task to use inside Gradle scripts.
     */
    public static final String NAME = "release";

    /**
     * Default constructor.
     */
    @Inject
    public ReleaseTask() {
        super();
        setDescription(DESCRIPTION);
    }
}
