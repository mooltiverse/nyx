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

/**
 * The abstract superclass for all Nyx lifecycle tasks.
 * 
 * <a href="https://docs.gradle.org/current/userguide/more_about_tasks.html#sec:lifecycle_tasks">Lifecycle tasks</a>,
 * as opposite to {@link CoreTask} are convenience tasks that do not perform any actions themselves but are
 * symbolic names used to group other tasks by dependencies.
 */
abstract class LifecycleTask extends AbstractTask {
    /**
     * Default constructor.
     */
    public LifecycleTask() {
        super();
    }
}