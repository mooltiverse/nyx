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
 * The abstract superclass for all Nyx core tasks.
 * 
 * Core tasks, as opposite to {@link LifecycleTask} are those performing some concrete actions.
 */
abstract class CoreTask extends AbstractTask {
    /**
     * Default constructor.
     */
    public CoreTask() {
        super();
    }

    /**
     * Configures the task by defining properties common to all core tasks. Also invokes the superclass
     * method.
     * 
     * Child classes should invoke this method during the configuration phase.
     * 
     * @param task the task to configure
     * 
     * @see AbstractTask#configure(AbstractTask)
     */
    protected static void configure(AbstractTask task) {
        AbstractTask.configure(task);
    }
}