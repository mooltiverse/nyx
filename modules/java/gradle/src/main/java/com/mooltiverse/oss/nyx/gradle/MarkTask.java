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

import static com.mooltiverse.oss.nyx.log.Markers.COMMAND;

import javax.inject.Inject;

import org.gradle.api.tasks.TaskAction;

import com.mooltiverse.oss.nyx.Nyx;
import com.mooltiverse.oss.nyx.NyxException;

/**
 * The task running the Mark command by invoking the {@link Nyx#mark()} method on the backing Nyx instance.
 */
public abstract class MarkTask extends CoreTask {
    /**
     * The description of the task. This also appears in Gradle help.
     */
    public static final String DESCRIPTION = "Marks the release by tagging and committing the repository";

    /**
     * The name of the task. This is the name of the task to use inside Gradle scripts.
     */
    public static final String NAME = "nyxMark";

    /**
     * Standard constructor.
     * 
     * @param extension the extension object. Cannot be {@code null}. This is injected by Gradle because it's passed
     * as an optional constructor argument by the {@link NyxPlugin} definition method.
     */
    @Inject
    public MarkTask(NyxExtension extension) {
        super(extension);
        setDescription(DESCRIPTION);
    }

    /**
     * The actual business method for this task. This method runs the {@link Nyx#mark()} method on the shared
     * singleton Nyx instance.
     * 
     * Gradle knows this is the method to run upon task execution thanks to the {@link TaskAction} annotation.
     * 
     * @throws NyxException in case of any exception when invoking the backing instance
     */
    @TaskAction
    public void mark()
        throws NyxException {
        getLogger().debug(COMMAND, "Running Nyx mark...");
        getLogger().quiet("Mark release: {}", nyx().state().getVersion());
        nyx().mark();
        getLogger().debug(COMMAND, "Nyx mark complete");
    }
}
