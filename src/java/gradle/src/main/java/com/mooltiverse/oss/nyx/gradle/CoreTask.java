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

import static com.mooltiverse.oss.nyx.gradle.Constants.GRADLE_VERSION_PROPERTY_NAME;
import static com.mooltiverse.oss.nyx.log.Markers.COMMAND;

import java.util.Objects;

import org.gradle.api.plugins.ExtensionContainer;
import org.gradle.api.tasks.Internal;

import com.mooltiverse.oss.nyx.Nyx;
import com.mooltiverse.oss.nyx.entities.IllegalPropertyException;
import com.mooltiverse.oss.nyx.io.DataAccessException;
import com.mooltiverse.oss.nyx.state.State;

/**
 * The abstract superclass for all Nyx core tasks.
 * 
 * Core tasks, as opposite to {@link LifecycleTask} are those performing some concrete actions.
 */
public abstract class CoreTask extends AbstractTask {
    /**
     * The name used to store and retrieve the extra property that holds the shared backing Nyx instance
     * that can be used by all tasks.
     * 
     * @see ExtensionContainer#getExtraProperties()
     */
    public static final String NYX_INSTANCE_PROPERTY = "nyxInstance";

    /**
     * The name used to store and retrieve the extra property that holds the state of the backing Nyx
     * instance that can be used by all tasks and also user scripts.
     * 
     * @see ExtensionContainer#getExtraProperties()
     */
    public static final String NYX_STATE_PROPERTY = "nyxState";

    /**
     * The private instance of the extension object.
     */
    private final NyxExtension extension;

    /**
     * Standard constructor.
     * 
     * @param extension the extension object. Cannot be {@code null}.
     */
    public CoreTask(NyxExtension extension) {
        super();
        Objects.requireNonNull(extension, "The task constructor requires a non null extension");
        this.extension = extension;
    }

    /**
     * Returns the instance of the extension for the task.
     * 
     * @return the extension for the task
     */
    @Internal
    protected NyxExtension getExtension() {
        return extension;
    }

    /**
     * Returns {@code true} if there is a project with the given name stored for the project.
     * 
     * @param name the name of the property to look up
     * 
     * @return {@code true} if there is a project with the given name stored for the project
     */
    protected boolean hasSharedProperty(String name) {
        return getProject().getExtensions().getExtraProperties().has(name);
    }

    /**
     * Stores the given value as a shared project property with the given name.
     * The name makes the property unique within the entire project.
     * 
     * @param name the name of the property to store
     * @param value the value of the property to store
     */
    protected void storeSharedProperty(String name, Object value) {
        getLogger().debug(COMMAND, "Storing shared extra property {}", name);
        getProject().getExtensions().getExtraProperties().set(name, value);
    }

    /**
     * Retrieves the shared project property with the given name.
     * The name makes the property unique within the entire project.
     * 
     * @param name the name of the property to retrieve
     * 
     * @return the value of the shared property with the given name, if any, or {@code null} if no property
     * with such name is available
     */
    protected Object retrieveSharedProperty(String name) {
        getLogger().trace(COMMAND, "Retrieving shared extra property {}", name);
        return getProject().getExtensions().getExtraProperties().get(name);
    }

    /**
     * Returns a shared backing Nyx instance to be used by all tasks within the project. The returned instance
     * is already configured with the values coming from the {@link NyxExtension}.
     * 
     * This method also stores the instance as a shared project property so that all subsequent calls can
     * retrieve the same instance to avoid creating multiple instances and saving configuration time.
     * Moreover, the backing instance state object reference is stores as an extra property for use by
     * user scripts.
     * 
     * The instance is lazily created upon the first invocation so the Nyx instance is only created when needed.
     * 
     * @return a shared backing Nyx instance to be used by all tasks within the project.
     * 
     * @throws DataAccessException in case the configuration can't be loaded for some reason.
     * @throws IllegalPropertyException in case the configuration has some illegal options.
     */
    protected synchronized Nyx nyx()
        throws DataAccessException, IllegalPropertyException {
        if (hasSharedProperty(NYX_INSTANCE_PROPERTY))
            return Nyx.class.cast(retrieveSharedProperty(NYX_INSTANCE_PROPERTY));
        else {
            getLogger().debug(COMMAND, "Creating a new Nyx backing instance...");
            Nyx instance = new Nyx(getProject().getProjectDir());
            getLogger().debug(COMMAND, "Injecting the Nyx plugin configuration layer into the Nyx backing instance configuration...");
            // The 'version' is a standard Gradle project property (https://docs.gradle.org/current/userguide/writing_build_scripts.html#sec:standard_project_properties)
            instance.configuration().withPluginConfiguration(new ConfigurationLayer(getExtension(), getProject().findProperty(GRADLE_VERSION_PROPERTY_NAME)));
            storeSharedProperty(NYX_INSTANCE_PROPERTY, instance);
            storeSharedProperty(NYX_STATE_PROPERTY, instance.state());
            return Nyx.class.cast(retrieveSharedProperty(NYX_INSTANCE_PROPERTY));
        }
    }

    /**
     * Returns the state of the shared backing Nyx instance.
     * 
     * @return the state of the shared backing Nyx instance.
     * 
     * @throws DataAccessException in case the configuration can't be loaded for some reason.
     * @throws IllegalPropertyException in case the configuration has some illegal options.
     * 
     * @see #nyx()
     */
    public State state()
        throws DataAccessException, IllegalPropertyException {
        getLogger().trace(COMMAND, "Retrieving the Nyx state reference");
        return nyx().state();
    }
}