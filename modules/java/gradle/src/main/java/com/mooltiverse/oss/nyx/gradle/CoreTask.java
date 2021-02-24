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

import javax.annotation.Nullable;

import org.gradle.api.Project;
import org.gradle.api.plugins.ExtensionContainer;

import com.mooltiverse.oss.nyx.Nyx;
import com.mooltiverse.oss.nyx.configuration.ConfigurationException;

/**
 * The abstract superclass for all Nyx core tasks.
 * 
 * Core tasks, as opposite to {@link LifecycleTask} are those performing some concrete actions.
 */
abstract class CoreTask extends AbstractTask {
    /**
     * The name used to store and retrieve the extra property that holds the shared backing Nyx instance
     * that can be used by all tasks.
     * 
     * @see ExtensionContainer#getExtraProperties()
     */
    public static final String NYX_INSTANCE_PROPERTY = "nyxInstance";

    /**
     * Default constructor.
     */
    public CoreTask() {
        super();
    }

    /**
     * Configures the task by defining properties common to all core tasks. Also invokes the same method from superclass method.
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

    /**
     * Returns the instance of the extension for the given project.
     * 
     * @param project the project to return the extension for
     * 
     * @return the extension for the given project
     */
    protected static NyxExtension retrieveExtension(Project project) {
        return project.getExtensions().getByType(NyxExtension.class);
    }

    /**
     * Returns the instance of the extension for the task.
     * 
     * @return the extension for the task
     */
    protected NyxExtension retrieveExtension() {
        return retrieveExtension(getProject());
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
    protected void storeSharedProperty(String name, @Nullable Object value) {
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
    @Nullable
    protected Object retrieveSharedProperty(String name) {
        return getProject().getExtensions().getExtraProperties().get(name);
    }

    /**
     * Returns a shared backing Nyx instance to be used by all tasks within the project. The returned instance
     * is already configured with the values coming from the {@link NyxExtension}.
     * 
     * This method also stores the instance as a shared project property so that all subsequent calls can
     * retrieve the same instance to avoid creating multiple instances and saving configuration time.
     * 
     * On the other hand, the instance is lazily created upon the first invocation so the Nyx instance
     * is only created when needed.
     * 
     * @return a shared backing Nyx instance to be used by all tasks within the project.
     * 
     * @throws ConfigurationException in case of issues when passing the extension values to the Nyx configuration
     */
    protected synchronized Nyx nyx()
        throws ConfigurationException {
        if (hasSharedProperty(NYX_INSTANCE_PROPERTY))
            return Nyx.class.cast(retrieveSharedProperty(NYX_INSTANCE_PROPERTY));
        else {
            Nyx instance = new Nyx();
            instance.configuration().withPluginConfiguration(new ConfigurationLayer(retrieveExtension()));
            storeSharedProperty(NYX_INSTANCE_PROPERTY, instance);
            return Nyx.class.cast(retrieveSharedProperty(NYX_INSTANCE_PROPERTY));
        }
    }
}