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

import org.gradle.api.NamedDomainObjectContainer;
import org.gradle.api.file.DirectoryProperty;
import org.gradle.api.file.ProjectLayout;
import org.gradle.api.model.ObjectFactory;
import org.gradle.api.provider.Property;
import org.gradle.api.provider.Provider;

/**
 * The plugin configuration object.
 * 
 * See <a href="https://docs.gradle.org/current/userguide/implementing_gradle_plugins.html#modeling_dsl_like_apis">Modeling DSL-like APIs</a>
 * for more o developing extension.
 * 
 * See <a href="https://docs.gradle.org/current/userguide/custom_gradle_types.html">Developing Custom Gradle Types</a>,
 * <a href="https://docs.gradle.org/current/userguide/custom_plugins.html">Developing Custom Gradle Plugins</a>,
 * <a href="https://docs.gradle.org/current/userguide/implementing_gradle_plugins.html">Implementing Gradle plugins</a> and 
 * <a href="https://docs.gradle.org/current/userguide/lazy_configuration.html">Lazy Configuration</a>
 * for an introduction on custom Gradle types development.
 * 
 * @see org.gradle.api.plugins.ExtensionAware
 * @see org.gradle.api.plugins.ExtensionContainer
 */
public abstract class NyxExtension {
    /**
     * The name of the extension object. This is the name of the configuration block inside Gradle scripts.
     */
    public static final String NAME = "nyx";

    /**
     * Returns an object factory instance.
     * 
     * The instance is injected by Gradle as soon as this getter method is invoked.
     * 
     * Using <a href="https://docs.gradle.org/current/userguide/custom_gradle_types.html#property_injection">property injection</a>
     * instead of <a href="https://docs.gradle.org/current/userguide/custom_gradle_types.html#constructor_injection">constructor injection</a>
     * has a few advantages: it allows Gradle to refer injecting the object until it's required and is safer for backward
     * compatibility (older versions can be supported).
     * 
     * @return the object factory instance
     */
    @Inject
    protected abstract ObjectFactory getObjectfactory();

    /**
     * Returns a project layout instance.
     * 
     * The instance is injected by Gradle as soon as this getter method is invoked.
     * 
     * Using <a href="https://docs.gradle.org/current/userguide/custom_gradle_types.html#property_injection">property injection</a>
     * instead of <a href="https://docs.gradle.org/current/userguide/custom_gradle_types.html#constructor_injection">constructor injection</a>
     * has a few advantages: it allows Gradle to refer injecting the object until it's required and is safer for backward
     * compatibility (older versions can be supported).
     * 
     * @return the project layout instance
     */
    @Inject
    protected abstract ProjectLayout getProjectLayout();

    /**
     * The 'bump' property.
     * Default is "".
     * 
     * This property uses the {@link Property#convention(Object)} to define the default value so
     * when users are good with the default value they don't need to define it in the build script.
     */
    private Property<String> bump = getObjectfactory().property(String.class).convention(""); // TODO: read the default value from Nyx configuration classes

    /**
     * The 'directory' property.
     * Default is taken from the Gradle project directory.
     * 
     * This property uses the {@link Property#convention(Provider)} to define the default value so
     * when users are good with the default value they don't need to define it in the build script.
     */
    private DirectoryProperty directory = getObjectfactory().directoryProperty().value(getProjectLayout().getProjectDirectory()); // TODO: read the default value from Nyx configuration classes

    /**
     * The 'dryRun' property.
     * Default is <code>false</code>.
     * 
     * This property uses the {@link Property#convention(Object)} to define the default value so
     * when users are good with the default value they don't need to define it in the build script.
     */
    private Property<Boolean> dryRun = getObjectfactory().property(Boolean.class).convention(Boolean.FALSE); // TODO: read the default value from Nyx configuration classes

    /**
     * The 'releasePrefix' property.
     * Default is "v".
     * 
     * This property uses the {@link Property#convention(Object)} to define the default value so
     * when users are good with the default value they don't need to define it in the build script.
     */
    private Property<String> releasePrefix = getObjectfactory().property(String.class).convention("v"); // TODO: read the default value from Nyx configuration classes

    /**
     * The 'releasePrefixLenient' property.
     * Default is <code>true</code>.
     * 
     * This property uses the {@link Property#convention(Object)} to define the default value so
     * when users are good with the default value they don't need to define it in the build script.
     */
    private Property<Boolean> releasePrefixLenient = getObjectfactory().property(Boolean.class).convention(Boolean.TRUE); // TODO: read the default value from Nyx configuration classes

    /**
     * The 'scheme' property.
     * Default is "semver".
     * 
     * This property uses the {@link Property#convention(Object)} to define the default value so
     * when users are good with the default value they don't need to define it in the build script.
     */
    private Property<String> scheme = getObjectfactory().property(String.class).convention("semver"); // TODO: read the default value from Nyx configuration classes

    /**
     * The 'verbosity' property.
     * Default is "quiet".
     * 
     * This property uses the {@link Property#convention(Object)} to define the default value so
     * when users are good with the default value they don't need to define it in the build script.
     */
    private Property<String> verbosity = getObjectfactory().property(String.class).convention("quiet"); // TODO: read the default value Gradle logger and map it to Nyx supported levels

    /**
     * The nested 'services' block.
     * Default is an empty list of Service objects.
     * 
     * @see Service
     */
    private NamedDomainObjectContainer<Service> services = getObjectfactory().domainObjectContainer(Service.class); // TODO: read the default value from Nyx configuration classes

    /**
     * Returns the name of the version identifier to bump. When this is set by the user it overrides
     * the inference performed by Nyx.
     * 
     * We provide an implementation of this method instead of using the abstract definition as it's
     * safer for old Gradle versions we support.
     * 
     * @return the name of the version identifier to bump
     * 
     * TODO: add a link to constants from Nyx configuration classes
     */
    public Property<String> getBump() {
        return bump;
    }

    /**
     * Returns the directory to use as the base repository location.
     * 
     * We provide an implementation of this method instead of using the abstract definition as it's
     * safer for old Gradle versions we support.
     * 
     * @return the directory to use as the base repository location
     * 
     * TODO: add a link to constants from Nyx configuration classes
     */
    public DirectoryProperty getDirectory() {
        return directory;
    }

    /**
     * Returns the flag that, when <code>true</code>, prevents Nyx from applying any change to the repository or any
     * other resource.
     * 
     * We provide an implementation of this method instead of using the abstract definition as it's
     * safer for old Gradle versions we support.
     * 
     * @return the flag that, when <code>true</code>, prevents Nyx from applying any change to the repository or any
     * other resource
     * 
     * TODO: add a link to constants from Nyx configuration classes
     */
    public Property<Boolean> getDryRun() {
        return dryRun;
    }

    /**
     * Returns the prefix used to generate release names.
     * 
     * We provide an implementation of this method instead of using the abstract definition as it's
     * safer for old Gradle versions we support.
     * 
     * @return the prefix used to generate release names
     * 
     * TODO: add a link to constants from Nyx configuration classes
     */
    public Property<String> getReleasePrefix() {
        return releasePrefix;
    }

    /**
     * Returns the flag that, when <code>true</code>, lets Nyx interpret release names with whatever prefix.
     * 
     * We provide an implementation of this method instead of using the abstract definition as it's
     * safer for old Gradle versions we support.
     * 
     * @return the flag that, when <code>true</code>, lets Nyx interpret release names with whatever prefix
     * 
     * TODO: add a link to constants from Nyx configuration classes
     */
    public Property<Boolean> getReleasePrefixLenient() {
        return releasePrefixLenient;
    }

    /**
     * Returns the versioning scheme to use.
     * 
     * We provide an implementation of this method instead of using the abstract definition as it's
     * safer for old Gradle versions we support.
     * 
     * @return the versioning scheme to use
     * 
     * TODO: add a link to constants from Nyx configuration classes
     */
    public Property<String> getScheme() {
        return scheme;
    }

    /**
     * Returns the logging verbosity.
     * 
     * We provide an implementation of this method instead of using the abstract definition as it's
     * safer for old Gradle versions we support.
     * 
     * @return the logging verbosity
     * 
     * TODO: add a link to constants from Nyx configuration classes
     */
    public Property<String> getVerbosity() {
        return verbosity;
    }

    /**
     * Returns the nested 'service' blocks.
     * 
     * We provide an implementation of this method instead of using the abstract definition as it's
     * safer for old Gradle versions we support.
     * 
     * @return the nested 'service' blocks
     * 
     * TODO: add a link to constants from Nyx configuration classes
     */
    public NamedDomainObjectContainer<Service> getServices() {
        return services;
    }

    /**
     * The class to model a single service item within the 'services' block within the extension.
     */
    public static class Service {
        /**
         * The service name.
         */
        private final String name;

        /**
         * The service provider.
         */
        private String provider;

        /**
         * Constructor.
         * 
         * This constructor is required as per the {@link NamedDomainObjectContainer} specification.
         * 
         * @param name the service name
         */
        public Service(String name) {
            super();
            this.name = name;
        }

        /**
         * Returns the name read-only mandatory property.
         * 
         * @return the name read-only mandatory property.
         */
        public String getName() {
            return name;
        }

        /**
         * Returns the provider for the service.
         * 
         * @return the provider for the service.
         */
        public String getProvider() {
            return provider;
        }

        /**
         * Sets the provider for the service.
         * 
         * @param provider the provider for the service.
         */
        public void setProvider(String provider) {
            this.provider = provider;
        }

        // TODO: add the Services properties to this class
    }
}
