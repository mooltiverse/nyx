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
import javax.inject.Inject;

import org.gradle.api.Action;
import org.gradle.api.NamedDomainObjectContainer;
import org.gradle.api.Project;
import org.gradle.api.initialization.Settings;
import org.gradle.api.model.ObjectFactory;
import org.gradle.api.provider.ListProperty;
import org.gradle.api.provider.MapProperty;
import org.gradle.api.provider.Property;
import org.gradle.api.provider.Provider;

import com.mooltiverse.oss.nyx.entities.Defaults;

/**
 * The plugin configuration object. This object is responsible for reading the {@code nyx {...} } configuration block that users
 * define within the {@code build.gradle} script.
 * <br>
 * See <a href="https://docs.gradle.org/current/userguide/implementing_gradle_plugins.html#modeling_dsl_like_apis">Modeling DSL-like APIs</a>
 * for more o developing extension.
 * <br>
 * See <a href="https://docs.gradle.org/current/userguide/custom_gradle_types.html">Developing Custom Gradle Types</a>,
 * <a href="https://docs.gradle.org/current/userguide/custom_plugins.html">Developing Custom Gradle Plugins</a>,
 * <a href="https://docs.gradle.org/current/userguide/implementing_gradle_plugins.html">Implementing Gradle plugins</a> and 
 * <a href="https://docs.gradle.org/current/userguide/lazy_configuration.html">Lazy Configuration</a>
 * for an introduction on custom Gradle types development.
 * <br><br>
 * <b>NOTES ON NESTED OBJECTS</b>:<br>
 * In order to support DSL (the curly braces syntax) for nested objects we also need to support one additional method for each nested object
 * to accept and configure the closure. Take the {@code commitMessageConventions} for example. According to the API docs, defining the
 * field and the getter method as as:<br><br>
 * <pre>
 *      private CommitMessageConventions commitMessageConventions = getObjectfactory().newInstance(CommitMessageConventions.class);
 * 
 *      public CommitMessageConventions getCommitMessageConventions() {
 *          return commitMessageConventions;
 *          }
 * </pre>
 * should be enough but it's not, in fact at runtime, when using the curly braces syntax to define that property like:<br><br>
 * <pre>
 * nyx {
 *   commitMessageConventions {
 *      enabled = ['conventionalCommits']
 *   }
 * }
 * </pre>
 * throws an exception like:<br><br>
 * <pre>
 * org.gradle.api.GradleScriptException: A problem occurred evaluating root project...
 *      ...
 *      Caused by: groovy.lang.MissingMethodException: No signature of method: build_araz4us455w31lfualzy83h1f.nyx() is applicable for argument types: (build_araz4us455w31lfualzy83h1f$_run_closure1) values: [build_araz4us455w31lfualzy83h1f$_run_closure1@22b6a7c8]
 *      Possible solutions: any(), any(groovy.lang.Closure), run(), run(), sync(org.gradle.api.Action), uri(java.lang.Object)
 *      ...
 * </pre>
 * So thanks to posts like <a href="https://stackoverflow.com/questions/33606861/nested-object-inside-custom-plugin-extension">this</a>
 * (which points to this example <a href="https://github.com/Opalo/stackoverflow/blob/master/33606861/buildSrc/src/main/java/org/opal/LolExtension.java">here</a>),
 * <a href="https://discuss.gradle.org/t/nested-extension-plugin-written-in-java/22264/10">this</a> and
 * <a href="https://discuss.gradle.org/t/multi-level-dsl-for-plugin-extension/19029/13">this</a>
 * it turns out there must be another method to accept the closure. I don't know Groovy so I'm not sure what it's needed for but the
 * additional method can be in one of the two following variants (still using the {@code commitMessageConventions} example):<br><br>
 * <pre>
 *      public void commitMessageConventions(groovy.lang.Closure closure) {
 *          closure.setResolveStrategy(groovy.lang.Closure.DELEGATE_FIRST);
 *          closure.setDelegate(commitMessageConventions);
 *          closure.call();
 *      }
 * </pre>
 * or:<br>
 * <pre>
 *      public void commitMessageConventions(org.gradle.api.Action&lt;? super CommitMessageConventions&gt; configuration) {
 *          configuration.execute(commitMessageConventions);
 *      }
 * </pre>
 * I opted for the second one, as it's a little more comprehensible, although I don't know exactly what happens under the hood.
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
     * The 'bump' property.
     */
    private final Property<String> bump = getObjectfactory().property(String.class);

    /**
     * The nested 'changelog' block.
     */
    private final ChangelogConfiguration changelog = getObjectfactory().newInstance(ChangelogConfiguration.class);

    /**
     * The nested 'commitMessageConventions' block.
     */
    private final CommitMessageConventions commitMessageConventions = getObjectfactory().newInstance(CommitMessageConventions.class);

    /**
     * The 'configurationFile' property.
     */
    private final Property<String> configurationFile = getObjectfactory().property(String.class);

    /**
     * The 'directory' property.
     * Default is taken from the Gradle project directory but the method to retrieve that is
     * based on where this object is applied (see the {@link #create(Project)} and
     * {@link #create(Settings)} methods).
     * 
     * This property uses the {@link Property#convention(Provider)} to define the default value so
     * when users are good with the default value they don't need to define it in the build script.
     * 
     * @see #create(Project)
     * @see #create(Settings)
     */
    private final Property<File> directory = getObjectfactory().property(File.class);

    /**
     * The 'dryRun' property.
     */
    private final Property<Boolean> dryRun = getObjectfactory().property(Boolean.class);

    /**
     * The nested 'git' block.
     */
    private final GitConfiguration git = getObjectfactory().newInstance(GitConfiguration.class);

    /**
     * The 'initialVersion' property.
     */
    private final Property<String> initialVersion = getObjectfactory().property(String.class);

    /**
     * The 'preset' property.
     */
    private final Property<String> preset = getObjectfactory().property(String.class);

    /**
     * The nested 'releaseAssets' block.
     * 
     * @see AssetConfiguration
     */
    private NamedDomainObjectContainer<AssetConfiguration> releaseAssets = getObjectfactory().domainObjectContainer(AssetConfiguration.class);

    /**
     * The 'releaseLenient' property.
     */
    private final Property<Boolean> releaseLenient = getObjectfactory().property(Boolean.class);

    /**
     * The 'releasePrefix' property.
     */
    private final Property<String> releasePrefix = getObjectfactory().property(String.class);

    /**
     * The nested 'releaseTypes' block.
     */
    private final ReleaseTypes releaseTypes = getObjectfactory().newInstance(ReleaseTypes.class);

    /**
     * The 'resume' property.
     */
    private final Property<Boolean> resume = getObjectfactory().property(Boolean.class);

    /**
     * The 'scheme' property.
     */
    private final Property<String> scheme = getObjectfactory().property(String.class);

    /**
     * The nested 'services' block.
     * 
     * @see ServiceConfiguration
     */
    private NamedDomainObjectContainer<ServiceConfiguration> services = getObjectfactory().domainObjectContainer(ServiceConfiguration.class);

    /**
     * The 'sharedConfigurationFile' property.
     */
    private final Property<String> sharedConfigurationFile = getObjectfactory().property(String.class);

    /**
     * The 'stateFile' property.
     */
    private final Property<String> stateFile = getObjectfactory().property(String.class);

    /**
     * The 'verbosity' property.
     * 
     * Please note that the verbosity option is actually ignored in this plugin implementation and the backing Nyx implementation
     * as it's controlled by Gradle.
     */
    private final Property<String> verbosity = getObjectfactory().property(String.class);

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
     * Creates the extension into the given project.
     * 
     * @param project the project to create the extension into
     * 
     * @return the extension instance, within the given project
     */
    public static NyxExtension create(Project project) {
        NyxExtension extension = project.getExtensions().create(NyxExtension.NAME, NyxExtension.class);
        extension.directory.convention(project.getLayout().getProjectDirectory().getAsFile());
        return extension;
    }

    /**
     * Creates the extension into the given settings.
     * 
     * @param settings the settings to create the extension into
     * 
     * @return the extension instance, within the given settings
     */
    public static NyxExtension create(Settings settings) {
        NyxExtension extension = settings.getExtensions().create(NyxExtension.NAME, NyxExtension.class);
        extension.directory.convention(settings.getRootDir());
        return extension;
    }

    /**
     * Returns the name of the version identifier to bump. When this is set by the user it overrides
     * the inference performed by Nyx.
     * 
     * We provide an implementation of this method instead of using the abstract definition as it's
     * safer for old Gradle versions we support.
     * 
     * @return the name of the version identifier to bump
     */
    public Property<String> getBump() {
        return bump;
    }

    /**
     * Returns the object mapping the {@code changelog} block.
     * 
     * We provide an implementation of this method instead of using the abstract definition as it's
     * safer for old Gradle versions we support.
     * 
     * @return the object mapping the {@code changelog} block
     */
    public ChangelogConfiguration getChangelog() {
        return changelog;
    }

    /**
     * Accepts the DSL configuration for the {@code changelog} block, needed for defining
     * the block using the curly braces syntax in Gradle build scripts.
     * See the documentation on top of this class for more.
     * 
     * @param configurationAction the configuration action for the {@code changelog} block
     */
    public void changelog(Action<? super ChangelogConfiguration> configurationAction) {
        configurationAction.execute(changelog);
    }

    /**
     * Returns the object mapping the {@code commitMessageConventions} block.
     * 
     * We provide an implementation of this method instead of using the abstract definition as it's
     * safer for old Gradle versions we support.
     * 
     * @return the object mapping the {@code commitMessageConventions} block
     */
    public CommitMessageConventions getCommitMessageConventions() {
        return commitMessageConventions;
    }

    /**
     * Accepts the DSL configuration for the {@code commitMessageConventions} block, needed for defining
     * the block using the curly braces syntax in Gradle build scripts.
     * See the documentation on top of this class for more.
     * 
     * @param configurationAction the configuration action for the {@code commitMessageConventions} block
     */
    public void commitMessageConventions(Action<? super CommitMessageConventions> configurationAction) {
        configurationAction.execute(commitMessageConventions);
    }

    /**
     * Returns the custom configuration file to use.
     * 
     * We provide an implementation of this method instead of using the abstract definition as it's
     * safer for old Gradle versions we support.
     * 
     * @return the custom configuration file to use
     * 
     * @see Defaults#SHARED_CONFIGURATION_FILE
     */
    public Property<String> getConfigurationFile() {
        return configurationFile;
    }

    /**
     * Returns the directory to use as the base repository location.
     * 
     * We provide an implementation of this method instead of using the abstract definition as it's
     * safer for old Gradle versions we support.
     * 
     * @return the directory to use as the base repository location
     */
    public Property<File> getDirectory() {
        return directory;
    }

    /**
     * Returns the flag that, when {@code true}, prevents Nyx from applying any change to the repository or any
     * other resource.
     * 
     * We provide an implementation of this method instead of using the abstract definition as it's
     * safer for old Gradle versions we support.
     * 
     * @return the flag that, when {@code true}, prevents Nyx from applying any change to the repository or any
     * other resource
     * 
     * @see Defaults#DRY_RUN
     */
    public Property<Boolean> getDryRun() {
        return dryRun;
    }

    /**
     * Returns the object mapping the {@code git} block.
     * 
     * We provide an implementation of this method instead of using the abstract definition as it's
     * safer for old Gradle versions we support.
     * 
     * @return the object mapping the {@code git} block
     */
    public GitConfiguration getGit() {
        return git;
    }

    /**
     * Accepts the DSL configuration for the {@code git} block, needed for defining
     * the block using the curly braces syntax in Gradle build scripts.
     * See the documentation on top of this class for more.
     * 
     * @param configurationAction the configuration action for the {@code git} block
     */
    public void git(Action<? super GitConfiguration> configurationAction) {
        configurationAction.execute(git);
    }

    /**
     * Returns the initial version to use when no past version can be inferred from the commit history.
     * 
     * We provide an implementation of this method instead of using the abstract definition as it's
     * safer for old Gradle versions we support.
     * 
     * @return the flag that, when {@code true}, prevents Nyx from applying any change to the repository or any
     * other resource
     * 
     * @see Defaults#DRY_RUN
     */
    public Property<String> getInitialVersion() {
        return initialVersion;
    }

    /**
     * Returns the selected preset configuration name.
     * 
     * We provide an implementation of this method instead of using the abstract definition as it's
     * safer for old Gradle versions we support.
     * 
     * @return the selected preset configuration name.
     * 
     * @see Defaults#PRESET
     */
    public Property<String> getPreset() {
        return preset;
    }

    /**
     * Returns the object mapping the {@code releaseAssets} block.
     * 
     * We provide an implementation of this method instead of using the abstract definition as it's
     * safer for old Gradle versions we support.
     * 
     * @return the object mapping the {@code releaseAssets} block
     */
    public NamedDomainObjectContainer<AssetConfiguration> getReleaseAssets() {
        return releaseAssets;
    }

    /**
     * Accepts the DSL configuration for the {@code releaseAssets} block, needed for defining
     * the block using the curly braces syntax in Gradle build scripts.
     * See the documentation on top of this class for more.
     * 
     * @param configurationAction the configuration action for the {@code releaseAssets} block
     */
    public void releaseAssets(Action<? super NamedDomainObjectContainer<AssetConfiguration>> configurationAction) {
        configurationAction.execute(releaseAssets);
    }

    /**
     * Returns the flag that, when {@code true}, lets Nyx interpret release names with whatever prefix.
     * 
     * We provide an implementation of this method instead of using the abstract definition as it's
     * safer for old Gradle versions we support.
     * 
     * @return the flag that, when {@code true}, lets Nyx interpret release names with whatever prefix
     * 
     * @see Defaults#RELEASE_LENIENT
     */
    public Property<Boolean> getReleaseLenient() {
        return releaseLenient;
    }

    /**
     * Returns the prefix used to generate release names.
     * 
     * We provide an implementation of this method instead of using the abstract definition as it's
     * safer for old Gradle versions we support.
     * 
     * @return the prefix used to generate release names
     * 
     * @see Defaults#RELEASE_PREFIX
     */
    public Property<String> getReleasePrefix() {
        return releasePrefix;
    }

    /**
     * Returns the object mapping the {@code releaseTypes} block.
     * 
     * We provide an implementation of this method instead of using the abstract definition as it's
     * safer for old Gradle versions we support.
     * 
     * @return the object mapping the {@code releaseTypes} block
     */
    public ReleaseTypes getReleaseTypes() {
        return releaseTypes;
    }

    /**
     * Accepts the DSL configuration for the {@code releaseTypes} block, needed for defining
     * the block using the curly braces syntax in Gradle build scripts.
     * See the documentation on top of this class for more.
     * 
     * @param configurationAction the configuration action for the {@code releaseTypes} block
     */
    public void releaseTypes(Action<? super ReleaseTypes> configurationAction) {
        configurationAction.execute(releaseTypes);
    }

    /**
     * Returns the flag that, when {@code true}, loads a previously saved state file (if any) to resume execution
     * from there.
     * 
     * We provide an implementation of this method instead of using the abstract definition as it's
     * safer for old Gradle versions we support.
     * 
     * @return the flag that, when {@code true}, prevents Nyx from applying any change to the repository or any
     * other resource
     * 
     * @see Defaults#DRY_RUN
     */
    public Property<Boolean> getResume() {
        return resume;
    }

    /**
     * Returns the versioning scheme to use.
     * 
     * We provide an implementation of this method instead of using the abstract definition as it's
     * safer for old Gradle versions we support.
     * 
     * @return the versioning scheme to use
     * 
     * @see Defaults#SCHEME
     */
    public Property<String> getScheme() {
        return scheme;
    }

    /**
     * Returns the object mapping the {@code services} block.
     * 
     * We provide an implementation of this method instead of using the abstract definition as it's
     * safer for old Gradle versions we support.
     * 
     * @return the object mapping the {@code services} block
     */
    public NamedDomainObjectContainer<ServiceConfiguration> getServices() {
        return services;
    }

    /**
     * Accepts the DSL configuration for the {@code services} block, needed for defining
     * the block using the curly braces syntax in Gradle build scripts.
     * See the documentation on top of this class for more.
     * 
     * @param configurationAction the configuration action for the {@code services} block
     */
    public void services(Action<? super NamedDomainObjectContainer<ServiceConfiguration>> configurationAction) {
        configurationAction.execute(services);
    }

    /**
     * Returns the custom shared configuration file to use.
     * 
     * We provide an implementation of this method instead of using the abstract definition as it's
     * safer for old Gradle versions we support.
     * 
     * @return the custom shared configuration file to use
     * 
     * @see Defaults#SHARED_CONFIGURATION_FILE
     */
    public Property<String> getSharedConfigurationFile() {
        return sharedConfigurationFile;
    }

    /**
     * Returns the optional path where to save the state file.
     * 
     * We provide an implementation of this method instead of using the abstract definition as it's
     * safer for old Gradle versions we support.
     * 
     * @return the directory to use as the base repository location
     */
    public Property<String> getStateFile() {
        return stateFile;
    }

    /**
     * Returns the logging verbosity.
     * 
     * Please note that the verbosity option is actually ignored in this plugin implementation and the backing Nyx implementation
     * as it's controlled by Gradle.
     * 
     * We provide an implementation of this method instead of using the abstract definition as it's
     * safer for old Gradle versions we support.
     * 
     * @return the logging verbosity
     */
    public Property<String> getVerbosity() {
        return verbosity;
    }

    /**
     * The class to model the 'changelog' block within the extension.
     */
    public abstract static class ChangelogConfiguration {
        /**
         * The path to the destination file.
         */
        private final Property<String> path = getObjectfactory().property(String.class);

        /**
         * The path to the optional template file.
         */
        private final Property<String> template = getObjectfactory().property(String.class);

        /**
         * The nested 'sections' block.
         */
        private final MapProperty<String,String> sections = getObjectfactory().mapProperty(String.class, String.class);

        /**
         * The nested 'substitutions' block.
         */
        private final MapProperty<String,String> substitutions = getObjectfactory().mapProperty(String.class, String.class);

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
         * Returns the path to the destination file. When this is set by the user it overrides
         * the inference performed by Nyx.
         * 
         * We provide an implementation of this method instead of using the abstract definition as it's
         * safer for old Gradle versions we support.
         * 
         * @return the path to the destination file.
         */
        public Property<String> getPath() {
            return path;
        }

        /**
         * Returns the path to the optional template file. When this is set by the user it overrides
         * the inference performed by Nyx.
         * 
         * We provide an implementation of this method instead of using the abstract definition as it's
         * safer for old Gradle versions we support.
         * 
         * @return the path to the optional template file.
         */
        public Property<String> getTemplate() {
            return template;
        }

        /**
         * Returns the map of changelog sections.
         * 
         * @return the map of changelog sections.
         */
        public MapProperty<String,String> getSections() {
            return sections;
        }
        
        /**
         * Accepts the DSL configuration for the {@code sections} block, needed for defining
         * the block using the curly braces syntax in Gradle build scripts.
         * See the documentation on top of this class for more.
         * 
         * @param configurationAction the configuration action for the {@code sections} block
         */
        public void sections(Action<? super MapProperty<String,String>> configurationAction) {
            configurationAction.execute(sections);
        }

        /**
         * Returns the map of changelog substitutions.
         * 
         * @return the map of changelog substitutions.
         */
        public MapProperty<String,String> getSubstitutions() {
            return substitutions;
        }
        
        /**
         * Accepts the DSL configuration for the {@code substitutions} block, needed for defining
         * the block using the curly braces syntax in Gradle build scripts.
         * See the documentation on top of this class for more.
         * 
         * @param configurationAction the configuration action for the {@code substitutions} block
         */
        public void substitutions(Action<? super MapProperty<String,String>> configurationAction) {
            configurationAction.execute(substitutions);
        }
    }

    /**
     * The class to model the 'commitMessageConventions' block within the extension.
     */
    public abstract static class CommitMessageConventions {
        /**
         * The list of enabled convention names.
         */
        private final ListProperty<String> enabled = getObjectfactory().listProperty(String.class);

        /**
         * The nested 'items' block.
         * 
         * @see CommitMessageConvention
         */
        private NamedDomainObjectContainer<CommitMessageConvention> items = getObjectfactory().domainObjectContainer(CommitMessageConvention.class);

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
         * Returns list of enabled convention names.
         * 
         * @return list of enabled convention names.
         */
        public ListProperty<String> getEnabled() {
            return enabled;
        }

        /**
         * Returns the map of commit message convention items.
         * 
         * @return the map of commit message convention items.
         */
        public NamedDomainObjectContainer<CommitMessageConvention> getItems() {
            return items;
        }

        /**
         * Accepts the DSL configuration for the {@code items} block, needed for defining
         * the block using the curly braces syntax in Gradle build scripts.
         * See the documentation on top of this class for more.
         * 
         * @param configurationAction the configuration action for the {@code items} block
         */
        public void items(Action<? super NamedDomainObjectContainer<CommitMessageConvention>> configurationAction) {
            configurationAction.execute(items);
        }

        /**
         * The class to model a single 'commitMessageConventions' item within the extension.
         */
        public abstract static class CommitMessageConvention {
            /**
             * The convention name.
             */
            private final String name;

            /**
             * The convention regular expression property.
             */
            private final Property<String> expression = getObjectfactory().property(String.class);

            /**
             * The nested 'bumpExpressions' block.
             * 
             * @see CommitMessageConvention
             */
            private final MapProperty<String,String> bumpExpressions = getObjectfactory().mapProperty(String.class, String.class);

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
             * Constructor.
             * 
             * This constructor is required as per the {@link NamedDomainObjectContainer} specification.
             * 
             * @param name the convention name
             */
            public CommitMessageConvention(String name) {
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
             * Returns the regular expression to infer fields from a commit message. When this is set by the user it overrides
             * the inference performed by Nyx.
             * 
             * We provide an implementation of this method instead of using the abstract definition as it's
             * safer for old Gradle versions we support.
             * 
             * @return the regular expression to infer fields from a commit message
             */
            public Property<String> getExpression() {
                return expression;
            }

            /**
             * Returns the map of bump expression items.
             * 
             * @return the map of bump expression items.
             */
            public MapProperty<String,String> getBumpExpressions() {
                return bumpExpressions;
            }
            
            /**
             * Accepts the DSL configuration for the {@code bumpExpressions} block, needed for defining
             * the block using the curly braces syntax in Gradle build scripts.
             * See the documentation on top of this class for more.
             * 
             * @param configurationAction the configuration action for the {@code bumpExpressions} block
             */
            public void bumpExpressions(Action<? super MapProperty<String,String>> configurationAction) {
                configurationAction.execute(bumpExpressions);
            }
        }
    }

    /**
     * The class to model the 'git' block within the extension.
     */
    public abstract static class GitConfiguration {
        /**
         * The nested 'remotes' block.
         * 
         * @see GitRemoteConfiguration
         */
        private NamedDomainObjectContainer<GitRemoteConfiguration> remotes = getObjectfactory().domainObjectContainer(GitRemoteConfiguration.class);

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
         * Returns the map of remotes.
         * 
         * @return the map of remotes.
         */
        public NamedDomainObjectContainer<GitRemoteConfiguration> getRemotes() {
            return remotes;
        }

        /**
         * Accepts the DSL configuration for the {@code remotes} block, needed for defining
         * the block using the curly braces syntax in Gradle build scripts.
         * See the documentation on top of this class for more.
         * 
         * @param configurationAction the configuration action for the {@code items} block
         */
        public void remotes(Action<? super NamedDomainObjectContainer<GitRemoteConfiguration>> configurationAction) {
            configurationAction.execute(remotes);
        }

        /**
         * The class to model a single 'remotes' item within the extension.
         */
        public abstract static class GitRemoteConfiguration {
            /**
             * The remote name.
             */
            private final String name;

            /**
             * The remote authentication method property.
             */
            private final Property<String> authenticationMethod = getObjectfactory().property(String.class);

            /**
             * The remote password property.
             */
            private final Property<String> password = getObjectfactory().property(String.class);

            /**
             * The remote user property.
             */
            private final Property<String> user = getObjectfactory().property(String.class);

            /**
             * The remote private key property.
             */
            private final Property<String> privateKey = getObjectfactory().property(String.class);

            /**
             * The remote passphrase for the private key property.
             */
            private final Property<String> passphrase = getObjectfactory().property(String.class);

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
             * Constructor.
             * 
             * This constructor is required as per the {@link NamedDomainObjectContainer} specification.
             * 
             * @param name the remote name
             */
            public GitRemoteConfiguration(String name) {
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
             * Returns the remote authentication method. When this is set by the user it overrides
             * the inference performed by Nyx.
             * 
             * We provide an implementation of this method instead of using the abstract definition as it's
             * safer for old Gradle versions we support.
             * 
             * @return the remote authentication method
             */
            public Property<String> getAuthenticationMethod() {
                return authenticationMethod;
            }

            /**
             * Returns the remote password. When this is set by the user it overrides
             * the inference performed by Nyx.
             * 
             * We provide an implementation of this method instead of using the abstract definition as it's
             * safer for old Gradle versions we support.
             * 
             * @return the remote password
             */
            public Property<String> getPassword() {
                return password;
            }

            /**
             * Returns the remote user. When this is set by the user it overrides
             * the inference performed by Nyx.
             * 
             * We provide an implementation of this method instead of using the abstract definition as it's
             * safer for old Gradle versions we support.
             * 
             * @return the remote user
             */
            public Property<String> getUser() {
                return user;
            }

            /**
             * Returns the remote private key. When this is set by the user it overrides
             * the inference performed by Nyx.
             * 
             * We provide an implementation of this method instead of using the abstract definition as it's
             * safer for old Gradle versions we support.
             * 
             * @return the remote private key
             */
            public Property<String> getPrivateKey() {
                return privateKey;
            }

            /**
             * Returns the remote passphrase for the private key. When this is set by the user it overrides
             * the inference performed by Nyx.
             * 
             * We provide an implementation of this method instead of using the abstract definition as it's
             * safer for old Gradle versions we support.
             * 
             * @return the remote passphrase for the private key
             */
            public Property<String> getPassphrase() {
                return passphrase;
            }
        }
    }

    /**
     * The class to model a single 'releaseAssets' item within the extension.
     */
    public abstract static class AssetConfiguration {
        /**
         * The asset name.
         */
        private final String name;

        /**
         * The asset file name property.
         */
        private final Property<String> fileName = getObjectfactory().property(String.class);

        /**
         * The asset (short) description (or label) property.
         */
        private final Property<String> description = getObjectfactory().property(String.class);

        /**
         * The asset path property (local file or URL).
         */
        private final Property<String> path = getObjectfactory().property(String.class);

        /**
         * The asset MIME type property.
         */
        private final Property<String> type = getObjectfactory().property(String.class);

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
         * Constructor.
         * 
         * This constructor is required as per the {@link NamedDomainObjectContainer} specification.
         * 
         * @param name the asset name (the map key)
         */
        public AssetConfiguration(String name) {
            super();
            this.name = name;
        }

        /**
         * Returns the asset name read-only mandatory property.
         * 
         * @return the asset name read-only mandatory property.
         */
        public String getName() {
            return name;
        }

        /**
         * Returns the asset file name.
         * 
         * We provide an implementation of this method instead of using the abstract definition as it's
         * safer for old Gradle versions we support.
         * 
         * @return the asset name (usually the file name)
         */
        public Property<String> getFileName() {
            return fileName;
        }

        /**
         * Returns the asset (short) description (or label) property.
         * 
         * We provide an implementation of this method instead of using the abstract definition as it's
         * safer for old Gradle versions we support.
         * 
         * @return the asset (short) description (or label)
         */
        public Property<String> getDescription() {
            return description;
        }

        /**
         * Returns the asset path (local file or URL).
         * 
         * We provide an implementation of this method instead of using the abstract definition as it's
         * safer for old Gradle versions we support.
         * 
         * @return the asset path (local file or URL)
         */
        public Property<String> getPath() {
            return path;
        }

        /**
         * Returns the asset MIME type.
         * 
         * We provide an implementation of this method instead of using the abstract definition as it's
         * safer for old Gradle versions we support.
         * 
         * @return the asset MIME type
         */
        public Property<String> getType() {
            return type;
        }
    }

    /**
     * The class to model the 'releaseTypes' block within the extension.
     */
    public abstract static class ReleaseTypes {
        /**
         * The list of enabled release type names.
         */
        private final ListProperty<String> enabled = getObjectfactory().listProperty(String.class);

        /**
         * The list of publication service names.
         */
        private final ListProperty<String> publicationServices = getObjectfactory().listProperty(String.class);

        /**
         * The list of remote repository names.
         */
        private final ListProperty<String> remoteRepositories = getObjectfactory().listProperty(String.class);

        /**
         * The nested 'items' block.
         * 
         * @see ReleaseType
         */
        private NamedDomainObjectContainer<ReleaseType> items = getObjectfactory().domainObjectContainer(ReleaseType.class);

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
         * Returns list of enabled convention names.
         * 
         * @return list of enabled convention names.
         */
        public ListProperty<String> getEnabled() {
            return enabled;
        }

        /**
         * Returns list of publication service names.
         * 
         * @return list of publication service names.
         */
        public ListProperty<String> getPublicationServices() {
            return publicationServices;
        }

        /**
         * Returns list of remote repository names.
         * 
         * @return list of remote repository names.
         */
        public ListProperty<String> getRemoteRepositories() {
            return remoteRepositories;
        }

        /**
         * Returns the map of release type items.
         * 
         * @return the map of release type items.
         */
        public NamedDomainObjectContainer<ReleaseType> getItems() {
            return items;
        }

        /**
         * Accepts the DSL configuration for the {@code items} block, needed for defining
         * the block using the curly braces syntax in Gradle build scripts.
         * See the documentation on top of this class for more.
         * 
         * @param configurationAction the configuration action for the {@code items} block
         */
        public void items(Action<? super NamedDomainObjectContainer<ReleaseType>> configurationAction) {
            configurationAction.execute(items);
        }

        /**
         * The class to model a single 'releaseTypes' item within the extension.
         */
        public abstract static class ReleaseType {
            /**
             * The release type name.
             */
            private final String name;

            /**
             * The list of enabled assets for the release type.
             */
            // We need to let Gradle parse this as a string rather than a list of strings.
            // As documented at https://github.com/mooltiverse/nyx/issues/110, ListProperty.isPresent() never returns 'false', even when the
            // property hasn't been defined by the user, so we are unable to distinguish when it was defined as empty or not defined at all.
            // Within Nyx we have different semantics for the two cases o we need to read it as a simple string of comma separated items and
            // then (in the ConfigurationLayer class) split them to a list, if the string property was defined.
            private final Property<String> assets = getObjectfactory().property(String.class);

            /**
             * The flag indicating whether or not the 'collapsed' versioning (pre-release style) must be used.
             */
            private final Property<Boolean> collapseVersions = getObjectfactory().property(Boolean.class);

            /**
             * The optional qualifier or the template to render the qualifier to use for the pre-release identifier when versions are collapsed.
             */
            private final Property<String> collapsedVersionQualifier = getObjectfactory().property(String.class);

            /**
             * The optional template to render as a regular expression used for the release description.
             */
            private final Property<String> description = getObjectfactory().property(String.class);

            /**
             * The optional template to render as a regular expression used to match tags from the commit history.
             */
            private final Property<String> filterTags = getObjectfactory().property(String.class);

            /**
             * The optional flag or the template to render indicating whether or not a new commit must be generated in case new artifacts are generated.
             */
            private final Property<String> gitCommit = getObjectfactory().property(String.class);

            /**
             * The optional string or the template to render to use as the commit message if a commit has to be made.
             */
            private final Property<String> gitCommitMessage = getObjectfactory().property(String.class);

            /**
             * The optional flag or the template to render indicating whether or not a new commit must be generated and pushed in case new artifacts are generated.
             */
            private final Property<String> gitPush = getObjectfactory().property(String.class);

            /**
             * The optional flag or the template to render indicating whether or not a new tag must be generated.
             */
            private final Property<String> gitTag = getObjectfactory().property(String.class);

            /**
             * The optional string or the template to render to use as the tag message if a tag has to be made.
             */
            private final Property<String> gitTagMessage = getObjectfactory().property(String.class);

            /**
             * The nested 'identifiers' block.
             */
            // TODO: remove this member if and when https://github.com/mooltiverse/nyx/issues/77 is solved
            private NamedDomainObjectContainer<Identifier> identifiers = getObjectfactory().domainObjectContainer(Identifier.class);
            
            /**
             * The nested 'identifiers' block.
             */
            // TODO: uncomment this member if and when https://github.com/mooltiverse/nyx/issues/77 is solved
            //private final ListProperty<Identifier> identifiers = getObjectfactory().listProperty(Identifier.class);

            /**
             * The optional template to render as a regular expression used to match branch names.
             */
            private final Property<String> matchBranches = getObjectfactory().property(String.class);

            /**
             * The identifier of a specific workspace status to be matched.
             */
            private final Property<String> matchWorkspaceStatus = getObjectfactory().property(String.class);

            /**
             * The optional flag or the template to render indicating whether or not releases must be published.
             */
            private final Property<String> publish = getObjectfactory().property(String.class);

            /**
             * The optional template to render as a regular expression used to constrain versions issued by this release type.
             */
            private final Property<String> versionRange = getObjectfactory().property(String.class);

            /**
             * The optional flag telling if the version range must be inferred from the branch name.
             */
            private final Property<Boolean> versionRangeFromBranchName = getObjectfactory().property(Boolean.class);

            /**
             * The nested 'matchEnvironmentVariables' block.
             */
            private final MapProperty<String,String> matchEnvironmentVariables = getObjectfactory().mapProperty(String.class, String.class);

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
             * Constructor.
             * 
             * This constructor is required as per the {@link NamedDomainObjectContainer} specification.
             * 
             * @param name the release type name
             */
            public ReleaseType(String name) {
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
             * Returns a string with the comma separatedlist of selected asset names to publish with the release.
             * When {@code null} all assets configured globally (if any) must be published, otherwise only the assets
             * in this list must be published for this release type.
             * 
             * We provide an implementation of this method instead of using the abstract definition as it's
             * safer for old Gradle versions we support.
             * 
             * @return the list of selected asset names to publish with the release.
             */
            public Property<String> getAssets() {
                return assets;
            }

            /**
             * Returns the flag indicating whether or not the 'collapsed' versioning (pre-release style) must be used.
             * When this is set by the user it overrides the inference performed by Nyx.
             * 
             * We provide an implementation of this method instead of using the abstract definition as it's
             * safer for old Gradle versions we support.
             * 
             * @return the flag indicating whether or not the 'collapsed' versioning (pre-release style) must be used.
             */
            public Property<Boolean> getCollapseVersions() {
                return collapseVersions;
            }

            /**
             * Returns the optional qualifier or the template to render the qualifier to use for the pre-release
             * identifier when versions are collapsed. When this is set by the user it overrides
             * the inference performed by Nyx.
             * 
             * We provide an implementation of this method instead of using the abstract definition as it's
             * safer for old Gradle versions we support.
             * 
             * @return the optional qualifier or the template to render the qualifier to use for the pre-release
             * identifier when versions are collapsed
             */
            public Property<String> getCollapsedVersionQualifier() {
                return collapsedVersionQualifier;
            }

            /**
             * Returns the optional flag or the template to render indicating the release description.
             * 
             * We provide an implementation of this method instead of using the abstract definition as it's
             * safer for old Gradle versions we support.
             * 
             * @return the optional flag or the template to render indicating the release description.
             */
            public Property<String> getDescription() {
                return description;
            }

            /**
             * Returns the optional template to render as a regular expression used to match tags
             * from the commit history. When this is set by the user it overrides
             * the inference performed by Nyx.
             * 
             * We provide an implementation of this method instead of using the abstract definition as it's
             * safer for old Gradle versions we support.
             * 
             * @return the optional template to render as a regular expression used to match tags
             * from the commit history.
             */
            public Property<String> getFilterTags() {
                return filterTags;
            }

            /**
             * Returns the optional flag or the template to render indicating whether or not a new commit must be generated in case new artifacts are generated.
             * 
             * We provide an implementation of this method instead of using the abstract definition as it's
             * safer for old Gradle versions we support.
             * 
             * @return the optional flag or the template to render indicating whether or not a new commit must be generated in case new artifacts are generated.
             */
            public Property<String> getGitCommit() {
                return gitCommit;
            }

            /**
             * Returns the optional string or the template to render to use as the commit message if a commit has to be made.
             * When this is set by the user it overrides the inference performed by Nyx.
             * 
             * We provide an implementation of this method instead of using the abstract definition as it's
             * safer for old Gradle versions we support.
             * 
             * @return the optional string or the template to render to use as the commit message if a commit has to be made.
             */
            public Property<String> getGitCommitMessage() {
                return gitCommitMessage;
            }

            /**
             * Returns the optional flag or the template to render indicating whether or not a new commit must be generated and pushed in case new artifacts are generated.
             * When this is set by the user it overrides the inference performed by Nyx.
             * 
             * We provide an implementation of this method instead of using the abstract definition as it's
             * safer for old Gradle versions we support.
             * 
             * @return the optional flag or the template to render indicating whether or not a new commit must be generated and pushed in case new artifacts are generated.
             */
            public Property<String> getGitPush() {
                return gitPush;
            }

            /**
             * Returns the optional flag or the template to render indicating whether or not a new tag must be generated.
             * When this is set by the user it overrides the inference performed by Nyx.
             * 
             * We provide an implementation of this method instead of using the abstract definition as it's
             * safer for old Gradle versions we support.
             * 
             * @return the optional flag or the template to render indicating whether or not a new tag must be generated.
             */
            public Property<String> getGitTag() {
                return gitTag;
            }

            /**
             * Returns the optional string or the template to render to use as the tag message if a tag has to be made.
             * When this is set by the user it overrides the inference performed by Nyx.
             * 
             * We provide an implementation of this method instead of using the abstract definition as it's
             * safer for old Gradle versions we support.
             * 
             * @return the optional string or the template to render to use as the tag message if a tag has to be made.
             */
            public Property<String> getGitTagMessage() {
                return gitTagMessage;
            }

            /**
             * Returns the map of identifier items.
             * 
             * @return the map of identifier items.
             */
            public NamedDomainObjectContainer<Identifier> getIdentifiers() {
                // TODO: remove this method if and when https://github.com/mooltiverse/nyx/issues/77 is solved
                return identifiers;
            }

            /**
             * Accepts the DSL configuration for the {@code identifiers} block, needed for defining
             * the block using the curly braces syntax in Gradle build scripts.
             * See the documentation on top of this class for more.
             * 
             * @param configurationAction the configuration action for the {@code identifiers} block
             */
            public void identifiers(Action<? super NamedDomainObjectContainer<Identifier>> configurationAction) {
                // TODO: remove this method if and when https://github.com/mooltiverse/nyx/issues/77 is solved
                configurationAction.execute(identifiers);
            }

            /**
             * Returns the object mapping the {@code identifiers} block.
             * 
             * We provide an implementation of this method instead of using the abstract definition as it's
             * safer for old Gradle versions we support.
             * 
             * @return the object mapping the {@code identifiers} block
             */
            /*public ListProperty<Identifier> getIdentifiers() {
                // TODO: uncomment this method if and when https://github.com/mooltiverse/nyx/issues/77 is solved
                return identifiers;
            }*/

            /**
             * Accepts the DSL configuration for the {@code identifiers} block, needed for defining
             * the block using the curly braces syntax in Gradle build scripts.
             * See the documentation on top of this class for more.
             * 
             * @param configuration the configuration object for the {@code identifiers} block
             */
            /*public void identifiers(Action<? super ListProperty<Identifier>> configurationAction) {
                // TODO: uncomment this method if and when https://github.com/mooltiverse/nyx/issues/77 is solved
                configurationAction.execute(identifiers);
            }*/

            /**
             * Returns the optional template to render as a regular expression used to match branch names.
             * When this is set by the user it overrides the inference performed by Nyx.
             * 
             * We provide an implementation of this method instead of using the abstract definition as it's
             * safer for old Gradle versions we support.
             * 
             * @return the optional template to render as a regular expression used to match branch names.
             */
            public Property<String> getMatchBranches() {
                return matchBranches;
            }

            /**
             * Returns the map of match environment variables names and regular expressions items.
             * 
             * @return the map of match environment variables names and regular expressions items.
             */
            public MapProperty<String,String> getMatchEnvironmentVariables() {
                return matchEnvironmentVariables;
            }
            
            /**
             * Accepts the DSL configuration for the {@code matchEnvironmentVariables} block, needed for defining
             * the block using the curly braces syntax in Gradle build scripts.
             * See the documentation on top of this class for more.
             * 
             * @param configurationAction the configuration action for the {@code matchEnvironmentVariables} block
             */
            public void matchEnvironmentVariables(Action<? super MapProperty<String,String>> configurationAction) {
                configurationAction.execute(matchEnvironmentVariables);
            }

            /**
             * Returns the identifier of a specific workspace status to be matched.
             * When this is set by the user it overrides the inference performed by Nyx.
             * 
             * We provide an implementation of this method instead of using the abstract definition as it's
             * safer for old Gradle versions we support.
             * 
             * @return the identifier of a specific workspace status to be matched.
             */
            public Property<String> getMatchWorkspaceStatus() {
                return matchWorkspaceStatus;
            }

            /**
             * Returns the optional flag or the template to render indicating whether or not releases must be published.
             * When this is set by the user it overrides the inference performed by Nyx.
             * 
             * We provide an implementation of this method instead of using the abstract definition as it's
             * safer for old Gradle versions we support.
             * 
             * @return the optional flag or the template to render indicating whether or not releases must be published.
             */
            public Property<String> getPublish() {
                return publish;
            }

            /**
             * Returns the optional template to render as a regular expression used to constrain versions issued by this release type.
             * When this is set by the user it overrides the inference performed by Nyx.
             * 
             * We provide an implementation of this method instead of using the abstract definition as it's
             * safer for old Gradle versions we support.
             * 
             * @return the optional template to render as a regular expression used to constrain versions issued by this release type.
             */
            public Property<String> getVersionRange() {
                return versionRange;
            }

            /**
             * Returns the optional flag telling if the version range must be inferred from the branch name.
             * When this is set by the user it overrides the inference performed by Nyx.
             * 
             * We provide an implementation of this method instead of using the abstract definition as it's
             * safer for old Gradle versions we support.
             * 
             * @return the optional flag telling if the version range must be inferred from the branch name.
             */
            public Property<Boolean> getVersionRangeFromBranchName() {
                return versionRangeFromBranchName;
            }

            /**
             * The class to model a single 'identifiers' item within the extension.
             */
            public abstract static class Identifier {
                /**
                 * The identifier name.
                 */
                // TODO: remove this member if and when https://github.com/mooltiverse/nyx/issues/77 is solved
                private final String name;

                /**
                 * The identifier position.
                 */
                private final Property<String> position = getObjectfactory().property(String.class);

                /**
                 * The identifier qualifier.
                 */
                private final Property<String> qualifier = getObjectfactory().property(String.class);

                /**
                 * The identifier value.
                 */
                private final Property<String> value = getObjectfactory().property(String.class);

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
                 * Constructor.
                 * 
                 * This constructor is required as per the {@link NamedDomainObjectContainer} specification.
                 * 
                 * @param name the identifier name
                 */
                public Identifier(String name) {
                    super();
                    // TODO: remove this member if and when https://github.com/mooltiverse/nyx/issues/77 is solved
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
                 * Returns the identifier qualifier.
                 * When this is set by the user it overrides the inference performed by Nyx.
                 * 
                 * We provide an implementation of this method instead of using the abstract definition as it's
                 * safer for old Gradle versions we support.
                 * 
                 * @return the identifier qualifier.
                 */
                public Property<String> getPosition() {
                    return position;
                }

                /**
                 * Returns the identifier qualifier.
                 * When this is set by the user it overrides the inference performed by Nyx.
                 * 
                 * We provide an implementation of this method instead of using the abstract definition as it's
                 * safer for old Gradle versions we support.
                 * 
                 * @return the identifier qualifier.
                 */
                public Property<String> getQualifier() {
                    return qualifier;
                }

                /**
                 * Returns the identifier value.
                 * When this is set by the user it overrides the inference performed by Nyx.
                 * 
                 * We provide an implementation of this method instead of using the abstract definition as it's
                 * safer for old Gradle versions we support.
                 * 
                 * @return the identifier value.
                 */
                public Property<String> getValue() {
                    return value;
                }
            }
        }        
    }

    /**
     * The class to model a single 'services' item within the extension.
     */
    public abstract static class ServiceConfiguration {
        /**
         * The service name.
         */
        private final String name;

        /**
         * The service type property.
         */
        private final Property<String> type = getObjectfactory().property(String.class);

        /**
         * The nested 'options' block.
         */
        private final MapProperty<String,String> options = getObjectfactory().mapProperty(String.class, String.class);

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
         * Constructor.
         * 
         * This constructor is required as per the {@link NamedDomainObjectContainer} specification.
         * 
         * @param name the service name
         */
        public ServiceConfiguration(String name) {
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
         * Returns the service type. When this is set by the user it overrides
         * the inference performed by Nyx.
         * 
         * We provide an implementation of this method instead of using the abstract definition as it's
         * safer for old Gradle versions we support.
         * 
         * @return the service type
         */
        public Property<String> getType() {
            return type;
        }

        /**
         * Returns the map of service options.
         * 
         * @return the map of service options.
         */
        public MapProperty<String,String> geOptions() {
            return options;
        }
        
        /**
         * Accepts the DSL configuration for the {@code options} block, needed for defining
         * the block using the curly braces syntax in Gradle build scripts.
         * See the documentation on top of this class for more.
         * 
         * @param configurationAction the configuration action for the {@code options} block
         */
        public void options(Action<? super MapProperty<String,String>> configurationAction) {
            configurationAction.execute(options);
        }
    }
}
