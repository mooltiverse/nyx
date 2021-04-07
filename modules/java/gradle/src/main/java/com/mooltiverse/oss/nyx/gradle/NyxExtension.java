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

import com.mooltiverse.oss.nyx.configuration.Defaults;

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
     * The nested 'commitMessageConventions' block.
     */
    private final CommitMessageConventions commitMessageConventions = getObjectfactory().newInstance(CommitMessageConventions.class);

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
     * The 'initialVersion' property.
     */
    private final Property<String> initialVersion = getObjectfactory().property(String.class);

    /**
     * The 'releasePrefix' property.
     */
    private final Property<String> releasePrefix = getObjectfactory().property(String.class);

    /**
     * The 'releaseLenient' property.
     */
    private final Property<Boolean> releaseLenient = getObjectfactory().property(Boolean.class);

    /**
     * The 'scheme' property.
     */
    private final Property<String> scheme = getObjectfactory().property(String.class);

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
     * @param configuration the configuration object for the {@code commitMessageConventions} block
     */
    public void commitMessageConventions(Action<? super CommitMessageConventions> configuration) {
        configuration.execute(commitMessageConventions);
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
         * @param configuration the configuration object for the {@code items} block
         */
        public void items(Action<? super NamedDomainObjectContainer<CommitMessageConvention>> configuration) {
            configuration.execute(items);
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
             * Returns the name read-only mandatory property.
             * 
             * @return the name read-only mandatory property.
             */
            public String getName() {
                return name;
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
             * @param configuration the configuration object for the {@code bumpExpressions} block
             */
            public void bumpExpressions(Action<? super MapProperty<String,String>> configuration) {
                configuration.execute(bumpExpressions);
            }
        }
    }
}
