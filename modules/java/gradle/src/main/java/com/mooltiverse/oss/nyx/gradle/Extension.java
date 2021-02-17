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

import org.gradle.api.model.ObjectFactory;
import org.gradle.api.provider.Property;

import com.mooltiverse.oss.nyx.version.Scheme;

/**
 * The plugin configuration object.
 * 
 * See <a href="https://docs.gradle.org/current/userguide/implementing_gradle_plugins.html#modeling_dsl_like_apis">Modeling DSL-like APIs</a>
 * for more o developing extension.
 * 
 * See <a href="https://docs.gradle.org/current/userguide/custom_gradle_types.html">Developing Custom Gradle Types</a> for an introduction
 * on custom Gradle types development.
 * 
 * @see org.gradle.api.plugins.ExtensionAware
 * @see org.gradle.api.plugins.ExtensionContainer
 */
public class Extension {
    /**
     * The name of the extension object for this plugin. This is the name of the configuration block inside
     * Gradle scripts.
     */
    public static final String NAME = "nyx";

    //private final CustomData customData;

    /*public NyxExtension (ObjectFactory objects) {
        super();
        //customData = objects.newInstance(CustomData.class);
    }*/

    /**
     * The working directory where the Git repository is, also used as the base path for other relative paths.
     * 
     * By default is the current user directory.
     */
    private String directory = System.getProperty("user.dir");

    /**
     * When <code>true</code> no changes will be applied by any mean. Instead they are just logged.
     * 
     * Reading and parsing is done as usual.
     */
    private boolean dryRun = false;

    /**
     * The prefix used to generate release names.
     */
    private String releasePrefix = "v";

    /**
     * Whether or not prefixes other than {@link #releasePrefix} are to be tolerated when reading release tags.
     */
    private boolean releasePrefixLenient = true;

    /**
     * The versioning scheme to use.
     * 
     * @see Scheme for the available schemes
     */
    private String scheme = Scheme.SEMVER.toString();

    /**
     * The logging verbosity.
     */
    private String verbosity = "quiet";

    /**
     * A nested object for the extension.
     */
    /**public abstract class CustomData {

        abstract public Property<String> getProperty1();

        abstract public Property<String> getProperty2();
    }*/
}
