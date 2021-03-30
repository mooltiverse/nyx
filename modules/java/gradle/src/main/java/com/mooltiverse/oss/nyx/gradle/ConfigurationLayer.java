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

import static org.gradle.api.Project.DEFAULT_VERSION;

import static com.mooltiverse.oss.nyx.gradle.Constants.GRADLE_VERSION_PROPERTY_NAME;

import java.io.File;
import java.util.Objects;

import com.mooltiverse.oss.nyx.data.IllegalPropertyException;
import com.mooltiverse.oss.nyx.data.Scheme;
import com.mooltiverse.oss.nyx.data.Verbosity;
import com.mooltiverse.oss.nyx.version.Version;
import com.mooltiverse.oss.nyx.version.VersionFactory;

/**
 * This class is an adapter to allow the extension to be used as a Nyx configuration layer.
 */
class ConfigurationLayer implements com.mooltiverse.oss.nyx.configuration.ConfigurationLayer {
    /**
     * The private instance of the extension object to adapt
     */
    private final NyxExtension extension;

    /**
     * The project version, as defined by the user in Gradle's {@code version} property.
     * May be {@code null} if the user has not defined any {@code version} property in the script
     */
    private final Object projectVersion;

    /**
     * Standard constructor.
     * 
     * @param extension the extension instance to be adapted to a configuration layer
     * @param projectVersion the Gradle project version, which may be {@code null} if the user has not defined
     * any {@code version} property in the script
     */
    ConfigurationLayer(NyxExtension extension, Object projectVersion) {
        super();
        if (Objects.isNull(extension))
            throw new IllegalArgumentException("Cannot build a configuration layer adapter with a null extension");
        this.extension = extension;
        this.projectVersion = projectVersion;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getBump() {
        return extension.getBump().getOrNull();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public File getDirectory() {
        return extension.getDirectory().getOrNull();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Boolean getDryRun() {
        return extension.getDryRun().getOrNull();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Version getInitialVersion()
        throws IllegalPropertyException {
        return extension.getInitialVersion().isPresent() ? VersionFactory.valueOf(getScheme().getScheme(), extension.getInitialVersion().get(), getReleaseLenient()) : null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getReleasePrefix() {
        return extension.getReleasePrefix().getOrNull();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Boolean getReleaseLenient() {
        return extension.getReleaseLenient().getOrNull();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Scheme getScheme()
        throws IllegalPropertyException {
        if (extension.getScheme().isPresent() && !Objects.isNull(extension.getScheme().getOrNull())) {
            try {
                return Scheme.from(extension.getScheme().get());
            }
            catch (IllegalArgumentException iae) {
                throw new IllegalPropertyException(String.format("Illegal value '%s' provided for configuration option '%s'", extension.getScheme().get(), "scheme"), iae);
            }
        }
        else return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Verbosity getVerbosity()
        throws IllegalPropertyException {
        if (extension.getVerbosity().isPresent() && !Objects.isNull(extension.getVerbosity().getOrNull())) {
            try {
                return Verbosity.from(extension.getVerbosity().get());
            }
            catch (IllegalArgumentException iae) {
                throw new IllegalPropertyException(String.format("Illegal value '%s' provided for configuration option '%s'", extension.getVerbosity().get(), "verbosity"), iae);
            }
        }
        else return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Version getVersion()
        throws IllegalPropertyException {
        // when to 'version' property is defined, Gradle does not return null but instead the 'unspecified' string which, to us,
        // means there is no version defined, just like it was null
        if (Objects.isNull(projectVersion) || DEFAULT_VERSION.equals(projectVersion))
            return null;
        else {
            try {
                // TODO: replace the hardcoded use of SEMVER with a resolved scheme
                //
                // This issue is common to all configuration layers when they need to resolve options using the entire configuration
                // (not just their local values) because some options may be defined with higher priority in other layers.
                // This example is significant for all layers because in order to resolve the Version option, the layer also needs
                // to resolve the Scheme and ReleaseLenient options.
                // One caveat is to prevent circular dependencies, as per https://github.com/mooltiverse/nyx/issues/37
                //
                // While using SEMVER as the scheme here is suitable for now as it's the only supported scheme, this must
                // be resolved against all configuration layers. See also #37 (https://github.com/mooltiverse/nyx/issues/37).
                //
                // Moreover, we also need to consider if the getReleaseLenient option has been set and, if not, the getReleasePrefix
                // to know if the parsing has to tolerate prefixes and, if so, if any prefix or just one.
                // Again, we enable sanitization as it's suitable for now but this must be fixed as soon as possible
                return VersionFactory.valueOf(Scheme.SEMVER.getScheme(), projectVersion.toString(), true);
            }
            catch (IllegalArgumentException iae) {
                throw new IllegalPropertyException(String.format("Illegal value '%s' provided for project property '%s'", projectVersion, GRADLE_VERSION_PROPERTY_NAME), iae);
            }
        }
    }
}
