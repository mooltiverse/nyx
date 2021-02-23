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
import java.util.Objects;

import org.gradle.api.file.DirectoryProperty;

import org.slf4j.event.Level;

import com.mooltiverse.oss.nyx.version.Scheme;

/**
 * This class is an adapter to allow the extension to be used as a Nyx configuration layer.
 */
class ConfigurationLayer implements com.mooltiverse.oss.nyx.configuration.ConfigurationLayer {
    /**
     * The private instance of the extension object to adapt
     */
    private final NyxExtension extension;

    /**
     * Standard constructor.
     * 
     * @param extension the extension instance to be adapted to a configuration layer
     */
    ConfigurationLayer(NyxExtension extension) {
        super();
        if (Objects.isNull(extension))
            throw new IllegalArgumentException("Cannot build a configuration layer adapter with a null extension");
        this.extension = extension;
    }

    /**
     * {@inheritDoc}
     */
    public String getBump() {
        return extension.getBump().getOrNull();
    }

    /**
     * {@inheritDoc}
     */
    public File getDirectory() {
        DirectoryProperty directoryProperty = extension.getDirectory();
        return Objects.isNull(directoryProperty) ? null : directoryProperty.getAsFile().get();
    }

    /**
     * {@inheritDoc}
     */
    public Boolean getDryRun() {
        return extension.getDryRun().getOrNull();
    }

    /**
     * {@inheritDoc}
     */
    public String getReleasePrefix() {
        return extension.getReleasePrefix().getOrNull();
    }

    /**
     * {@inheritDoc}
     */
    public Boolean getReleasePrefixLenient() {
        return extension.getReleasePrefixLenient().getOrNull();
    }

    /**
     * {@inheritDoc}
     */
    public Scheme getScheme() {
        return null; // TODO: adapt the value to a Scheme
    }

    /**
     * {@inheritDoc}
     */
    public Level getVerbosity(){
        return null; // TODO: adapt the value to the Nyx verbosity
    }
}
