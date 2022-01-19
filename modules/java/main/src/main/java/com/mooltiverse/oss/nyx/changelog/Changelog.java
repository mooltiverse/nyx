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
package com.mooltiverse.oss.nyx.changelog;

import static com.mooltiverse.oss.nyx.log.Markers.COMMAND;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.Reader;
import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mooltiverse.oss.nyx.entities.ChangelogConfiguration;
import com.mooltiverse.oss.nyx.io.DataAccessException;
import com.mooltiverse.oss.nyx.template.Templates;

/**
 * The entry point to the service producing changelogs.
 */
public class Changelog {
    /**
     * The name of the resource to load for the default template. Value: {@value}.
     */
    public static final String DEFAULT_TEMPLATE_RESOURCE_NAME = "changelog.tpl";
    /**
     * The logger instance.
     */
    static final Logger logger = LoggerFactory.getLogger(Changelog.class);

    /**
     * The private instance of the changelog configuration.
     */
    private final ChangelogConfiguration configuration;

    /**
     * Standard constructor.
     * 
     * @param configuration the configuration object. It can't be {@code null}.
     * 
     * @throws NullPointerException if the given configuration map is {@code null}
     */
    private Changelog(ChangelogConfiguration configuration) {
        super();
        Objects.requireNonNull(configuration, "Can't create a new instance with a null configuration");
        this.configuration = configuration;
    }

    /**
     * Returns an instance using the given configuration.
     * 
     * @param configuration the configuration object. It can't be {@code null}.
     * 
     * @return an instance using the given configuration.
     * 
     * @throws NullPointerException if the given configuration map is {@code null}
     * @throws IllegalArgumentException if some entries in the given configuration map are missing or illegal for some reason
     */
    public static Changelog instance(ChangelogConfiguration configuration) {
        return new Changelog(configuration);
    }

    /**
     * Returns a reader object that reads the template to be used for rendering.
     * If the configuretion overrides the template then the reader will point to that
     * template otherwise the default template will be returned.
     * 
     * @return a reader object that reads the template to be used for rendering. Never {@code null}.
     * 
     * @throws DataAccessException if an exception occurs when reading the template.
     */
    private Reader getTemplateReader()
        throws DataAccessException {
        if (Objects.isNull(configuration.getTemplate()) || configuration.getTemplate().isBlank()) {
            logger.debug(COMMAND, "The changelog template has not been overridden by configuration. Loading the default template.");
            try {
                InputStream is = getClass().getClassLoader().getResourceAsStream(DEFAULT_TEMPLATE_RESOURCE_NAME);
                if (Objects.isNull(is))
                    throw new DataAccessException(String.format("Unable to load the default changelog template resource from '%s'", DEFAULT_TEMPLATE_RESOURCE_NAME));
                else return new InputStreamReader(is);
            }
            catch (SecurityException se) {
                throw new DataAccessException(String.format("Unable to load the default changelog template resource from '%s'", DEFAULT_TEMPLATE_RESOURCE_NAME), se);
            }
        }
        else {
            try {
                return new FileReader(configuration.getTemplate());
            }
            catch (FileNotFoundException fnfe) {
                throw new DataAccessException(String.format("Unable to load the configured changelog template file from '%s'", configuration.getTemplate()), fnfe);
            }
        }
    }

    /**
     * Returns the structured data of a changelog built according to the configuration.
     * 
     * @return the structured data of a changelog built according to the configuration.
     * Never {@code null}.
     */
    public Model getModel() {
        Model model = new Model();

        return model;
    }

    /**
     * Builds the changelog using the internal configuration and stores it to the given file.
     * This implies {@link #getModel() creating} the internal model if it hasn't been done yet.
     * <br>
     * Please note that this method ignores the
     * {@link ChangelogConfiguration#getPath() configured destination path}.
     * 
     * @param destination the destination file. It can't be {@code null}.
     * 
     * @throws DataAccessException if an exception occurs when reading the template or
     * storing the output to the configured destination file.
     */
    public void saveTo(File destination)
        throws DataAccessException {
        Objects.requireNonNull(destination, "Can't save the changelog to a null file");
        try {
            FileWriter writer = new FileWriter(destination);
            Templates.render(getTemplateReader(), getModel(), writer);
            writer.flush();
            writer.close();
        }
        catch (IOException ioe) {
            throw new DataAccessException(String.format("Unable to render the changelog to file '%s'", destination.getAbsolutePath()));
        }
    }

    /**
     * The class modelling the changelog entities.
     */
    public static final class Model {
        /**
         * Default constructor is hidden on purpose.
         */
        private Model() {
            super();
        }
    }
}
