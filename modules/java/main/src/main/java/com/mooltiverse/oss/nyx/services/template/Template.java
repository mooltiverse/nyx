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
package com.mooltiverse.oss.nyx.services.template;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.StringReader;

import java.net.URI;
import java.util.Map;
import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.Marker;
import org.slf4j.MarkerFactory;

import com.mooltiverse.oss.nyx.io.DataAccessException;
import com.mooltiverse.oss.nyx.io.TransportException;
import com.mooltiverse.oss.nyx.services.AssetService;
import com.mooltiverse.oss.nyx.services.Service;
import com.mooltiverse.oss.nyx.services.git.Repository;
import com.mooltiverse.oss.nyx.state.State;
import com.mooltiverse.oss.nyx.template.Templates;

/**
 * The entry point to the service producing assets using a template starting from the state object.
 */
public class Template implements AssetService {
    /**
     * The template to use for rendering. It can't {@code null}.
     */
    private String template = null;

    /**
     * The {@code SERVICE} marker, used when logging command events.
     */
    static final Marker SERVICE = MarkerFactory.getMarker("SERVICE");

    /**
     * The logger instance.
     */
    static final Logger logger = LoggerFactory.getLogger(Template.class);

    /**
     * The name of the option used to pass the template to this object instance.
     * This is the value of the key inside the options passed to get a new instance of this class.
     * If this option is not passed the service will raise an exception.
     * Value: {@value}
     */
    public static final String TEMPLATE_OPTION_NAME = "TEMPLATE";

    /**
     * Builds an instance.
     * 
     * @param template the template to use for rendering. It can't be {@code null}.
     */
    private Template(String template) {
        super();
        this.template = template;
    }

    /**
     * Returns an instance using the given options.
     * 
     * @param options the map of options for the requested service. It can't be {@code null}.
     * Valid options are documented as constants on this class.
     * 
     * @return an instance using the given options.
     * 
     * @throws NullPointerException if the given options map is {@code null}
     * @throws IllegalArgumentException if some entries in the given options map are missing or illegal for some reason
     */
    public static Template instance(Map<String,String> options) {
        Objects.requireNonNull(options, "Can't create a new instance with a null options map");

        String template = null;
        if (Objects.isNull(options.get(TEMPLATE_OPTION_NAME)))
            logger.warn(SERVICE, "No template passed to the '{}' service. Use the '{}' option to set this value", Template.class.getSimpleName(), TEMPLATE_OPTION_NAME);
        else template = options.get(TEMPLATE_OPTION_NAME);

        return new Template(template);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public URI buildAsset(String destination, State state, Repository repository)
        throws SecurityException, TransportException, DataAccessException {
        Objects.requireNonNull(state, "Can't render the template with a null state reference");
        Objects.requireNonNull(destination, "Can't render the template with a null or empty destination file path");
        Objects.requireNonNull(template, "Can't render the asset with a null template");

        logger.info(SERVICE, "Template.buildAsset()");

        File destinationFile = new File(destination);
        try {
            Templates.render(new StringReader(template), state, new FileWriter(destinationFile));
        }
        catch (IOException ioe) {
            throw new DataAccessException(ioe);
        }

        return destinationFile.toURI();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean supports(Service.Feature feature) {
        Objects.requireNonNull(feature, "Can't check if the feature is supported for a null feature");
        switch (feature)
        {
            case ASSET:         return true;
            default:            return false;
        }
    }
}
