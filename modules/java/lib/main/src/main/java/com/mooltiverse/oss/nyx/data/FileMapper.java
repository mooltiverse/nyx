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
package com.mooltiverse.oss.nyx.data;

import static com.mooltiverse.oss.nyx.log.Markers.DATA;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;

/**
 * This class is used to load and save data files like configuration or state files.
 */
public class FileMapper {
    /**
     * The private logger instance
     */
    private static final Logger logger = LoggerFactory.getLogger(FileMapper.class);

    /**
     * Default constructor is private on purpose.
     */
    private FileMapper() {
        super();
    }

    /**
     * Returns an object mapper instance, with the required custom features set.
     * 
     * @param filePath the target file path, used to infer the file format. If it doesn't have a recognized
     * extension JSON is used by default.
     * 
     * @return the object mapper instance.
     * 
     * @throws IllegalArgumentException if the given file extension is not valid or among supported ones
     */
    private static ObjectMapper getObjectMapper(String filePath) {
        ObjectMapper objectMapper = new ObjectMapper();

        logger.trace(DATA, "Retrieving an object mapper instance for file '{}'", filePath);

        if (filePath.toLowerCase().endsWith(".json")) {
            objectMapper = new ObjectMapper(new JsonFactory());
        }
        else if (filePath.toLowerCase().endsWith(".yaml") || filePath.toLowerCase().endsWith(".yml")) {
            objectMapper = new ObjectMapper(new YAMLFactory());
        }
        else {
            logger.debug(DATA, "Unable to infer the extension from file '{}', using JSON by default", filePath);
            objectMapper = new ObjectMapper(new JsonFactory());
        }

        // Only serialize objects and attributes with non null values. Here we set it at the global level.
        // If this turns out to be an issue the same attribute can be set at a class or attribute level.
        objectMapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);

        objectMapper.enable(SerializationFeature.INDENT_OUTPUT);
        objectMapper.enable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
        objectMapper.enable(SerializationFeature.WRITE_ENUMS_USING_TO_STRING);

        objectMapper.disable(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES);

        return objectMapper;
    }

    /**
     * Unmarshals the content of the given file to an object of the given type.
     * 
     * @param file the file to load from. The file path must end with one of the supported
     * extensions: {@code json}, {@code yaml}, {@code yml} (or JSON is used by default).
     * @param type the class representing the type of object to unmarshal.
     * 
     * @param <T> the type of object to unmarshal.
     * 
     * @return the object instance unmarshalled from the given file.
     * 
     * @throws DataAccessException in case of any exception due to data access
     * @throws IllegalArgumentException if the given file path does not contain a supported extension
     */
    public static <T> T load(File file, Class<T> type)
        throws DataAccessException {
        logger.trace(DATA, "Unmarshalling object from file '{}' to type '{}'", file.getAbsolutePath(), type.getName());
        try {
            return getObjectMapper(file.getAbsolutePath()).readValue(file, type);
        }
        catch (IOException ioe) {
            throw new DataAccessException(String.format("Unable to unmarshal content from file '%s'", file.getAbsolutePath()), ioe);
        }
    }

    /**
     * Unmarshals the content of the given URL to an object of the given type.
     * 
     * @param url the URL to load from. The file path must end with one of the supported
     * extensions: {@code json}, {@code yaml}, {@code yml} (or JSON is used by default).
     * @param type the class representing the type of object to unmarshal.
     * 
     * @param <T> the type of object to unmarshal.
     * 
     * @return the object instance unmarshalled from the given URL.
     * 
     * @throws DataAccessException in case of any exception due to data access
     * @throws IllegalArgumentException if the given file path does not contain a supported extension
     */
    public static <T> T load(URL url, Class<T> type)
        throws DataAccessException {
        logger.trace(DATA, "Unmarshalling object from URL '{}' to type '{}'", url.toString(), type.getName());
        try {
            return getObjectMapper(url.getFile()).readValue(url, type);
        }
        catch (IOException ioe) {
            throw new DataAccessException(String.format("Unable to unmarshal content from URL '%s'", url.toString()), ioe);
        }
    }

    /**
     * Marshals the content of the given object to a file represented by the given path.
     * 
     * @param filePath the path of the file to save to. If it's a relative path it will be
     * considered relative to the current working directory. The file path must end with one of the supported
     * extensions: {@code json}, {@code yaml}, {@code yml} (or JSON is used by default).
     * @param content the object to marshal.
     * 
     * @throws DataAccessException in case of any exception due to data access
     * 
     * @throws IllegalArgumentException if the given file path does not contain a supported extension
     */
    public static void save(String filePath, Object content)
        throws DataAccessException {
        logger.trace(DATA, "Marshalling object to file '{}' to type '{}'", filePath, content.getClass().getName());
        try {
            File file = new File(filePath);
            // create parent directories, if any and if needed
            File parentDirectory = file.getParentFile();
            if (!Objects.isNull(parentDirectory))
                parentDirectory.mkdirs();
                
            getObjectMapper(filePath).writeValue(file, content);
        }
        catch (IOException ioe) {
            throw new DataAccessException(String.format("Unable to marshal content to file '%s'", filePath), ioe);
        }
    }
}
