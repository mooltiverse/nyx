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

import java.io.File;
import java.io.IOException;

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
     * Default constructor is private on purpose.
     */
    private FileMapper() {
        super();
    }

    /**
     * Returns an object mapper instance, with the required custom features set.
     * 
     * @param filePath the target file path, used to infer the file format
     * 
     * @return the object mapper instance.
     * 
     * @throws IllegalArgumentException if the given file extension is not valid or among supported ones
     */
    private static ObjectMapper getObjectMapper(String filePath) {
        ObjectMapper objectMapper = new ObjectMapper();

        if (filePath.toLowerCase().endsWith(".json")) {
            objectMapper = new ObjectMapper(new JsonFactory());
        }
        else if (filePath.toLowerCase().endsWith(".yaml") || filePath.toLowerCase().endsWith(".yml")) {
            objectMapper = new ObjectMapper(new YAMLFactory());
        }
        else throw new IllegalArgumentException(String.format("Unsupported extension in file %s. Supported extensions are .json, .yaml, .yml, .properties", filePath));

        objectMapper.enable(SerializationFeature.INDENT_OUTPUT);
        objectMapper.enable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
        objectMapper.enable(SerializationFeature.WRITE_ENUMS_USING_TO_STRING);

        objectMapper.disable(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES);

        return objectMapper;
    }

    /**
     * Marshals the content of the given object to a file represented by the given path
     * 
     * @param filePath the path of the file to save to. If it's a relative path it will be
     * considered relative to the current working directory. The file path must end with one of the supported
     * extensions: {@code json}, {@code yaml}, {@code properties}
     * @param content the object to marshal.
     * 
     * @throws DataAccessException in case of any exception due to data access
     * 
     * @throws IllegalArgumentException if the given file path does not contain a supported extension
     */
    public static void save(String filePath, Object content)
        throws DataAccessException {
        try {
            getObjectMapper(filePath).writeValue(new File(filePath), content);
        }
        catch (IOException ioe) {
            throw new DataAccessException(String.format("Unable to marshal content to file %s", filePath), ioe);
        }
    }
}
