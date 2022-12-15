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
package com.mooltiverse.oss.nyx.configuration;

import static org.junit.jupiter.api.Assertions.*;

import java.io.File;
import java.io.FileReader;
import java.io.StringWriter;

import com.mooltiverse.oss.nyx.io.FileMapper;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

/**
 * These are not actual tests but rather examples printed to the standard output for a few configuration files.
 */
@DisplayName("ConfigurationExamples")
public class ConfigurationExamplesTests {
    /**
     * The name of the system property used to pass the path of the
     * extended JSON configuration file example to tests.
     */
    public static final String EXTENDED_JSON_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY = "extendedJSONExampleConfigurationFile";

    /**
     * The name of the system property used to pass the path of the
     * medium JSON configuration file example to tests.
     */
    public static final String MEDIUM_JSON_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY = "mediumJSONExampleConfigurationFile";

    /**
     * The name of the system property used to pass the path of the
     * simple JSON configuration file example to tests.
     */
    public static final String SIMPLE_JSON_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY = "simpleJSONExampleConfigurationFile";
    
    /**
     * The name of the system property used to pass the path of the
     * simplest JSON configuration file example to tests.
     */
    public static final String SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY = "simplestJSONExampleConfigurationFile";

    /**
     * The name of the system property used to pass the path of the
     * extended YAML configuration file example to tests.
     */
    public static final String EXTENDED_YAML_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY = "extendedYAMLExampleConfigurationFile";

    /**
     * The name of the system property used to pass the path of the
     * medium YAML configuration file example to tests.
     */
    public static final String MEDIUM_YAML_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY = "mediumYAMLExampleConfigurationFile";

    /**
     * The name of the system property used to pass the path of the
     * simple YAML configuration file example to tests.
     */
    public static final String SIMPLE_YAML_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY = "simpleYAMLExampleConfigurationFile";

    /**
     * The name of the system property used to pass the path of the
     * simplest YAML configuration file example to tests.
     */
    public static final String SIMPLEST_YAML_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY = "simplestYAMLExampleConfigurationFile";

    /**
     * Reads the contents of the given file and returns its content as a string.
     * 
     * @param file the file to read
     * 
     * @return the file content
     * 
     * @throws Exception in case of any issue
     */
    private String readFile(File file)
        throws Exception {
        StringWriter buffer = new StringWriter();
        FileReader reader = new FileReader(file);
        reader.transferTo(buffer);
        reader.close();
        return buffer.toString();
    }

    @Test
    @DisplayName("Save and Load Simplest JSON configuration")
    void saveAndLoadSimplestJSONExample()
        throws Exception {
        assertNotNull(System.getProperty(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY), "A configuration file path must be passed to this test as a system property but it was not set");
        File exampleFile = new File(System.getProperty(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY));
        File savedFile = new File(System.getProperty("java.io.tmpdir"), "simplest"+this.hashCode()+".json");
        savedFile.deleteOnExit();
        FileMapper.save(savedFile.getAbsolutePath(), FileMapper.load(exampleFile, SimpleConfigurationLayer.class));

        // print the file to standard output for inspection purpose
        System.out.println("------ Simplest JSON configuration ------");
        System.out.println("Loading from: "+exampleFile.getAbsolutePath());
        System.out.println("-----------------------------------------");
        System.out.println(readFile(exampleFile));
        System.out.println("-----------------------------------------");
        System.out.println("Saving to: "+savedFile.getAbsolutePath());
        System.out.println("-----------------------------------------");
        System.out.println(readFile(savedFile));
        System.out.println("-----------------------------------------");
        System.out.flush();
    }

    @Test
    @DisplayName("Save and Load Simple JSON configuration")
    void saveAndLoadSimpleJSONExample()
        throws Exception {
        assertNotNull(System.getProperty(SIMPLE_JSON_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY), "A configuration file path must be passed to this test as a system property but it was not set");
        File exampleFile = new File(System.getProperty(SIMPLE_JSON_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY));
        File savedFile = new File(System.getProperty("java.io.tmpdir"), "simple"+this.hashCode()+".json");
        savedFile.deleteOnExit();
        FileMapper.save(savedFile.getAbsolutePath(), FileMapper.load(exampleFile, SimpleConfigurationLayer.class));

        // print the file to standard output for inspection purpose
        System.out.println("------  Simple JSON configuration  ------");
        System.out.println("-----------------------------------------");
        System.out.println(readFile(exampleFile));
        System.out.println("-----------------------------------------");
        System.out.println("Saving to: "+savedFile.getAbsolutePath());
        System.out.println("-----------------------------------------");
        System.out.println(readFile(savedFile));
        System.out.println("-----------------------------------------");
        System.out.flush();
    }

    @Test
    @DisplayName("Save and Load Medium JSON configuration")
    void saveAndLoadMediumJSONExample()
        throws Exception {
        assertNotNull(System.getProperty(MEDIUM_JSON_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY), "A configuration file path must be passed to this test as a system property but it was not set");
        File exampleFile = new File(System.getProperty(MEDIUM_JSON_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY));
        File savedFile = new File(System.getProperty("java.io.tmpdir"), "medium"+this.hashCode()+".json");
        savedFile.deleteOnExit();
        FileMapper.save(savedFile.getAbsolutePath(), FileMapper.load(exampleFile, SimpleConfigurationLayer.class));

        // print the file to standard output for inspection purpose
        System.out.println("------  Medium JSON configuration  ------");
        System.out.println("Loading from: "+exampleFile.getAbsolutePath());
        System.out.println("-----------------------------------------");
        System.out.println(readFile(exampleFile));
        System.out.println("-----------------------------------------");
        System.out.println("Saving to: "+savedFile.getAbsolutePath());
        System.out.println("-----------------------------------------");
        System.out.println(readFile(savedFile));
        System.out.println("-----------------------------------------");
        System.out.flush();
    }

    @Test
    @DisplayName("Save and Load Extended JSON configuration")
    void saveAndLoadExtendedJSONExample()
        throws Exception {
        assertNotNull(System.getProperty(EXTENDED_JSON_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY), "A configuration file path must be passed to this test as a system property but it was not set");
        File exampleFile = new File(System.getProperty(EXTENDED_JSON_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY));
        File savedFile = new File(System.getProperty("java.io.tmpdir"), "extended"+this.hashCode()+".json");
        savedFile.deleteOnExit();
        FileMapper.save(savedFile.getAbsolutePath(), FileMapper.load(exampleFile, SimpleConfigurationLayer.class));

        // print the file to standard output for inspection purpose
        System.out.println("------ Extended JSON configuration ------");
        System.out.println("Loading from: "+exampleFile.getAbsolutePath());
        System.out.println("-----------------------------------------");
        System.out.println(readFile(exampleFile));
        System.out.println("-----------------------------------------");
        System.out.println("Saving to: "+savedFile.getAbsolutePath());
        System.out.println("-----------------------------------------");
        System.out.println(readFile(savedFile));
        System.out.println("-----------------------------------------");
        System.out.flush();
    }

    @Test
    @DisplayName("Save and Load Simplest YAML configuration")
    void saveAndLoadSimplestYAMLExample()
        throws Exception {
        assertNotNull(System.getProperty(SIMPLEST_YAML_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY), "A configuration file path must be passed to this test as a system property but it was not set");
        File exampleFile = new File(System.getProperty(SIMPLEST_YAML_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY));
        File savedFile = new File(System.getProperty("java.io.tmpdir"), "simplest"+this.hashCode()+".yaml");
        FileMapper.save(savedFile.getAbsolutePath(), FileMapper.load(exampleFile, SimpleConfigurationLayer.class));

        // print the file to standard output for inspection purpose
        System.out.println("------ Simplest YAML configuration ------");
        System.out.println("Loading from: "+exampleFile.getAbsolutePath());
        System.out.println("-----------------------------------------");
        System.out.println(readFile(exampleFile));
        System.out.println("-----------------------------------------");
        System.out.println("Saving to: "+savedFile.getAbsolutePath());
        System.out.println("-----------------------------------------");
        System.out.println(readFile(savedFile));
        System.out.println("-----------------------------------------");
        System.out.flush();
    }

    @Test
    @DisplayName("Save and Load Simple YAML configuration")
    void simpleYAMLExample()
        throws Exception {
        assertNotNull(System.getProperty(SIMPLE_YAML_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY), "A configuration file path must be passed to this test as a system property but it was not set");
        File exampleFile = new File(System.getProperty(SIMPLE_YAML_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY));
        File savedFile = new File(System.getProperty("java.io.tmpdir"), "simple"+this.hashCode()+".yaml");
        savedFile.deleteOnExit();
        FileMapper.save(savedFile.getAbsolutePath(), FileMapper.load(exampleFile, SimpleConfigurationLayer.class));

        // print the file to standard output for inspection purpose
        System.out.println("------  Simple YAML configuration  ------");
        System.out.println("Loading from: "+exampleFile.getAbsolutePath());
        System.out.println("-----------------------------------------");
        System.out.println(readFile(exampleFile));
        System.out.println("-----------------------------------------");
        System.out.println("Saving to: "+savedFile.getAbsolutePath());
        System.out.println("-----------------------------------------");
        System.out.println(readFile(savedFile));
        System.out.println("-----------------------------------------");
        System.out.flush();
    }

    @Test
    @DisplayName("Save and Load Medium YAML configuration")
    void saveAndLoadMediumYAMLExample()
        throws Exception {
        assertNotNull(System.getProperty(MEDIUM_YAML_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY), "A configuration file path must be passed to this test as a system property but it was not set");
        File exampleFile = new File(System.getProperty(MEDIUM_YAML_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY));
        File savedFile = new File(System.getProperty("java.io.tmpdir"), "medium"+this.hashCode()+".yaml");
        savedFile.deleteOnExit();
        FileMapper.save(savedFile.getAbsolutePath(), FileMapper.load(exampleFile, SimpleConfigurationLayer.class));

        // print the file to standard output for inspection purpose
        System.out.println("------  Medium YAML configuration  ------");
        System.out.println("Loading from: "+exampleFile.getAbsolutePath());
        System.out.println("-----------------------------------------");
        System.out.println(readFile(exampleFile));
        System.out.println("-----------------------------------------");
        System.out.println("Saving to: "+savedFile.getAbsolutePath());
        System.out.println("-----------------------------------------");
        System.out.println(readFile(savedFile));
        System.out.println("-----------------------------------------");
        System.out.flush();
    }

    @Test
    @DisplayName("Save and Load Extended YAML configuration")
    void saveAndLoadExtendedYAMLExample()
        throws Exception {
        assertNotNull(System.getProperty(EXTENDED_YAML_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY), "A configuration file path must be passed to this test as a system property but it was not set");
        File exampleFile = new File(System.getProperty(EXTENDED_YAML_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY));
        File savedFile = new File(System.getProperty("java.io.tmpdir"), "extended"+this.hashCode()+".yaml");
        savedFile.deleteOnExit();
        FileMapper.save(savedFile.getAbsolutePath(), FileMapper.load(exampleFile, SimpleConfigurationLayer.class));

        // print the file to standard output for inspection purpose
        System.out.println("------ Extended YAML configuration ------");
        System.out.println("Loading from: "+exampleFile.getAbsolutePath());
        System.out.println("-----------------------------------------");
        System.out.println(readFile(exampleFile));
        System.out.println("-----------------------------------------");
        System.out.println("Saving to: "+savedFile.getAbsolutePath());
        System.out.println("-----------------------------------------");
        System.out.println(readFile(savedFile));
        System.out.println("-----------------------------------------");
        System.out.flush();
    }
}