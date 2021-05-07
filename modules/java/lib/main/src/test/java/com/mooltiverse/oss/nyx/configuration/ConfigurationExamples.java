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

import java.io.File;
import java.io.FileReader;
import java.io.StringWriter;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import com.mooltiverse.oss.nyx.data.FileMapper;
import com.mooltiverse.oss.nyx.configuration.examples.ExtendedConfigurationExample;
import com.mooltiverse.oss.nyx.configuration.examples.MediumConfigurationExample;
import com.mooltiverse.oss.nyx.configuration.examples.SimpleConfigurationExample;
import com.mooltiverse.oss.nyx.configuration.examples.SimplestConfigurationExample;

/**
 * These are not actual tests but rather examples printed to the standard output for a few configuration files.
 */
@DisplayName("ConfigurationExamples")
public class ConfigurationExamples {
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
    @DisplayName("Simplest YAML configuration")
    void simplestYAMLExample()
        throws Exception {
        File savedFile = new File(System.getProperty("java.io.tmpdir"), "simplest"+this.hashCode()+".yaml");
        savedFile.deleteOnExit();
        FileMapper.save(savedFile.getAbsolutePath(), new SimplestConfigurationExample());

        // print the file to standard output for inspection purpose
        System.out.println("------ Simplest YAML configuration ------");
        System.out.println(readFile(savedFile));
        System.out.println("-----------------------------------------");
        System.out.flush();
    }

    @Test
    @DisplayName("Simple YAML configuration")
    void simpleYAMLExample()
        throws Exception {
        File savedFile = new File(System.getProperty("java.io.tmpdir"), "simple"+this.hashCode()+".yaml");
        savedFile.deleteOnExit();
        FileMapper.save(savedFile.getAbsolutePath(), new SimpleConfigurationExample());

        // print the file to standard output for inspection purpose
        System.out.println("------  Simple YAML configuration  ------");
        System.out.println(readFile(savedFile));
        System.out.println("-----------------------------------------");
        System.out.flush();
    }

    @Test
    @DisplayName("Medium YAML configuration")
    void mediumYAMLExample()
        throws Exception {
        File savedFile = new File(System.getProperty("java.io.tmpdir"), "medium"+this.hashCode()+".yaml");
        savedFile.deleteOnExit();
        FileMapper.save(savedFile.getAbsolutePath(), new MediumConfigurationExample());

        // print the file to standard output for inspection purpose
        System.out.println("------  Medium YAML configuration  ------");
        System.out.println(readFile(savedFile));
        System.out.println("-----------------------------------------");
        System.out.flush();
    }

    @Test
    @DisplayName("Extended YAML configuration")
    void extendedYAMLExample()
        throws Exception {
        File savedFile = new File(System.getProperty("java.io.tmpdir"), "extended"+this.hashCode()+".yaml");
        savedFile.deleteOnExit();
        FileMapper.save(savedFile.getAbsolutePath(), new ExtendedConfigurationExample());

        // print the file to standard output for inspection purpose
        System.out.println("------ Extended YAML configuration ------");
        System.out.println(readFile(savedFile));
        System.out.println("-----------------------------------------");
        System.out.flush();
    }

    @Test
    @DisplayName("Simplest JSON configuration")
    void simplestJSONExample()
        throws Exception {
        File savedFile = new File(System.getProperty("java.io.tmpdir"), "simplest"+this.hashCode()+".json");
        savedFile.deleteOnExit();
        FileMapper.save(savedFile.getAbsolutePath(), new SimplestConfigurationExample());

        // print the file to standard output for inspection purpose
        System.out.println("------ Simplest JSON configuration ------");
        System.out.println(readFile(savedFile));
        System.out.println("-----------------------------------------");
        System.out.flush();
    }

    @Test
    @DisplayName("Simple JSON configuration")
    void simpleJSONExample()
        throws Exception {
        File savedFile = new File(System.getProperty("java.io.tmpdir"), "simple"+this.hashCode()+".json");
        savedFile.deleteOnExit();
        FileMapper.save(savedFile.getAbsolutePath(), new SimpleConfigurationExample());

        // print the file to standard output for inspection purpose
        System.out.println("------  Simple JSON configuration  ------");
        System.out.println(readFile(savedFile));
        System.out.println("-----------------------------------------");
        System.out.flush();
    }

    @Test
    @DisplayName("Medium JSON configuration")
    void mediumJSONExample()
        throws Exception {
        File savedFile = new File(System.getProperty("java.io.tmpdir"), "medium"+this.hashCode()+".json");
        savedFile.deleteOnExit();
        FileMapper.save(savedFile.getAbsolutePath(), new MediumConfigurationExample());

        // print the file to standard output for inspection purpose
        System.out.println("------  Medium JSON configuration  ------");
        System.out.println(readFile(savedFile));
        System.out.println("-----------------------------------------");
        System.out.flush();
    }

    @Test
    @DisplayName("Extended JSON configuration")
    void extendedJSONExample()
        throws Exception {
        File savedFile = new File(System.getProperty("java.io.tmpdir"), "extended"+this.hashCode()+".json");
        savedFile.deleteOnExit();
        FileMapper.save(savedFile.getAbsolutePath(), new ExtendedConfigurationExample());

        // print the file to standard output for inspection purpose
        System.out.println("------ Extended JSON configuration ------");
        System.out.println(readFile(savedFile));
        System.out.println("-----------------------------------------");
        System.out.flush();
    }
}