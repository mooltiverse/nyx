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
package com.mooltiverse.oss.nyx.configuration.presets;

import com.mooltiverse.oss.nyx.entities.Substitution;

/**
 * This class provides reusable configuration chunks for substitutions.
 */
public class Substitutions {
    /**
     * The <a href="https://doc.rust-lang.org/cargo/reference/manifest.html">Cargo (Rust)</a> version substitution configuration
     * that replaces the version number inside Cargo.toml files within the project.
     */
    public static final Substitution CARGO_VERSION = new Substitution() {
        {
            // Capture all Cargo.toml files in the project directory in any folder
            setFiles("**/Cargo.toml");
            // Capture any string like version = "SEMANTIC_VERSION", tolerating extra spaces and using single or double quotes for the version value
            setMatch("version(\\s)*=(\\s)*(\"|')?(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:-((?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\\.(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\\+([0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?(\"|')?");
            // Replace with the current version
            setReplace("version = \"{{version}}\"");
        }
    };

    /**
     * The <a href="https://getcomposer.org/doc/01-basic-usage.md">Composer (PHP)</a> version substitution configuration
     * that replaces the version number inside composer.json files within the project.
     */
    public static final Substitution COMPOSER_VERSION = new Substitution() {
        {
            // Capture all composer.json files in the project directory in any folder
            setFiles("**/composer.json");
            // Capture any string like "version": "SEMANTIC_VERSION", tolerating extra spaces
            setMatch("\"version\"(\\s)*:(\\s)*\"(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:-((?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\\.(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\\+([0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?\"");
            // Replace with the current version
            setReplace("\"version\": \"{{version}}\"");
        }
    };

    /**
     * The <a href="https://dart.dev/tools/pub/pubspec">Dart (Flutter)</a> version substitution configuration
     * that replaces the version number inside pubspec.yaml files within the project.
     */
    public static final Substitution DART_VERSION = new Substitution() {
        {
            // Capture all pubspec.yaml files in the project directory in any folder
            setFiles("**/pubspec.yaml");
            // Capture any string like version = "SEMANTIC_VERSION", tolerating extra spaces and using single, double or no quotes for the the attribute name ("version") and the version value
            setMatch("(\"|')?version(\"|')?(\\s)*:(\\s)*(\"|')?(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:-((?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\\.(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\\+([0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?(\"|')?");
            // Replace with the current version
            setReplace("version: \"{{version}}\"");
        }
    };

    /**
     * The <a href="https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html">Elixir</a> version substitution configuration
     * that replaces the version number inside mix.exs files within the project.
     */
    public static final Substitution ELIXIR_VERSION = new Substitution() {
        {
            // Capture all mix.exs files in the project directory in any folder
            setFiles("**/mix.exs");
            // Capture any string like version: "SEMANTIC_VERSION", tolerating extra spaces
            setMatch("version(\\s)*:(\\s)*\"(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:-((?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\\.(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\\+([0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?\"");
            // Replace with the current version
            setReplace("version: \"{{version}}\"");
        }
    };

    /**
     * The <a href="https://docs.expo.dev/versions/latest/config/app/">Expo (React)</a> version substitution configuration
     * that replaces the version number inside app.json and app.config.json files within the project.
     */
    public static final Substitution EXPO_VERSION = new Substitution() {
        {
            // Capture all app.json and app.config.json files in the project directory in any folder
            setFiles("**/{app,app.config}.json");
            // Capture any string like "version": "SEMANTIC_VERSION", tolerating extra spaces
            setMatch("\"version\"(\\s)*:(\\s)*\"(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:-((?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\\.(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\\+([0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?\"");
            // Replace with the current version
            setReplace("\"version\": \"{{version}}\"");
        }
    };

    /**
     * The <a href="https://v2.helm.sh/docs/developing_charts/#the-chart-yaml-file">Helm</a> version substitution configuration
     * that replaces the version number inside Chart.yaml files within the project.
     */
    public static final Substitution HELM_VERSION = new Substitution() {
        {
            // Capture all Chart.yaml files in the project directory in any folder
            setFiles("**/Chart.yaml");
            // Capture any string like version = "SEMANTIC_VERSION", tolerating extra spaces and using single, double or no quotes for the the attribute name ("version") and the version value
            setMatch("(\"|')?version(\"|')?(\\s)*:(\\s)*(\"|')?(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:-((?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\\.(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\\+([0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?(\"|')?");
            // Replace with the current version
            setReplace("version: \"{{version}}\"");
        }
    };

    /**
     * The <a href="https://docs.npmjs.com/cli/v9/configuring-npm/package-json">Node</a> version substitution configuration
     * that replaces the version number inside package.json files within the project.
     */
    public static final Substitution NODE_VERSION = new Substitution() {
        {
            // Capture all package.json files in the project directory in any folder
            setFiles("**/package.json");
            // Capture any string like "version": "SEMANTIC_VERSION", tolerating extra spaces
            setMatch("\"version\"(\\s)*:(\\s)*\"(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:-((?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\\.(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\\+([0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?\"");
            // Replace with the current version
            setReplace("\"version\": \"{{version}}\"");
        }
    };

    /**
     * The version substitution configuration that replaces the contents of any version.txt file within the project with the version number.
     */
    public static final Substitution TEXT_VERSION = new Substitution() {
        {
            // Capture all version.txt files in the project directory in any folder
            setFiles("**/version.txt");
            // Capture anything to replace the whole content
            setMatch("(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:-((?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\\.(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\\+([0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?");
            // Replace with the current version
            setReplace("{{version}}");
        }
    };

    /**
     * Default constructor.
     */
    public Substitutions() {
        super();
    }
}
