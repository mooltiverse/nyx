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

package configuration

import (
	ent "github.com/mooltiverse/nyx/modules/go/nyx/entities"
	utl "github.com/mooltiverse/nyx/modules/go/utils"
)

var (
	// The Cargo (Rust) version substitution configuration
	// that replaces the version number inside Cargo.toml files within the project.
	CARGO_VERSION = ent.NewSubstitutionWith(utl.PointerToString("**/Cargo.toml"), utl.PointerToString("version(\\s)*=(\\s)*(\"|')?(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:-((?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\\.(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\\+([0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?(\"|')?"), utl.PointerToString("version = \"{{version}}\""))

	// The Composer (PHP) version substitution configuration
	// that replaces the version number inside composer.json files within the project.
	COMPOSER_VERSION = ent.NewSubstitutionWith(utl.PointerToString("**/composer.json"), utl.PointerToString("\"version\"(\\s)*:(\\s)*\"(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:-((?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\\.(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\\+([0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?\""), utl.PointerToString("\"version\": \"{{version}}\""))

	// The Dart (Flutter) version substitution configuration
	// that replaces the version number inside pubspec.yaml files within the project.
	DART_VERSION = ent.NewSubstitutionWith(utl.PointerToString("**/pubspec.yaml"), utl.PointerToString("(\"|')?version(\"|')?(\\s)*:(\\s)*(\"|')?(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:-((?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\\.(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\\+([0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?(\"|')?"), utl.PointerToString("version: \"{{version}}\""))

	// The Elixir version substitution configuration
	// that replaces the version number inside mix.exs files within the project.
	ELIXIR_VERSION = ent.NewSubstitutionWith(utl.PointerToString("**/mix.exs"), utl.PointerToString("version(\\s)*:(\\s)*\"(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:-((?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\\.(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\\+([0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?\""), utl.PointerToString("version: \"{{version}}\""))

	// The Expo (React) version substitution configuration
	// that replaces the version number inside app.json and app.config.json files within the project.
	EXPO_VERSION = ent.NewSubstitutionWith(utl.PointerToString("**/{app,app.config}.json"), utl.PointerToString("\"version\"(\\s)*:(\\s)*\"(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:-((?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\\.(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\\+([0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?\""), utl.PointerToString("\"version\": \"{{version}}\""))

	// The Helm version substitution configuration
	// that replaces the version number inside Chart.yaml files within the project.
	HELM_VERSION = ent.NewSubstitutionWith(utl.PointerToString("**/Chart.yaml"), utl.PointerToString("(\"|')?version(\"|')?(\\s)*:(\\s)*(\"|')?(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:-((?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\\.(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\\+([0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?(\"|')?"), utl.PointerToString("version: \"{{version}}\""))

	// The Node version substitution configuration
	// that replaces the version number inside package.json files within the project.
	NODE_VERSION = ent.NewSubstitutionWith(utl.PointerToString("**/package.json"), utl.PointerToString("\"version\"(\\s)*:(\\s)*\"(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:-((?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\\.(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\\+([0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?\""), utl.PointerToString("\"version\": \"{{version}}\""))

	// The version substitution configuration that replaces the contents of any version.txt file within the project with the version number.
	TEXT_VERSION = ent.NewSubstitutionWith(utl.PointerToString("**/version.txt"), utl.PointerToString("(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:-((?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\\.(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\\+([0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?"), utl.PointerToString("{{version}}"))
)
