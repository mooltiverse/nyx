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
package com.mooltiverse.oss.nyx.version;

/**
 * The values of this enum are used to select the versioning scheme to use.
 */
public enum Scheme {
    /**
     * The Maven versioning scheme.
     * 
     * TODO: uncomment this value as per https://github.com/mooltiverse/nyx/issues/4. As of now this class is just a placeholder.
     */
    //MAVEN,

    /**
     * The <a href="https://semver.org/">Semantic Versioning</a> scheme.
     */
    SEMVER;
}