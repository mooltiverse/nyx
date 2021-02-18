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
package com.mooltiverse.oss.nyx.core;

import java.io.File;
import java.io.IOException;

import java.util.Objects;

//import com.mooltiverse.oss.nyx.version.SemanticVersion;
import com.mooltiverse.oss.nyx.version.Version;

import org.eclipse.jgit.lib.Repository;
import org.eclipse.jgit.internal.storage.file.FileRepository;

/**
 * The Nyx main class.
 */
public class Nyx {
    /**
     * The Git repository instance.
     */
    private final Repository repository;

    /**
     * Default constructor hidden on purpose.
     */
    private Nyx() {
        super();
        repository = null;
    }

    /**
     * Create an instance of Nyx using the given workspace directory.
     * 
     * @param workspace the directory containing the current Git workspace. The workspace must already exist
     * in the workspace directory.
     * 
     * @throws NullPointerException if the given workspace directory is <code>null</code>
     * @throws IllegalArgumentException if the given workspace directory does not exists or does not
     * contain a valid Git workspace
     * @throws IOException if some other IO-related exception occurs
     */
    public Nyx(File workspace) 
        throws IOException {
        super();
        repository = new FileRepository(workspace);
    }

    /**
     * Create an instance of Nyx using the given workspace directory.
     * 
     * @param workspace the directory containing the current Git workspace. The workspace must already exist
     * in the workspace directory.
     * 
     * @throws NullPointerException if the given workspace directory is <code>null</code>
     * @throws IllegalArgumentException if the given workspace directory does not exists or does not
     * contain a valid Git workspace
     * @throws IOException if some other IO-related exception occurs
     */
    public Nyx(String workspace)
        throws IOException {
        super();
        repository = new FileRepository(workspace);
    }

    /**
     * Returns the version that was previously released by the current repository.
     * 
     * @return the version that was previously released by the current repository.
     * 
     * @throws IllegalStateException if the repository was not initialized or no previous
     * version could be detected.
     */
    public Version getPreviousVersion() {
return null;
    }

    /**
     * Returns the version that was previously released by the current repository. This is a safer
     * shorthand for {@link #getPreviousVersion()} as when no previous version could be detected
     * a default one is returned instead of throwing an exception.
     * 
     * @param defaultVersion the string representation to return in case no previous version could
     * be detected.
     * 
     * @return the version that was previously released by the current repository, which might be
     * the same as the given default version if none could be detected.
     * 
     * @throws NullPointerException if the given default version is <code>null</code>
     * @throws IllegalArgumentException if the given default version is not allowed as a value for
     * the version type <code>V</code>
     */
    public Version getPreviousVersion(String defaultVersion) {
        Objects.requireNonNull(defaultVersion, "Default version can't be null");
        if (defaultVersion.isBlank())
            throw new IllegalArgumentException("Default version cannot be empty");


return null;
    }
}