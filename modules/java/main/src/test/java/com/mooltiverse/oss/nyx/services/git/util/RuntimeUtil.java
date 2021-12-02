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
package com.mooltiverse.oss.nyx.services.git.util;

import java.io.File;
import java.io.OutputStream;

/**
 * Runtime utilities used in tests
 */
public class RuntimeUtil {
    /**
     * Default constructor is private on purpose.
     */
    private RuntimeUtil() {
        super();
    }

    /**
     * Runs the command (and options) given in the input array in the given directory and redirecting the output to the given stream.
     * 
     * @param command the command and options as they must appear on the command line, in order. The first element is the command.
     * @param env the environment vaiables to set, or {@code null} if no environment variable needs to be set
     * @param directory the directory to run the command in, or {@code null} to use the current working directory
     * @param out the output stream to redirect the output to
     */
    public static void runCommand(String[] command, String[] env, File directory, OutputStream out)
        throws Exception {
        Process p = new ProcessBuilder(command).directory(directory).redirectErrorStream(true).start();
        p.waitFor();
        out.write(p.getInputStream().readAllBytes());
        out.flush();
        p.destroy();
    }
}