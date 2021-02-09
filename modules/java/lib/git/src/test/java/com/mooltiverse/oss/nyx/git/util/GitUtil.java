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
package com.mooltiverse.oss.nyx.git.util;

import java.io.File;
import java.io.OutputStream;
import java.io.PrintStream;

import java.util.Objects;

/**
 * Git utilities used in tests
 */
public class GitUtil {
    /**
     * Default constructor is private on purpose.
     */
    private GitUtil() {
        super();
    }

    /**
     * Prints a formatted separator with the given marker. This separator can be used to split various sections
     * in the output.
     * 
     * @param out the stream to print to
     * @param marker an optional string that can be used as a marker in the separator, to make
     * it easier to jump to one dumo or the other when the output contains a lot of dumps.
     * If <code>null</code> a default marker is printed.
     * @param start prints different characters if this flac is <code>true</code> or <code>false</code>
     * improving readability.
     * 
     * @return
     */
    private static void printSeparator(PrintStream out, String marker, boolean start) {
        String ch = start ? ">" : "<";
        out.print(ch.repeat(10)+" ");
        out.print(Objects.isNull(marker) ? ch.repeat(20) : marker);
        out.print(" "+ch.repeat(10));
        out.println();
    }

    /**
     * Dumps a whole lot of informations about a Git repository in the given directory to the given output stream.
     * 
     * @param directory the repository to dump the informations about
     * @param out the stream to write to
     * @param marker an optional string that can be used as a marker at the beginning and end of the dump, to make
     * it easier to jump to one dumo or the other when the output contains a lot of dumps.
     */
    public static void printRepositoryInfo(File directory, OutputStream out, String marker)
        throws Exception {
        PrintStream ps = new PrintStream(out);
        printSeparator(ps, marker, true);

        printSeparator(ps, "REPOSITORY FOLDER", true);
        FileSystemUtil.renderDirectoryTree(directory, out);
        printSeparator(ps, "REPOSITORY FOLDER", false);

        printSeparator(ps, "REPOSITORY STATUS", true);
        printRepositoryStatus(directory, out);
        printSeparator(ps, "REPOSITORY STATUS", false);

        printSeparator(ps, "REPOSITORY OBJECTS", true);
        printRepositoryObjects(directory, out);
        printSeparator(ps, "REPOSITORY OBJECTS", false);

        printSeparator(ps, "REPOSITORY TAGS", true);
        printRepositoryTags(directory, out);
        printSeparator(ps, "REPOSITORY TAGS", false);

        printSeparator(ps, "REPOSITORY COMMIT HISTORY", true);
        printRepositoryCommitHistory(directory, out);
        printSeparator(ps, "REPOSITORY COMMIT HISTORY", false);

        printSeparator(ps, marker, false);
        ps.flush();
    }

    /**
     * Dumps the repository status.
     * 
     * @param directory the repository to dump the informations about
     * @param out the stream to write to
     */
    public static void printRepositoryStatus(File directory, OutputStream out)
        throws Exception {
        runCommand(new String[]{"git", "status"}, null, directory, out);
    }

    /**
     * Dumps the list of repository objects.
     * 
     * @param directory the repository to dump the informations about
     * @param out the stream to write to
     */
    public static void printRepositoryObjects(File directory, OutputStream out)
        throws Exception {
        runCommand(new String[]{"git", "rev-list", "--objects", "--all"}, null, directory, out);
    }

    /**
     * Dumps the list of repository tags.
     * 
     * @param directory the repository to dump the informations about
     * @param out the stream to write to
     */
    public static void printRepositoryTags(File directory, OutputStream out)
        throws Exception {
        runCommand(new String[]{"git", "tag", "--list", "-n"}, null, directory, out);
    }

    /**
     * Dumps the list of repository commits.
     * 
     * @param directory the repository to dump the informations about
     * @param out the stream to write to
     */
    public static void printRepositoryCommitHistory(File directory, OutputStream out)
        throws Exception {
        runCommand(new String[]{"git", "log", "--decorate=full", "--source"}, null, directory, out);
    }

    /**
     * Runs the command (and options) given in the input array in the given directory and redirecting the output to the given stream.
     * 
     * @param command the command and options as they must appear on the command line, in order. The first element is the command.
     * @param env the environment vaiables to set, or <code>null</code> if no environment variable needs to be set
     * @param directory the directory to run the command in, or <code>null</code> to use the current working directory
     * @param out the output stream to redirect the output to
     */
    private static void runCommand(String[] command, String[] env, File directory, OutputStream out)
        throws Exception {
        Process p = new ProcessBuilder(command).directory(directory).redirectErrorStream(true).start();
        p.waitFor();
        out.write(p.getInputStream().readAllBytes());
        out.flush();
        p.destroy();
    }
}