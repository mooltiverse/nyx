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

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.OutputStream;
import java.nio.file.Files;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

/**
 * File system utilities used in tests
 */
public class FileSystemUtil {
    /**
     * Default constructor is private on purpose.
     */
    private FileSystemUtil() {
        super();
    }

    /**
     * Returns a string with the hierarchical representation of the directory contents
     * 
     * @param directory the directory to render
     * 
     * @return a string representation of the given directory
     */
    public static String renderDirectoryTree(File directory) {
        return renderDirectoryTree(directory, Integer.MAX_VALUE);
    }

    /**
     * Prints the hierarchical representation of the directory contents to the given output stream.
     * The output stream is flushed once written.
     * 
     * @param directory the directory to render
     * @param out the stream to write to
     */
    public static void renderDirectoryTree(File directory, OutputStream out)
        throws Exception {
        renderDirectoryTree(directory, Integer.MAX_VALUE, out);
    }

    /**
     * Prints the hierarchical representation of the directory contents to the given output stream.
     * The output stream is flushed once written.
     * 
     * @param directory the directory to render
     * @param maxDepth the maximum depth of objects to render (starting from 1 with the given directory)
     * @param out the stream to write to
     */
    public static void renderDirectoryTree(File directory, int maxDepth, OutputStream out)
        throws Exception {
        out.write(renderDirectoryTree(directory, maxDepth).getBytes());
        out.flush();
    }

    /**
     * Returns a string with the hierarchical representation of the directory contents.
     * 
     * @param directory the directory to render
     * @param maxDepth the maximum depth of objects to render (starting from 1 with the given directory)
     * 
     * @return a string representation of the given directory
     */
    public static String renderDirectoryTree(File directory, int maxDepth) {
        StringBuilder sb = new StringBuilder();
        sb.append(directory.getAbsolutePath());
        sb.append(System.getProperty("line.separator"));
        if (directory.isDirectory()) {
            File[] items = directory.listFiles();
            // first render directories
            for (int i=0; i<items.length; i++)
                if (items[i].isDirectory()) renderDirectoryItem(sb, "", items[i], maxDepth-1, i==items.length-1);
            // then render files
            for (int i=0; i<items.length; i++)
                if (items[i].isFile()) renderDirectoryItem(sb, "", items[i], maxDepth-1, i==items.length-1);
        }

        return sb.toString();
    }

    /**
     * Returns a string with the hierarchical representation of the directory item (file or directory). This method
     * is recursive and allows to render any level of depth.
     * 
     * @param sb the string builder to append the output to
     * @param lineStart is the initial part of the line
     * @param item the item to render
     * @param maxDepth the maximum depth of objects to render
     * @param last a boolean telling if this is the last item within the parent
     * 
     * @return a string representation of the given item
     */
    private static void renderDirectoryItem(StringBuilder sb, String lineStart, File item, int maxDepth, boolean last) {
        if (maxDepth > 0) {
            lineStart = lineStart + (last ? "   " : "   ");
            sb.append(lineStart);
            sb.append(item.getName());
            if (item.isFile()) {
                sb.append(" (");
                sb.append(item.length());
                sb.append(" bytes)");
            }
            sb.append(System.getProperty("line.separator"));
            if (item.isDirectory()) {
                File[] items = item.listFiles();
                for (int i=0; i<items.length; i++) {
                    renderDirectoryItem(sb, lineStart, items[i], maxDepth-1, i==items.length-1);
                }
            }
        }
    }

    /**
     * Returns all files in the given directory (and its sub directories).
     * 
     * @param directory the directory to list the files from.
     * @param subdirs if {@code true} also include subdirs.
     * 
     * @return the collection of files resulting from the search
     */
    public static Collection<File> getFiles(File directory, boolean subdirs) {
        return getFiles(directory, null, subdirs);
    }

    /**
     * Returns all files in the given directory (and its sub directories).
     * 
     * @param directory the directory to list the files from.
     * @param filters a collection of strings representing filters for file names not to be included.
     * If a file name is equal or {@link String#matches(String) matches} (using a regex) one of these strings
     * it will be skipped. If the collection is {@code null} or empty no filters will be applied.
     * @param subdirs if {@code true} also include subdirs.
     * 
     * @return the collection of files resulting from the search
     */
    public static Collection<File> getFiles(File directory, Collection<String> filters, boolean subdirs) {
        Collection<File> res = new ArrayList<File>();
        for (File f: directory.listFiles()) {
            if (Objects.isNull(filters)) {
                if (subdirs && f.isDirectory())
                    res.addAll(getFiles(f, filters, subdirs));
                else if (f.isFile())
                    res.add(f);
            } else {
                boolean include = true;
                for (String filter: filters) {
                    if (filter.equals(f.getName()) || f.getName().matches(filter)) {
                        include = false;
                        break;
                    }
                }
                if (include) {
                    if (subdirs && f.isDirectory())
                        res.addAll(getFiles(f, filters, subdirs));
                    else if (f.isFile())
                        res.add(f);
                }
            }
        }

        return res;
    }

    /**
     * Creates a new temporary file with a random name in the given directory.
     * 
     * @param dir the directory to create the file in.
     * @param suffix the file name prefix. If {@code null} the file will have a random prefix.
     * @param suffix the file extension. If {@code null} the file will have the {@code .tmp} extension.
     * 
     * @return the file that has been created
     * 
     * @throws Exception in case of any exception
     */
    public static File newTempFile(File dir, String prefix, String extension)
        throws Exception {
        return File.createTempFile(Objects.isNull(prefix) ? RandomUtil.randomAlphabeticString(3) : prefix, extension, dir);
    }

    /**
     * Creates a new temporary directory with a random name in the given directory.
     * 
     * @param dir the parent directory to create the subdirectory in. If {@code null} the default temporary directory is used
     * @param prefix an optional prefix for the directory name. It may be {@code null}
     * 
     * @return the directory that has been created
     * 
     * @throws Exception in case of any exception
     */
    public static File newTempDirectory(File parent, String prefix)
        throws Exception {
        File res = Objects.isNull(parent) ? Files.createTempDirectory(prefix).toFile() : Files.createTempDirectory(parent.toPath(), prefix).toFile();
        res.deleteOnExit();
        return res;
    }

    /**
     * Creates a new directory with a random name in the given directory.
     * 
     * @param dir the directory to create the subdirectory in.
     * 
     * @return the directory that has been created
     * 
     * @throws Exception in case of any exception
     */
    public static File newDirectory(File dir)
        throws Exception {
        File res = new File(dir.getAbsolutePath()+File.pathSeparator+RandomUtil.randomAlphabeticString(5));
        res.mkdirs();
        return res;
    }

    /**
     * Creates a number of new temporary files with both binary and text content in the given directory.
     * Binary files have the {@code .bin} extension and an increasing size depending on how
     * many are created.
     * Text files have the {@code .txt} extension and an increasing size depending on how
     * many are created.
     * 
     * @param dir the directory to create the file in.
     * @param count the number of files to create
     * 
     * @return the set of files that has been created
     * 
     * @throws Exception in case of any exception
     */
    public static Set<File> newTempFilesWithRandomContent(File dir, int count)
        throws Exception {
        Set<File> res = new HashSet<File>(count);
        for (int i=0; i< count; i++) {
            // alternate binary and text files
            res.add(i % 2 == 0 ? newTempFileWithRandomBytes(dir, "file", ".bin", 50*(i+1)) : newTempFileWithRandomText(dir, "file", ".txt", 5*(i+1), 10*(i+1)));
        }
        return res;
    }

    /**
     * Creates a number of new temporary files with text content in the given directory.
     * 
     * @param dir the directory to create the file in.
     * @param count the number of files to create
     * 
     * @return the set of files that has been created
     * 
     * @throws Exception in case of any exception
     */
    public static Set<File> newTempFilesWithRandomText(File dir, int count)
        throws Exception {
        Set<File> res = new HashSet<File>(count);
        for (int i=0; i< count; i++) {
            res.add(newTempFileWithRandomText(dir, "file", ".txt", 5*(i+1), 10*(i+1)));
        }
        return res;
    }

    /**
     * Creates a number of new temporary files with binary content in the given directory.
     * 
     * @param dir the directory to create the file in.
     * @param count the number of files to create
     * 
     * @return the set of files that has been created
     * 
     * @throws Exception in case of any exception
     */
    public static Set<File> newTempFilesWithRandomBytes(File dir, int count)
        throws Exception {
        Set<File> res = new HashSet<File>(count);
        for (int i=0; i< count; i++) {
            res.add(newTempFileWithRandomBytes(dir, "file", ".bin", 50*(i+1)));
        }
        return res;
    }

    /**
     * Creates a new temporaty file with a random name and binary content in the given directory.
     * 
     * @param dir the directory to create the file in.
     * @param suffix the file name prefix. If {@code null} the file will have a random prefix.
     * @param suffix the file extension. If {@code null} the file will have the {@code .tmp} extension.
     * @param length the number of bytes to write to the target file.
     * 
     * @return the file that has been created
     * 
     * @throws Exception in case of any exception
     */
    public static File newTempFileWithRandomBytes(File dir, String prefix, String extension, int length)
        throws Exception {
        File res = newTempFile(dir, prefix, extension);
        writeRandomBytes(res, length);
        return res;
    }

    /**
     * Creates a new temporary file with a random name and text content in the given directory.
     * 
     * @param dir the directory to create the file in.
     * @param suffix the file name prefix. If {@code null} the file will have a random prefix.
     * @param suffix the file extension. If {@code null} the file will have the {@code .tmp} extension.
     * @param lines the number of lines to write to the target file.
     * @param length the length of lines to write to the target file.
     * 
     * @return the file that has been created
     * 
     * @throws Exception in case of any exception
     */
    public static File newTempFileWithRandomText(File dir, String prefix, String extension, int lines, int length)
        throws Exception {
        File res = newTempFile(dir, prefix, extension);
        writeRandomText(res, lines, length);
        return res;
    }

    /**
     * Writes random binary content to the given files.
     * 
     * @param files the files to write to. If they doen't exist yet then they are created.
     * @param length the number of bytes to write to the target files.
     * 
     * @throws Exception in case of any exception
     */
    public static void writeRandomBytes(Collection<File> files, int length)
        throws Exception {
        for (File f: files)
            writeRandomBytes(f, length);
    }

    /**
     * Writes random text content to the given files.
     * 
     * @param files the files to write to. If they doen't exist yet then they are created.
     * @param lines the number of lines to write to the target files.
     * @param length the length of lines to write to the target files.
     * 
     * @throws Exception in case of any exception
     */
    public static void writeRandomText(Collection<File> files, int lines, int length)
        throws Exception {
        for (File f: files)
            writeRandomText(f, lines, length);
    }

    /**
     * Writes random binary content to the given file.
     * 
     * @param file the file to write to. If it doesn't exist yet then it is created.
     * @param length the number of bytes to write to the target file.
     * 
     * @throws Exception in case of any exception
     */
    public static void writeRandomBytes(File file, int length)
        throws Exception {
        if (!file.exists())
            file.createNewFile();
        
        FileOutputStream writer = new FileOutputStream(file);
        writer.write(RandomUtil.randomBytes(length));
        writer.flush();
        writer.close();
    }

    /**
     * Writes random text content to the given file.
     * 
     * @param file the file to write to. If it doesn't exist yet then it is created.
     * @param lines the number of lines to write to the target file.
     * @param length the length of lines to write to the target file.
     * 
     * @throws Exception in case of any exception
     */
    public static void writeRandomText(File file, int lines, int length)
        throws Exception {
        String[] contentLines = new String[lines];
        for (int i=0; i<lines; i++)
            contentLines[i] = RandomUtil.randomAlphabeticString(length);
        appendLines(file, contentLines);
    }

    /**
     * Appends the given lines to the end of the given file.
     * 
     * @param file the file to write to. If it doesn't exist yet then it is created.
     * @param lines the text lines to append 
     * 
     * @throws Exception in case of any exception
     */
    public static void appendLines(File file, String... lines)
        throws Exception {
        if (!file.exists())
            file.createNewFile();

        FileWriter fw = new FileWriter(file, true);
        BufferedWriter bw = new BufferedWriter(fw);
        for (String line: lines) {
            bw.write(line);
            bw.newLine();
        }
        bw.flush();
        bw.close();
    }
}