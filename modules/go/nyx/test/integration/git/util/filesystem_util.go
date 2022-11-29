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

package util

import (
	"io"            // https://pkg.go.dev/io
	"io/fs"         // https://pkg.go.dev/fs
	"math"          // https://pkg.go.dev/math
	"os"            // https://pkg.go.dev/os
	"path/filepath" // https://pkg.go.dev/path/filepath
	"strconv"       // https://pkg.go.dev/strconv
	"strings"       // https://pkg.go.dev/strings

	regexp2 "github.com/dlclark/regexp2" // https://pkg.go.dev/github.com/dlclark/regexp2, we need to use this instead of the standard 'regexp' to have support for lookarounds (look ahead), even if this implementation is a little slower
)

/*
Returns a string with the hierarchical representation of the directory contents

Arguments are as follows:

- directory the directory to render
*/
func RenderDirectoryTree(directory string) string {
	return RenderDirectoryTreeWithDepth(directory, math.MaxInt)
}

/*
Prints the hierarchical representation of the directory contents to the given output stream.

Arguments are as follows:

- directory the directory to render
- out the stream to write to
*/
func RenderDirectoryTreeTo(directory string, out io.Writer) {
	RenderDirectoryTreeWithDepthTo(directory, math.MaxInt, out)
}

/*
Prints the hierarchical representation of the directory contents to the given output stream.

Arguments are as follows:

- directory the directory to render
- maxDepth the maximum depth of objects to render (starting from 1 with the given directory)
- out the stream to write to
*/
func RenderDirectoryTreeWithDepthTo(directory string, maxDepth int, out io.Writer) {
	out.Write([]byte(RenderDirectoryTreeWithDepth(directory, maxDepth)))
}

/*
Returns a string with the hierarchical representation of the directory contents.

Arguments are as follows:

  - directory the directory to render
    maxDepth the maximum depth of objects to render (starting from 1 with the given directory)
*/
func RenderDirectoryTreeWithDepth(directory string, maxDepth int) string {
	var sb strings.Builder
	absolutePath, err := filepath.Abs(directory)
	if err != nil {
		panic(err)
	}
	sb.WriteString(absolutePath)
	sb.WriteString("\n")

	file, err := os.Open(absolutePath)
	if err != nil {
		panic(err)
	}
	fileInfo, err := file.Stat()
	if err != nil {
		panic(err)
	}
	if fileInfo.IsDir() {
		children, err := os.ReadDir(absolutePath)
		if err != nil {
			panic(err)
		}
		// first render directories
		for i, child := range children {
			if child.IsDir() {
				renderDirectoryItem(&sb, "", child, filepath.Join(absolutePath, child.Name()), maxDepth-1, i == len(children)-1)
			}
		}
		// then render files
		for i, child := range children {
			if !child.IsDir() {
				renderDirectoryItem(&sb, "", child, filepath.Join(absolutePath, child.Name()), maxDepth-1, i == len(children)-1)
			}
		}
	}

	return sb.String()
}

/*
Returns a string with the hierarchical representation of the directory item (file or directory). This method
is recursive and allows to render any level of depth.

Arguments are as follows:

- sb the string builder to append the output to
- lineStart is the initial part of the line
- item the item to render
- itemPath the full path of the item to render
- maxDepth the maximum depth of objects to render
- last a boolean telling if this is the last item within the parent
*/
func renderDirectoryItem(sb *strings.Builder, lineStart string, item fs.DirEntry, itemPath string, maxDepth int, last bool) {
	if maxDepth > 0 {
		lineStart := lineStart + "   "

		sb.WriteString(lineStart)
		sb.WriteString(item.Name())

		if !item.IsDir() {
			sb.WriteString(" (")
			itemInfo, err := item.Info()
			if err != nil {
				panic(err)
			}
			sb.WriteString(strconv.FormatInt(itemInfo.Size(), 10))
			sb.WriteString(" bytes)")
		}
		sb.WriteString("\n")
		if item.IsDir() {
			children, err := os.ReadDir(itemPath)
			if err != nil {
				panic(err)
			}
			for i, child := range children {
				renderDirectoryItem(sb, lineStart, child, filepath.Join(itemPath, child.Name()), maxDepth-1, i == len(children)-1)
			}
		}
	}
}

/*
Returns all files in the given directory (and its sub directories).

Arguments are as follows:

- directory the directory to list the files from.
- subdirs if true also include subdirs.
*/
func GetFiles(directory string, subdirs bool) []string {
	return GetFilesFiltered(directory, nil, subdirs)
}

/*
Returns all files in the given directory (and its sub directories).

Arguments are as follows:

  - directory the directory to list the files from.
  - filters a collection of strings representing filters for file names not to be included.
    If a file name is equal or matches (using a regex) one of these strings
    it will be skipped. If the collection is nil or empty no filters will be applied.
  - subdirs if true also include subdirs.
*/
func GetFilesFiltered(directory string, filters []string, subdirs bool) []string {
	res := []string{}
	children, err := os.ReadDir(directory)
	if err != nil {
		panic(err)
	}
	for _, f := range children {
		if filters == nil || len(filters) == 0 {
			if subdirs && f.IsDir() {
				res = append(res, GetFilesFiltered(filepath.Join(directory, f.Name()), filters, subdirs)...)
			} else {
				res = append(res, f.Name())
			}
		} else {
			include := true
			for _, filter := range filters {

				if filter == f.Name() {
					include = false
					break
				} else {
					re := regexp2.MustCompile(filter, 0)
					if isMatch, _ := re.MatchString(f.Name()); isMatch {
						include = false
						break
					}
				}
			}
			if include {
				if subdirs && f.IsDir() {
					res = append(res, GetFilesFiltered(filepath.Join(directory, f.Name()), filters, subdirs)...)
				} else {
					res = append(res, f.Name())
				}
			}
		}
	}

	return res
}

/*
Creates a new temporary file with a random name in the given directory.

Arguments are as follows:

- dir the directory to create the file in.
- prefix the file name prefix. If nil the file will have a random prefix.
*/
func NewTempFile(dir string, prefix *string) string {
	var prf string
	if prefix == nil {
		prf = RandomAlphabeticString(3, 11)
	} else {
		prf = *prefix
	}

	f, err := os.CreateTemp(dir, prf)
	if err != nil {
		panic(err)
	}
	defer os.Remove(f.Name())
	return f.Name()
}

/*
Creates a new temporary directory with a random name in the given directory.

Arguments are as follows:

- dir the parent directory to create the subdirectory in. If empty the default temporary directory is used
- prefix an optional prefix for the directory name. It may be nil
*/
func NewTempDirectory(parent string, prefix *string) string {
	prf := ""
	if prefix != nil {
		prf = *prefix
	}
	dir, err := os.MkdirTemp(parent, prf)
	if err != nil {
		panic(err)
	}

	return dir
}

/*
Creates a new directory with a random name in the given directory.

Arguments are as follows:

- dir the directory to create the subdirectory in.
*/
func NewDirectory(dir string) string {
	dirName := filepath.Join(dir, RandomAlphabeticString(5, 13))
	err := os.MkdirAll(dirName, 0750)
	if err != nil {
		panic(err)
	}
	return dirName
}

/*
Creates a number of new temporary files with both binary and text content in the given directory.

Arguments are as follows:

- dir the directory to create the file in.
- count the number of files to create
*/
func NewTempFilesWithRandomContent(dir string, count int) []string {
	res := make([]string, count)
	prefix := "file"
	for i := 0; i < count; i++ {
		// alternate binary and text files
		if i%2 == 0 {
			res[i] = NewTempFileWithRandomBytes(dir, &prefix, 50*(i+1))
		} else {
			res[i] = NewTempFileWithRandomText(dir, &prefix, 5*(i+1), 10*(i+1))
		}
	}
	return res
}

/*
Creates a number of new temporary files with text content in the given directory.

Arguments are as follows:

- dir the directory to create the file in.
- count the number of files to create
*/
func NewTempFilesWithRandomText(dir string, count int) []string {
	res := make([]string, count)
	prefix := "file"
	for i := 0; i < count; i++ {
		res[i] = NewTempFileWithRandomText(dir, &prefix, 5*(i+1), 10*(i+1))
	}
	return res
}

/*
Creates a number of new temporary files with binary content in the given directory.

Arguments are as follows:

- dir the directory to create the file in.
- count the number of files to create
*/
func NewTempFilesWithRandomBytes(dir string, count int) []string {
	res := make([]string, count)
	prefix := "file"
	for i := 0; i < count; i++ {
		res[i] = NewTempFileWithRandomBytes(dir, &prefix, 50*(i+1))
	}
	return res
}

/*
Creates a new temporaty file with a random name and binary content in the given directory.

Arguments are as follows:

- dir the directory to create the file in.
- suffix the file name prefix. If nil the file will have a random prefix.
- length the number of bytes to write to the target file.
*/
func NewTempFileWithRandomBytes(dir string, prefix *string, length int) string {
	res := NewTempFile(dir, prefix)
	WriteRandomBytesToFile(res, length)
	return res
}

/*
Creates a new temporary file with a random name and text content in the given directory.

Arguments are as follows:

- dir the directory to create the file in.
- suffix the file name prefix. If nil the file will have a random prefix.
- lines the number of lines to write to the target file.
- length the length of lines to write to the target file.
*/
func NewTempFileWithRandomText(dir string, prefix *string, lines int, length int) string {
	res := NewTempFile(dir, prefix)
	WriteRandomTextToFile(res, lines, length)
	return res
}

/*
Writes random binary content to the given files.

Arguments are as follows:

- files the files to write to. If they doen't exist yet then they are created.
- length the number of bytes to write to the target files.
*/
func WriteRandomBytesToFiles(files []string, length int) {
	for _, f := range files {
		WriteRandomBytesToFile(f, length)
	}
}

/*
Writes random text content to the given files.

Arguments are as follows:

- files the files to write to. If they doen't exist yet then they are created.
- lines the number of lines to write to the target files.
- length the length of lines to write to the target files.
*/
func WriteRandomTextToFiles(files []string, lines int, length int) {
	for _, f := range files {
		WriteRandomTextToFile(f, lines, length)
	}
}

/*
Writes random binary content to the given file.

Arguments are as follows:

- file the file to write to. If it doesn't exist yet then it is created.
- length the number of bytes to write to the target file.
*/
func WriteRandomBytesToFile(file string, length int) {
	f, err := os.OpenFile(file, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0644)
	if err != nil {
		panic(err)
	}
	_, err = f.Write(RandomBytes(length, 21))
	if err != nil {
		panic(err)
	}

	if err := f.Close(); err != nil {
		panic(err)
	}
}

/*
Writes random text content to the given file.

Arguments are as follows:

- file the file to write to. If it doesn't exist yet then it is created.
- lines the number of lines to write to the target file.
- length the length of lines to write to the target file.
*/
func WriteRandomTextToFile(file string, lines int, length int) {
	contentLines := make([]string, lines)
	for i := 0; i < lines; i++ {
		contentLines[i] = RandomAlphabeticString(length, 17)
	}
	AppendLines(file, contentLines...)
}

/*
Appends the given lines to the end of the given file.

Arguments are as follows:

- file the file to write to. If it doesn't exist yet then it is created.
- lines the text lines to append
*/
func AppendLines(file string, lines ...string) {
	f, err := os.OpenFile(file, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0644)
	if err != nil {
		panic(err)
	}
	for _, line := range lines {
		_, err := f.Write([]byte(line + "\n"))
		if err != nil {
			panic(err)
		}
	}
	if err := f.Close(); err != nil {
		panic(err)
	}
}
