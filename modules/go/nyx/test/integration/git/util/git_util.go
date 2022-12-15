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
	"io"      // https://pkg.go.dev/io
	"strings" // https://pkg.go.dev/strings
)

/*
Prints a formatted separator with the given marker. This separator can be used to split various sections
in the output.

Arguments are as follows:

  - out the stream to print to
  - marker an optional string that can be used as a marker in the separator, to make
    it easier to jump to one dumo or the other when the output contains a lot of dumps.
    If empty a default marker is printed.
  - start prints different characters if this flag is true or false improving readability.
*/
func PrintSeparator(out io.Writer, marker string, start bool) {
	ch := ""
	if start {
		ch = ">"
	} else {
		ch = "<"
	}

	out.Write([]byte(strings.Repeat(ch, 10) + " "))
	if marker == "" {
		out.Write([]byte(strings.Repeat(ch, 20)))
	} else {
		out.Write([]byte(marker))
	}
	out.Write([]byte(" " + strings.Repeat(ch, 10)))
	out.Write([]byte("\n"))
}

/*
Dumps a whole lot of informations about a Git repository in the given directory to the given output stream.

Arguments are as follows:

  - directory the repository to dump the informations about
  - out the stream to write to
  - marker an optional string that can be used as a marker at the beginning and end of the dump, to make
    it easier to jump to one dumo or the other when the output contains a lot of dumps.
*/
func PrintRepositoryInfo(directory string, out io.Writer, marker string) {
	PrintSeparator(out, marker, true)

	PrintSeparator(out, "REPOSITORY FOLDER", true)
	RenderDirectoryTreeTo(directory, out)
	PrintSeparator(out, "REPOSITORY FOLDER", false)

	PrintSeparator(out, "REPOSITORY STATUS", true)
	PrintRepositoryStatus(directory, out)
	PrintSeparator(out, "REPOSITORY STATUS", false)

	PrintSeparator(out, "REPOSITORY OBJECTS", true)
	PrintRepositoryObjects(directory, out)
	PrintSeparator(out, "REPOSITORY OBJECTS", false)

	PrintSeparator(out, "REPOSITORY TAGS", true)
	PrintRepositoryTags(directory, out)
	PrintSeparator(out, "REPOSITORY TAGS", false)

	PrintSeparator(out, "REPOSITORY COMMIT HISTORY", true)
	PrintRepositoryCommitHistory(directory, out)
	PrintSeparator(out, "REPOSITORY COMMIT HISTORY", false)

	PrintSeparator(out, "REPOSITORY COMMIT GRAPH BY DATE", true)
	PrintRepositoryCommitGraphByDate(directory, out)
	PrintSeparator(out, "REPOSITORY COMMIT GRAPH BY DATE", false)

	PrintSeparator(out, "REPOSITORY COMMIT GRAPH BY TOPOLOGY", true)
	PrintRepositoryCommitGraphByTopology(directory, out)
	PrintSeparator(out, "REPOSITORY COMMIT GRAPH BY TOPOLOGY", false)

	PrintSeparator(out, marker, false)
}

/*
Dumps the repository status.

Arguments are as follows:

- directory the repository to dump the informations about
- out the stream to write to
*/
func PrintRepositoryStatus(directory string, out io.Writer) {
	RunCommand([]string{"git", "status"}, nil, &directory, out)
}

/*
Dumps the list of repository objects.

Arguments are as follows:

- directory the repository to dump the informations about
- out the stream to write to
*/
func PrintRepositoryObjects(directory string, out io.Writer) {
	RunCommand([]string{"git", "rev-list", "--objects", "--all"}, nil, &directory, out)
}

/*
Dumps the list of repository tags.

Arguments are as follows:

- directory the repository to dump the informations about
- out the stream to write to
*/
func PrintRepositoryTags(directory string, out io.Writer) {
	RunCommand([]string{"git", "tag", "--list", "-n"}, nil, &directory, out)
}

/*
Dumps the list of repository commits.

Arguments are as follows:

- directory the repository to dump the informations about
- out the stream to write to
*/
func PrintRepositoryCommitHistory(directory string, out io.Writer) {
	RunCommand([]string{"git", "log", "--decorate=full", "--source"}, nil, &directory, out)
}

/*
Dumps the list of repository commits with a graph sorted by date.

Arguments are as follows:

- directory the repository to dump the informations about
- out the stream to write to
*/
func PrintRepositoryCommitGraphByDate(directory string, out io.Writer) {
	RunCommand([]string{"git", "log", "--all", "--decorate", "--oneline", "--graph", "--date-order"}, nil, &directory, out)
}

/*
Dumps the list of repository commits with a graph sorted by topology.

Arguments are as follows:

- directory the repository to dump the informations about
- out the stream to write to
*/
func PrintRepositoryCommitGraphByTopology(directory string, out io.Writer) {
	RunCommand([]string{"git", "log", "--all", "--decorate", "--oneline", "--graph", "--topo-order"}, nil, &directory, out)
}
