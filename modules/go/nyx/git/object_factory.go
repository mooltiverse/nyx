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

package git

import (
	"bufio"   // https://pkg.go.dev/bufio
	"strings" // https://pkg.go.dev/strings

	ggit "github.com/go-git/go-git/v5"                       // https://pkg.go.dev/github.com/go-git/go-git/v5
	ggitplumbing "github.com/go-git/go-git/v5/plumbing"      // https://pkg.go.dev/github.com/go-git/go-git/v5
	ggitobject "github.com/go-git/go-git/v5/plumbing/object" // https://pkg.go.dev/github.com/go-git/go-git/v5

	gitent "github.com/mooltiverse/nyx/modules/go/nyx/entities/git"
)

/*
Returns the new value object from the given git reference.

Arguments are as follows:

- signature the git reference to get the data from.
*/
func ActionFrom(signature ggitobject.Signature) gitent.Action {
	return gitent.Action{Identity: IdentityFrom(signature), TimeStamp: TimeStampFrom(signature)}
}

/*
Returns the new value object from the given git reference.

Arguments are as follows:

- commit the git reference to get the data from.
- tags the set of tags to this commit
*/
func CommitFrom(commit ggitobject.Commit, tags []gitent.Tag) gitent.Commit {
	parents := make([]string, len(commit.ParentHashes))
	for i, parent := range commit.ParentHashes {
		parents[i] = parent.String()
	}
	return gitent.Commit{Sha: commit.ID().String(), AuthorAction: ActionFrom(commit.Author), CommitAction: ActionFrom(commit.Committer), Date: commit.Committer.When.UnixMilli(), Message: MessageFrom(commit), Parents: parents, Tags: tags}
}

/*
Returns the new value object from the given git reference.

Arguments are as follows:

- signature the git reference to get the data from.
*/
func IdentityFrom(signature ggitobject.Signature) gitent.Identity {
	return gitent.Identity{Name: signature.Name, Email: signature.Email}
}

/*
Returns the new value object from the given git reference.

Arguments are as follows:

- commit the git reference to get the data from.
*/
func MessageFrom(commit ggitobject.Commit) gitent.Message {
	return messageFromString(commit.Message)
}

/*
Returns the new value object from the given string.
The given message can be multi-line and its fields will be parsed according to Git specifications.

Arguments are as follows:

- message the git message.
*/
func messageFromString(message string) gitent.Message {
	var shortMessage string
	footers := make(map[string]string)
	footersAllowed := false // this becomes true after a blank line is met
	// read the message line by line
	// we only parse the header and trailers as the body remains in the full message
	// trailers need to be separated by the body with at least one blank like
	scanner := bufio.NewScanner(strings.NewReader(message))
	for i := 0; scanner.Scan(); i++ {
		line := scanner.Text()
		if i == 0 {
			// it's the header
			shortMessage = line
		} else {
			if strings.Trim(line, " ") == "" {
				footersAllowed = true
			} else if footersAllowed && strings.Contains(line, ": ") {
				// we consider them as trailers if they have a separator like ": " between the name and the value
				nameAndValue := strings.SplitN(line, ": ", 2)
				footers[nameAndValue[0]] = nameAndValue[1]
			}
		}
	}

	return gitent.Message{ShortMessage: shortMessage, FullMessage: message, Footers: footers}
}

/*
Returns the new value object from the given git reference.

Arguments are as follows:

- repository the pointer to the repository. This is used to disambiguate between annotated and lightweight tags
- ref the git reference to get the data from.
*/
func TagFrom(repository *ggit.Repository, ref ggitplumbing.Reference) gitent.Tag {
	var annotated bool
	var target string
	annotatedTag, err := repository.TagObject(ref.Hash())
	if err == nil {
		// it's an annotated tag, annotatedTag is valid
		annotated = true
		target = annotatedTag.Target.String()
	} else {
		// it's a lightweight tag, annotatedTag is not valid
		annotated = false
		target = ref.Hash().String()
	}

	// also strip the leading "refs/tags/" from the tag name
	return gitent.Tag{Name: strings.Replace(string(ref.Name()), "refs/tags/", "", 1), Target: target, Annotated: annotated}
}

/*
Returns the new value object from the given git reference.

Arguments are as follows:

- signature the git reference to get the data from.
*/
func TimeStampFrom(signature ggitobject.Signature) gitent.TimeStamp {
	return gitent.TimeStamp{signature.When.UnixMilli(), signature.When.Location()}
}
