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
package com.mooltiverse.oss.nyx.git.tools;

import java.io.File;
import java.util.Objects;

import com.mooltiverse.oss.nyx.git.util.FileSystemUtil;
import com.mooltiverse.oss.nyx.git.util.RandomUtil;

/**
 * An utility class built on top of a test repository and providing some coarse grained commands to be used fluently.
 */
public class Script extends Workbench {
    /**
     * Creates a new script instance in the given directory.
     * 
     * @param directory the directory to create the repository in
     * 
     * @throws Exception in case of any issue
     */
    public Script(File directory)
        throws Exception {
        super(directory);
    }

    /**
     * Creates a Git script on a new repository.
     * 
     * @param directory the directory to create the repository in. It can't be {@code null}.
     * @param bare if {@code true} the repository is initialized as a bare repository, otherwise it will have a work tree.
     * If {@code initialize} is {@code false} this parameter is ignored.
     * @param initialize if {@code true} the repository has to be initialized, otherwise {@code false}.
     * 
     * @throws Exception in case of any exception
     */
    public Script(File directory, boolean bare, boolean initialize)
        throws Exception {
        super(directory, bare, initialize);
    }

    /**
     * Returns a new script instance for a repository cloned from the given URI in a new temporary directory.
     * No credendials are used for cloning.
     * 
     * @param uri the URI to clone from
     * 
     * @return the new instance
     * 
     * @throws Exception in case of any issue
     * 
     * @see #getWorkingDirectory() to get the repository directory
     */
    public static Script cloneFrom(String uri)
        throws Exception {
        return cloneFrom(uri, null, null);
    }

    /**
     * Returns a new script instance for a repository cloned from the given URI in a new temporary directory.
     * 
     * @param uri the URI to clone from
     * @param user the optional user name to use when credentials are required.
     * @param password the optional password to use when credentials are required.
     * 
     * @return the new instance
     * 
     * @throws Exception in case of any issue
     * 
     * @see #getWorkingDirectory() to get the repository directory
     */
    public static Script cloneFrom(String uri, String user, String password)
        throws Exception {
        File directory = FileSystemUtil.newTempDirectory(null, "nyx-test-script-");
        return cloneFrom(uri, directory, user, password);
    }

    /**
     * Returns a new script instance for a repository cloned from the given URI in the given directory.
     * No credendials are used for cloning.
     * 
     * @param uri the URI to clone from
     * @param directory the directory to clone to. It must exist and be empty
     * 
     * @return the new instance
     * 
     * @throws Exception in case of any issue
     * 
     * @see #getWorkingDirectory() to get the repository directory
     */
    public static Script cloneFrom(String uri, File directory)
        throws Exception {
        return cloneFrom(uri, directory, null, null);
    }

    /**
     * Returns a new script instance for a repository cloned from the given URI in the given directory.
     * 
     * @param uri the URI to clone from
     * @param directory the directory to clone to. It must exist and be empty
     * @param user the optional user name to use when credentials are required.
     * @param password the optional password to use when credentials are required.
     * 
     * @return the new instance
     * 
     * @throws Exception in case of any issue
     * 
     * @see #getWorkingDirectory() to get the repository directory
     */
    public static Script cloneFrom(String uri, File directory, String user, String password)
        throws Exception {
        cloneInto(uri, directory, user, password);
        return new Script(directory);
    }

    /**
     * Returns a new script instance for a new bare repository to be created and initialized in the given directory
     * 
     * @param directory the working directory for the script. It must exist and be empty
     * 
     * @return the new instance
     * 
     * @throws Exception in case of any issue
     */
    public static Script bare(File directory)
        throws Exception {
        return new Script(directory, true, true);
    }

    /**
     * Returns a new script instance for a new bare repository to be created and initialized in a new temporary directory
     * 
     * @return the new instance
     * 
     * @throws Exception in case of any issue
     * 
     * @see #getWorkingDirectory() to get the repository directory
     */
    public static Script bare()
        throws Exception {
        return new Script(FileSystemUtil.newTempDirectory(null, "nyx-test-script-"), true, true);
    }

    /**
     * Returns a new script instance working in the given directory
     * 
     * @param directory the working directory for the script. It must exist and already have a Git repository in it
     * 
     * @return the new instance
     * 
     * @throws Exception in case of any issue
     */
    public static Script from(File directory)
        throws Exception {
        return new Script(directory);
    }

    /**
     * Returns a new script instance for a new repository to be created and initialized in the given directory
     * 
     * @param directory the working directory for the script. It must exist and be empty
     * 
     * @return the new instance
     * 
     * @throws Exception in case of any issue
     */
    public static Script fromScratch(File directory)
        throws Exception {
        return new Script(directory, false, true);
    }

    /**
     * Returns a new script instance for a new repository to be created and initialized in a new temporary directory
     * 
     * @return the new instance
     * 
     * @throws Exception in case of any issue
     * 
     * @see #getWorkingDirectory() to get the repository directory
     */
    public static Script fromScratch()
        throws Exception {
        return new Script(FileSystemUtil.newTempDirectory(null, "nyx-test-script-"), false, true);
    }

    /**
     * Adds some files (one) to the repository. Files have content but they are not staged or committed.
     * 
     * @return this same instance
     * 
     * @throws Exception in case of any issue
     */
    public Script andAddFiles()
        throws Exception {
        return andAddFiles(1);
    }

    /**
     * Adds the given number of files to the repository. Files have content but they are not staged or committed.
     * 
     * @param count the number of files to add
     * 
     * @return this same instance
     * 
     * @throws Exception in case of any issue
     */
    public Script andAddFiles(int count)
        throws Exception {
        addRandomTextWorkbenchFiles(count);
        return this;
    }

    /**
     * Changes the contents of all files in the repository. Changes are not staged or committed.
     * 
     * @return this same instance
     * 
     * @throws Exception in case of any issue
     */
    public Script andUpdateFiles()
        throws Exception {
        updateAllWorkbenchFiles();
        return this;
    }

    /**
     * Adds the modified contents to the sraging area.
     * 
     * @return this same instance
     * 
     * @throws Exception in case of any issue
     */
    public Script andStage()
        throws Exception {
        stage();
        return this;
    }

    /**
     * Commits the staged changes with a random commit message
     * 
     * @return this same instance
     * 
     * @throws Exception in case of any issue
     */
    public Script andCommit()
        throws Exception {
        return andCommit(null);
    }

    /**
     * Commits the staged changes with the given commit message
     * 
     * @param message the commit message. If {@code null} a random commit message is generated
     * 
     * @return this same instance
     * 
     * @throws Exception in case of any issue
     */
    public Script andCommit(String message)
        throws Exception {
        commit(Objects.isNull(message) ? "Commit ".concat(RandomUtil.randomAlphabeticString(3)) : message);
        return this;
    }

    /**
     * Tags the latest commit with the given name. The tag is a lightweight tag unless the given message is not {@code null},
     * in which case the message is used for the annotation.
     * 
     * @param name the tag value
     * @param message the optional tag message, if {@code null} no message is applied and the tag is lightweight,
     * otherwise it's an annotated tag
     * 
     * @return this same instance
     * 
     * @throws Exception in case of any issue
     */
    public Script andTag(String name, String message)
        throws Exception {
        tag(name, message, getLastCommit());
        return this;
    }

    /**
     * Moves to the given branch and creates it if it doesn't exist.
     * 
     * @param name the name of the branch to checkout
     * 
     * @return this same instance
     * 
     * @throws Exception in case of any issue
     */
    public Script inBranch(String name)
        throws Exception {
        checkout(name);
        return this;
    }

    /**
     * Adds a batch of operations to the git repository in the current branch.
     * A batch is made of change to all the files in the repository (new files are added if there is none), a commit
     * and a lightweight tag on the commit.
     * 
     * @param tagName the tag name to create on the commit
     * 
     * @return this same instance
     * 
     * @throws Exception in case of any issue
     */
    public Script andCommitWithTag(String tagName)
        throws Exception {
        return andCommitWithTag(tagName, null);
    }

    /**
     * Adds a batch of operations to the git repository in the current branch.
     * A batch is made of change to all the files in the repository (new files are added if there is none), a commit
     * and a tag on the commit. The tag is lightweight if {@code tagMessage} is {@code null}, otherwise
     * it is annotated.
     * 
     * @param tagName the tag name to create on the commit
     * @param tagMessage the tag message to create on the commit, if not {@code null} makes the tag annotated
     * otherwise the tag will be lightweight
     * 
     * @return this same instance
     * 
     * @throws Exception in case of any issue
     */
    public Script andCommitWithTag(String tagName, String tagMessage)
        throws Exception {
        return andCommitWithTag(null, tagName, tagMessage);
    }

    /**
     * Adds a batch of operations to the git repository in the current branch.
     * A batch is made of change to all the files in the repository (new files are added if there is none), a commit
     * and a tag on the commit. The tag is lightweight if {@code tagMessage} is {@code null}, otherwise
     * it is annotated.
     * 
     * @param commitMessage the commit message. If {@code null} a random message is generated
     * @param tagName the tag name to create on the commit
     * @param tagMessage the tag message to create on the commit, if not {@code null} makes the tag annotated
     * otherwise the tag will be lightweight
     * 
     * @return this same instance
     * 
     * @throws Exception in case of any issue
     */
    public Script andCommitWithTag(String commitMessage, String tagName, String tagMessage)
        throws Exception {
        if (getFiles().isEmpty())
            andAddFiles();
        return andStage().andCommit(commitMessage).andTag(tagName, tagMessage);
    }

    /**
     * Adds a batch of operations to the git repository in the given branch.
     * A batch is made of change to all the files in the repository (new files are added if there is none), a commit
     * and a lightweight tag on the commit.
     * 
     * @param branchName the name of the branch to create the commit in
     * @param tagName the tag name to create on the commit
     * 
     * @return this same instance
     * 
     * @throws Exception in case of any issue
     */
    public Script andCommitWithTagInBranch(String branchName, String tagName)
        throws Exception {
        checkout(branchName);
        return andCommitWithTag(tagName);
    }

    /**
     * Adds a batch of operations to the git repository in the given branch.
     * A batch is made of change to all the files in the repository (new files are added if there is none), a commit
     * and a tag on the commit. The tag is lightweight if {@code tagMessage} is {@code null}, otherwise
     * it is annotated.
     * 
     * @param branchName the name of the branch to create the commit in
     * @param tagName the tag name to create on the commit
     * @param tagMessage the tag message to create on the commit, if not {@code null} makes the tag annotated
     * otherwise the tag will be lightweight
     * 
     * @return this same instance
     * 
     * @throws Exception in case of any issue
     */
    public Script andCommitWithTagInBranch(String branchName, String tagName, String tagMessage)
        throws Exception {
        return andCommitWithTagInBranch(branchName, null, tagName, tagMessage);
    }

    /**
     * Adds a batch of operations to the git repository in the given branch.
     * A batch is made of change to all the files in the repository (new files are added if there is none), a commit
     * and a tag on the commit. The tag is lightweight if {@code tagMessage} is {@code null}, otherwise
     * it is annotated.
     * 
     * @param branchName the name of the branch to create the commit in
     * @param commitMessage the commit message. If {@code null} a random message is generated
     * @param tagName the tag name to create on the commit
     * @param tagMessage the tag message to create on the commit, if not {@code null} makes the tag annotated
     * otherwise the tag will be lightweight
     * 
     * @return this same instance
     * 
     * @throws Exception in case of any issue
     */
    public Script andCommitWithTagInBranch(String branchName, String commitMessage, String tagName, String tagMessage)
        throws Exception {
        checkout(branchName);
        return andCommitWithTag(commitMessage, tagName, tagMessage);
    }

    /**
     * Commits merge the contents of the given branch into the current one creating a commit with a random message.
     * 
     * @param fromBranch the name of the branch to merge from
     * 
     * @return this same instance
     * 
     * @throws Exception in case of any issue
     */
    public Script andMergeFrom(String fromBranch)
        throws Exception {
        return andMergeFrom(fromBranch, null);
    }

    /**
     * Commits merge the contents of the given branch into the current one creating a commit with the given message.
     * 
     * @param fromBranch the name of the branch to merge from
     * @param commitMessage the commit message. If {@code null} a random message is generated
     * 
     * @return this same instance
     * 
     * @throws Exception in case of any issue
     */
    public Script andMergeFrom(String fromBranch, String commitMessage)
        throws Exception {
        merge(fromBranch, Objects.isNull(commitMessage) ? "Merge ".concat(RandomUtil.randomAlphabeticString(3)) : commitMessage);
        return this;
    }

    /**
     * Commits merge the contents of the given branch into the current one creating a commit with the given message.
     * 
     * @param fromBranch the name of the branch to merge from
     * @param commitMessage the commit message. If {@code null} a random message is generated
     * @param tagName the name of the tag to apply to the merge commit
     * 
     * @return this same instance
     * 
     * @throws Exception in case of any issue
     */
    public Script andMergeFromWithTag(String fromBranch, String commitMessage, String tagName)
        throws Exception {
        return andMergeFromWithTag(fromBranch, commitMessage, tagName, null);
    }

    /**
     * Commits merge the contents of the given branch into the current one creating a commit with the given message.
     * 
     * @param fromBranch the name of the branch to merge from
     * @param commitMessage the commit message. If {@code null} a random message is generated
     * @param tagName the name of the tag to apply to the merge commit
     * @param tagMessage the tag message to create on the commit, if not {@code null} makes the tag annotated
     * otherwise the tag will be lightweight
     * 
     * @return this same instance
     * 
     * @throws Exception in case of any issue
     */
    public Script andMergeFromWithTag(String fromBranch, String commitMessage, String tagName, String tagMessage)
        throws Exception {
        merge(fromBranch, Objects.isNull(commitMessage) ? "Merge ".concat(RandomUtil.randomAlphabeticString(3)) : commitMessage);
        tag(tagName, tagMessage);
        return this;
    }

    /**
     * Commits merge the contents of the current branch into the given one creating a commit with a random message. When this method
     * returns the target branch is the current one.
     * 
     * @param toBranch the name of the branch to merge to, it will also be the current branch after the commit
     * 
     * @return this same instance
     * 
     * @throws Exception in case of any issue
     */
    public Script andMergeInto(String toBranch)
        throws Exception {
        return andMergeInto(toBranch, "Merge ".concat(RandomUtil.randomAlphabeticString(3)));
    }

    /**
     * Commits merge the contents of the current branch into the given one creating a commit with the given message. When this method
     * returns the target branch is the current one.
     * 
     * @param toBranch the name of the branch to merge to, it will also be the current branch after the commit
     * @param commitMessage the commit message. If {@code null} a random message is generated
     * 
     * @return this same instance
     * 
     * @throws Exception in case of any issue
     */
    public Script andMergeInto(String toBranch, String commitMessage)
        throws Exception {
        return andMergeInto(toBranch, getCurrentBranch(), commitMessage);
    }

    /**
     * Commits merge the contents of the given branch into the target one creating a commit with the given message. When this method
     * returns the target branch is the current one.
     * 
     * @param toBranch the name of the branch to merge to, it will also be the current branch after the commit
     * @param fromBranch the name of the branch to merge from
     * @param commitMessage the commit message. If {@code null} a random message is generated
     * 
     * @return this same instance
     * 
     * @throws Exception in case of any issue
     */
    public Script andMergeInto(String toBranch, String fromBranch, String commitMessage)
        throws Exception {
        checkout(toBranch);
        return andMergeFrom(fromBranch, commitMessage);
    }

    /**
     * Commits merge the contents of the current branch into the given one creating a commit with a random message. When this method
     * returns the target branch is the current one.
     * 
     * @param toBranch the name of the branch to merge to, it will also be the current branch after the commit
     * @param tagName the name of the tag to apply to the merge commit
     * 
     * @return this same instance
     * 
     * @throws Exception in case of any issue
     */
    public Script andMergeIntoWithTag(String toBranch, String tagName)
        throws Exception {
        return andMergeIntoWithTag(toBranch, tagName, null);
    }

    /**
     * Commits merge the contents of the current branch into the given one creating a commit with a random message. When this method
     * returns the target branch is the current one.
     * 
     * @param toBranch the name of the branch to merge to, it will also be the current branch after the commit
     * @param tagName the name of the tag to apply to the merge commit
     * @param tagMessage the tag message to create on the commit, if not {@code null} makes the tag annotated
     * otherwise the tag will be lightweight
     * 
     * @return this same instance
     * 
     * @throws Exception in case of any issue
     */
    public Script andMergeIntoWithTag(String toBranch, String tagName, String tagMessage)
        throws Exception {
        andMergeInto(toBranch, "Merge ".concat(RandomUtil.randomAlphabeticString(3)));
        tag(tagName, tagMessage);
        return this;
    }

    /**
     * Commits merge the contents of the given branch into the target one creating a commit with the given message. When this method
     * returns the target branch is the current one.
     * 
     * @param toBranch the name of the branch to merge to, it will also be the current branch after the commit
     * @param fromBranch the name of the branch to merge from
     * @param commitMessage the commit message. If {@code null} a random message is generated
     * @param tagName the name of the tag to apply to the merge commit
     * @param tagMessage the tag message to create on the commit, if not {@code null} makes the tag annotated
     * otherwise the tag will be lightweight
     * 
     * @return this same instance
     * 
     * @throws Exception in case of any issue
     */
    public Script andMergeIntoWithTag(String toBranch, String fromBranch, String commitMessage, String tagName, String tagMessage)
        throws Exception {
        checkout(toBranch);
        andMergeFrom(fromBranch, commitMessage);
        tag(tagName, tagMessage);
        return this;
    }

    /**
     * Adds the given lines to the {@code .gitignore} file. The file is created if doesn't exist yet.
     * 
     * @param items the lines to write to the {@code .gitignore} file.
     * 
     * @returnthis same instance
     * 
     * @throws Exception in case of any issue
     */
    public Script andIgnore(String... items)
        throws Exception {
        FileSystemUtil.appendLines(new File(getWorkingDirectory(), ".gitignore"), items);
        return this;
    }
}
