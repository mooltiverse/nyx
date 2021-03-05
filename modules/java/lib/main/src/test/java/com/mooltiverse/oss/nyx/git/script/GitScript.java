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
package com.mooltiverse.oss.nyx.git.script;

import com.mooltiverse.oss.nyx.git.util.FileSystemUtil;
import com.mooltiverse.oss.nyx.git.util.RandomUtil;

import java.io.File;

/**
 * An utility class built on top of a test repository and providing some coarse grained commands to be used fluently.
 */
public class GitScript extends GitTestRepository {
    /**
     * Creates a new script instance in the given directory.
     * 
     * @param directory the directory to create the repository in
     * 
     * @throws Exception in case of any issue
     */
    protected GitScript(File directory)
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
    protected GitScript(File directory, boolean bare, boolean initialize)
        throws Exception {
        super(directory, bare, initialize);
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
    public static GitScript from(File directory)
        throws Exception {
        return new GitScript(directory);
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
    public static GitScript fromScratch(File directory)
        throws Exception {
        return new GitScript(directory, false, true);
    }

    /**
     * Returns a new script instance for a new repository to be created and initialized in a new temporary directory
     * @return the new instance
     * 
     * @throws Exception in case of any issue
     * 
     * @see #getWorkingDirectory() to get the repository directory
     */
    public static GitScript fromScratch()
        throws Exception {
        return new GitScript(FileSystemUtil.newTempDirectory(null, null), false, true);
    }

    /**
     * Commits the staged changes with the given commit message
     * 
     * @param message the commit message
     * 
     * @return this same instance
     * 
     * @throws Exception in case of any issue
     */
    public GitScript andCommit(String message)
        throws Exception {
        commit(message);
        return this;
    }

    /**
     * Commits the staged changes with a random commit message
     * 
     * @return this same instance
     * 
     * @throws Exception in case of any issue
     */
    public GitScript andCommit()
        throws Exception {
        return andCommit("Commit ".concat(RandomUtil.randomAlphabeticString(3)));
    }

    /**
     * Adds the modified contents to the sraging area.
     * 
     * @return this same instance
     * 
     * @throws Exception in case of any issue
     */
    public GitScript andStage()
        throws Exception {
        stage();
        return this;
    }

    /**
     * Tags the latest commit with the given name. The tag is a lightweight tag unless the given message is not {@code null},
     * in which case the message is used for the annotation.
     * 
     * @param name the tag value
     * @param message the optional tag message
     * 
     * @return this same instance
     * 
     * @throws Exception in case of any issue
     */
    public GitScript andTag(String name, String message)
        throws Exception {
        tag(name, message, getLastCommit());
        return this;
    }

    /**
     * Changes the contents of all files in the repository. Changes are not staged or committed.
     * 
     * @return this same instance
     * 
     * @throws Exception in case of any issue
     */
    public GitScript andUpdateFiles()
        throws Exception {
        updateAllFiles();
        return this;
    }

    /**
     * Adds some files (one) to the repository. Files have content but they are not staged or committed.
     * 
     * @return this same instance
     * 
     * @throws Exception in case of any issue
     */
    public GitScript withFiles()
        throws Exception {
        return withFiles(1);
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
    public GitScript withFiles(int count)
        throws Exception {
        addRandomTextFiles(count);
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
    public GitScript inBranch(String name)
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
    public GitScript addCommitWithTag(String tagName)
        throws Exception {
        return addCommitWithTag(tagName, null);
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
    public GitScript addCommitWithTagInBranch(String branchName, String tagName)
        throws Exception {
        checkout(branchName);
        return addCommitWithTag(tagName);
    }

    /**
     * Adds a batch of operations to the git repository in the current branch.
     * A batch is made of change to all the files in the repository (new files are added if there is none), a commit
     * and a tag on the commit. The tag is lightweight if {@code tagMessage} is {@code null}, otherwise
     * it is annotated.
     * 
     * @param tagName the tag name to create on the commit
     * @param tagMessage the tag message to create on the commit, makes the tag annotated
     * 
     * @return this same instance
     * 
     * @throws Exception in case of any issue
     */
    public GitScript addCommitWithTag(String tagName, String tagMessage)
        throws Exception {
        if (getFiles().isEmpty())
            withFiles();
        return andStage().andCommit().andTag(tagName, tagMessage);
    }

    /**
     * Adds a batch of operations to the git repository in the given branch.
     * A batch is made of change to all the files in the repository (new files are added if there is none), a commit
     * and a tag on the commit. The tag is lightweight if {@code tagMessage} is {@code null}, otherwise
     * it is annotated.
     * 
     * @param branchName the name of the branch to create the commit in
     * @param tagName the tag name to create on the commit
     * @param tagMessage the tag message to create on the commit, makes the tag annotated
     * 
     * @return this same instance
     * 
     * @throws Exception in case of any issue
     */
    public GitScript addCommitWithTagInBranch(String branchName, String tagName, String tagMessage)
        throws Exception {
        checkout(branchName);
        return addCommitWithTag(tagName, tagMessage);
    }

    /**
     * Commits merge the contents of the given branch into the current one creating a commit with the given message.
     * 
     * @param fromBranch the name of the branch to merge from
     * @param message the commit message
     * 
     * @return this same instance
     * 
     * @throws Exception in case of any issue
     */
    public GitScript andMergeFrom(String fromBranch, String message)
        throws Exception {
        merge(fromBranch, message);
        return this;
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
    public GitScript andMergeFrom(String fromBranch)
        throws Exception {
        return andMergeFrom(fromBranch, "Merge ".concat(RandomUtil.randomAlphabeticString(3)));
    }

    /**
     * Commits merge the contents of the given branch into the current one creating a commit with the given message.
     * 
     * @param fromBranch the name of the branch to merge from
     * @param message the commit message
     * @param tagName the name of the tag to apply to the merge commit
     * 
     * @return this same instance
     * 
     * @throws Exception in case of any issue
     */
    public GitScript andMergeFromWithTag(String fromBranch, String message, String tagName)
        throws Exception {
        return andMergeFromWithTag(fromBranch, message, tagName, null);
    }

    /**
     * Commits merge the contents of the given branch into the current one creating a commit with the given message.
     * 
     * @param fromBranch the name of the branch to merge from
     * @param message the commit message
     * @param tagName the name of the tag to apply to the merge commit
     * @param tagMessage the message of the tag, if {@code null} the tag will be lightweight, otherwise it will be annotated
     * 
     * @return this same instance
     * 
     * @throws Exception in case of any issue
     */
    public GitScript andMergeFromWithTag(String fromBranch, String message, String tagName, String tagMessage)
        throws Exception {
        merge(fromBranch, message);
        tag(tagName, tagMessage);
        return this;
    }

    /**
     * Commits merge the contents of the given branch into the target one creating a commit with the given message. When this method
     * returns the target branch is the current one.
     * 
     * @param toBranch the name of the branch to merge to, it will also be the current branch after the commit
     * @param fromBranch the name of the branch to merge from
     * @param message the commit message
     * 
     * @return this same instance
     * 
     * @throws Exception in case of any issue
     */
    public GitScript andMergeInto(String toBranch, String fromBranch, String message)
        throws Exception {
        checkout(toBranch);
        return andMergeFrom(fromBranch, message);
    }

    /**
     * Commits merge the contents of the current branch into the given one creating a commit with the given message. When this method
     * returns the target branch is the current one.
     * 
     * @param toBranch the name of the branch to merge to, it will also be the current branch after the commit
     * @param message the commit message
     * 
     * @return this same instance
     * 
     * @throws Exception in case of any issue
     */
    public GitScript andMergeInto(String toBranch, String message)
        throws Exception {
        return andMergeInto(toBranch, getCurrentBranch(), message);
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
    public GitScript andMergeInto(String toBranch)
        throws Exception {
        return andMergeInto(toBranch, "Merge ".concat(RandomUtil.randomAlphabeticString(3)));
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
    public GitScript andMergeIntoWithTag(String toBranch, String tagName)
        throws Exception {
        return andMergeIntoWithTag(toBranch, tagName, null);
    }

    /**
     * Commits merge the contents of the current branch into the given one creating a commit with a random message. When this method
     * returns the target branch is the current one.
     * 
     * @param toBranch the name of the branch to merge to, it will also be the current branch after the commit
     * @param tagName the name of the tag to apply to the merge commit
     * @param tagMessage the message of the tag, if {@code null} the tag will be lightweight, otherwise it will be annotated
     * 
     * @return this same instance
     * 
     * @throws Exception in case of any issue
     */
    public GitScript andMergeIntoWithTag(String toBranch, String tagName, String tagMessage)
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
     * @param message the commit message
     * @param tagName the name of the tag to apply to the merge commit
     * @param tagMessage the message of the tag, if {@code null} the tag will be lightweight, otherwise it will be annotated
     * 
     * @return this same instance
     * 
     * @throws Exception in case of any issue
     */
    public GitScript andMergeIntoWithTag(String toBranch, String fromBranch, String message, String tagName, String tagMessage)
        throws Exception {
        checkout(toBranch);
        andMergeFrom(fromBranch, message);
        tag(tagName, tagMessage);
        return this;
    }
}

