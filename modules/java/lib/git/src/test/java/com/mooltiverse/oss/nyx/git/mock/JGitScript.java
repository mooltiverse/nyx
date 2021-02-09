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
package com.mooltiverse.oss.nyx.git.mock;

import com.mooltiverse.oss.nyx.git.util.FileSystemUtil;
import com.mooltiverse.oss.nyx.git.util.RandomUtil;

import java.io.File;

import java.util.HashMap;
import java.util.Map;
import java.util.List;
import java.util.Objects;

import org.eclipse.jgit.api.AddNoteCommand;
import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.TagCommand;

import org.eclipse.jgit.dircache.DirCache;

import org.eclipse.jgit.lib.Ref;

import org.eclipse.jgit.notes.Note;

import org.eclipse.jgit.revwalk.RevBlob;
import org.eclipse.jgit.revwalk.RevCommit;
import org.eclipse.jgit.revwalk.RevObject;
import org.eclipse.jgit.revwalk.RevTag;
import org.eclipse.jgit.revwalk.RevTree;

import org.eclipse.jgit.transport.RemoteConfig;
import org.eclipse.jgit.transport.URIish;

/**
 * An utility class used to run a sequence of commands on a JGit repository. This class is used
 * to dynamically create a repository that can be used for tests.
 */
public class JGitScript {
    /**
     * The backing Git instance
     */
    public final Git git;

    /**
     * Creates a new repository in the given directory.
     * 
     * @param directory the directory to create the repository in
     * 
     * @throws Exception in case of any issue
     */
    public JGitScript(File directory)
        throws Exception {
        this(newRepository(directory, false, true));
    }

    /**
     * Creates a new instance on the given instance.
     * 
     * @param git the backing git instance
     * 
     * @throws Exception in case of any issue
     */
    public JGitScript(Git git)
        throws Exception {
        super();
        Objects.requireNonNull(git);
        this.git = git;
    }

    /**
     * Returns a Git instance on a new repository.
     * 
     * @param dir the directory to create the repository in. It can't be <code>null</code>.
     * @param bare if <code>true</code> the repository is initialized as a bare repository, otherwise it will have a work tree.
     * If <code>initialize</code> is <code>false</code> this parameter is ignored.
     * @param initialize if <code>true</code> the repository has to be initialized, otherwise <code>false</code>.
     * 
     * @return the new Git instance.
     * 
     * @throws Exception in case of any exception
     */
    public static Git newRepository(File dir, boolean bare, boolean initialize)
        throws Exception {
        if (initialize) {
            Git.init().setBare(bare).setDirectory(dir).call();
        }
        return Git.open(dir);
    }

    /**
     * Stages changes and commits to the Git repository using the current branch.
     * 
     * @param message the commit message.
     * 
     * @return the new commit object
     * 
     * @throws Exception in case of any issue
     */
    public RevCommit createCommit(String message)
        throws Exception {

        // stage contents
        DirCache dirCache = git.add().setUpdate(false).addFilepattern(".").call();

        // commit
        return git.commit().setMessage(message).call();
    }

    /**
     * Creates a new branch and checks it out.
     * 
     * @param name the branch name
     * 
     * @return the new branch
     * 
     * @throws Exception in case of any issue
     */
    public Ref createBranch(String name)
        throws Exception {
        // create the branch and check it out
        return git.checkout().setCreateBranch(true).setName(name).call();
    }

    /**
     * Tags the given object with the given name.
     * 
     * @param name the tag name
     * @param message the tag message (used only for annotated tags, otherwise can be <code>null</code>)
     * @param target the object to tag
     * @param annotated if <code>true</code> the tag will be an annotated tag, otherwise it  will be a lightweight tag
     * 
     * @return the new tag
     * 
     * @throws Exception in case of any issue
     */
    public Ref createTag(String name, String message, RevObject target, boolean annotated)
        throws Exception {
        return annotated ? git.tag().setAnnotated(true).setObjectId(target).setName(name).setMessage(message).call() : git.tag().setAnnotated(false).setObjectId(target).setName(name).setForceUpdate(true).call();
    }

    /**
     * Creates a note on given object with the given message.
     * 
     * @param message the note message
     * @param target the object to put the note on
     * 
     * @return the new note
     * 
     * @throws Exception in case of any issue
     */
    public Note createNote(String message, RevObject target)
        throws Exception {
        return git.notesAdd().setObjectId(target).setMessage(message).call();
    }

    /**
     * Creates a new remote.
     * 
     * @param name the remote name
     * @param uri the remote URI
     * 
     * @return the new remote configuration object
     * 
     * @throws Exception in case of any issue
     */
    public RemoteConfig createRemote(String name, String uri)
        throws Exception {
        return git.remoteAdd().setName(name).setUri(new URIish(uri)).call();
    }

    /**
     * A coarse command that creates a number of repository objects.
     * This script is repeatable over the same repository.
     * 
     * @param directoryCount the number of directories to create
     * @param fileCount the number of files to create for each directory
     * @param commit set it to <code>true</code> to make a commit
     * @param annotatedTag set it to <code>true</code> to apply an annotated tag on the commit (ignored if <code>commit</code> is <code>false</code>)
     * 
     * @return the batch ID, a string with a random sequence used as a seed for names and IDs in this run.
     * 
     * @throws Exception in case of any issue
     */
    public String batchCreateObjects(int directoryCount, int fileCount, boolean commit, boolean annotatedTag)
        throws Exception {

        String batchID = RandomUtil.randomAlphabeticString(4);

        // create a bunch of random files in the root directory
        FileSystemUtil.newTempFilesWithRandomText(git.getRepository().getWorkTree(), fileCount);
        // create directories and files within
        for (int d = 0; d<directoryCount; d++) {
            File newDir = FileSystemUtil.newDirectory(git.getRepository().getWorkTree());
            FileSystemUtil.newTempFilesWithRandomText(newDir, fileCount);
        }

        if (commit) {
            // stage and commit
            RevCommit repositoryCommit = createCommit("Commit "+batchID);
            if (annotatedTag) {
                // create an annotated tag over the commit
                createTag("atag-"+batchID, "AnnotatedTag "+batchID, repositoryCommit, true);
            }
        }

        return batchID;
    }

    /**
     * A coarse command that creates a number of repository references.
     * This script is repeatable over the same repository.
     * 
     * @param branchesCount the number of branches to create
     * @param commitCount the number of commits to create for each branch
     * @param note set it to <code>true</code> to create a note for each commit
     * @param lightweightTag set it to <code>true</code> to apply a lightweight tag on the commits
     * 
     * @return the batch ID, a string with a random sequence used as a seed for names and IDs in this run.
     * 
     * @throws Exception in case of any issue
     */
    public String batchCreateReferences(int branchesCount, int commitCount, boolean note, boolean lightweightTag)
        throws Exception {

        String batchID = RandomUtil.randomAlphabeticString(4);
        for (int b = 0; b<branchesCount; b++) {
            // create the branch and check it out
            String branchName = "branch-"+batchID+"-"+b;
            createBranch(branchName);

            // change the files contents and commit
            for (int c = 0; c<commitCount; c++) {
                FileSystemUtil.writeRandomText(FileSystemUtil.getFiles(git.getRepository().getWorkTree(), List.<String>of(".git"), true), 2, 2);
                RevCommit repositoryCommit = createCommit("Commit #"+c+" on branch "+branchName);
                // create the note
                if (note)
                    createNote("This is a note on "+repositoryCommit.getId().getName()+" in branch "+branchName, repositoryCommit);
                // create a lightweight tag over the commit
                if (lightweightTag)
                    createTag("ltag-"+batchID+"-"+c, null, repositoryCommit, false);
            }
        }

        return batchID;
    }

    /**
     * A coarse command that creates a number of remote references.
     * This script is repeatable over the same repository.
     * 
     * @param remotesCount the number of remotes to create
     * 
     * @return the batch ID, a string with a random sequence used as a seed for names and IDs in this run.
     * 
     * @throws Exception in case of any issue
     */
    public String batchCreateRemotes(int remotesCount)
        throws Exception {

        String batchID = RandomUtil.randomAlphabeticString(4);
        for (int r = 0; r<remotesCount; r++) {
            String remoteName = batchID+"-"+r;
            createRemote(remoteName, "http://"+remoteName+".example.com/");
        }

        return batchID;
    }
}