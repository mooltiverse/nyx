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
package com.mooltiverse.oss.nyx.git;

import com.mooltiverse.oss.nyx.git.util.FileSystemUtil;
import com.mooltiverse.oss.nyx.git.util.GitUtil;
import com.mooltiverse.oss.nyx.git.util.RandomUtil;

import java.io.File;
import java.io.FileWriter;
import java.io.OutputStream;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.errors.RefNotFoundException;
import org.eclipse.jgit.lib.Constants;
import org.eclipse.jgit.lib.Ref;
import org.eclipse.jgit.lib.RefDatabase;
import org.eclipse.jgit.lib.ObjectId;
import org.eclipse.jgit.revwalk.RevCommit;
import org.eclipse.jgit.revwalk.RevObject;
import org.eclipse.jgit.transport.RemoteConfig;
import org.eclipse.jgit.transport.URIish;

/**
 * A Git repository utility class used to test. This class is used to dynamically create a repository that can be used for tests
 * and exposes some high level methods and attributes.
 */
public class Workbench {
    /**
     * The backing Git instance
     */
    private final Git git;

    /**
     * The map of all annotated tags created by this object. Keys are commit SHA, values are tag names.
     */
    private final Map<String,String> annotatedTags = new HashMap<String,String>();

    /**
     * The map of all lightweight tags created by this object. Keys are commit SHA, values are tag names.
     */
    private final Map<String,String> lightweightTags = new HashMap<String,String>();

    /**
     * The map of all commits created by this object. Keys are branch names, values are lists of commit SHA (all commits created for that branch).
     */
    private final Map<String,List<String>> commits = new HashMap<String,List<String>>();

    /**
     * Creates a new repository instance using the given backing git instance.
     * 
     * @param git the backing git instance
     * 
     * @throws Exception in case of any issue
     */
    private Workbench(Git git)
        throws Exception {
        super();
        Objects.requireNonNull(git);
        this.git = git;
    }

    /**
     * Creates a new repository instance in the given directory.
     * 
     * @param directory the directory to create the repository in
     * 
     * @throws Exception in case of any issue
     */
    public Workbench(File directory)
        throws Exception {
        this(Git.open(directory));
    }

    /**
     * Creates a Git instance on a new repository.
     * 
     * @param directory the directory to create the repository in. It can't be {@code null}.
     * @param bare if {@code true} the repository is initialized as a bare repository, otherwise it will have a work tree.
     * If {@code initialize} is {@code false} this parameter is ignored.
     * @param initialize if {@code true} the repository has to be initialized, otherwise {@code false}.
     * 
     * @throws Exception in case of any exception
     */
    public Workbench(File directory, boolean bare, boolean initialize)
        throws Exception {
        super();
        if (initialize) {
            Git.init().setBare(bare).setDirectory(directory).call();
        }
        this.git = Git.open(directory);
    }

    /**
     * Creates a new repository instance in a new temporary directory.
     * 
     * @param initialize if {@code true} the new repository is also initialized
     * 
     * @throws Exception in case of any issue
     */
    public Workbench(boolean initialize)
        throws Exception {
        this(FileSystemUtil.newTempDirectory(null, null), false, initialize);
    }

    /**
     * Creates a new initialized repository instance in a new temporary directory.
     * 
     * @throws Exception in case of any issue
     */
    public Workbench()
        throws Exception {
        this(true);
    }

    /**
     * Returns the Git metadata directory for the repository
     * 
     * @return the Git metadata directory for the repository
     */
    public final File getGitDirectory() {
        return git.getRepository().getDirectory();
    }

    /**
     * Returns the working directory for the repository
     * 
     * @return the working directory for the repository
     */
    public final File getWorkingDirectory() {
        return git.getRepository().getWorkTree();
    }

    /**
     * Returns a map of all annotated tags created by this repository, where keys are commit SHA and values are tag names.
     * 
     * @return a map of all annotated tags created by this repository, where keys are commit SHA and values are tag names.
     */
    public Map<String,String> getAnnotatedTags() {
        return annotatedTags;
    }

    /**
     * Returns a map of all lightweight tags created by this repository, where keys are commit SHA and values are tag names.
     * 
     * @return a map of all lightweight tags created by this repository, where keys are commit SHA and values are tag names.
     */
    public Map<String,String> getLightweightTags() {
        return lightweightTags;
    }

    /**
     * Returns a map of all tags created by this repository, where keys are commit SHA and values are tag names.
     * 
     * @return a map of all tags created by this repository, where keys are commit SHA and values are tag names.
     */
    public Map<String,String> getAllTags() {
        Map<String,String> res = new HashMap<String,String>();
        res.putAll(annotatedTags);
        res.putAll(lightweightTags);
        return res;
    }

    /**
     * Returns the a map in which keys are branch names and values are lists of all commits for those branches.
     * 
     * @return the a map in which keys are branch names and values are lists of all commits for those branches.
     */
    public Map<String,List<String>> getAllCommits() {
        return commits;
    }

    /**
     * Returns the SHA-1 of the commit the given tag points to, if any, or {@code null} otherwise.
     * 
     * @param tag the tag to search the commit for
     * 
     * @return the SHA-1 of the commit the given tag points to, if any, or {@code null} otherwise.
     * 
     * @throws Exception in case of any issue
     */
    public String getCommitByTag(String tag)
        throws Exception {
        RefDatabase refDatabase = git.getRepository().getRefDatabase();
        for (Ref tagRef: refDatabase.getRefsByPrefix(Constants.R_TAGS)) {
            if (tagRef.getName().replace(Constants.R_TAGS, "").equals(tag)) {
                tagRef = refDatabase.peel(tagRef);
                return Objects.isNull(tagRef.getPeeledObjectId()) ? tagRef.getObjectId().getName() : tagRef.getPeeledObjectId().getName();
            }
        }
        return null;
    }

    /**
     * Returns the list of all commit SHAs created in this repository.
     * 
     * @return the list of all commit SHAs created in this repository.
     */
    public List<String> getCommits() {
        List<String> res = new ArrayList<String>();
        for (List<String> value: commits.values()) {
            res.addAll(value);
        }
        return res;
    }

    /**
     * Returns the list of all commit SHAs created in this repository for the given branch.
     * 
     * @param branch the name of the branch the commits are requested
     * 
     * @return the list of all commit SHAs created in this repository for the given branch.
     */
    public List<String> getCommitsByBranch(String branch) {
        return commits.get(branch);
    }

    /**
     * Returns the list of commits for the given branch. If the entry does not exist yet in the commits map
     * it is created first, so this method is safer than commits.get(branch).
     * 
     * @param branch the branch to get the commits for
     * 
     * @return the list of commits for the given branch
     */
    private List<String> commitsForBranch(String branch) {
        List<String> res = commits.get(branch);
        if (Objects.isNull(res)) {
            res = new ArrayList<String>();
            commits.put(branch, res);
        }
        return res;
    }

    /**
     * Adds the given number of text files to the repository root directory. Files have content but they are not staged or committed.
     * 
     * @param count the number of files to add
     * 
     * @return the collection of the new files
     * 
     * @throws Exception in case of any issue
     * 
     * @see #getFiles()
     */
    public Collection<File> addRandomTextFiles(int count)
        throws Exception {
        Collection<File> res = new ArrayList<File>();
        for (int i=0; i<=count; i++) {
            File f = new File(git.getRepository().getWorkTree(), RandomUtil.randomAlphabeticString(5).concat(".txt"));
            FileWriter fw = new FileWriter(f);
            fw.write(RandomUtil.randomAlphabeticString(5));
            fw.flush();
            fw.close();
            res.add(f);
        }

        return res;
    }

    /**
     * Returns the collection of regular files in the repository root directory.
     * 
     * @return the collection of the files in the repository directory
     * 
     * @throws Exception in case of any issue
     * 
     * @see #addRandomTextFiles(int)
     */
    public Collection<File> getFiles()
        throws Exception {
        Collection<File> res = new ArrayList<File>();
        Collections.addAll(res, git.getRepository().getWorkTree().listFiles((File f) -> f.isFile())); // avoid adding the ".git" directory (and any other directory)
        return res;
    }

    /**
     * Replaces the content of the given files with new random content.
     * 
     * @param files the collection of files to update
     * 
     * @throws Exception in case of any issue
     * 
     * @see #getFiles()
     */
    public void updateFiles(Collection<File> files)
        throws Exception {
        for (File f: files) {
            FileWriter fw = new FileWriter(f);
            fw.write(RandomUtil.randomAlphabeticString(5));
            fw.flush();
            fw.close();
        }
    }

    /**
     * Replaces the content of all the files in the repository.

     * @throws Exception in case of any issue
     * 
     * @see #getFiles()
     */
    public void updateAllFiles()
        throws Exception {
        updateFiles(getFiles());
    }

    /**
     * Adds all the local changed files to the staging area, without committing.
     * 
     * @throws Exception in case of any issue
     */
    public void stage()
        throws Exception {
        git.add().setUpdate(false).addFilepattern(".").call();
    }

    /**
     * Returns the last commit in the current branch.
     * 
     * @return the last commit in the current branch or {@code null} if the repository has no commits yet
     * 
     * @throws Exception in case of any issue
     */
    public RevCommit getLastCommit()
        throws Exception {
        Iterator<RevCommit> iterator = git.log().add(git.getRepository().resolve(Constants.HEAD)).setMaxCount(1).call().iterator();
        if (iterator.hasNext())
            return iterator.next();
        else return null;
    }

    /**
     * Commits the staged files with the given commit message.
     * 
     * @param message the commit message
     * 
     * @return the resulting commit object
     * 
     * @throws Exception in case of any issue
     */
    public RevCommit commit(String message)
        throws Exception {
        RevCommit res = git.commit().setMessage(message).call();
        commitsForBranch(getCurrentBranch()).add(res.getId().getName());
        return res;
    }

    /**
     * Tags the given object with the given name.
     * 
     * @param name the tag name
     * @param message the tag message (used only for annotated tags, otherwise can be {@code null})
     * @param target the object to tag
     * 
     * @return the resulting tag
     * 
     * @throws Exception in case of any issue
     */
    public Ref tag(String name, String message, RevObject target)
        throws Exception {
        if (Objects.isNull(message)) {
            Ref res = git.tag().setAnnotated(false).setObjectId(target).setName(name).setForceUpdate(true).call();
            lightweightTags.put(target.getId().getName(), name);
            return res;
        }
        else {
            Ref res = git.tag().setAnnotated(true).setObjectId(target).setName(name).setMessage(message).call();
            annotatedTags.put(target.getId().getName(), name);
            return res;
        }
    }

    /**
     * Tags the last commit with the given name.
     * 
     * @param name the tag name
     * @param message the tag message (used only for annotated tags, otherwise can be {@code null})
     * 
     * @return the resulting tag
     * 
     * @throws Exception in case of any issue
     * 
     * @see #getLastCommit()
     */
    public Ref tag(String name, String message)
        throws Exception {
        return tag(name, message, getLastCommit());
    }

    /**
     * Returns the names of all branches in the repository.
     * 
     * @return the names of all branches in the repository.
     * 
     * @throws Exception in case of any issue
     */
    public List<String> getBranches()
        throws Exception {
        List<Ref> refs = git.branchList().call();
        List<String> res = new ArrayList<String>(refs.size());
        for (Ref ref: refs) {
            res.add(ref.getName().replaceFirst(Constants.R_HEADS, ""));
        }
        return res;
    }

    /**
     * Returns the name of the current branch.
     * 
     * @return the name of the current branch.
     * 
     * @throws Exception in case of any issue
     */
    public String getCurrentBranch()
        throws Exception {
        return git.getRepository().getBranch();
    }

    /**
     * Creates a new branch if none with the given name exists yet and checks it out.
     * Watch out as there must be one commit before this command runs without exception because the HEAD
     * ref may not be initialized yet.
     * 
     * @param name the branch name
     * 
     * @return the branch to checkout after optional creation
     * 
     * @throws Exception in case of any issue
     */
    public Ref checkout(String name)
        throws Exception {
        // try to check it out if it already exists
        try {
            return git.checkout().setName(name).call();
        }
        catch (RefNotFoundException e) {
            // the branch does not exist yet, create it
            Ref res = git.checkout().setCreateBranch(true).setName(name).call();
            return res;
        }
    }

    /**
     * Commits merge the contents of the given branch into the current one creating a commit with the given message.
     * 
     * @param fromBranch the name of the branch to merge from
     * @param message the commit message
     * 
     * @return the resulting commit object if a commit has been generated by the merge, {@code null} otherwise
     * 
     * @throws Exception in case of any issue
     */
    public RevCommit merge(String fromBranch, String message)
        throws Exception {
        ObjectId mergeCommitId = git.merge().include(git.getRepository().resolve(fromBranch)).setCommit(true).setMessage(message).call().getBase();

        if (Objects.isNull(mergeCommitId)) {
            return null;
        }
        else {
            RevCommit res = git.getRepository().parseCommit(mergeCommitId);
            commitsForBranch(getCurrentBranch()).add(res.getId().getName());
            return res;
        }
    }

    /**
     * Adds the repository in the given metadata directory to the configured remotes of this repository, using the given name.
     * This is only suitable for local repositories to be added as remotes to other local repositories.
     * 
     * @param gitDir the Git metadata directory of the repository to add as remote
     * @param name the name to use for the new remote repository in the local one
     * 
     * @return
     * 
     * @throws Exception in case of any issue
     */
    public RemoteConfig addRemote(File gitDir, String name)
        throws Exception {
        return git.remoteAdd().setName(name).setUri(new URIish(gitDir.toURI().toURL())).call();
    }

    /**
     * Peels the given object using the repository Ref Database.
     * 
     * @param ref the object to peel.
     * 
     * @return the peeled object.
     * 
     * @throws Exception in case of any issue
     */
    public Ref peel(Ref ref)
        throws Exception {
        return git.getRepository().getRefDatabase().peel(ref);
    }

    /**
     * Returns the number of entries in the repository index.
     * 
     * @return the number of entries in the repository index.
     * 
     * @throws Exception in case of any issue
     */
    public int getIndexEntryCount()
        throws Exception {
        return git.getRepository().readDirCache().getEntryCount();
    }

    /**
     * Prints repository informations to the given output stream.
     * 
     * @param out the stream to print the info to
     * 
     * @return this same instance
     * 
     * @throws Exception in case of any issue
     */
    public void printInfo(OutputStream out)
        throws Exception {
        GitUtil.printRepositoryInfo(git.getRepository().getWorkTree(), out, null);
    }
}