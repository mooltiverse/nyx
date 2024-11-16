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
import java.util.Properties;

import com.mooltiverse.oss.nyx.git.util.FileSystemUtil;
import com.mooltiverse.oss.nyx.git.util.GitUtil;
import com.mooltiverse.oss.nyx.git.util.RandomUtil;

import com.jcraft.jsch.JSch;
import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.Session;
import com.jcraft.jsch.UserInfo;
import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.TransportConfigCallback;
import org.eclipse.jgit.api.errors.RefNotFoundException;
import org.eclipse.jgit.errors.UnsupportedCredentialItem;
import org.eclipse.jgit.internal.transport.ssh.jsch.CredentialsProviderUserInfo;
import org.eclipse.jgit.lib.Constants;
import org.eclipse.jgit.lib.Ref;
import org.eclipse.jgit.lib.RefDatabase;
import org.eclipse.jgit.lib.ObjectId;
import org.eclipse.jgit.revwalk.RevCommit;
import org.eclipse.jgit.revwalk.RevObject;
import org.eclipse.jgit.transport.CredentialsProvider;
import org.eclipse.jgit.transport.CredentialItem;
import org.eclipse.jgit.transport.RefSpec;
import org.eclipse.jgit.transport.SshTransport;
import org.eclipse.jgit.transport.Transport;
import org.eclipse.jgit.transport.URIish;
import org.eclipse.jgit.transport.UsernamePasswordCredentialsProvider;
import org.eclipse.jgit.transport.ssh.jsch.JschConfigSessionFactory;
import org.eclipse.jgit.transport.ssh.jsch.OpenSshConfig.Host;
import org.eclipse.jgit.util.FS;

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
     * Opens a new repository instance in the given directory.
     * 
     * @param directory the directory to open the repository in
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
        if (initialize)
            this.git = Git.init().setBare(bare).setDirectory(directory).call();
        else this.git = Git.open(directory);
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
        this(FileSystemUtil.newTempDirectory(null, "nyx-test-workbench-"), false, initialize);
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
     * Returns a new transport config callback to use when using Git SSH authentication.
     * 
     * @param privateKey the SSH private key.
     * @param passphrase the optional password to use to open the private key, in case it's protected by a passphrase.
     * 
     * @return the transport config callback built on the given credentials.
     */
    private static TransportConfigCallback getTransportConfigCallback(String privateKey, byte[] passphrase) 
        throws Exception {
        // These code examples have been taken from
        // - https://www.codeaffine.com/2014/12/09/jgit-authentication/ and then elaborated
        // - https://stackoverflow.com/questions/31931574/java-sftp-client-that-takes-private-key-as-a-string
        // - https://stackoverflow.com/questions/56758040/loading-private-key-from-string-or-resource-in-java-jsch-in-android-app
        // - https://stackoverflow.com/questions/33637481/jsch-to-add-private-key-from-a-string
        JschConfigSessionFactory sshSessionFactory = new JschConfigSessionFactory() {
            // Override this method as per the Javadoc instructions
            @Override
            protected void configure(Host host, Session session) {
                // Configure in order to use the passphrase (when using the private key from the default location,
                // which means the user passed the passphrase but not the private key).
                // When the private key is passed in memory instead of using the default location, the passphrase
                // is passed along with it (see below in createDefaultJSch(FS fs)).
                if ((Objects.isNull(privateKey) || privateKey.isEmpty()) && ((!Objects.isNull(passphrase)) && (passphrase.length > 0))) {
                    // Implement an anonymous credentials provider to provide the passphrase when needed.
                    // An alternative could be to just implement an anonymous UserInfo object and make its getPassphrase()
                    // method return the passphrase.
                    CredentialsProvider credentialsProvider = new CredentialsProvider() {
                        @Override
                        public boolean get(URIish uri, CredentialItem... items)
                            throws UnsupportedCredentialItem {
                            for (CredentialItem item : items) {
                                // set the value for string types and ignore others
                                if (CredentialItem.StringType.class.isInstance(item)) {
                                    CredentialItem.StringType.class.cast(item).setValue(new String(passphrase));
                                }
                            }
                            return true;
                        }

                        @Override
                        public boolean isInteractive() {
                            return false;
                        }

                        @Override
                        public boolean supports(CredentialItem... items) {
                            // just return true for any type of items, maybe this needs to be refined
                            return true;
                        }
                    };
                    UserInfo userInfo = new CredentialsProviderUserInfo(session, credentialsProvider);
                    session.setUserInfo(userInfo);
                }

                Properties sessionProperties = new Properties();
                //sessionProperties.put("PreferredAuthentications", "publickey");
                if (!Objects.isNull(privateKey) && !privateKey.isEmpty()) {
                    // disable host key checking if the in-memory key is used
                    sessionProperties.put("StrictHostKeyChecking", "no");
                }
                session.setConfig(sessionProperties);
            }

            // Override this method in order to load the in-memory key, as the default one only loads them from
            // default locations on the local filesystem
            @Override
            protected JSch createDefaultJSch(FS fs)
                throws JSchException {
                JSch defaultJSch = super.createDefaultJSch(fs);
                JSch.setConfig("PreferredAuthentications", "publickey");
                if (!Objects.isNull(privateKey) && !privateKey.isEmpty()) {
                    // the key name is not relevant
                    // the public key is not required
                    defaultJSch.addIdentity("nyxUserKey", privateKey.getBytes(), null, passphrase);

                    // disable host key checking
                    JSch.setConfig("StrictHostKeyChecking", "no");
                }

                return defaultJSch;                  
            }
        };

        return new TransportConfigCallback() {
            @Override
            public void configure(Transport transport) {
                // How can we be sure the given Transport is an SshTransport? Docs don't explain...
                if (SshTransport.class.isInstance(transport)) {
                    SshTransport.class.cast(transport).setSshSessionFactory(sshSessionFactory);
                }
            }
        };
    }

    /**
     * Clones the repository at the given URI into the given directory.
     * This method allows using user name and password authentication (also used for tokens).
     * 
     * @param uri the URI of the repository to clone from. It can't be {@code null}.
     * @param directory the directory to clone the repository in. It can't be {@code null}.
     * @param user the optional user name to use when credentials are required.
     * @param password the optional password to use when credentials are required.
     * 
     * @throws Exception in case of any issue
     */
    public static void cloneInto(String uri, File directory, String user, String password)
        throws Exception {
        Git.cloneRepository().setDirectory(directory).setURI(uri).setCredentialsProvider((Objects.isNull(user) && Objects.isNull(password)) ? null : new UsernamePasswordCredentialsProvider(user, password)).call();
    }

    /**
     * Clones the repository at the given URI into the given directory.
     * This method allows using SSH authentication.
     * 
     * @param uri the URI of the repository to clone from. It can't be {@code null}.
     * @param directory the directory to clone the repository in. It can't be {@code null}.
     * @param privateKey the SSH private key.
     * @param passphrase the optional password to use to open the private key, in case it's protected by a passphrase.
     * 
     * @throws Exception in case of any issue
     */
    public static void cloneInto(String uri, File directory, String privateKey, byte[] passphrase)
        throws Exception {
        Git.cloneRepository().setDirectory(directory).setURI(uri).setTransportConfigCallback(getTransportConfigCallback(privateKey, passphrase)).call();
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
    public Collection<File> addRandomTextWorkbenchFiles(int count)
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
     * Adds the repository in the given metadata directory to the configured remotes of this repository, using the given name.
     * This is only suitable for local repositories to be added as remotes to other local repositories.
     * 
     * @param gitDir the Git metadata directory of the repository to add as remote
     * @param name the name to use for the new remote repository in the local one
     * 
     * @throws Exception in case of any issue
     */
    public void addRemote(File gitDir, String name)
        throws Exception {
        git.remoteAdd().setName(name).setUri(new URIish(gitDir.toURI().toURL())).call();
    }

    /**
     * Closes this repository and frees resources like file handles used by it.
     * After calling this method this object is no longer usable.
     * <br>
     * If you encouner errors like:<br>
     * <pre>
     *      java.io.IOException: The process cannot access the file because another process has locked a portion of the file
     * </pre>
     * it means you are accessing the repository from multiple instances so you have to invoke this method on
     * instances you're no longer using.
     * 
     * @throws Exception in case of any issue
     */
    public void close()
        throws Exception {
        git.getRepository().close();
        git.close();
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
        return res;
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
     * Returns the list of all commit SHAs by querying the repository starting from HEAD.
     * 
     * @return the list of all commit SHAs by querying the repository starting from HEAD.
     * 
     * @throws Exception in case of any issue
     */
    public List<String> getCommitIDs()
        throws Exception {
        List<String> res = new ArrayList<String>();
        
        ObjectId resolvedHead = git.getRepository().resolve(Constants.HEAD);
        if (Objects.isNull(resolvedHead))
            return res;
        Iterator<RevCommit> commitIterator = git.log().add(resolvedHead).call().iterator();
        while (commitIterator.hasNext())
            res.add(commitIterator.next().getId().getName());

        return res;
    }

    /**
     * Returns the list of all commits by querying the repository starting from HEAD.
     * 
     * @return the list of all commits by querying the repository starting from HEAD.
     * 
     * @throws Exception in case of any issue
     */
    public List<RevCommit> getCommits()
        throws Exception {
        List<RevCommit> res = new ArrayList<RevCommit>();
        
        ObjectId resolvedHead = git.getRepository().resolve(Constants.HEAD);
        if (Objects.isNull(resolvedHead))
            return res;
        Iterator<RevCommit> commitIterator = git.log().add(resolvedHead).call().iterator();
        while (commitIterator.hasNext())
            res.add(commitIterator.next());

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
     * Returns the collection of regular files in the repository root directory.
     * 
     * @return the collection of regular files in the repository root directory.
     * 
     * @throws Exception in case of any issue
     * 
     * @see #addRandomTextWorkbenchFiles(int)
     */
    public Collection<File> getFiles()
        throws Exception {
        Collection<File> res = new ArrayList<File>();
        Collections.addAll(res, git.getRepository().getWorkTree().listFiles((File f) -> f.isFile())); // avoid adding the ".git" directory (and any other directory)
        return res;
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
     * Returns the last commit in the current branch.
     * 
     * @return the last commit in the current branch or {@code null} if the repository has no commits yet
     * 
     * @throws Exception in case of any issue
     */
    public RevCommit getLastCommit()
        throws Exception {
        ObjectId resolvedHead = git.getRepository().resolve(Constants.HEAD);
        if (Objects.isNull(resolvedHead))
            return null;
        Iterator<RevCommit> iterator = git.log().add(resolvedHead).setMaxCount(1).call().iterator();
        if (iterator.hasNext())
            return iterator.next();
        else return null;
    }

    /**
     * Returns the SHA-1 ID of the last commit in the current branch.
     * 
     * @return the SHA-1 ID of the last commit in the current branch or {@code null} if the repository has no commits yet
     * 
     * @throws Exception in case of any issue
     */
    public String getLastCommitID()
        throws Exception {
        RevCommit lastCommit = getLastCommit();
        return Objects.isNull(lastCommit) ? null : lastCommit.getId().getName();
    }

    /**
     * Returns the root commit.
     * 
     * @return the root commit {@code null} if the repository has no commits yet
     * 
     * @throws Exception in case of any issue
     */
    public RevCommit getRootCommit()
        throws Exception {
        ObjectId resolvedHead = git.getRepository().resolve(Constants.HEAD);
        if (Objects.isNull(resolvedHead))
            return null;
        Iterator<RevCommit> iterator = git.log().add(resolvedHead).call().iterator();
        RevCommit res = null;
        while (iterator.hasNext()) {
            res = iterator.next();
            if (res.getParentCount() == 0)
                return res;
        }
        return res;
    }

    /**
     * Returns the SHA-1 ID of the root commit.
     * 
     * @return the SHA-1 ID of the root commit {@code null} if the repository has no commits yet
     * 
     * @throws Exception in case of any issue
     */
    public String getRootCommitID()
        throws Exception {
            return getRootCommit().getId().getName();
    }

    /**
     * Return a list of tag objects, resulting from a query to the underlying repository.
     * 
     * @return a list of tag objects, resulting from a query to the underlying repository. Keys are tag
     * names (without prefix) and values are the tagged objects.
     */
    public Map<String,String> getTags()
        throws Exception {
        Map<String,String> res = new HashMap<String,String>();
        RefDatabase refDatabase = git.getRepository().getRefDatabase();
        for (Ref tagRef: refDatabase.getRefsByPrefix(Constants.R_TAGS)) {
            // refs must be peeled in order to see if they're annotated or lightweight
            tagRef = refDatabase.peel(tagRef);

            // when it's an annotated tag tagRef.getPeeledObjectId() is not null,
            // while for lightweight tags tagRef.getPeeledObjectId() is null
            if (Objects.isNull(tagRef.getPeeledObjectId()))
            {
                res.put(tagRef.getName().replace(Constants.R_TAGS, ""), tagRef.getObjectId().getName());
            }
            else {
                // it's an annotated tag
                res.put(tagRef.getName().replace(Constants.R_TAGS, ""), tagRef.getPeeledObjectId().getName());
            }
        }

        return res;
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
            return res;
        }
    }

    /**
     * Adds all the local changed files to the staging area, without committing.
     * 
     * @throws Exception in case of any issue
     */
    public void stage()
        throws Exception {
        stage(".");
    }

    /**
     * Adds files matched by the given pattern to the staging area, without committing.
     * 
     * @param pattern the pattern of the files to stage (i.e. {@code .} to stage anything)
     * 
     * @throws Exception in case of any issue
     */
    public void stage(String pattern)
        throws Exception {
        git.add().setUpdate(false).addFilepattern(pattern).call();
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
            return git.tag().setAnnotated(false).setObjectId(target).setName(name).call();
        }
        else {
            return git.tag().setAnnotated(true).setObjectId(target).setName(name).setMessage(message).call();
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
     * Returns the working directory for the repository
     * 
     * @return the working directory for the repository
     */
    public final File getWorkingDirectory() {
        return git.getRepository().getWorkTree();
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

    /**
     * Pushes all commits to the default remote repository using no credentials.
     */
    public void push()
        throws Exception {
        push(null, null);
    }

    /**
     * Pushes all commits to the default remote repository using the given credentials.
     * 
     * @param user the optional user name to use when credentials are required.
     * @param password the optional password to use when credentials are required.
     */
    public void push(String user, String password)
        throws Exception {
        push(Constants.DEFAULT_REMOTE_NAME, user, password);
    }

    /**
     * Pushes all commits to the given remote repository using no credentials.
     * 
     * @param remote the remote repository to push to.
     */
    public void push(String remote)
        throws Exception {
        push(remote, null, null);
    }

    /**
     * Pushes all commits to the given remote repository using the given credentials.
     * 
     * @param remote the remote repository to push to.
     * @param user the optional user name to use when credentials are required.
     * @param password the optional password to use when credentials are required.
     */
    public void push(String remote, String user, String password)
        throws Exception {
        git.push().setRefSpecs(new RefSpec(getCurrentBranch().concat(":").concat(getCurrentBranch()))).setRemote(remote).setPushTags().setCredentialsProvider((Objects.isNull(user) && Objects.isNull(password)) ? null : new UsernamePasswordCredentialsProvider(user, password)).call();
    }

    /**
     * Replaces the content of all the files in the repository created using this workbench.

     * @throws Exception in case of any issue
     * 
     * @see #getFiles()
     */
    public void updateAllWorkbenchFiles()
        throws Exception {
        updateWorkbenchFiles(getFiles());
    }

    /**
     * Replaces the content of the given files created using this workbench with new random content.
     * 
     * @param files the collection of files to update
     * 
     * @throws Exception in case of any issue
     * 
     * @see #getFiles()
     */
    public void updateWorkbenchFiles(Collection<File> files)
        throws Exception {
        for (File f: files) {
            FileWriter fw = new FileWriter(f);
            fw.write(RandomUtil.randomAlphabeticString(5));
            fw.flush();
            fw.close();
        }
    }
}