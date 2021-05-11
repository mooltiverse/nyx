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

import java.io.File;
import java.util.Objects;

import com.mooltiverse.oss.nyx.git.util.FileSystemUtil;

/**
 * A scenario is predefined state of a Git repository that you can realize consistently
 * for test purposes.
 * 
 * Each scenario modelled here provides informations about the outcome of the applied scenario.
 */
public enum Scenario {
    /**
     * The scenario where the Git repository has been created but has no commits at all, not even
     * an initial commit. In this scenario the {@code HEAD} can't be resolved either.
     */
    FROM_SCRATCH( f -> Script.fromScratch(f) ),

    /**
     * The scenario where the Git repository has been created with just the initial commit.
     * The initial commit is not tagged.
     * This yields to a repository like:
     * 
     * <pre>
     *   * 1607ec8 (HEAD -> master) Initial commit
     * </pre>
     */
    INITIAL_COMMIT( f -> Script.fromScratch(f).andAddFiles()
        .andCommit("Initial commit")
    ),

    /**
     * The scenario where the Git repository has been created with just one commit after the initial commit.
     * The latest commit is tagged as {@code 0.1.0}.
     * This yields to a repository like:
     * 
     * <pre>
     *   * 4f4ae06 (HEAD -> master, tag: 0.1.0) Initial version
     *   * 1607ec8 Initial commit
     * </pre>
     */
    INITIAL_VERSION( f -> Script.fromScratch(f).andAddFiles()
        .andCommit("Initial commit")
        .andCommitWithTag("Initial version", "0.1.0", null)
    ),

    /**
     * The scenario where the Git repository has been created with a few tagged commits plus a couple of trailing
     * untagged commits.
     * The latest commit is tagged as {@code 0.4.0}.
     * This yields to a repository like:
     * 
     * <pre>
     *   * cb9423c (HEAD -> master) Untagged commit #2
     *   * ddb0b9f Untagged commit #1
     *   * 44944e7 (tag: 0.0.4) Commit lij
     *   * 92cd45c (tag: 0.0.3) Commit aem
     *   * 9709d14 (tag: 0.0.2) Commit gin
     *   * c996caa (tag: 0.0.1) Commit rfi
     *   * 2b0ce8c Initial commit
     * </pre>
     */
    ONE_BRANCH_SHORT( f -> Script.fromScratch(f).andAddFiles()
        .andCommit("Initial commit")
        .andCommitWithTag("0.0.1")
        .andCommitWithTag("0.0.2", "Annotated tag to commit 0.0.2")
        .andCommitWithTag("0.0.3")
        .andCommitWithTag("0.0.4", "Annotated tag to commit 0.0.4")
        .andCommit("Untagged commit #1")
        .andCommit("Untagged commit #2")
    ),

    /**
     * The scenario where the Git repository has been created with just one commit after the initial commit.
     * The latest tagged commit has overlapping tags, applied in reverse order, and is useful to test which
     * one is selected for bumping.
     * This yields to a repository like:
     * 
     * <pre>
     *   * b875514 (HEAD -> master) Untagged commit #2
     *   * 7c88def Untagged commit #1
     *   * 7d410cf (tag: 0.0.6, tag: 0.0.5, tag: 0.0.4) Commit smm
     *   * c99087c (tag: 0.0.3) Commit vkh
     *   * 7a24383 (tag: 0.0.2) Commit liu
     *   * 5b53015 (tag: 0.0.1) Commit tjk
     *   * 6018fc3 Initial commit
     * </pre>
     */
    ONE_BRANCH_WITH_OVERLAPPING_TAGS( f -> Script.fromScratch(f).andAddFiles()
        .andCommit("Initial commit")
        .andCommitWithTag("0.0.1")
        .andCommitWithTag("0.0.2", "Annotated tag to commit 0.0.2")
        .andCommitWithTag("0.0.3")
        .andCommitWithTag("0.0.6", "Annotated tag to commit 0.0.6")
        .andTag("0.0.5", null)
        .andTag("0.0.4", null)
        .andCommit("Untagged commit #1")
        .andCommit("Untagged commit #2")
    ),

    /**
     * The scenario where the Git repository has been created with two unmerged branches.
     * This yields to a repository like:
     * 
     * <pre>
     *   * 69b89ba (tag: 0.0.5-alpha.4, alpha) Commit lhj
     *   | * 4c18c69 (HEAD -> master, tag: 0.1.5) Commit exx
     *   * | e9a99f3 (tag: 0.0.5-alpha.3) Commit lib
     *   | * 20265d3 (tag: 0.1.4) Commit ftb
     *   * | f45f9d7 (tag: 0.0.5-alpha.2) Commit wys
     *   | * c4525f0 (tag: 0.1.3) Commit mui
     *   * | 0c63c69 (tag: 0.0.5-alpha.1) Commit khw
     *   | * c8d4839 (tag: 0.1.2) Commit olj
     *   | * c9885fa (tag: 0.1.1) Commit pfo
     *   | * 428ee21 (tag: 0.1.0) Commit unp
     *   |/  
     *   * ed65004 (tag: 0.0.4) Commit hvp
     *   * f82dae8 (tag: 0.0.3) Commit nvf
     *   * 4804d5c (tag: 0.0.2) Commit kwd
     *   * c4e58a1 (tag: 0.0.1) Commit wvi
     *   * 74bbba1 Initial commit
     * </pre>
     */
    TWO_BRANCH_SHORT_UNMERGED( f -> Script.fromScratch(f).andAddFiles()
        .andCommit("Initial commit")
        .andCommitWithTagInBranch("master", "0.0.1")
        .andCommitWithTagInBranch("master", "0.0.2", "Annotated tag to commit 0.0.2")
        .andCommitWithTagInBranch("master", "0.0.3")
        .andCommitWithTagInBranch("master", "0.0.4", "Annotated tag to commit 0.0.4")
        .andCommitWithTagInBranch("alpha", "0.0.5-alpha.1", "Annotated tag to commit 0.0.5-alpha.1")
        .andCommitWithTagInBranch("master", "0.1.0")
        .andCommitWithTagInBranch("alpha", "0.0.5-alpha.2")
        .andCommitWithTagInBranch("alpha", "0.0.5-alpha.3", "Annotated tag to commit 0.0.5-alpha.3")
        .andCommitWithTagInBranch("master", "0.1.1")
        .andCommitWithTagInBranch("alpha", "0.0.5-alpha.4")
        .andCommitWithTagInBranch("master", "0.1.2")
        .andCommitWithTagInBranch("master", "0.1.3")
        .andCommitWithTagInBranch("master", "0.1.4")
        .andCommitWithTagInBranch("master", "0.1.5")
    ),

    /**
     * The scenario where the Git repository has been created with two merged branches.
     * This yields to a repository like:
     * 
     * <pre>
     *   *   d372fcf (HEAD -> master, tag: 0.0.9) Merge alpha > master
     *   |\  
     *   * | b59e872 (tag: 0.0.8) Commit zmg
     *   | * 7044f5c (tag: 0.0.9-alpha.2, alpha) Commit jmy
     *   * | 89b80e6 (tag: 0.0.7) Commit voy
     *   | * e7398b5 (tag: 0.0.9-alpha.1) Commit xod
     *   |/  
     *   *   c77fae5 (tag: 0.0.6) Merge alpha > master
     *   |\  
     *   * | 7557baa (tag: 0.0.5) Commit gfq
     *   | * 666b6d3 (tag: 0.0.6-alpha.2) Commit dix
     *   | * 6f2f878 (tag: 0.0.6-alpha.1) Commit owf
     *   |/  
     *   * 6dab481 (tag: 0.0.4) Commit fle
     *   * fb5e77e (tag: 0.0.3) Commit xmi
     *   * 77dd131 (tag: 0.0.2) Commit vdw
     *   * 9c50fe4 (tag: 0.0.1) Commit aei
     *   * 29affd6 Initial commit
     * </pre>
     */
    TWO_BRANCH_SHORT_MERGED( f -> Script.fromScratch(f).andAddFiles()
        .andCommit("Initial commit")
        .andCommitWithTagInBranch("master", "0.0.1")
        .andCommitWithTagInBranch("master", "0.0.2", "Annotated tag to commit 0.0.2")
        .andCommitWithTagInBranch("master", "0.0.3")
        .andCommitWithTagInBranch("master", "0.0.4", "Annotated tag to commit 0.0.4")
        .andCommitWithTagInBranch("alpha", "0.0.6-alpha.1", "Annotated tag to commit 0.0.6-alpha.1")
        .andCommitWithTagInBranch("master", "0.0.5")
        .andCommitWithTagInBranch("alpha", "0.0.6-alpha.2")
        .andMergeIntoWithTag("master", "alpha", "Merge alpha > master", "0.0.6", null)
        .andMergeInto("alpha")
        .andCommitWithTagInBranch("alpha", "0.0.9-alpha.1", "Annotated tag to commit 0.0.9-alpha.1")
        .andCommitWithTagInBranch("master", "0.0.7")
        .andCommitWithTagInBranch("alpha", "0.0.9-alpha.2")
        .andCommitWithTagInBranch("master", "0.0.8")
        .andMergeIntoWithTag("master", "alpha", "Merge alpha > master", "0.0.9", null)
    );

    /**
     * The function to run in order to apply the scenario to a given directory. The function
     * takes the Git repository directory as input and returns the script used to realize
     * the scenario.
     */
    private final ScenarioFunction function;

    /**
     * Constructor.
     * 
     * @param function the function to run in order to apply the scenario to a given directory.
     */
    private Scenario(ScenarioFunction function) {
        Objects.requireNonNull(function, "Scenario can't be built with a null function");
        this.function = function;
    }

    /**
     * Applies the scenario in the given directory and returns the script that was used.
     * The returned script can be used to inspect the repository or perform further actions.
     * 
     * @param the directory to apply the scenario in. It must be not {@code null}, it must exist
     * and contain a valid Git repository. Previous history of the repository is left unchanged.
     * The script is being applied starting from the current branch in the given repository.
     * 
     * @return the script used to realize the scenario. It can be used for further operations.
     * 
     * @throws Exception in case of any issue.
     */
    public Script apply(File directory)
        throws Exception {
        return function.apply(directory);
    }

    /**
     * Realizes the scenario in a new temporary directory and returns the script that was used.
     * The returned script can be used to inspect the repository or perform further actions.
     * 
     * @return the script used to realize the scenario. It can be used for further operations.
     * 
     * @throws Exception in case of any issue.
     */
    public Script realize()
        throws Exception {
        return function.apply(FileSystemUtil.newTempDirectory(null, "scenario"));
    }

    /**
     * The function that realizes a scenario in a given directory and returns the script
     * used to realize the scenario.
     * 
     * This functional interface looks pretty much like {@link java.util.function.Function} but
     * allows exceptions to be thrown.
     */
    @FunctionalInterface
    private interface ScenarioFunction {
        /**
         * Applies the function to the given directory and returns the script used to
         * realize the scenario.
         * 
         * @param directory the directory to apply the scenario in
         * 
         * @return the script used to apply the scenario
         * 
         * @throws Exception in case of any issue
         */
        Script apply(File directory)
            throws Exception;
    }
}
