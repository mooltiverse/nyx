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

/**
 * A scenario is a precooked script that you can run all at once to get a repository to a specific state
 * and also retrieve meaningful informations used for testing.
 * 
 * This class provides several scenarios.
 */
public class GitScenario {
    /**
     * Default constructor is private on purpose.
     */
    private GitScenario() {
        super();
    }

    /**
     * A simple scenario with just the initial commit.
     */
    public static class InitialCommit {
        /**
         * Constructor is private on purpose.
         */
        private InitialCommit() {
            super();
        }

        /**
         * Realizes this scenario in a new temporary directory and returns the script that was used.
         * This yields to a repository like:
         * 
         * <pre>
         *   * 1f34b85 (HEAD -> master) Initial commit
         * </pre>
         * 
         * @return the script used to realize the scenario. It can be used for further operations.
         * 
         * @throws Exception in case of any issue.
         */
        public static GitScript realize()
            throws Exception {
            return GitScript.fromScratch().withFiles().andCommit("Initial commit");
        }
    }

    /**
     * A simple scenario with just the initial commit and another one, tagged as '1.2.3'.
     */
    public static class OneTaggedCommitCommit {
        /**
         * Constructor is private on purpose.
         */
        private OneTaggedCommitCommit() {
            super();
        }

        /**
         * Realizes this scenario in a new temporary directory and returns the script that was used.
         * This yields to a repository like:
         * 
         * <pre>
         *   * 4f4ae06 (HEAD -> master, tag: 1.2.3) Commit sry
         *   * 1607ec8 Initial commit
         * </pre>
         * 
         * @return the script used to realize the scenario. It can be used for further operations.
         * 
         * @throws Exception in case of any issue.
         */
        public static GitScript realize()
            throws Exception {
            return GitScript.fromScratch().withFiles().andCommit("Initial commit").addCommitWithTag("1.2.3");
        }
    }

    /**
     * A simple scenario with few commits in the master branch only.
     */
    public static class OneBranchShort {
        /**
         * Constructor is private on purpose.
         */
        private OneBranchShort() {
            super();
        }

        /**
         * Realizes this scenario in a new temporary directory and returns the script that was used.
         * This yields to a repository like:
         * 
         * <pre>
         *   * d84f8b5 (HEAD -> master) Commit mcv
         *   * 285a204 (tag: 0.0.4) Commit trg
         *   * eee5b39 (tag: 0.0.3) Commit kek
         *   * 9acf054 (tag: 0.0.2) Commit vsg
         *   * 3ef4102 (tag: 0.0.1) Commit oof
         *   * 816da12 Initial commit
         * </pre>
         * 
         * @return the script used to realize the scenario. It can be used for further operations.
         * 
         * @throws Exception in case of any issue.
         */
        public static GitScript realize()
            throws Exception {
            return GitScript.fromScratch().withFiles().andCommit("Initial commit")
                .addCommitWithTag("0.0.1")
                .addCommitWithTag("0.0.2", "Annotated tag to commit 0.0.2")
                .addCommitWithTag("0.0.3")
                .addCommitWithTag("0.0.4", "Annotated tag to commit 0.0.4")
                .andCommit();
        }
    }

    /**
     * A simple scenario with two branches (master and alpha) with a no merges between the two
     */
    public static class TwoUnmergedBranchesShort {
        /**
         * Constructor is private on purpose.
         */
        private TwoUnmergedBranchesShort() {
            super();
        }

        /**
         * Realizes this scenario in a new temporary directory and returns the script that was used.
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
         * 
         * @return the script used to realize the scenario. It can be used for further operations.
         * 
         * @throws Exception in case of any issue.
         */
        public static GitScript realize()
            throws Exception {
            return GitScript.fromScratch().withFiles().andCommit("Initial commit")
                .addCommitWithTagInBranch("master", "0.0.1")
                .addCommitWithTagInBranch("master", "0.0.2", "Annotated tag to commit 0.0.2")
                .addCommitWithTagInBranch("master", "0.0.3")
                .addCommitWithTagInBranch("master", "0.0.4", "Annotated tag to commit 0.0.4")
                .addCommitWithTagInBranch("alpha", "0.0.5-alpha.1", "Annotated tag to commit 0.0.5-alpha.1")
                .addCommitWithTagInBranch("master", "0.1.0")
                .addCommitWithTagInBranch("alpha", "0.0.5-alpha.2")
                .addCommitWithTagInBranch("alpha", "0.0.5-alpha.3", "Annotated tag to commit 0.0.5-alpha.3")
                .addCommitWithTagInBranch("master", "0.1.1")
                .addCommitWithTagInBranch("alpha", "0.0.5-alpha.4")
                .addCommitWithTagInBranch("master", "0.1.2")
                .addCommitWithTagInBranch("master", "0.1.3")
                .addCommitWithTagInBranch("master", "0.1.4")
                .addCommitWithTagInBranch("master", "0.1.5");
        }
    }

    /**
     * A simple scenario with two branches (master and alpha) with a few merges between the two,
     * merged at the end and in the middle.
     */
    public static class TwoMergedBranchesShort {
        /**
         * Constructor is private on purpose.
         */
        private TwoMergedBranchesShort() {
            super();
        }

        /**
         * Realizes this scenario in a new temporary directory and returns the script that was used.
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
         * 
         * @return the script used to realize the scenario. It can be used for further operations.
         * 
         * @throws Exception in case of any issue.
         */
        public static GitScript realize()
            throws Exception {
            return GitScript.fromScratch().withFiles().andCommit("Initial commit")
                .addCommitWithTagInBranch("master", "0.0.1")
                .addCommitWithTagInBranch("master", "0.0.2", "Annotated tag to commit 0.0.2")
                .addCommitWithTagInBranch("master", "0.0.3")
                .addCommitWithTagInBranch("master", "0.0.4", "Annotated tag to commit 0.0.4")
                .addCommitWithTagInBranch("alpha", "0.0.6-alpha.1", "Annotated tag to commit 0.0.6-alpha.1")
                .addCommitWithTagInBranch("master", "0.0.5")
                .addCommitWithTagInBranch("alpha", "0.0.6-alpha.2")
                .andMergeIntoWithTag("master", "alpha", "Merge alpha > master", "0.0.6", null)
                .andMergeInto("alpha")
                .addCommitWithTagInBranch("alpha", "0.0.9-alpha.1", "Annotated tag to commit 0.0.9-alpha.1")
                .addCommitWithTagInBranch("master", "0.0.7")
                .addCommitWithTagInBranch("alpha", "0.0.9-alpha.2")
                .addCommitWithTagInBranch("master", "0.0.8")
                .andMergeIntoWithTag("master", "alpha", "Merge alpha > master", "0.0.9", null);
        }
    }
}
