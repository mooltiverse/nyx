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
         * * b171fb9 (HEAD -> master) Initial commit
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
         * * b171fb9 (HEAD -> master) Initial commit
         * * 65e445d (tag: 1.2.3) Commit xfp
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
         * * 65e445d (tag: 0.0.4) Commit xfp
         * * 5685420 (tag: 0.0.3) Commit rit
         * * 1c8e48d (tag: 0.0.2) Commit fux
         * * 86f2d0d (tag: 0.0.1) Commit sfl
         * * f3c543a Initial commit
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
         * * 7740039 (tag: 0.0.5-alpha.4, alpha) Commit fkn
         * * 69dccb3 (tag: 0.0.5-alpha.3) Commit afr
         * * da6971a (tag: 0.0.5-alpha.2) Commit mzt
         * * d8a4cce (tag: 0.0.5-alpha.1) Commit fss
         * | * b9829ae (HEAD -> master, tag: 0.1.5) Commit jux
         * | * 501227b (tag: 0.1.4) Commit tvy
         * | * 7cb8fea (tag: 0.1.3) Commit oxw
         * | * b2aa5ff (tag: 0.1.2) Commit bot
         * | * 9c82c72 (tag: 0.1.1) Commit wyo
         * | * 91567bd (tag: 0.1.0) Commit urq
         * |/  
         * * 23a7e27 (tag: 0.0.4) Commit crp
         * * 78d621d (tag: 0.0.3) Commit bdf
         * * 541b69c (tag: 0.0.2) Commit sdd
         * * 4a0a0f7 (tag: 0.0.1) Commit ott
         * * 1cdc665 Initial commit
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
         * *   571ca16 (HEAD -> master, tag: 0.0.9) Merge alpha > master
         * |\  
         * | * d3112a7 (tag: 0.0.9-alpha.2, alpha) Commit bzx
         * | * 3d98871 (tag: 0.0.9-alpha.1) Commit tsu
         * * | 521a1f0 (tag: 0.0.8) Commit lpu
         * * | 480d11f (tag: 0.0.7) Commit kcv
         * |/  
         * *   1b1b10c (tag: 0.0.6) Merge alpha > master
         * |\  
         * | * 911a9c9 (tag: 0.0.6-alpha.2) Commit kob
         * | * 32c2d8a (tag: 0.0.6-alpha.1) Commit lyb
         * * | d2852d4 (tag: 0.0.5) Commit zuz
         * |/  
         * * 4dcceaf (tag: 0.0.4) Commit dlc
         * * c8c56e4 (tag: 0.0.3) Commit dru
         * * d01ba0d (tag: 0.0.2) Commit pjl
         * * 03df2ba (tag: 0.0.1) Commit kvl
         * * e0ed7ff Initial commit
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
