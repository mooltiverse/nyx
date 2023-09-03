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

package tools

import (
	gitutil "github.com/mooltiverse/nyx/modules/go/nyx/test/integration/git/util"
	utl "github.com/mooltiverse/nyx/modules/go/utils"
)

/*
The type that actual scenario functions must use to apply/realize the scenario using the given script.

Arguments are as follows:

- directory the Git working directory
*/
type scenarioFunction func(directory string) Script

/*
A scenario is predefined state of a Git repository that you can realize consistently
for test purposes.
PointerToString
Each scenario modelled here provides informations about the outcome of the applied scenario.
*/
type Scenario struct {
	// the function to use to realize the scenario
	function scenarioFunction
}

/*
The scenario with a bare repository (useful as a remote).
*/
func BARE() Scenario {
	return Scenario{function: func(directory string) Script {
		return From(directory)
	}}
}

/*
The scenario where the Git repository has been created but has no commits at all, not even
an initial commit. In this scenario the HEAD can't be resolved either.
*/
func FROM_SCRATCH() Scenario {
	return Scenario{function: func(directory string) Script {
		return From(directory)
	}}
}

/*
The scenario where the Git repository has been created with just the initial commit.
The initial commit is not tagged.
This yields to a repository like:

  - 1607ec8 (HEAD -> master) Initial commit
*/
func INITIAL_COMMIT() Scenario {
	return Scenario{function: func(directory string) Script {
		return From(directory).AndAddFiles().AndCommitWith(utl.PointerToString("Initial commit"))
	}}
}

/*
The scenario where the Git repository has been created with just one commit after the initial commit.
The latest commit is tagged as 0.1.0.
This yields to a repository like:

  - 4f4ae06 (HEAD -> master, tag: 0.1.0) Initial version
  - 1607ec8 Initial commit
*/
func INITIAL_VERSION() Scenario {
	return Scenario{function: func(directory string) Script {
		return From(directory).AndAddFiles().AndCommitWith(utl.PointerToString("Initial commit")).AndCommitWithMessageAndTagNameAndMessage(utl.PointerToString("Initial commit"), "0.1.0", nil)
	}}
}

/*
The scenario where the Git repository has been created with a few tagged commits plus a couple of trailing
untagged commits.
The latest commit is tagged as 0.4.0.
This yields to a repository like:

  - cb9423c (HEAD -> master) Untagged commit #2
  - ddb0b9f Untagged commit #1
  - 44944e7 (tag: 0.0.4) Commit lij
  - 92cd45c (tag: 0.0.3) Commit aem
  - 9709d14 (tag: 0.0.2) Commit gin
  - c996caa (tag: 0.0.1) Commit rfi
  - 2b0ce8c Initial commit
*/
func ONE_BRANCH_SHORT() Scenario {
	return Scenario{function: func(directory string) Script {
		return From(directory).AndAddFiles().AndCommitWith(utl.PointerToString("Initial commit")).AndCommitWithTag("0.0.1").AndCommitWithTagNameAndMessage("0.0.2", utl.PointerToString("Annotated tag to commit 0.0.2")).AndCommitWithTag("0.0.3").AndCommitWithTagNameAndMessage("0.0.4", utl.PointerToString("Annotated tag to commit 0.0.4")).AndCommitWith(utl.PointerToString("Untagged commit #1")).AndCommitWith(utl.PointerToString("Untagged commit #2"))
	}}
}

/*
The scenario where the Git repository has been created with a few tagged commits plus a couple of trailing
untagged commits.
Commit messages are formatted as conventional commits.
The latest commit is tagged as 0.4.0.
This yields to a repository like:

  - cb9423c (HEAD -> master) feat: Untagged commit #2
  - ddb0b9f fix: Untagged commit #1
  - 44944e7 (tag: 0.0.4) Commit lij
  - 92cd45c (tag: 0.0.3) Commit aem
  - 9709d14 (tag: 0.0.2) Commit gin
  - c996caa (tag: 0.0.1) Commit rfi
  - 2b0ce8c Initial commit
*/
func ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS() Scenario {
	return Scenario{function: func(directory string) Script {
		return From(directory).AndAddFiles().AndCommitWith(utl.PointerToString("Initial commit")).AndCommitWithTag("0.0.1").AndCommitWithTagNameAndMessage("0.0.2", utl.PointerToString("fix: Annotated tag to commit 0.0.2")).AndCommitWithTag("0.0.3").AndCommitWithTagNameAndMessage("0.0.4", utl.PointerToString("feat: Annotated tag to commit 0.0.4")).AndCommitWith(utl.PointerToString("fix: Untagged commit #1")).AndCommitWith(utl.PointerToString("feat: Untagged commit #2"))
	}}
}

/*
The scenario where the Git repository has been created with a few tagged commits plus an untagged commit.
Commit messages are formatted as conventional commits for merge.
The latest commit is tagged as 0.4.0.
This yields to a repository like:

  - cb9423c (HEAD -> master) feat: Alpha
  - 44944e7 (tag: 0.0.4) Commit lij
  - 92cd45c (tag: 0.0.3) Commit aem
  - 9709d14 (tag: 0.0.2) Commit gin
  - c996caa (tag: 0.0.1) Commit rfi
  - 2b0ce8c Initial commit
*/
func ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS_FOR_MERGE() Scenario {
	return Scenario{function: func(directory string) Script {
		return From(directory).AndAddFiles().AndCommitWith(utl.PointerToString("Initial commit")).AndCommitWithTag("0.0.1").AndCommitWithTagNameAndMessage("0.0.2", utl.PointerToString("fix: Annotated tag to commit 0.0.2")).AndCommitWithTag("0.0.3").AndCommitWithTagNameAndMessage("0.0.4", utl.PointerToString("feat: Annotated tag to commit 0.0.4")).AndCommitWith(utl.PointerToString("Alpha\n\n* fix: patch #1\n\n* feat: minor #1\n\n* feat!: major #1"))
	}}
}

/*
The scenario where the Git repository has been created with just one commit after the initial commit.
The latest tagged commit has overlapping tags, applied in reverse order, and is useful to test which
one is selected for bumping.
This yields to a repository like:

  - b875514 (HEAD -> master) Untagged commit #2
  - 7c88def Untagged commit #1
  - 7d410cf (tag: 0.0.6, tag: 0.0.5, tag: 0.0.4) Commit smm
  - c99087c (tag: 0.0.3) Commit vkh
  - 7a24383 (tag: 0.0.2) Commit liu
  - 5b53015 (tag: 0.0.1) Commit tjk
  - 6018fc3 Initial commit
*/
func ONE_BRANCH_WITH_OVERLAPPING_TAGS() Scenario {
	return Scenario{function: func(directory string) Script {
		return From(directory).AndAddFiles().AndCommitWith(utl.PointerToString("Initial commit")).AndCommitWithTag("0.0.1").AndCommitWithTagNameAndMessage("0.0.2", utl.PointerToString("Annotated tag to commit 0.0.2")).AndCommitWithTag("0.0.3").AndCommitWithTagNameAndMessage("0.0.6", utl.PointerToString("Annotated tag to commit 0.0.6")).AndTag("0.0.5", nil).AndTag("0.0.4", nil).AndCommitWith(utl.PointerToString("Untagged commit #1")).AndCommitWith(utl.PointerToString("Untagged commit #2"))
	}}
}

/*
The scenario where the Git repository has been created with two unmerged branches.
This yields to a repository like:

  - 69b89ba (tag: 0.0.5-alpha.4, alpha) Commit lhj
    | * 4c18c69 (HEAD -> master, tag: 0.1.5) Commit exx
  - | e9a99f3 (tag: 0.0.5-alpha.3) Commit lib
    | * 20265d3 (tag: 0.1.4) Commit ftb
  - | f45f9d7 (tag: 0.0.5-alpha.2) Commit wys
    | * c4525f0 (tag: 0.1.3) Commit mui
  - | 0c63c69 (tag: 0.0.5-alpha.1) Commit khw
    | * c8d4839 (tag: 0.1.2) Commit olj
    | * c9885fa (tag: 0.1.1) Commit pfo
    | * 428ee21 (tag: 0.1.0) Commit unp
    |/
  - ed65004 (tag: 0.0.4) Commit hvp
  - f82dae8 (tag: 0.0.3) Commit nvf
  - 4804d5c (tag: 0.0.2) Commit kwd
  - c4e58a1 (tag: 0.0.1) Commit wvi
  - 74bbba1 Initial commit
*/
func TWO_BRANCH_SHORT_UNMERGED() Scenario {
	return Scenario{function: func(directory string) Script {
		return From(directory).AndAddFiles().AndCommitWith(utl.PointerToString("Initial commit")).AndCommitWithTagInBranch("master", "0.0.1").AndCommitWithTagMessageInBranch("master", "0.0.2", utl.PointerToString("Annotated tag to commit 0.0.2")).AndCommitWithTagInBranch("master", "0.0.3").AndCommitWithTagMessageInBranch("master", "0.0.4", utl.PointerToString("Annotated tag to commit 0.0.4")).AndCommitWithTagMessageInBranch("alpha", "0.0.5-alpha.1", utl.PointerToString("Annotated tag to commit 0.0.5-alpha.1")).AndCommitWithTagInBranch("master", "0.1.0").AndCommitWithTagInBranch("alpha", "0.0.5-alpha.2").AndCommitWithTagMessageInBranch("alpha", "0.0.5-alpha.3", utl.PointerToString("Annotated tag to commit 0.0.5-alpha.3")).AndCommitWithTagInBranch("master", "0.1.1").AndCommitWithTagInBranch("alpha", "0.0.5-alpha.4").AndCommitWithTagInBranch("master", "0.1.2").AndCommitWithTagInBranch("master", "0.1.3").AndCommitWithTagInBranch("master", "0.1.4").AndCommitWithTagInBranch("master", "0.1.5")
	}}
}

/*
The scenario where the Git repository has been created with two merged branches.
This yields to a repository like:

  - d372fcf (HEAD -> master, tag: 0.0.9) Merge alpha > master
    |\
  - | b59e872 (tag: 0.0.8) Commit zmg
    | * 7044f5c (tag: 0.0.9-alpha.2, alpha) Commit jmy
  - | 89b80e6 (tag: 0.0.7) Commit voy
    | * e7398b5 (tag: 0.0.9-alpha.1) Commit xod
    |/
  - c77fae5 (tag: 0.0.6) Merge alpha > master
    |\
  - | 7557baa (tag: 0.0.5) Commit gfq
    | * 666b6d3 (tag: 0.0.6-alpha.2) Commit dix
    | * 6f2f878 (tag: 0.0.6-alpha.1) Commit owf
    |/
  - 6dab481 (tag: 0.0.4) Commit fle
  - fb5e77e (tag: 0.0.3) Commit xmi
  - 77dd131 (tag: 0.0.2) Commit vdw
  - 9c50fe4 (tag: 0.0.1) Commit aei
  - 29affd6 Initial commit
*/
func TWO_BRANCH_SHORT_MERGED() Scenario {
	return Scenario{function: func(directory string) Script {
		return From(directory).AndAddFiles().AndCommitWith(utl.PointerToString("Initial commit")).AndCommitWithTagInBranch("master", "0.0.1").AndCommitWithTagMessageInBranch("master", "0.0.2", utl.PointerToString("Annotated tag to commit 0.0.2")).AndCommitWithTagInBranch("master", "0.0.3").AndCommitWithTagMessageInBranch("master", "0.0.4", utl.PointerToString("Annotated tag to commit 0.0.4")).AndCommitWithTagMessageInBranch("alpha", "0.0.6-alpha.1", utl.PointerToString("Annotated tag to commit 0.0.6-alpha.1")).AndCommitWithTagInBranch("master", "0.0.5").AndCommitWithTagInBranch("alpha", "0.0.6-alpha.2").AndMergeFromIntoWithMessageAndTagMessage("master", "alpha", utl.PointerToString("'Merge alpha > master'"), "0.0.6", nil).AndMergeInto("alpha").AndCommitWithTagMessageInBranch("alpha", "0.0.9-alpha.1", utl.PointerToString("Annotated tag to commit 0.0.9-alpha.1")).AndCommitWithTagInBranch("master", "0.0.7").AndCommitWithTagInBranch("alpha", "0.0.9-alpha.2").AndCommitWithTagInBranch("master", "0.0.8").AndMergeFromIntoWithMessageAndTagMessage("master", "alpha", utl.PointerToString("'Merge alpha > master'"), "0.0.9", nil)
	}}
}

/*
The scenario where the Git repository has been created with five unmerged branches
and commit messages bring the identifier to bump.
This yields to a repository like:

  - 4138eb0 (tag: 0.3.3, taggedwithbump) patch 3
  - ea77e30 (tag: 0.3.2) minor 2
  - 0a5a452 (tag: 0.3.1) patch 1
    | * 58f0450 (tag: 0.2.3, taggedwithoutbump) Commit cxl
    | * 5a25593 (tag: 0.2.2) Commit ale
    | * 125a5f4 (tag: 0.2.1) Commit its
    |/
    | * fe94837 (untaggedwithbump) patch 6
    | * f6819f5 minor 5
    | * 79977b5 patch 4
    |/
    | * 387677a (untaggedwithoutbump) Commit kyh
    | * 095690e Commit gdq
    | * 5484622 Commit fhy
    |/
  - cd259bc (HEAD -> gamma, tag: 0.0.1, master)
  - 48f4b7f Initial commit
*/
func FIVE_BRANCH_UNMERGED_BUMPING() Scenario {
	return Scenario{function: func(directory string) Script {
		return From(directory).AndAddFiles().AndCommitWith(utl.PointerToString("Initial commit")).InBranch("master").AndAddFiles().AndCommitWith(utl.PointerToString("")).AndTag("0.0.1", nil).InBranch("master").InBranch("taggedwithoutbump").AndAddFiles().AndCommitWith(nil).AndTag("0.2.1", nil).InBranch("taggedwithoutbump").AndAddFiles().AndCommitWith(nil).AndTag("0.2.2", nil).InBranch("taggedwithoutbump").AndAddFiles().AndCommitWith(nil).AndTag("0.2.3", nil).InBranch("master").InBranch("taggedwithbump").AndAddFiles().AndCommitWith(utl.PointerToString("patch 1")).AndTag("0.3.1", nil).InBranch("taggedwithbump").AndAddFiles().AndCommitWith(utl.PointerToString("minor 2")).AndTag("0.3.2", nil).InBranch("taggedwithbump").AndAddFiles().AndCommitWith(utl.PointerToString("patch 3")).AndTag("0.3.3", nil).InBranch("master").InBranch("untaggedwithoutbump").AndAddFiles().AndCommitWith(nil).InBranch("untaggedwithoutbump").AndAddFiles().AndCommitWith(nil).InBranch("untaggedwithoutbump").AndAddFiles().AndCommitWith(nil).InBranch("master").InBranch("untaggedwithbump").AndAddFiles().AndCommitWith(utl.PointerToString("patch 4")).InBranch("untaggedwithbump").AndAddFiles().AndCommitWith(utl.PointerToString("minor 5")).InBranch("untaggedwithbump").AndAddFiles().AndCommitWith(utl.PointerToString("patch 6")).InBranch("master")
	}}
}

/*
The scenario where the Git repository has been created with five unmerged branches
and commit messages bring the identifier to bump. The repository is assumed to use
collapsed versioning.
This yields to a repository like:

  - a9ee0e0 (eta) Commit glq
  - 519c3e8 (tag: 1.0.0-eta.3) patch 12
  - ba9df35 (tag: 1.0.0-eta.2) minor 11
  - d8602a5 (tag: 1.0.0-eta.1) patch 10
    | * b172638 (theta) patch 15
    | * 51f98e1 minor 14
    | * df952e6 patch 13
    |/
    | * 3a4ea57 (tag: 1.0.0-alpha.3, alpha) Commit blo
    | * 34db654 (tag: 1.0.0-alpha.2) Commit ubp
    | * bf0ac9e (tag: 1.0.0-alpha.1) Commit mms
    |/
    | * 98a699c (tag: 1.0.0-beta.3, beta) patch 3
    | * a18393b (tag: 1.0.0-beta.2) minor 2
    | * d6f23df (tag: 1.0.0-beta.1) patch 1
    |/
    | * f146e94 (HEAD -> delta) patch 6
    | * 3516d03 minor 5
    | * 70c1691 patch 4
    |/
    | * 0e8d6c2 (epsilon) Commit syw
    | * 4dd91b8 (tag: 1.0.0-epsilon.3) Commit zdr
    | * ff60ee7 (tag: 1.0.0-epsilon.2) Commit sbq
    | * e8e2e83 (tag: 1.0.0-epsilon.1) Commit umm
    |/
    | * 025a281 (gamma) Commit nrj
    | * e61fd7a Commit bmq
    | * 0cf681d Commit fsv
    |/
    | * 12dd283 (tag: 1.0.0-zeta.4, zeta) Commit dyf
    | * 3093108 (tag: 1.0.0-zeta.3) patch 9
    | * 2700ace (tag: 1.0.0-zeta.2) minor 8
    | * 6e37362 (tag: 1.0.0-zeta.1) patch 7
    |/
  - eb38e63 (tag: 0.0.1, master)
  - c853750 Initial commit
*/
func FIVE_BRANCH_UNMERGED_BUMPING_COLLAPSED() Scenario {
	return Scenario{function: func(directory string) Script {
		return From(directory).AndAddFiles().AndCommitWith(utl.PointerToString("Initial commit")).InBranch("master").AndAddFiles().AndCommitWith(utl.PointerToString("")).AndTag("0.0.1", nil).InBranch("master").InBranch("alpha").AndAddFiles().AndCommitWith(nil).AndTag("1.0.0-alpha.1", nil).InBranch("alpha").AndAddFiles().AndCommitWith(nil).AndTag("1.0.0-alpha.2", nil).InBranch("alpha").AndAddFiles().AndCommitWith(nil).AndTag("1.0.0-alpha.3", nil).InBranch("master").InBranch("beta").AndAddFiles().AndCommitWith(utl.PointerToString("patch 1")).AndTag("1.0.0-beta.1", nil).InBranch("beta").AndAddFiles().AndCommitWith(utl.PointerToString("minor 2")).AndTag("1.0.0-beta.2", nil).InBranch("beta").AndAddFiles().AndCommitWith(utl.PointerToString("patch 3")).AndTag("1.0.0-beta.3", nil).InBranch("master").InBranch("gamma").AndAddFiles().AndCommitWith(nil).InBranch("gamma").AndAddFiles().AndCommitWith(nil).InBranch("gamma").AndAddFiles().AndCommitWith(nil).InBranch("master").InBranch("delta").AndAddFiles().AndCommitWith(utl.PointerToString("patch 4")).InBranch("delta").AndAddFiles().AndCommitWith(utl.PointerToString("minor 5")).InBranch("delta").AndAddFiles().AndCommitWith(utl.PointerToString("patch 6")).InBranch("master").InBranch("epsilon").AndAddFiles().AndCommitWith(nil).AndTag("1.0.0-epsilon.1", nil).InBranch("epsilon").AndAddFiles().AndCommitWith(nil).AndTag("1.0.0-epsilon.2", nil).InBranch("epsilon").AndAddFiles().AndCommitWith(nil).AndTag("1.0.0-epsilon.3", nil).InBranch("epsilon").AndAddFiles().AndCommitWith(nil).InBranch("master").InBranch("zeta").AndAddFiles().AndCommitWith(utl.PointerToString("patch 7")).AndTag("1.0.0-zeta.1", nil).InBranch("zeta").AndAddFiles().AndCommitWith(utl.PointerToString("minor 8")).AndTag("1.0.0-zeta.2", nil).InBranch("zeta").AndAddFiles().AndCommitWith(utl.PointerToString("patch 9")).AndTag("1.0.0-zeta.3", nil).InBranch("zeta").AndAddFiles().AndCommitWith(nil).AndTag("1.0.0-zeta.4", nil).InBranch("master").InBranch("eta").AndAddFiles().AndCommitWith(utl.PointerToString("patch 10")).AndTag("1.0.0-eta.1", nil).InBranch("eta").AndAddFiles().AndCommitWith(utl.PointerToString("minor 11")).AndTag("1.0.0-eta.2", nil).InBranch("eta").AndAddFiles().AndCommitWith(utl.PointerToString("patch 12")).AndTag("1.0.0-eta.3", nil).InBranch("eta").AndAddFiles().AndCommitWith(nil).InBranch("master").InBranch("theta").AndAddFiles().AndCommitWith(utl.PointerToString("patch 13")).InBranch("theta").AndAddFiles().AndCommitWith(utl.PointerToString("minor 14")).InBranch("theta").AndAddFiles().AndCommitWith(utl.PointerToString("patch 15")).InBranch("master")
	}}
}

/*
The scenario where the Git repository has been created with several branches
(all those contemplated in the Extended preset).

Branches are:

- master: a mainline branch with a few tagged commits
- main: a mainline branch with a a few non tagged commits
- integration: an integration branch with a few tagged commits
- development: an integration branch with a few non tagged commits
- alpha: a maturity branch with a few tagged commits
- beta: a maturity branch with a few tagged commits
- gamma: a maturity branch with a few non tagged commits
- v0.x: a maintenance branch with a few tagged commits
- v1.x: a maintenance branch with a few non tagged commits
- rel/0.x: a release branch with a few tagged commits
- rel/1.x: a release branch with a few non tagged commits
- feature/SSO: a feature branch with a few tagged commits
- feature/IN-12345: a feature branch with a few non tagged commits
- hotfix-98765: an hotfix branch with a few tagged commits
- somebranch: a generic branch for 'internal' contributions with a few tagged commits
- someotherbranch: a generic branch for 'internal' contributions with a few non tagged commits

This yields to a repository like:

  - 8f3dd93 (beta) Untagged commit #1 in branch beta
  - a297e00 (tag: 0.0.6-beta.2) Commit xde
  - d6b24a8 (tag: 0.0.6-beta.1) Commit xjl
  - 46ec855 (alpha) Untagged commit in branch alpha
  - c162a55 (tag: 0.0.6-alpha.2) Commit lxy
  - abbb668 (tag: 0.0.6-alpha.1) Commit djf
    | * e6781ca (gamma) Untagged commit #3 in branch gamma
    | * c2c5458 Untagged commit #2 in branch gamma
    | * 53ab7c1 Untagged commit #1 in branch gamma
    |/
    | * 3c1bd3e (HEAD -> internal) Untagged commit #1 in branch internal
    | * 1180b5e (tag: 0.0.6-internal.1+timestamp.003) Commit gqe
    | * 5363e1c (tag: 0.0.6-internal.1+timestamp.002) Commit vfi
    | * c753c3c (tag: 0.0.6-internal.1+timestamp.001) Commit mto
    |/
    | * af9a14c (tag: tag3, somebranch) Commit fho
    | * 5c5a667 (tag: tag2) Commit yyo
    | * 1560350 (tag: tag1) Commit ehw
    |/
    | * 5abba0a (someotherbranch) Untagged commit #3 in branch someotherbranch
    | * a04254a Untagged commit #2 in branch someotherbranch
    | * 0a10479 Untagged commit #1 in branch someotherbranch
    | | * 5245a52 (feature/IN-12345) Untagged commit #3 in branch feature/IN-12345
    | | * 4d5a3c2 Untagged commit #2 in branch feature/IN-12345
    | | * c0ec7d2 Untagged commit #1 in branch feature/IN-12345
    | |/
    |/|
    | | * f14c4c5 (feature/SSO) Untagged commit in branch feature/SSO
    | | * fd313d1 (tag: 0.0.6-featuresso.2) Commit swa
    | | * 0c93cdc (tag: 0.0.6-featuresso.1) Commit nxl
    | |/
    |/|
  - | c0cface (integration) Untagged commit in branch integration
  - | db26bd2 (tag: 0.0.6-integration.2) Commit xkk
    |/
  - 8673f95 (tag: 0.0.6-integration.1, master) Commit jad
    | * fe6db90 (hotfix-98765) Untagged commit in branch hotfix-98765
    | * f352f79 (tag: 0.0.8-hotfix98765.2) Commit fee
    | * 4b5687a (tag: 0.0.8-hotfix98765.1) Commit ogx
    | * 890e188 (v0.x) Untagged commit in branch v0.x
    | * d69e85b (tag: 0.0.7-v0x.1) Commit vvv
    | * 47aa910 (tag: 0.0.7) Commit uie
    | * 5b54a35 (tag: 0.0.6-v0x.3) Commit azr
    | * 39e3879 (tag: 0.0.6-v0x.2) Commit hdl
    | * 40c12cf (tag: 0.0.6-v0x.1) Commit aqi
    |/
    | * 7505531 (rel/1.x) Untagged commit #3 in branch rel/1.x
    | * 82eab49 Untagged commit #2 in branch rel/1.x
    | * fb6fa06 Untagged commit #1 in branch rel/1.x
    |/
    | * c55f82d (development) Untagged commit #3 in branch development
    | * dd47ac7 Untagged commit #2 in branch development
    | * 913e54e Untagged commit #1 in branch development
    | | * bc81e17 (rel/0.x) Untagged commit in branch rel/0.x
    | | * 636dff5 (tag: 0.0.6-rel.2) Commit kos
    | | * 99ae8fd (tag: 0.0.6-rel.1) Commit ecs
    | |/
    |/|
    | | * 290ef69 (v1.x) Untagged commit #3 in branch v1.x
    | | * b0e518b Untagged commit #2 in branch v1.x
    | | * b558e3d Untagged commit #1 in branch v1.x
    | |/
    |/|
  - | b282b60 Untagged commit in branch master
  - | 1a775e1 (tag: 0.0.5) Commit uap
  - | a7e5922 (tag: 0.0.4) Commit iqy
  - | 0861112 (tag: 0.0.3) Commit els
  - | 4d725fd (tag: 0.0.2) Commit wrj
  - | 330768c (tag: 0.0.1) Commit zwj
    |/
    | * 341ce36 (main) Untagged commit #3 in branch main
    | * 51fac9e Untagged commit #2 in branch main
    | * a985699 Untagged commit #1 in branch main
    |/
  - eaaa818 Initial commit
*/
func EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED() Scenario {
	return Scenario{function: func(directory string) Script {
		return From(directory).AndAddFiles().AndCommitWith(utl.PointerToString("Initial commit")).InBranch("master").InBranch("main").AndAddFiles().AndStage().AndCommitWith(utl.PointerToString("Untagged commit #1 in branch main")).AndAddFiles().AndStage().AndCommitWith(utl.PointerToString("Untagged commit #2 in branch main")).AndAddFiles().AndStage().AndCommitWith(utl.PointerToString("Untagged commit #3 in branch main")).InBranch("master").InBranch("development").AndAddFiles().AndStage().AndCommitWith(utl.PointerToString("Untagged commit #1 in branch development")).AndAddFiles().AndStage().AndCommitWith(utl.PointerToString("Untagged commit #2 in branch development")).AndAddFiles().AndStage().AndCommitWith(utl.PointerToString("Untagged commit #3 in branch development")).InBranch("master").AndCommitWithTagInBranch("master", "0.0.1").AndCommitWithTagMessageInBranch("master", "0.0.2", utl.PointerToString("Annotated tag to commit 0.0.2")).AndCommitWithTagInBranch("master", "0.0.3").AndCommitWithTagMessageInBranch("master", "0.0.4", utl.PointerToString("Annotated tag to commit 0.0.4")).AndCommitWithTagInBranch("master", "0.0.5").AndAddFiles().AndStage().AndCommitWith(utl.PointerToString("Untagged commit in branch master")).InBranch("master").AndCommitWithTagMessageInBranch("v0.x", "0.0.6-v0x.1", utl.PointerToString("Annotated tag to commit 0.0.6-v0x.1")).AndCommitWithTagMessageInBranch("v0.x", "0.0.6-v0x.2", utl.PointerToString("Annotated tag to commit 0.0.6-v0x.2")).AndCommitWithTagInBranch("v0.x", "0.0.6-v0x.3").AndCommitWithTagMessageInBranch("v0.x", "0.0.7", utl.PointerToString("Annotated tag to commit 0.0.7")).AndCommitWithTagMessageInBranch("v0.x", "0.0.7-v0x.1", utl.PointerToString("Annotated tag to commit 0.0.7-v0x.1")).AndAddFiles().AndStage().AndCommitWith(utl.PointerToString("Untagged commit in branch v0.x")).InBranch("master").InBranch("v1.x").AndAddFiles().AndStage().AndCommitWith(utl.PointerToString("Untagged commit #1 in branch v1.x")).AndAddFiles().AndStage().AndCommitWith(utl.PointerToString("Untagged commit #2 in branch v1.x")).AndAddFiles().AndStage().AndCommitWith(utl.PointerToString("Untagged commit #3 in branch v1.x")).InBranch("master").AndCommitWithTagMessageInBranch("rel/0.x", "0.0.6-rel.1", utl.PointerToString("Annotated tag to commit 0.0.6-rel.1")).AndCommitWithTagInBranch("rel/0.x", "0.0.6-rel.2").AndAddFiles().AndStage().AndCommitWith(utl.PointerToString("Untagged commit in branch rel/0.x")).InBranch("master").InBranch("rel/1.x").AndAddFiles().AndStage().AndCommitWith(utl.PointerToString("Untagged commit #1 in branch rel/1.x")).AndAddFiles().AndStage().AndCommitWith(utl.PointerToString("Untagged commit #2 in branch rel/1.x")).AndAddFiles().AndStage().AndCommitWith(utl.PointerToString("Untagged commit #3 in branch rel/1.x")).InBranch("master").AndCommitWithTagMessageInBranch("integration", "0.0.6-integration.1", utl.PointerToString("Annotated tag to commit 0.0.6-integration.1")).AndMergeInto("master").InBranch("integration").AndCommitWithTagInBranch("integration", "0.0.6-integration.2").AndAddFiles().AndStage().AndCommitWith(utl.PointerToString("Untagged commit in branch integration")).InBranch("integration").AndCommitWithTagMessageInBranch("feature/SSO", "0.0.6-featuresso.1", utl.PointerToString("Annotated tag to commit 0.0.6-featuresso.1")).AndCommitWithTagInBranch("feature/SSO", "0.0.6-featuresso.2").AndAddFiles().AndStage().AndCommitWith(utl.PointerToString("Untagged commit in branch feature/SSO")).InBranch("integration").InBranch("feature/IN-12345").AndAddFiles().AndStage().AndCommitWith(utl.PointerToString("Untagged commit #1 in branch feature/IN-12345")).AndAddFiles().AndStage().AndCommitWith(utl.PointerToString("Untagged commit #2 in branch feature/IN-12345")).AndAddFiles().AndStage().AndCommitWith(utl.PointerToString("Untagged commit #3 in branch feature/IN-12345")).InBranch("v0.x").AndCommitWithTagMessageInBranch("hotfix-98765", "0.0.8-hotfix98765.1", utl.PointerToString("Annotated tag to commit 0.0.8-hotfix98765.1")).AndCommitWithTagInBranch("hotfix-98765", "0.0.8-hotfix98765.2").AndAddFiles().AndStage().AndCommitWith(utl.PointerToString("Untagged commit in branch fix-98765")).InBranch("integration").AndCommitWithTagMessageInBranch("alpha", "0.0.6-alpha.1", utl.PointerToString("Annotated tag to commit 0.0.6-alpha.1")).AndCommitWithTagInBranch("alpha", "0.0.6-alpha.2").AndAddFiles().AndStage().AndCommitWith(utl.PointerToString("Untagged commit in branch alpha")).InBranch("alpha").AndCommitWithTagMessageInBranch("beta", "0.0.6-beta.1", utl.PointerToString("Annotated tag to commit 0.0.6-beta.1")).AndCommitWithTagInBranch("beta", "0.0.6-beta.2").AndAddFiles().AndStage().AndCommitWith(utl.PointerToString("Untagged commit #1 in branch beta")).InBranch("integration").InBranch("gamma").AndAddFiles().AndStage().AndCommitWith(utl.PointerToString("Untagged commit #1 in branch gamma")).AndAddFiles().AndStage().AndCommitWith(utl.PointerToString("Untagged commit #2 in branch gamma")).AndAddFiles().AndStage().AndCommitWith(utl.PointerToString("Untagged commit #3 in branch gamma")).InBranch("integration").InBranch("internal").AndCommitWithTagInBranch("internal", "0.0.6-internal.1+timestamp.001").AndCommitWithTagInBranch("internal", "0.0.6-internal.1+timestamp.002").AndCommitWithTagInBranch("internal", "0.0.6-internal.1+timestamp.003").AndAddFiles().AndStage().AndCommitWith(utl.PointerToString("Untagged commit #1 in branch internal")).InBranch("integration").InBranch("somebranch").AndCommitWithTagInBranch("somebranch", "tag1").AndCommitWithTagInBranch("somebranch", "tag2").AndCommitWithTagInBranch("somebranch", "tag3").InBranch("master").InBranch("someotherbranch").AndAddFiles().AndStage().AndCommitWith(utl.PointerToString("Untagged commit #1 in branch someotherbranch")).AndAddFiles().AndStage().AndCommitWith(utl.PointerToString("Untagged commit #2 in branch someotherbranch")).AndAddFiles().AndStage().AndCommitWith(utl.PointerToString("Untagged commit #3 in branch someotherbranch")).InBranch("master")
	}}
}

/*
Applies the scenario in the given directory and returns the script that was used.
The returned script can be used to inspect the repository or perform further actions.

Arguments are as follows:

  - directory the directory to apply the scenario in. It must exist
    and contain a valid Git repository. Previous history of the repository is left unchanged.
    The script is being applied starting from the current branch in the given repository.
*/
func (s Scenario) ApplyIn(directory string) Script {
	return s.function(directory)
}

/*
Applies the scenario in a new temporary directory after cloning the repository from the given URI
and returns the script that was used.
The returned script can be used to inspect the repository or perform further actions.
This method allows using user name and password authentication (also used for tokens).

Arguments are as follows:

- uri the URI to of the repository to clone from
- user the optional user name to use when credentials are required.
- password the optional password to use when credentials are required.
*/
func (s Scenario) ApplyOnCloneFromWithUserNameAndPassword(uri string, user *string, password *string) Script {
	return s.ApplyOnCloneFromToWithUserNameAndPassword(gitutil.NewTempDirectory("", utl.PointerToString("nyx-test-scenario-")), uri, user, password)
}

/*
Applies the scenario in a new temporary directory after cloning the repository from the given URI
and returns the script that was used.
The returned script can be used to inspect the repository or perform further actions.
This method allows using SSH authentication.

Arguments are as follows:

  - uri the URI to of the repository to clone from
  - privateKey the SSH private key. If nil the private key will be searched in its default location
    (i.e. in the users' $HOME/.ssh directory).
  - passphrase the optional password to use to open the private key, in case it's protected by a passphrase.
    This is required when the private key is password protected as this implementation does not support prompting
    the user interactively for entering the password.
*/
func (s Scenario) ApplyOnCloneFromWithPublicKey(uri string, privateKey *string, passphrase *string) Script {
	return s.ApplyOnCloneFromToWithPublicKey(gitutil.NewTempDirectory("", utl.PointerToString("nyx-test-scenario-")), uri, privateKey, passphrase)
}

/*
Applies the scenario in the given directory after cloning the repository from the given URI
and returns the script that was used. No credendials are used for cloning.
The returned script can be used to inspect the repository or perform further actions.
This method allows using user name and password authentication (also used for tokens).

Arguments are as follows:

  - directory the directory to apply the scenario in. It must exist and be empty.
    The script is being applied starting from the current branch in the given repository.
  - uri the URI to of the repository to clone from
  - user the optional user name to use when credentials are required.
  - password the optional password to use when credentials are required.
*/
func (s Scenario) ApplyOnCloneFromToWithUserNameAndPassword(directory string, uri string, user *string, password *string) Script {
	CloneFromToWithUserNameAndPassword(uri, directory, user, password)
	return s.function(directory)
}

/*
Applies the scenario in the given directory after cloning the repository from the given URI
and returns the script that was used. No credendials are used for cloning.
The returned script can be used to inspect the repository or perform further actions.
This method allows using SSH authentication.

Arguments are as follows:

  - directory the directory to apply the scenario in. It must exist and be empty.
    The script is being applied starting from the current branch in the given repository.
  - uri the URI to of the repository to clone from
  - privateKey the SSH private key. If nil the private key will be searched in its default location
    (i.e. in the users' $HOME/.ssh directory).
  - passphrase the optional password to use to open the private key, in case it's protected by a passphrase.
    This is required when the private key is password protected as this implementation does not support prompting
    the user interactively for entering the password.
*/
func (s Scenario) ApplyOnCloneFromToWithPublicKey(directory string, uri string, privateKey *string, passphrase *string) Script {
	CloneFromToWithPublicKey(uri, directory, privateKey, passphrase)
	return s.function(directory)
}

/*
Realizes the scenario in a new temporary directory and returns the script that was used.
The returned script can be used to inspect the repository or perform further actions.
*/
func (s Scenario) Realize() Script {
	return s.RealizeBare(false)
}

/*
Realizes the scenario in a new temporary directory and returns the script that was used.
The returned script can be used to inspect the repository or perform further actions.

Arguments are as follows:

- bare if true a bare repository is created, otherwise a regular repository is initialized
*/
func (s Scenario) RealizeBare(bare bool) Script {
	if bare {
		return s.function(Bare().GetWorkingDirectory())
	} else {
		return s.function(FromScratch().GetWorkingDirectory())
	}
}
