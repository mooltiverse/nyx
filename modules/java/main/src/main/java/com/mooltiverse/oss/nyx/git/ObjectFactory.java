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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import com.mooltiverse.oss.nyx.entities.git.Action;
import com.mooltiverse.oss.nyx.entities.git.Commit;
import com.mooltiverse.oss.nyx.entities.git.Identity;
import com.mooltiverse.oss.nyx.entities.git.Message;
import com.mooltiverse.oss.nyx.entities.git.Tag;
import com.mooltiverse.oss.nyx.entities.git.TimeStamp;

import org.eclipse.jgit.lib.Constants;
import org.eclipse.jgit.lib.PersonIdent;
import org.eclipse.jgit.lib.Ref;
import org.eclipse.jgit.lib.RefDatabase;
import org.eclipse.jgit.revwalk.FooterLine;
import org.eclipse.jgit.revwalk.RevCommit;

/**
 * This class provides methods to convert objects between one format and another.
 */
class ObjectFactory {
    /**
     * Default constructor is private on purpose.
     */
    private ObjectFactory() {
        super();
    }

    /**
     * Returns the new value object from the given git reference.
     * 
     * @param personIdent the git reference to get the data from.
     * 
     * @return the value object.
     */
    static Action actionFrom(PersonIdent personIdent) {
        return new Action(identityFrom(personIdent), timeStampFrom(personIdent));
    }

    /**
     * Returns the new value object from the given git reference.
     * 
     * @param commit the git reference to get the data from.
     * @param tags the set of tags to this commit
     * 
     * @return the value object.
     */
    static Commit commitFrom(RevCommit commit, Set<Tag> tags) {
        List<String> parents = new ArrayList<String>(commit.getParentCount());
        for (RevCommit parent: commit.getParents())
            parents.add(parent.getId().getName());

        return new Commit(commit.getId().getName(), commit.getCommitTime(), parents, actionFrom(commit.getAuthorIdent()), actionFrom(commit.getCommitterIdent()), messageFrom(commit), tags);
    }

    /**
     * Returns the new value object from the given git reference.
     * 
     * @param personIdent the git reference to get the data from.
     * 
     * @return the value object.
     */
    static Identity identityFrom(PersonIdent personIdent) {
        return new Identity(personIdent.getName(), personIdent.getEmailAddress());
    }

    /**
     * Returns the new value object from the given git reference.
     * 
     * @param commit the git reference to get the data from.
     * 
     * @return the value object.
     */
    static Message messageFrom(RevCommit commit) {
        Map<String,String> footers = new HashMap<String,String>(commit.getFooterLines().size());
        for (FooterLine footerLine: commit.getFooterLines()) {
            footers.put(footerLine.getKey(), footerLine.getValue());
        }

        return new Message(commit.getFullMessage(), commit.getShortMessage(), footers);
    }

    /**
     * Returns the new value object from the given git reference.
     * 
     * Note that the given reference must be peeled ({@link RefDatabase#peel(Ref)}) before this method
     * is invoked or this method will not be able to detect if the tag is annotated or lightweight.
     * 
     * @param ref the git reference to get the data from.
     * 
     * @return the value object.
     */
    static Tag tagFrom(Ref ref) {
        return new Tag(ref.getName().replace(Constants.R_TAGS, ""), Objects.isNull(ref.getPeeledObjectId()) ? ref.getObjectId().getName() : ref.getPeeledObjectId().getName(), !Objects.isNull(ref.getPeeledObjectId()));

    }

    /**
     * Returns the new value object from the given git reference.
     * 
     * @param personIdent the git reference to get the data from.
     * 
     * @return the value object.
     */
    static TimeStamp timeStampFrom(PersonIdent personIdent) {
        return new TimeStamp(personIdent.getWhen(), personIdent.getTimeZone());
    }
}
