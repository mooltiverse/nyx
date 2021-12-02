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
package com.mooltiverse.oss.nyx.services.git;

import com.mooltiverse.oss.nyx.entities.git.Commit;

/**
 * This functional interface is used when browsing Git commits and lets consumers receive summary
 * implementation-independent informations about a single commit.
 */
@FunctionalInterface
public interface CommitVisitor {
    /**
     * Visits a single commit and receives all of the commit simplified fields. Consumer returns {@code true}
     * to keep browsing next commits or {@code false} to stop.
     * 
     * @param commit the value object holding all data about the commit being visited.
     * 
     * @return {@code true} if to continue browsing to the next commit (if any), or {@code false} to stop
     */
    public boolean visit(Commit commit);
}
