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
package com.mooltiverse.oss.nyx.entities;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import com.mooltiverse.oss.nyx.entities.git.Commit;

/**
 * This object models the data model of a changelog.
 */
public class Changelog {
    /**
     * The internal list of releases.
     */
    private List<Release> releases = null;

    /**
     * Default constructor.
     */
    public Changelog() {
        super();
        this.releases = new ArrayList<Release>();
    }

    /**
     * Builds a changelog with the given list of releases.
     * 
     * @param releases the list of releases. It can't be {@code null}.
     */
    public Changelog(List<Release> releases) {
        super();
        this.releases = releases;
    }

    /**
     * Returns the list of releases. Never {@code null}.
     * 
     * @return the list of releases. Never {@code null}.
     */
    public List<Release> getReleases() {
        return releases;
    }

    /**
     * Sets the list of releases.
     * 
     * @param releases the list of releases. It can't be {@code null}.
     */
    public void setReleases(List<Release> releases) {
        Objects.requireNonNull(releases, "Can't set a null list of releases on the changelog");
        this.releases = releases;
    }

    /**
     * This object models a single release in a changelog.
     */
    public static class Release {
        /**
         * The release date attribute
         */
        private String date = null;

        /**
         * The release name attribute
         */
        private String name = null;

        /**
         * The changelog release sections.
         */
        private List<Section> sections = new ArrayList<Section>();

        /**
         * Default constructor.
         */
        public Release() {
            super();
        }

        /**
         * Builds a changelog release with the given name and date.
         * 
         * @param name the release name. It can't be {@code null}.
         * @param date the release date in a string format. It can't be {@code null}.
         */
        public Release(String name, String date) {
            super();
            Objects.requireNonNull(name, "Can't create a changelog release with a null name");
            Objects.requireNonNull(date, "Can't create a changelog release with a null date");
            this.name = name;
            this.date = date;
        }

        /**
         * Returns the release date in a string format.
         * 
         * @return the release date in a string format.
         */
        public String getDate() {
            return date;
        }

        /**
         * Sets the release date in a string format.
         * 
         * @param date the release name in a string format. It can't be {@code null}.
         */
        public void setDate(String date) {
            Objects.requireNonNull(date, "Can't set a null changelog release date");
            this.date = date;
        }

        /**
         * Returns the release name.
         * 
         * @return the release name.
         */
        public String getName() {
            return name;
        }

        /**
         * Sets the release name.
         * 
         * @param name the release name. It can't be {@code null}.
         */
        public void setName(String name) {
            Objects.requireNonNull(name, "Can't set a null changelog release name");
            this.name = name;
        }

        /**
         * Returns the changelog release sections.
         * 
         * @return the changelog release sections. Never {@code null}.
         */
        public List<Section> getSections() {
            return sections;
        }

        /**
         * Returns the changelog release section with the given name, if any, optionally creating 
         * if it doesn't exists and {@code create} is {@code true}.
         * 
         * @param name the name of the section to return. It can't be {@code null}
         * @param create when {@code true} and no section with the given name exists, the section
         * is created, appended to the end of existing sections, and returned,
         * otherwise {@code null} is returned when no section with the given name exists.
         * 
         * @return the changelog release section. May be {@code null} if no section with the given
         * name exists and {@code create} is {@code false}.
         */
        public Section getSection(String name, boolean create) {
            for (Section section: sections) {
                if (section.getName().equals(name))
                    return section;
            }
            if (create) {
                Section section = new Section(name);
                sections.add(section);
                return section;
            }
            return null;
        }

        /**
         * Sets the changelog release sections.
         * 
         * @param sections the changelog release sections. It can't be {@code null}.
         */
        public void setSections(List<Section> sections) {
            Objects.requireNonNull(sections, "Can't set a null changelog release sections list");
            this.sections = sections;
        }

        /**
         * This object models a single section in a changelog release.
         */
        public static class Section {
            /**
             * The section name attribute
             */
            private String name = null;

            /**
             * The changelog section commits.
             */
            private List<Commit> commits = new ArrayList<Commit>();

            /**
             * Default constructor.
             */
            public Section() {
                super();
            }

            /**
             * Builds a changelog section with the given name and an empty commits list.
             * 
             * @param name the section name. It can't be {@code null}.
             */
            public Section(String name) {
                super();
                Objects.requireNonNull(name, "Can't create a changelog section with a null name");
                this.name = name;
            }

            /**
             * Builds a changelog section with the given name and commits.
             * 
             * @param name the section name. It can't be {@code null}.
             * @param commits the section commits. It can't be {@code null}.
             */
            public Section(String name, List<Commit> commits) {
                super();
                Objects.requireNonNull(name, "Can't create a changelog section with a null name");
                Objects.requireNonNull(commits, "Can't create a changelog section with a list of commits");
                this.name = name;
                this.commits = commits;
            }

            /**
             * Returns the section name.
             * 
             * @return the section name.
             */
            public String getName() {
                return name;
            }

            /**
             * Sets the section name.
             * 
             * @param name the section name. It can't be {@code null}.
             */
            public void setName(String name) {
                Objects.requireNonNull(name, "Can't set a null changelog section name");
                this.name = name;
            }

            /**
             * Returns the section commits.
             * 
             * @return the section commits. Never {@code null}.
             */
            public List<Commit> getCommits() {
                return commits;
            }

            /**
             * Sets the section commits.
             * 
             * @param commits the section commits. It can't be {@code null}.
             */
            public void setCommits(List<Commit> commits) {
                Objects.requireNonNull(commits, "Can't set a null changelog section commits list");
                this.commits = commits;
            }
        }
    }
}
