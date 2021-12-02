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
package com.mooltiverse.oss.nyx.configuration.presets;

import static org.junit.jupiter.api.Assertions.*;

import java.util.regex.Pattern;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import com.mooltiverse.oss.nyx.configuration.Configuration;
import com.mooltiverse.oss.nyx.configuration.SimpleConfigurationLayer;
import com.mooltiverse.oss.nyx.state.State;
import com.mooltiverse.oss.nyx.template.Templates;

/**
 * Runs static tests of release types definitions. Only a few fields can be tested statically.
 */
@DisplayName("ReleaseTypes")
public class ReleaseTypesTests {
    @Nested
    @DisplayName("Feature")
    class FeatureTests {
        @Nested
        @DisplayName("Feature filter tags")
        class FilterTagsTests {
            @Test
            @DisplayName("Feature positive filter tags")
            void positiveFilterTags()
                throws Exception {
                Configuration configuration = new Configuration();
                SimpleConfigurationLayer configurationLayer = new SimpleConfigurationLayer();
                configurationLayer.setReleasePrefix("v");
                configuration.withRuntimeConfiguration(configurationLayer);
                State state = new State(configuration);
                Pattern pattern = Pattern.compile(Templates.render(ReleaseTypes.FEATURE.getFilterTags(), state));

                assertTrue(pattern.matcher("1.2.3-feat").matches());
                assertTrue(pattern.matcher("1.2.3-feat.1").matches());
                assertTrue(pattern.matcher("1.2.3-featABC").matches());
                assertTrue(pattern.matcher("1.2.3-featABC.1").matches());
                
                assertTrue(pattern.matcher("1.2.3-feature").matches());
                assertTrue(pattern.matcher("1.2.3-feature.1").matches());
                assertTrue(pattern.matcher("1.2.3-featureABC").matches());
                assertTrue(pattern.matcher("1.2.3-featureABC.1").matches());

                assertTrue(pattern.matcher("v1.2.3-feat").matches());
                assertTrue(pattern.matcher("v1.2.3-feat.1").matches());
                assertTrue(pattern.matcher("v1.2.3-featABC").matches());
                assertTrue(pattern.matcher("v1.2.3-featABC.1").matches());
                
                assertTrue(pattern.matcher("v1.2.3-feature").matches());
                assertTrue(pattern.matcher("v1.2.3-feature.1").matches());
                assertTrue(pattern.matcher("v1.2.3-featureABC").matches());
                assertTrue(pattern.matcher("v1.2.3-featureABC.1").matches());
            }

            @Test
            @DisplayName("Feature tag does not filter")
            void negativeFilterTags()
                throws Exception {
                Configuration configuration = new Configuration();
                SimpleConfigurationLayer configurationLayer = new SimpleConfigurationLayer();
                configurationLayer.setReleasePrefix("v");
                configuration.withRuntimeConfiguration(configurationLayer);
                State state = new State(configuration);
                Pattern pattern = Pattern.compile(Templates.render(ReleaseTypes.FEATURE.getFilterTags(), state));

                assertFalse(pattern.matcher("1.2.3").matches());
                assertFalse(pattern.matcher("a.b.c").matches());

                assertFalse(pattern.matcher("1.2.3-fat").matches());
                assertFalse(pattern.matcher("1.2.3-fat.1").matches());
                assertFalse(pattern.matcher("1.2.3-fatABC").matches());
                assertFalse(pattern.matcher("1.2.3-fatABC.1").matches());
                
                assertFalse(pattern.matcher("1.2.3-fature").matches());
                assertFalse(pattern.matcher("1.2.3-fature.1").matches());
                assertFalse(pattern.matcher("1.2.3-fatureABC").matches());
                assertFalse(pattern.matcher("1.2.3-fatureABC.1").matches());

                assertFalse(pattern.matcher("1.2.3-feature-1").matches());
                assertFalse(pattern.matcher("1.2.3-featureABC-1").matches());

                assertFalse(pattern.matcher("v1.2.3").matches());
                assertFalse(pattern.matcher("va.b.c").matches());

                assertFalse(pattern.matcher("v1.2.3-fat").matches());
                assertFalse(pattern.matcher("v1.2.3-fat.1").matches());
                assertFalse(pattern.matcher("v1.2.3-fatABC").matches());
                assertFalse(pattern.matcher("v1.2.3-fatABC.1").matches());

                assertFalse(pattern.matcher("v1.2.3-feat-1").matches());
                assertFalse(pattern.matcher("v1.2.3-featABC-1").matches());

                assertFalse(pattern.matcher("z1.2.3-feat").matches());
                assertFalse(pattern.matcher("z1.2.3-feat.1").matches());
                assertFalse(pattern.matcher("z1.2.3-featABC").matches());
                assertFalse(pattern.matcher("z1.2.3-featABC.1").matches());
                
                assertFalse(pattern.matcher("v1.2.3-fature").matches());
                assertFalse(pattern.matcher("v1.2.3-fature.1").matches());
                assertFalse(pattern.matcher("v1.2.3-fatureABC").matches());
                assertFalse(pattern.matcher("v1.2.3-fatureABC.1").matches());
                
                assertFalse(pattern.matcher("z1.2.3-feature").matches());
                assertFalse(pattern.matcher("z1.2.3-feature.1").matches());
                assertFalse(pattern.matcher("z1.2.3-featureABC").matches());
                assertFalse(pattern.matcher("z1.2.3-featureABC.1").matches());
            }
        }

        @Nested
        @DisplayName("Feature branch match")
        class BranchMatchTests {
            @Test
            @DisplayName("Feature positive branch matches")
            void positiveMatchBranches()
                throws Exception {
                Configuration configuration = new Configuration();
                SimpleConfigurationLayer configurationLayer = new SimpleConfigurationLayer();
                configurationLayer.setReleasePrefix("v");
                configuration.withRuntimeConfiguration(configurationLayer);
                State state = new State(configuration);
                Pattern pattern = Pattern.compile(Templates.render(ReleaseTypes.FEATURE.getMatchBranches(), state));

                assertTrue(pattern.matcher("feat").matches());
                assertTrue(pattern.matcher("feat-123").matches());
                assertTrue(pattern.matcher("feat/abc").matches());
                
                assertTrue(pattern.matcher("feature").matches());
                assertTrue(pattern.matcher("feature-123").matches());
                assertTrue(pattern.matcher("feature/abc").matches());
            }

            @Test
            @DisplayName("Feature branch does not match")
            void negativeMatchBranches()
                throws Exception {
                Configuration configuration = new Configuration();
                SimpleConfigurationLayer configurationLayer = new SimpleConfigurationLayer();
                configurationLayer.setReleasePrefix("v");
                configuration.withRuntimeConfiguration(configurationLayer);
                State state = new State(configuration);
                Pattern pattern = Pattern.compile(Templates.render(ReleaseTypes.FEATURE.getMatchBranches(), state));

                assertFalse(pattern.matcher("").matches());
                assertFalse(pattern.matcher("f").matches());
                assertFalse(pattern.matcher("fea").matches());
                assertFalse(pattern.matcher("featu").matches());
                assertFalse(pattern.matcher("featuree").matches());
                assertFalse(pattern.matcher("feat-").matches());
                assertFalse(pattern.matcher("feature/").matches());

                assertFalse(pattern.matcher("something-").matches());
                assertFalse(pattern.matcher("something/").matches());
            }
        }
    }

    @Nested
    @DisplayName("Hotfix")
    class HotfixTests {
        @Nested
        @DisplayName("Hotfix filter tags")
        class FilterTagsTests {
            @Test
            @DisplayName("Hotfix positive filter tags")
            void positiveFilterTags()
                throws Exception {
                Configuration configuration = new Configuration();
                SimpleConfigurationLayer configurationLayer = new SimpleConfigurationLayer();
                configurationLayer.setReleasePrefix("v");
                configuration.withRuntimeConfiguration(configurationLayer);
                State state = new State(configuration);
                Pattern pattern = Pattern.compile(Templates.render(ReleaseTypes.HOTFIX.getFilterTags(), state));

                assertTrue(pattern.matcher("1.2.3-fix").matches());
                assertTrue(pattern.matcher("1.2.3-fix.1").matches());
                assertTrue(pattern.matcher("1.2.3-fixABC").matches());
                assertTrue(pattern.matcher("1.2.3-fixABC.1").matches());
                
                assertTrue(pattern.matcher("1.2.3-hotfix").matches());
                assertTrue(pattern.matcher("1.2.3-hotfix.1").matches());
                assertTrue(pattern.matcher("1.2.3-hotfixABC").matches());
                assertTrue(pattern.matcher("1.2.3-hotfixABC.1").matches());

                assertTrue(pattern.matcher("v1.2.3-fix").matches());
                assertTrue(pattern.matcher("v1.2.3-fix.1").matches());
                assertTrue(pattern.matcher("v1.2.3-fixABC").matches());
                assertTrue(pattern.matcher("v1.2.3-fixABC.1").matches());
                
                assertTrue(pattern.matcher("v1.2.3-hotfix").matches());
                assertTrue(pattern.matcher("v1.2.3-hotfix.1").matches());
                assertTrue(pattern.matcher("v1.2.3-hotfixABC").matches());
                assertTrue(pattern.matcher("v1.2.3-hotfixABC.1").matches());
            }

            @Test
            @DisplayName("Hotfix tag does not filter")
            void negativeFilterTags()
                throws Exception {
                Configuration configuration = new Configuration();
                SimpleConfigurationLayer configurationLayer = new SimpleConfigurationLayer();
                configurationLayer.setReleasePrefix("v");
                configuration.withRuntimeConfiguration(configurationLayer);
                State state = new State(configuration);
                Pattern pattern = Pattern.compile(Templates.render(ReleaseTypes.HOTFIX.getFilterTags(), state));

                assertFalse(pattern.matcher("1.2.3").matches());
                assertFalse(pattern.matcher("a.b.c").matches());

                assertFalse(pattern.matcher("1.2.3-fx").matches());
                assertFalse(pattern.matcher("1.2.3-fx.1").matches());
                assertFalse(pattern.matcher("1.2.3-fxABC").matches());
                assertFalse(pattern.matcher("1.2.3-fxABC.1").matches());
                
                assertFalse(pattern.matcher("1.2.3-htfix").matches());
                assertFalse(pattern.matcher("1.2.3-htfix.1").matches());
                assertFalse(pattern.matcher("1.2.3-htfixABC").matches());
                assertFalse(pattern.matcher("1.2.3-htfixABC.1").matches());

                assertFalse(pattern.matcher("1.2.3-hotfix-1").matches());
                assertFalse(pattern.matcher("1.2.3-hotfixABC-1").matches());

                assertFalse(pattern.matcher("v1.2.3").matches());
                assertFalse(pattern.matcher("va.b.c").matches());

                assertFalse(pattern.matcher("v1.2.3-fx").matches());
                assertFalse(pattern.matcher("v1.2.3-fx.1").matches());
                assertFalse(pattern.matcher("v1.2.3-fxABC").matches());
                assertFalse(pattern.matcher("v1.2.3-fxABC.1").matches());

                assertFalse(pattern.matcher("v1.2.3-fix-1").matches());
                assertFalse(pattern.matcher("v1.2.3-fixABC-1").matches());

                assertFalse(pattern.matcher("z1.2.3-fix").matches());
                assertFalse(pattern.matcher("z1.2.3-fix.1").matches());
                assertFalse(pattern.matcher("z1.2.3-fixABC").matches());
                assertFalse(pattern.matcher("z1.2.3-fixABC.1").matches());
                
                assertFalse(pattern.matcher("v1.2.3-htfix").matches());
                assertFalse(pattern.matcher("v1.2.3-htfix.1").matches());
                assertFalse(pattern.matcher("v1.2.3-htfixABC").matches());
                assertFalse(pattern.matcher("v1.2.3-htfixABC.1").matches());
                
                assertFalse(pattern.matcher("z1.2.3-hotfix").matches());
                assertFalse(pattern.matcher("z1.2.3-hotfix.1").matches());
                assertFalse(pattern.matcher("z1.2.3-hotfixABC").matches());
                assertFalse(pattern.matcher("z1.2.3-hotfixABC.1").matches());
            }
        }

        @Nested
        @DisplayName("Hotfix branch match")
        class BranchMatchTests {
            @Test
            @DisplayName("Hotfix positive branch matches")
            void positiveMatchBranches()
                throws Exception {
                Configuration configuration = new Configuration();
                SimpleConfigurationLayer configurationLayer = new SimpleConfigurationLayer();
                configurationLayer.setReleasePrefix("v");
                configuration.withRuntimeConfiguration(configurationLayer);
                State state = new State(configuration);
                Pattern pattern = Pattern.compile(Templates.render(ReleaseTypes.HOTFIX.getMatchBranches(), state));

                assertTrue(pattern.matcher("fix").matches());
                assertTrue(pattern.matcher("fix-123").matches());
                assertTrue(pattern.matcher("fix/abc").matches());
                
                assertTrue(pattern.matcher("hotfix").matches());
                assertTrue(pattern.matcher("hotfix-123").matches());
                assertTrue(pattern.matcher("hotfix/abc").matches());
            }

            @Test
            @DisplayName("Hotfix branch does not match")
            void negativeMatchBranches() 
                throws Exception {
                Configuration configuration = new Configuration();
                SimpleConfigurationLayer configurationLayer = new SimpleConfigurationLayer();
                configurationLayer.setReleasePrefix("v");
                configuration.withRuntimeConfiguration(configurationLayer);
                State state = new State(configuration);
                Pattern pattern = Pattern.compile(Templates.render(ReleaseTypes.HOTFIX.getMatchBranches(), state));

                assertFalse(pattern.matcher("").matches());
                assertFalse(pattern.matcher("f").matches());
                assertFalse(pattern.matcher("fi").matches());
                assertFalse(pattern.matcher("fixx").matches());
                assertFalse(pattern.matcher("h").matches());
                assertFalse(pattern.matcher("hot").matches());
                assertFalse(pattern.matcher("hotfixx").matches());
                assertFalse(pattern.matcher("fix-").matches());
                assertFalse(pattern.matcher("fix/").matches());
                assertFalse(pattern.matcher("hotfix-").matches());
                assertFalse(pattern.matcher("hotfix/").matches());

                assertFalse(pattern.matcher("something-").matches());
                assertFalse(pattern.matcher("something/").matches());
            }
        }
    }

    @Nested
    @DisplayName("Integration")
    class IntegrationTests {
        @Nested
        @DisplayName("Integration filter tags")
        class FilterTagsTests {
            @Test
            @DisplayName("Integration positive filter tags")
            void positiveFilterTags()
                throws Exception {
                Configuration configuration = new Configuration();
                SimpleConfigurationLayer configurationLayer = new SimpleConfigurationLayer();
                configurationLayer.setReleasePrefix("v");
                configuration.withRuntimeConfiguration(configurationLayer);
                State state = new State(configuration);
                Pattern pattern = Pattern.compile(Templates.render(ReleaseTypes.INTEGRATION.getFilterTags(), state));

                assertTrue(pattern.matcher("1.2.3-develop").matches());
                assertTrue(pattern.matcher("1.2.3-develop.1").matches());
                
                assertTrue(pattern.matcher("1.2.3-development").matches());
                assertTrue(pattern.matcher("1.2.3-development.1").matches());

                assertTrue(pattern.matcher("1.2.3-integration").matches());
                assertTrue(pattern.matcher("1.2.3-integration.1").matches());
                
                assertTrue(pattern.matcher("1.2.3-latest").matches());
                assertTrue(pattern.matcher("1.2.3-latest.1").matches());

                assertTrue(pattern.matcher("v1.2.3-develop").matches());
                assertTrue(pattern.matcher("v1.2.3-develop.1").matches());
                
                assertTrue(pattern.matcher("v1.2.3-development").matches());
                assertTrue(pattern.matcher("v1.2.3-development.1").matches());
                
                assertTrue(pattern.matcher("v1.2.3-integration").matches());
                assertTrue(pattern.matcher("v1.2.3-integration.1").matches());
                
                assertTrue(pattern.matcher("v1.2.3-latest").matches());
                assertTrue(pattern.matcher("v1.2.3-latest.1").matches());
            }

            @Test
            @DisplayName("Integration tag does not filter")
            void negativeFilterTags()
                throws Exception {
                Configuration configuration = new Configuration();
                SimpleConfigurationLayer configurationLayer = new SimpleConfigurationLayer();
                configurationLayer.setReleasePrefix("v");
                configuration.withRuntimeConfiguration(configurationLayer);
                State state = new State(configuration);
                Pattern pattern = Pattern.compile(Templates.render(ReleaseTypes.INTEGRATION.getFilterTags(), state));

                assertFalse(pattern.matcher("1.2.3").matches());
                assertFalse(pattern.matcher("a.b.c").matches());

                assertFalse(pattern.matcher("1.2.3-developABC").matches());
                assertFalse(pattern.matcher("1.2.3-developABC.1").matches());
                assertFalse(pattern.matcher("1.2.3-developmentABC").matches());
                assertFalse(pattern.matcher("1.2.3-developmentABC.1").matches());
                assertFalse(pattern.matcher("1.2.3-integrationABC").matches());
                assertFalse(pattern.matcher("1.2.3-integrationABC.1").matches());
                assertFalse(pattern.matcher("1.2.3-latestABC").matches());
                assertFalse(pattern.matcher("1.2.3-latestABC.1").matches());

                assertFalse(pattern.matcher("1.2.3-dvelop").matches());
                assertFalse(pattern.matcher("1.2.3-dvelop.1").matches());
                assertFalse(pattern.matcher("1.2.3-dvelopABC").matches());
                assertFalse(pattern.matcher("1.2.3-dvelopABC.1").matches());
                
                assertFalse(pattern.matcher("1.2.3-dvelopment").matches());
                assertFalse(pattern.matcher("1.2.3-dvelopment.1").matches());
                assertFalse(pattern.matcher("1.2.3-dvelopmentABC").matches());
                assertFalse(pattern.matcher("1.2.3-dvelopmentABC.1").matches());

                assertFalse(pattern.matcher("1.2.3-development-1").matches());
                assertFalse(pattern.matcher("1.2.3-developmentABC-1").matches());

                assertFalse(pattern.matcher("v1.2.3").matches());
                assertFalse(pattern.matcher("va.b.c").matches());

                assertFalse(pattern.matcher("v1.2.3-developABC").matches());
                assertFalse(pattern.matcher("v1.2.3-developABC.1").matches());
                assertFalse(pattern.matcher("v1.2.3-developmentABC").matches());
                assertFalse(pattern.matcher("v1.2.3-developmentABC.1").matches());
                assertFalse(pattern.matcher("v1.2.3-integrationABC").matches());
                assertFalse(pattern.matcher("v1.2.3-integrationABC.1").matches());
                assertFalse(pattern.matcher("v1.2.3-latestABC").matches());
                assertFalse(pattern.matcher("v1.2.3-latestABC.1").matches());

                assertFalse(pattern.matcher("v1.2.3-dvelop").matches());
                assertFalse(pattern.matcher("v1.2.3-dvelop.1").matches());
                assertFalse(pattern.matcher("v1.2.3-dvelopABC").matches());
                assertFalse(pattern.matcher("v1.2.3-dvelopABC.1").matches());

                assertFalse(pattern.matcher("v1.2.3-develop-1").matches());
                assertFalse(pattern.matcher("v1.2.3-developABC-1").matches());

                assertFalse(pattern.matcher("z1.2.3-develop").matches());
                assertFalse(pattern.matcher("z1.2.3-develop.1").matches());
                assertFalse(pattern.matcher("z1.2.3-developABC").matches());
                assertFalse(pattern.matcher("z1.2.3-developABC.1").matches());
                
                assertFalse(pattern.matcher("v1.2.3-dvelopment").matches());
                assertFalse(pattern.matcher("v1.2.3-dvelopment.1").matches());
                assertFalse(pattern.matcher("v1.2.3-dvelopmentABC").matches());
                assertFalse(pattern.matcher("v1.2.3-dvelopmentABC.1").matches());
                
                assertFalse(pattern.matcher("z1.2.3-development").matches());
                assertFalse(pattern.matcher("z1.2.3-development.1").matches());
                assertFalse(pattern.matcher("z1.2.3-developmentABC").matches());
                assertFalse(pattern.matcher("z1.2.3-developmentABC.1").matches());
            }
        }

        @Nested
        @DisplayName("Integration branch match")
        class BranchMatchTests {
            @Test
            @DisplayName("Integration positive branch matches")
            void positiveMatchBranches()
                throws Exception {
                Configuration configuration = new Configuration();
                SimpleConfigurationLayer configurationLayer = new SimpleConfigurationLayer();
                configurationLayer.setReleasePrefix("v");
                configuration.withRuntimeConfiguration(configurationLayer);
                State state = new State(configuration);
                Pattern pattern = Pattern.compile(Templates.render(ReleaseTypes.INTEGRATION.getMatchBranches(), state));

                assertTrue(pattern.matcher("develop").matches());
                assertTrue(pattern.matcher("development").matches());
                assertTrue(pattern.matcher("integration").matches());
                assertTrue(pattern.matcher("latest").matches());
            }

            @Test
            @DisplayName("Integration branch does not match")
            void negativeMatchBranches()
                throws Exception {
                Configuration configuration = new Configuration();
                SimpleConfigurationLayer configurationLayer = new SimpleConfigurationLayer();
                configurationLayer.setReleasePrefix("v");
                configuration.withRuntimeConfiguration(configurationLayer);
                State state = new State(configuration);
                Pattern pattern = Pattern.compile(Templates.render(ReleaseTypes.INTEGRATION.getMatchBranches(), state));

                assertFalse(pattern.matcher("").matches());
                assertFalse(pattern.matcher("d").matches());
                assertFalse(pattern.matcher("develo").matches());
                assertFalse(pattern.matcher("developm").matches());
                assertFalse(pattern.matcher("developmentt").matches());
                assertFalse(pattern.matcher("integrationn").matches());
                assertFalse(pattern.matcher("latestt").matches());

                assertFalse(pattern.matcher("DEVELOP").matches());
                assertFalse(pattern.matcher("DEVELOPMENT").matches());
                assertFalse(pattern.matcher("INTEGRATION").matches());
                assertFalse(pattern.matcher("LATEST").matches());
            }
        }
    }

    @Nested
    @DisplayName("Internal")
    class InternalTests {
        @Nested
        @DisplayName("Internal filter tags")
        class FilterTagsTests {
            @Test
            @DisplayName("Internal positive filter tags")
            void positiveFilterTags()
                throws Exception {
                // this release type has no branch pattern to test for matching
            }

            @Test
            @DisplayName("Internal tag does not filter")
            void negativeFilterTags()
                throws Exception {
                // this release type has no branch pattern to test for matching
            }
        }

        @Nested
        @DisplayName("Internal branch match")
        class BranchMatchTests {
            @Test
            @DisplayName("Internal positive branch matches")
            void positiveMatchBranches()
                throws Exception {
                // this release type has no branch pattern to test for matching
            }

            @Test
            @DisplayName("Internal branch does not match")
            void negativeMatchBranches()
                throws Exception {
                // this release type has no branch pattern to test for matching
            }
        }
    }

    @Nested
    @DisplayName("Mainline")
    class MainlineTests {
        @Nested
        @DisplayName("Mainline filter tags")
        class FilterTagsTests {
            @Test
            @DisplayName("Mainline positive filter tags")
            void positiveFilterTags()
                throws Exception {
                Configuration configuration = new Configuration();
                SimpleConfigurationLayer configurationLayer = new SimpleConfigurationLayer();
                configurationLayer.setReleasePrefix("v");
                configuration.withRuntimeConfiguration(configurationLayer);
                State state = new State(configuration);
                Pattern pattern = Pattern.compile(Templates.render(ReleaseTypes.MAINLINE.getFilterTags(), state));

                assertTrue(pattern.matcher("1.2.3").matches());
            }

            @Test
            @DisplayName("Mainline tag does not filter")
            void negativeFilterTags()
                throws Exception {
                Configuration configuration = new Configuration();
                SimpleConfigurationLayer configurationLayer = new SimpleConfigurationLayer();
                configurationLayer.setReleasePrefix("v");
                configuration.withRuntimeConfiguration(configurationLayer);
                State state = new State(configuration);
                Pattern pattern = Pattern.compile(Templates.render(ReleaseTypes.MAINLINE.getFilterTags(), state));

                assertFalse(pattern.matcher("a.b.c").matches());

                assertFalse(pattern.matcher("1.2.3-a").matches());
                assertFalse(pattern.matcher("1.2.3-a.1").matches());

                assertFalse(pattern.matcher("va.b.c").matches());

                assertFalse(pattern.matcher("v1.2.3-a").matches());
                assertFalse(pattern.matcher("v1.2.3-a.1").matches());
            }
        }

        @Nested
        @DisplayName("Mainline branch match")
        class BranchMatchTests {
            @Test
            @DisplayName("Mainline positive branch matches")
            void positiveMatchBranches()
                throws Exception {
                Configuration configuration = new Configuration();
                SimpleConfigurationLayer configurationLayer = new SimpleConfigurationLayer();
                configurationLayer.setReleasePrefix("v");
                configuration.withRuntimeConfiguration(configurationLayer);
                State state = new State(configuration);
                Pattern pattern = Pattern.compile(Templates.render(ReleaseTypes.MAINLINE.getMatchBranches(), state));

                assertTrue(pattern.matcher("main").matches());
                assertTrue(pattern.matcher("master").matches());
            }

            @Test
            @DisplayName("Mainline branch does not match")
            void negativeMatchBranches()
                throws Exception {
                Configuration configuration = new Configuration();
                SimpleConfigurationLayer configurationLayer = new SimpleConfigurationLayer();
                configurationLayer.setReleasePrefix("v");
                configuration.withRuntimeConfiguration(configurationLayer);
                State state = new State(configuration);
                Pattern pattern = Pattern.compile(Templates.render(ReleaseTypes.MAINLINE.getMatchBranches(), state));

                assertFalse(pattern.matcher("").matches());
                assertFalse(pattern.matcher("m").matches());
                assertFalse(pattern.matcher("ma").matches());
                assertFalse(pattern.matcher("mai").matches());
                assertFalse(pattern.matcher("mainn").matches());
                assertFalse(pattern.matcher("mas").matches());
                assertFalse(pattern.matcher("mast").matches());
                assertFalse(pattern.matcher("maste").matches());
                assertFalse(pattern.matcher("masterr").matches());

                assertFalse(pattern.matcher("MAIN").matches());
                assertFalse(pattern.matcher("MASTER").matches());
            }
        }
    }

    @Nested
    @DisplayName("Maintenance")
    class MaintenanceTests {
        @Nested
        @DisplayName("Maintenance filter tags")
        class FilterTagsTests {
            @Test
            @DisplayName("Maintenance positive filter tags")
            void positiveFilterTags()
                throws Exception {
                Configuration configuration = new Configuration();
                SimpleConfigurationLayer configurationLayer = new SimpleConfigurationLayer();
                configurationLayer.setReleasePrefix("v");
                configuration.withRuntimeConfiguration(configurationLayer);
                State state = new State(configuration);
                Pattern pattern = Pattern.compile(Templates.render(ReleaseTypes.MAINTENANCE.getFilterTags(), state));

                assertTrue(pattern.matcher("1.2.3").matches());
            }

            @Test
            @DisplayName("Maintenance tag does not filter")
            void negativeFilterTags()
                throws Exception {
                Configuration configuration = new Configuration();
                SimpleConfigurationLayer configurationLayer = new SimpleConfigurationLayer();
                configurationLayer.setReleasePrefix("v");
                configuration.withRuntimeConfiguration(configurationLayer);
                State state = new State(configuration);
                Pattern pattern = Pattern.compile(Templates.render(ReleaseTypes.MAINTENANCE.getFilterTags(), state));

                assertFalse(pattern.matcher("a.b.c").matches());

                assertFalse(pattern.matcher("1.2.3-a").matches());
                assertFalse(pattern.matcher("1.2.3-a.1").matches());

                assertFalse(pattern.matcher("va.b.c").matches());

                assertFalse(pattern.matcher("v1.2.3-a").matches());
                assertFalse(pattern.matcher("v1.2.3-a.1").matches());
            }
        }

        @Nested
        @DisplayName("Maintenance branch match")
        class BranchMatchTests {
            @Test
            @DisplayName("Maintenance positive branch matches")
            void positiveMatchBranches()
                throws Exception {
                Configuration configuration = new Configuration();
                SimpleConfigurationLayer configurationLayer = new SimpleConfigurationLayer();
                configurationLayer.setReleasePrefix("v");
                configuration.withRuntimeConfiguration(configurationLayer);
                State state = new State(configuration);
                Pattern pattern = Pattern.compile(Templates.render(ReleaseTypes.MAINTENANCE.getMatchBranches(), state));

                assertTrue(pattern.matcher("1").matches());
                assertTrue(pattern.matcher("1.x").matches());
                assertTrue(pattern.matcher("1.x.x").matches());
                assertTrue(pattern.matcher("12").matches());
                assertTrue(pattern.matcher("12.x").matches());
                assertTrue(pattern.matcher("12.x.x").matches());
                assertTrue(pattern.matcher("1.2").matches());
                assertTrue(pattern.matcher("1.2.x").matches());
                assertTrue(pattern.matcher("12").matches());
                assertTrue(pattern.matcher("12.22").matches());
                assertTrue(pattern.matcher("12.22.x").matches());
                assertTrue(pattern.matcher("1.2.3").matches());
                assertTrue(pattern.matcher("12.22.33").matches());
                assertTrue(pattern.matcher("v1").matches());
                assertTrue(pattern.matcher("v1.x").matches());
                assertTrue(pattern.matcher("v1.x.x").matches());
                assertTrue(pattern.matcher("v12").matches());
                assertTrue(pattern.matcher("v12.x").matches());
                assertTrue(pattern.matcher("v12.x.x").matches());
                assertTrue(pattern.matcher("v1.2").matches());
                assertTrue(pattern.matcher("v1.2.x").matches());
                assertTrue(pattern.matcher("v12").matches());
                assertTrue(pattern.matcher("v12.22").matches());
                assertTrue(pattern.matcher("v12.22.x").matches());
                assertTrue(pattern.matcher("v1.2.3").matches());
                assertTrue(pattern.matcher("v12.22.33").matches());
            }

            @Test
            @DisplayName("Maintenance branch does not match")
            void negativeMatchBranches()
                throws Exception {
                Configuration configuration = new Configuration();
                SimpleConfigurationLayer configurationLayer = new SimpleConfigurationLayer();
                configurationLayer.setReleasePrefix("v");
                configuration.withRuntimeConfiguration(configurationLayer);
                State state = new State(configuration);
                Pattern pattern = Pattern.compile(Templates.render(ReleaseTypes.MAINTENANCE.getMatchBranches(), state));

                assertFalse(pattern.matcher("").matches());
                assertFalse(pattern.matcher("v").matches());
                assertFalse(pattern.matcher("vv").matches());
                assertFalse(pattern.matcher("1.a").matches());
                assertFalse(pattern.matcher("1.a.b").matches());
                assertFalse(pattern.matcher("1,x").matches());
                assertFalse(pattern.matcher("1,x,x").matches());
                assertFalse(pattern.matcher("1.x.x.").matches());
                assertFalse(pattern.matcher("1.x.x.x").matches());
                assertFalse(pattern.matcher("v1.a").matches());
                assertFalse(pattern.matcher("v1.a.b").matches());
                assertFalse(pattern.matcher("v1,x").matches());
                assertFalse(pattern.matcher("v1,x,x").matches());
                assertFalse(pattern.matcher("v1.x.x.").matches());
                assertFalse(pattern.matcher("v1.x.x.x").matches());
                assertFalse(pattern.matcher("1.2.3.x").matches());
                assertFalse(pattern.matcher("v1.2.3.x").matches());
            }
        }
    }

    @Nested
    @DisplayName("Maturity")
    class MaturityTests {
        @Nested
        @DisplayName("Maturity filter tags")
        class FilterTagsTests {
            @Test
            @DisplayName("Maturity positive filter tags")
            void positiveFilterTags()
                throws Exception {
                Configuration configuration = new Configuration();
                SimpleConfigurationLayer configurationLayer = new SimpleConfigurationLayer();
                configurationLayer.setReleasePrefix("v");
                configuration.withRuntimeConfiguration(configurationLayer);
                State state = new State(configuration);
                Pattern pattern = Pattern.compile(Templates.render(ReleaseTypes.MATURITY.getFilterTags(), state));

                assertTrue(pattern.matcher("1.2.3").matches());

                assertTrue(pattern.matcher("1.2.3-alpha").matches());
                assertTrue(pattern.matcher("1.2.3-alpha.1").matches());
                
                assertTrue(pattern.matcher("1.2.3-beta").matches());
                assertTrue(pattern.matcher("1.2.3-beta.1").matches());

                assertTrue(pattern.matcher("1.2.3-gamma").matches());
                assertTrue(pattern.matcher("1.2.3-gamma.1").matches());
                
                assertTrue(pattern.matcher("1.2.3-delta").matches());
                assertTrue(pattern.matcher("1.2.3-delta.1").matches());

                assertTrue(pattern.matcher("v1.2.3").matches());

                assertTrue(pattern.matcher("v1.2.3-alpha").matches());
                assertTrue(pattern.matcher("v1.2.3-alpha.1").matches());
                
                assertTrue(pattern.matcher("v1.2.3-beta").matches());
                assertTrue(pattern.matcher("v1.2.3-beta.1").matches());
                
                assertTrue(pattern.matcher("v1.2.3-gamma").matches());
                assertTrue(pattern.matcher("v1.2.3-gamma.1").matches());
                
                assertTrue(pattern.matcher("v1.2.3-delta").matches());
                assertTrue(pattern.matcher("v1.2.3-delta.1").matches());
            }

            @Test
            @DisplayName("Maturity tag does not filter")
            void negativeFilterTags()
                throws Exception {
                Configuration configuration = new Configuration();
                SimpleConfigurationLayer configurationLayer = new SimpleConfigurationLayer();
                configurationLayer.setReleasePrefix("v");
                configuration.withRuntimeConfiguration(configurationLayer);
                State state = new State(configuration);
                Pattern pattern = Pattern.compile(Templates.render(ReleaseTypes.MATURITY.getFilterTags(), state));

                assertFalse(pattern.matcher("a.b.c").matches());

                assertFalse(pattern.matcher("1.2.3-alphaABC").matches());
                assertFalse(pattern.matcher("1.2.3-alphaABC.1").matches());
                assertFalse(pattern.matcher("1.2.3-betaABC").matches());
                assertFalse(pattern.matcher("1.2.3-betaABC.1").matches());
                assertFalse(pattern.matcher("1.2.3-gammaABC").matches());
                assertFalse(pattern.matcher("1.2.3-gammaABC.1").matches());
                assertFalse(pattern.matcher("1.2.3-deltaABC").matches());
                assertFalse(pattern.matcher("1.2.3-deltaABC.1").matches());

                assertFalse(pattern.matcher("1.2.3-dvelop").matches());
                assertFalse(pattern.matcher("1.2.3-dvelop.1").matches());
                assertFalse(pattern.matcher("1.2.3-dvelopABC").matches());
                assertFalse(pattern.matcher("1.2.3-dvelopABC.1").matches());
                
                assertFalse(pattern.matcher("1.2.3-dvelopment").matches());
                assertFalse(pattern.matcher("1.2.3-dvelopment.1").matches());
                assertFalse(pattern.matcher("1.2.3-dvelopmentABC").matches());
                assertFalse(pattern.matcher("1.2.3-dvelopmentABC.1").matches());

                assertFalse(pattern.matcher("1.2.3-beta-1").matches());
                assertFalse(pattern.matcher("1.2.3-betaABC-1").matches());

                assertFalse(pattern.matcher("va.b.c").matches());

                assertFalse(pattern.matcher("v1.2.3-alphaABC").matches());
                assertFalse(pattern.matcher("v1.2.3-alphaABC.1").matches());
                assertFalse(pattern.matcher("v1.2.3-betaABC").matches());
                assertFalse(pattern.matcher("v1.2.3-betaABC.1").matches());
                assertFalse(pattern.matcher("v1.2.3-gammaABC").matches());
                assertFalse(pattern.matcher("v1.2.3-gammaABC.1").matches());
                assertFalse(pattern.matcher("v1.2.3-deltaABC").matches());
                assertFalse(pattern.matcher("v1.2.3-deltaABC.1").matches());

                assertFalse(pattern.matcher("v1.2.3-dvelop").matches());
                assertFalse(pattern.matcher("v1.2.3-dvelop.1").matches());
                assertFalse(pattern.matcher("v1.2.3-dvelopABC").matches());
                assertFalse(pattern.matcher("v1.2.3-dvelopABC.1").matches());

                assertFalse(pattern.matcher("v1.2.3-alpha-1").matches());
                assertFalse(pattern.matcher("v1.2.3-alphaABC-1").matches());

                assertFalse(pattern.matcher("z1.2.3-alpha").matches());
                assertFalse(pattern.matcher("z1.2.3-alpha.1").matches());
                assertFalse(pattern.matcher("z1.2.3-alphaABC").matches());
                assertFalse(pattern.matcher("z1.2.3-alphaABC.1").matches());
                
                assertFalse(pattern.matcher("v1.2.3-dvelopment").matches());
                assertFalse(pattern.matcher("v1.2.3-dvelopment.1").matches());
                assertFalse(pattern.matcher("v1.2.3-dvelopmentABC").matches());
                assertFalse(pattern.matcher("v1.2.3-dvelopmentABC.1").matches());
                
                assertFalse(pattern.matcher("z1.2.3-beta").matches());
                assertFalse(pattern.matcher("z1.2.3-beta.1").matches());
                assertFalse(pattern.matcher("z1.2.3-betaABC").matches());
                assertFalse(pattern.matcher("z1.2.3-betaABC.1").matches());
            }
        }

        @Nested
        @DisplayName("Maturity branch match")
        class BranchMatchTests {
            @Test
            @DisplayName("Maturity positive branch matches")
            void positiveMatchBranches()
                throws Exception {
                Configuration configuration = new Configuration();
                SimpleConfigurationLayer configurationLayer = new SimpleConfigurationLayer();
                configurationLayer.setReleasePrefix("v");
                configuration.withRuntimeConfiguration(configurationLayer);
                State state = new State(configuration);
                Pattern pattern = Pattern.compile(Templates.render(ReleaseTypes.MATURITY.getMatchBranches(), state));

                assertTrue(pattern.matcher("alpha").matches());
                assertTrue(pattern.matcher("beta").matches());
                assertTrue(pattern.matcher("gamma").matches());
                assertTrue(pattern.matcher("delta").matches());
                assertTrue(pattern.matcher("epsilon").matches());
                assertTrue(pattern.matcher("zeta").matches());
                assertTrue(pattern.matcher("eta").matches());
                assertTrue(pattern.matcher("theta").matches());
                assertTrue(pattern.matcher("iota").matches());
                assertTrue(pattern.matcher("kappa").matches());
                assertTrue(pattern.matcher("lambda").matches());
                assertTrue(pattern.matcher("mu").matches());
                assertTrue(pattern.matcher("nu").matches());
                assertTrue(pattern.matcher("xi").matches());
                assertTrue(pattern.matcher("omicron").matches());
                assertTrue(pattern.matcher("pi").matches());
                assertTrue(pattern.matcher("rho").matches());
                assertTrue(pattern.matcher("sigma").matches());
                assertTrue(pattern.matcher("tau").matches());
                assertTrue(pattern.matcher("upsilon").matches());
                assertTrue(pattern.matcher("phi").matches());
                assertTrue(pattern.matcher("chi").matches());
                assertTrue(pattern.matcher("psi").matches());
                assertTrue(pattern.matcher("omega").matches());
            }

            @Test
            @DisplayName("Maturity branch does not match")
            void negativeMatchBranches()
                throws Exception {
                Configuration configuration = new Configuration();
                SimpleConfigurationLayer configurationLayer = new SimpleConfigurationLayer();
                configurationLayer.setReleasePrefix("v");
                configuration.withRuntimeConfiguration(configurationLayer);
                State state = new State(configuration);
                Pattern pattern = Pattern.compile(Templates.render(ReleaseTypes.MATURITY.getMatchBranches(), state));

                assertFalse(pattern.matcher("a").matches());
                assertFalse(pattern.matcher("alph").matches());

                assertFalse(pattern.matcher("alphaa").matches());
                assertFalse(pattern.matcher("betaa").matches());
                assertFalse(pattern.matcher("gammaa").matches());
                assertFalse(pattern.matcher("deltaa").matches());
                assertFalse(pattern.matcher("epsilonn").matches());
                assertFalse(pattern.matcher("zetaa").matches());
                assertFalse(pattern.matcher("etaa").matches());
                assertFalse(pattern.matcher("thetaa").matches());
                assertFalse(pattern.matcher("iotaa").matches());
                assertFalse(pattern.matcher("kappaa").matches());
                assertFalse(pattern.matcher("lambdaa").matches());
                assertFalse(pattern.matcher("muu").matches());
                assertFalse(pattern.matcher("nuu").matches());
                assertFalse(pattern.matcher("xii").matches());
                assertFalse(pattern.matcher("omicronn").matches());
                assertFalse(pattern.matcher("pii").matches());
                assertFalse(pattern.matcher("rhoo").matches());
                assertFalse(pattern.matcher("sigmaa").matches());
                assertFalse(pattern.matcher("tauu").matches());
                assertFalse(pattern.matcher("upsilonn").matches());
                assertFalse(pattern.matcher("phii").matches());
                assertFalse(pattern.matcher("chii").matches());
                assertFalse(pattern.matcher("psii").matches());
                assertFalse(pattern.matcher("omegaa").matches());

                assertFalse(pattern.matcher("ALPHA").matches());
            }
        }
    }

    @Nested
    @DisplayName("Release")
    class ReleaseTests {
        @Nested
        @DisplayName("Release filter tags")
        class FilterTagsTests {
            @Test
            @DisplayName("Release positive filter tags")
            void positiveFilterTags()
                throws Exception {
                /*Configuration configuration = new Configuration();
                SimpleConfigurationLayer configurationLayer = new SimpleConfigurationLayer();
                configurationLayer.setReleasePrefix("v");
                configuration.withRuntimeConfiguration(configurationLayer);
                State state = new State(configuration);
                Pattern pattern = Pattern.compile(Templates.render(ReleaseTypes.RELEASE.getFilterTags(), state));

                //assertTrue(pattern.matcher("1.2.3").matches());*/
            }

            @Test
            @DisplayName("Release tag does not filter")
            void negativeFilterTags()
                throws Exception {
                Configuration configuration = new Configuration();
                SimpleConfigurationLayer configurationLayer = new SimpleConfigurationLayer();
                configurationLayer.setReleasePrefix("v");
                configuration.withRuntimeConfiguration(configurationLayer);
                State state = new State(configuration);
                Pattern pattern = Pattern.compile(Templates.render(ReleaseTypes.RELEASE.getFilterTags(), state));

                assertFalse(pattern.matcher("1.2.3").matches());
                assertFalse(pattern.matcher("a.b.c").matches());

                assertFalse(pattern.matcher("1.2.3-a").matches());
                assertFalse(pattern.matcher("1.2.3-a.1").matches());

                assertFalse(pattern.matcher("va.b.c").matches());

                assertFalse(pattern.matcher("v1.2.3-a").matches());
                assertFalse(pattern.matcher("v1.2.3-a.1").matches());
            }
        }

        @Nested
        @DisplayName("Release positive branch match")
        class BranchMatchTests {
            @Test
            @DisplayName("Release branch matches")
            void positiveMatchBranches()
                throws Exception {
                Configuration configuration = new Configuration();
                SimpleConfigurationLayer configurationLayer = new SimpleConfigurationLayer();
                configurationLayer.setReleasePrefix("v");
                configuration.withRuntimeConfiguration(configurationLayer);
                State state = new State(configuration);
                Pattern pattern = Pattern.compile(Templates.render(ReleaseTypes.RELEASE.getMatchBranches(), state));

                assertTrue(pattern.matcher("rel-1").matches());
                assertTrue(pattern.matcher("rel-1.x").matches());
                assertTrue(pattern.matcher("rel-1.x.x").matches());
                assertTrue(pattern.matcher("rel-12").matches());
                assertTrue(pattern.matcher("rel-12.x").matches());
                assertTrue(pattern.matcher("rel-12.x.x").matches());
                assertTrue(pattern.matcher("rel-1.2").matches());
                assertTrue(pattern.matcher("rel-1.2.x").matches());
                assertTrue(pattern.matcher("rel-12").matches());
                assertTrue(pattern.matcher("rel-12.22").matches());
                assertTrue(pattern.matcher("rel-12.22.x").matches());
                assertTrue(pattern.matcher("rel-1.2.3").matches());
                assertTrue(pattern.matcher("rel-12.22.33").matches());
                assertTrue(pattern.matcher("rel-v1").matches());
                assertTrue(pattern.matcher("rel-v1.x").matches());
                assertTrue(pattern.matcher("rel-v1.x.x").matches());
                assertTrue(pattern.matcher("rel-v12").matches());
                assertTrue(pattern.matcher("rel-v12.x").matches());
                assertTrue(pattern.matcher("rel-v12.x.x").matches());
                assertTrue(pattern.matcher("rel-v1.2").matches());
                assertTrue(pattern.matcher("rel-v1.2.x").matches());
                assertTrue(pattern.matcher("rel-v12").matches());
                assertTrue(pattern.matcher("rel-v12.22").matches());
                assertTrue(pattern.matcher("rel-v12.22.x").matches());
                assertTrue(pattern.matcher("rel-v1.2.3").matches());
                assertTrue(pattern.matcher("rel-v12.22.33").matches());

                assertTrue(pattern.matcher("rel/1").matches());
                assertTrue(pattern.matcher("rel/1.x").matches());
                assertTrue(pattern.matcher("rel/1.x.x").matches());
                assertTrue(pattern.matcher("rel/12").matches());
                assertTrue(pattern.matcher("rel/12.x").matches());
                assertTrue(pattern.matcher("rel/12.x.x").matches());
                assertTrue(pattern.matcher("rel/1.2").matches());
                assertTrue(pattern.matcher("rel/1.2.x").matches());
                assertTrue(pattern.matcher("rel/12").matches());
                assertTrue(pattern.matcher("rel/12.22").matches());
                assertTrue(pattern.matcher("rel/12.22.x").matches());
                assertTrue(pattern.matcher("rel/1.2.3").matches());
                assertTrue(pattern.matcher("rel/12.22.33").matches());
                assertTrue(pattern.matcher("rel/v1").matches());
                assertTrue(pattern.matcher("rel/v1.x").matches());
                assertTrue(pattern.matcher("rel/v1.x.x").matches());
                assertTrue(pattern.matcher("rel/v12").matches());
                assertTrue(pattern.matcher("rel/v12.x").matches());
                assertTrue(pattern.matcher("rel/v12.x.x").matches());
                assertTrue(pattern.matcher("rel/v1.2").matches());
                assertTrue(pattern.matcher("rel/v1.2.x").matches());
                assertTrue(pattern.matcher("rel/v12").matches());
                assertTrue(pattern.matcher("rel/v12.22").matches());
                assertTrue(pattern.matcher("rel/v12.22.x").matches());
                assertTrue(pattern.matcher("rel/v1.2.3").matches());
                assertTrue(pattern.matcher("rel/v12.22.33").matches());

                assertTrue(pattern.matcher("release-1").matches());
                assertTrue(pattern.matcher("release-1.x").matches());
                assertTrue(pattern.matcher("release-1.x.x").matches());
                assertTrue(pattern.matcher("release-12").matches());
                assertTrue(pattern.matcher("release-12.x").matches());
                assertTrue(pattern.matcher("release-12.x.x").matches());
                assertTrue(pattern.matcher("release-1.2").matches());
                assertTrue(pattern.matcher("release-1.2.x").matches());
                assertTrue(pattern.matcher("release-12").matches());
                assertTrue(pattern.matcher("release-12.22").matches());
                assertTrue(pattern.matcher("release-12.22.x").matches());
                assertTrue(pattern.matcher("release-1.2.3").matches());
                assertTrue(pattern.matcher("release-12.22.33").matches());
                assertTrue(pattern.matcher("release-v1").matches());
                assertTrue(pattern.matcher("release-v1.x").matches());
                assertTrue(pattern.matcher("release-v1.x.x").matches());
                assertTrue(pattern.matcher("release-v12").matches());
                assertTrue(pattern.matcher("release-v12.x").matches());
                assertTrue(pattern.matcher("release-v12.x.x").matches());
                assertTrue(pattern.matcher("release-v1.2").matches());
                assertTrue(pattern.matcher("release-v1.2.x").matches());
                assertTrue(pattern.matcher("release-v12").matches());
                assertTrue(pattern.matcher("release-v12.22").matches());
                assertTrue(pattern.matcher("release-v12.22.x").matches());
                assertTrue(pattern.matcher("release-v1.2.3").matches());
                assertTrue(pattern.matcher("release-v12.22.33").matches());

                assertTrue(pattern.matcher("release/1").matches());
                assertTrue(pattern.matcher("release/1.x").matches());
                assertTrue(pattern.matcher("release/1.x.x").matches());
                assertTrue(pattern.matcher("release/12").matches());
                assertTrue(pattern.matcher("release/12.x").matches());
                assertTrue(pattern.matcher("release/12.x.x").matches());
                assertTrue(pattern.matcher("release/1.2").matches());
                assertTrue(pattern.matcher("release/1.2.x").matches());
                assertTrue(pattern.matcher("release/12").matches());
                assertTrue(pattern.matcher("release/12.22").matches());
                assertTrue(pattern.matcher("release/12.22.x").matches());
                assertTrue(pattern.matcher("release/1.2.3").matches());
                assertTrue(pattern.matcher("release/12.22.33").matches());
                assertTrue(pattern.matcher("release/v1").matches());
                assertTrue(pattern.matcher("release/v1.x").matches());
                assertTrue(pattern.matcher("release/v1.x.x").matches());
                assertTrue(pattern.matcher("release/v12").matches());
                assertTrue(pattern.matcher("release/v12.x").matches());
                assertTrue(pattern.matcher("release/v12.x.x").matches());
                assertTrue(pattern.matcher("release/v1.2").matches());
                assertTrue(pattern.matcher("release/v1.2.x").matches());
                assertTrue(pattern.matcher("release/v12").matches());
                assertTrue(pattern.matcher("release/v12.22").matches());
                assertTrue(pattern.matcher("release/v12.22.x").matches());
                assertTrue(pattern.matcher("release/v1.2.3").matches());
                assertTrue(pattern.matcher("release/v12.22.33").matches());
            }

            @Test
            @DisplayName("Release branch does not match")
            void negativeMatchBranches()
                throws Exception {
                Configuration configuration = new Configuration();
                SimpleConfigurationLayer configurationLayer = new SimpleConfigurationLayer();
                configurationLayer.setReleasePrefix("v");
                configuration.withRuntimeConfiguration(configurationLayer);
                State state = new State(configuration);
                Pattern pattern = Pattern.compile(Templates.render(ReleaseTypes.RELEASE.getMatchBranches(), state));

                assertFalse(pattern.matcher("").matches());
                assertFalse(pattern.matcher("v").matches());
                assertFalse(pattern.matcher("vv").matches());
                assertFalse(pattern.matcher("1.a").matches());
                assertFalse(pattern.matcher("1.a.b").matches());
                assertFalse(pattern.matcher("1,x").matches());
                assertFalse(pattern.matcher("1,x,x").matches());
                assertFalse(pattern.matcher("1.x.x.").matches());
                assertFalse(pattern.matcher("1.x.x.x").matches());
                assertFalse(pattern.matcher("v1.a").matches());
                assertFalse(pattern.matcher("v1.a.b").matches());
                assertFalse(pattern.matcher("v1,x").matches());
                assertFalse(pattern.matcher("v1,x,x").matches());
                assertFalse(pattern.matcher("v1.x.x.").matches());
                assertFalse(pattern.matcher("v1.x.x.x").matches());
                assertFalse(pattern.matcher("1.2.3.x").matches());
                assertFalse(pattern.matcher("v1.2.3.x").matches());

                assertFalse(pattern.matcher("r-1").matches());
                assertFalse(pattern.matcher("re-1.x").matches());
                assertFalse(pattern.matcher("rell-1.x.x").matches());
                assertFalse(pattern.matcher("rel.12").matches());
                assertFalse(pattern.matcher("rel:12.x").matches());
                assertFalse(pattern.matcher("rel12.x.x").matches());
                
                assertFalse(pattern.matcher("r/1").matches());
                assertFalse(pattern.matcher("re/1.x").matches());
                assertFalse(pattern.matcher("rell/1.x.x").matches());
                assertFalse(pattern.matcher("rel.12").matches());
                assertFalse(pattern.matcher("rel:12.x").matches());
                assertFalse(pattern.matcher("rel12.x.x").matches());
                
                assertFalse(pattern.matcher("releasee-1").matches());
                
                assertFalse(pattern.matcher("releasee/1").matches());
            }
        }
    }
}