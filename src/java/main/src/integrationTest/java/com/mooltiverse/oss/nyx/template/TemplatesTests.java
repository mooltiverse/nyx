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
package com.mooltiverse.oss.nyx.template;

import static org.junit.jupiter.api.Assertions.*;

import java.io.StringReader;
import java.io.StringWriter;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import com.mooltiverse.oss.nyx.configuration.Configuration;
import com.mooltiverse.oss.nyx.configuration.SimpleConfigurationLayer;
import com.mooltiverse.oss.nyx.entities.git.Action;
import com.mooltiverse.oss.nyx.entities.git.Commit;
import com.mooltiverse.oss.nyx.entities.git.Identity;
import com.mooltiverse.oss.nyx.entities.git.Message;
import com.mooltiverse.oss.nyx.entities.git.Tag;
import com.mooltiverse.oss.nyx.state.State;

@DisplayName("Templates")
public class TemplatesTests {
    @Nested
    @DisplayName("Templates.render with state")
    class RenderWithStateTests {
        /**
         * A test template
         */
        public static final String TEMPLATE = "Version: {{version}} (bumping '{{bump}}' on {{configuration.initialVersion}} using lenient ({{configuration.releaseLenient}}))\nScheme: {{scheme}}\nTimestamp: {{timestamp}}\nPrevious Version: {{releaseScope.previousVersion}} at {{#short5}}{{releaseScope.previousVersionCommit.sha}}{{/short5}}\n\nCommits:\n{{#releaseScope.commits}}\n  {{.}}\n{{/releaseScope.commits}}\n";

        /**
         * The expected output for the template
         */
        public static final String TEMPLATE_OUTPUT = "Version: 9.8.7 (bumping 'theta' on 1.2.3 using lenient (true))\nScheme: SEMVER\nTimestamp: 9223372036854775807\nPrevious Version: 4.5.6 at 05cbf\n\nCommits:\n  d40fcded9e516158a2901f5657794931528af106\n  9bed70fac8a27a4b14b6b12307d034bc59da85c3\n  ef6a6481adb2df26bc7eebfde465e5c2f3e93539\n";

        /**
         * Returns a State instance to be used as the scope for template rendering.
         * 
         * @return the scope for rendering
         * 
         * @throws Exception in case of any issue
         */
        private State getStateScope()
            throws Exception {
            Configuration configuration = new Configuration();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.setBump("theta");
            configurationLayerMock.setReleaseLenient(Boolean.TRUE);
            configurationLayerMock.setInitialVersion("1.2.3");

            configuration.withRuntimeConfiguration(configurationLayerMock);
            State state = new State(configuration);

            state.setVersion("9.8.7");
            state.setTimestamp(Long.MAX_VALUE);

            state.getReleaseScope().setPreviousVersion("4.5.6");
            state.getReleaseScope().setPreviousVersionCommit(new Commit("05cbfd58fadbec3d96b220a0054d96875aa37011", 0, List.<String>of(), new Action(new Identity("Jim", null), null), new Action(new Identity("Sam", null), null), new Message("full", "short", Map.<String,String>of()), Set.<Tag>of(new Tag("4.5.6", "05cbfd58fadbec3d96b220a0054d96875aa37011", false))));
            state.getReleaseScope().setPrimeVersion("1.0.0");
            state.getReleaseScope().setPrimeVersionCommit(new Commit("e8fa442504d91a0187865c74093a5a4212a805f9", 0, List.<String>of(), new Action(new Identity("Jim", null), null), new Action(new Identity("Sam", null), null), new Message("full", "short", Map.<String,String>of()), Set.<Tag>of(new Tag("1.0.0", "e8fa442504d91a0187865c74093a5a4212a805f9", false))));
            state.getReleaseScope().getCommits().add(new Commit("d40fcded9e516158a2901f5657794931528af106", 0, List.<String>of(), new Action(new Identity("Jim", null), null), new Action(new Identity("Sam", null), null), new Message("full", "short", Map.<String,String>of()), Set.<Tag>of()));
            state.getReleaseScope().getCommits().add(new Commit("9bed70fac8a27a4b14b6b12307d034bc59da85c3", 0, List.<String>of(), new Action(new Identity("Jim", null), null), new Action(new Identity("Sam", null), null), new Message("full", "short", Map.<String,String>of()), Set.<Tag>of()));
            state.getReleaseScope().getCommits().add(new Commit("ef6a6481adb2df26bc7eebfde465e5c2f3e93539", 0, List.<String>of(), new Action(new Identity("Jim", null), null), new Action(new Identity("Sam", null), null), new Message("full", "short", Map.<String,String>of()), Set.<Tag>of()));

            return state;
        }

        @Test
        @DisplayName("Templates.render(String, Object)")
        void renderWithStateTest1()
            throws Exception {
            assertEquals(TEMPLATE_OUTPUT, Templates.render(TEMPLATE, getStateScope()));
        }

        @Test
        @DisplayName("Templates.render(Reader, Object)")
        void renderWithStateTest2()
            throws Exception {
            assertEquals(TEMPLATE_OUTPUT, Templates.render(new StringReader(TEMPLATE), getStateScope()));
        }

        @Test
        @DisplayName("Templates.render(Reader, Object, Writer)")
        void renderWithStateTest3()
            throws Exception {
            StringWriter output = new StringWriter();
            Templates.render(new StringReader(TEMPLATE), getStateScope(), output);
            assertEquals(TEMPLATE_OUTPUT, output.toString());
        }
    }
}
