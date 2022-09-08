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
import static org.junit.jupiter.params.provider.Arguments.arguments;

import java.io.File;
import java.io.FileWriter;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import com.mooltiverse.oss.nyx.configuration.Configuration;
import com.mooltiverse.oss.nyx.configuration.SimpleConfigurationLayer;
import com.mooltiverse.oss.nyx.entities.git.Action;
import com.mooltiverse.oss.nyx.entities.git.Commit;
import com.mooltiverse.oss.nyx.entities.git.Identity;
import com.mooltiverse.oss.nyx.entities.git.Message;
import com.mooltiverse.oss.nyx.entities.git.Tag;
import com.mooltiverse.oss.nyx.template.mock.TestScope;
import com.mooltiverse.oss.nyx.state.State;

@DisplayName("Templates")
public class TemplatesTests {
    

    /**
     * A {@link MethodSource} method that returns valid structured data to test templates.
     * Each returned argument has the fields:<br>
     * - template: the template string<br>
     *
     * @return a stream of arguments representing correct templates
     */
    static Stream<Arguments> wellKnownValidTemplates() {
        return Stream.of(
            arguments("{{}}"),
            arguments("{{ }}"),
            arguments(" {{ }} "),
            arguments("{{scheme}}"),
            arguments(" {{ scheme }} ")
        );
    }

    /**
     * A {@link MethodSource} method that returns invalid structured data to test templates.
     * Each returned argument has the fields:<br>
     * - template: the template string<br>
     *
     * @return a stream of arguments representing incorrect templates
     */
    static Stream<Arguments> wellKnownInvalidTemplates() {
        return Stream.of(
            arguments("{}}"),
            arguments("}}"),
            arguments("{{}"),
            arguments("{{"),
            arguments("{}"),
            arguments("{"),
            arguments("}"),
            arguments("")
        );
    }

    @Nested
    @DisplayName("Templates.isTemplate")
    class IsTemplateTests {
        @ParameterizedTest(name = "Templates.isTemplate(''{0}'') == true")
        @MethodSource("com.mooltiverse.oss.nyx.template.TemplatesTests#wellKnownValidTemplates")
        void isTemplateWIthValidStringTest(String template)
            throws Exception {
            assertTrue(Templates.isTemplate(template));
        }

        @ParameterizedTest(name = "Templates.isTemplate(''{0}'') == false")
        @MethodSource("com.mooltiverse.oss.nyx.template.TemplatesTests#wellKnownInvalidTemplates")
        void isTemplateWIthInvalidStringTest(String template)
            throws Exception {
            assertFalse(Templates.isTemplate(template));
        }
    }

    @Nested
    @DisplayName("Templates.toBoolean")
    class ToBooleanTests {
        @Test
        @DisplayName("Templates.toBoolean(String)")
        void toBooleanTest()
            throws Exception {
            assertFalse(Templates.toBoolean(null));
            assertFalse(Templates.toBoolean(""));
            assertFalse(Templates.toBoolean("    "));
            assertFalse(Templates.toBoolean("false"));
            assertFalse(Templates.toBoolean("FALSE"));
            
            assertFalse(Templates.toBoolean("1"));
            assertFalse(Templates.toBoolean("a"));
            assertFalse(Templates.toBoolean("!"));
            assertFalse(Templates.toBoolean("lkjhewm,òlkàòld wq dsedewdwedw"));

            assertTrue(Templates.toBoolean("true"));
            assertTrue(Templates.toBoolean("TRUE"));
        }
    }

    @Nested
    @DisplayName("Templates.toInteger")
    class ToIntegerTests {
        @Test
        @DisplayName("Templates.toInteger(String)")
        void toIntegerTest()
            throws Exception {
            assertEquals(0, Templates.toInteger(null));
            assertEquals(0, Templates.toInteger(""));
            assertEquals(0, Templates.toInteger("    "));
            assertEquals(0, Templates.toInteger("a"));
            assertEquals(0, Templates.toInteger("!"));
            assertEquals(0, Templates.toInteger("lkjhewm,òlkàòld wq dsedewdwedw"));
            assertEquals(0, Templates.toInteger("1.0"));
            assertEquals(0, Templates.toInteger("1,0"));

            assertEquals(1, Templates.toInteger("1"));
            assertEquals(100, Templates.toInteger("100"));
            assertEquals(9999999, Templates.toInteger("9999999"));
        }
    }

    @Nested
    @DisplayName("Templates.render functions")
    class RenderFunctionsTests {
        @Nested
        @DisplayName("Templates.render Lower function")
        class RenderLowerFunctionTests {
            @Test
            @DisplayName("Templates.render(String, null)")
            void renderWithNullScopeTest1()
                throws Exception {
                assertEquals("abcde 12345 fghij", Templates.render("{{#lower}}abcde 12345 FGHIJ{{/lower}}", null));
            }
        }

        @Nested
        @DisplayName("Templates.render Trim function")
        class RenderTrimFunctionTests {
            @Test
            @DisplayName("Templates.render(String, null)")
            void renderWithNullScopeTest1()
                throws Exception {
                assertEquals("abcde 12345 FGHIJ", Templates.render("{{#trim}} abcde 12345 FGHIJ  {{/trim}}", null));
            }
        }

        @Nested
        @DisplayName("Templates.render Upper function")
        class RenderUpperFunctionTests {
            @Test
            @DisplayName("Templates.render(String, null)")
            void renderWithNullScopeTest1()
                throws Exception {
                assertEquals("ABCDE 12345 FGHIJ", Templates.render("{{#upper}}abcde 12345 FGHIJ{{/upper}}", null));
            }
        }

        @Nested
        @DisplayName("Templates.render First function")
        class RenderFirstFunctionTests {
            @Test
            @DisplayName("Templates.render(String, null)")
            void renderWithNullScopeTest1()
                throws Exception {
                assertEquals("ABCDE", Templates.render("{{#first}}ABCDE 12345 FGHIJ{{/first}}", null));
            }
        }

        @Nested
        @DisplayName("Templates.render FirstLower function")
        class RenderFirstLowerFunctionTests {
            @Test
            @DisplayName("Templates.render(String, null)")
            void renderWithNullScopeTest1()
                throws Exception {
                assertEquals("abcde", Templates.render("{{#firstLower}}ABCDE 12345 FGHIJ{{/firstLower}}", null));
            }
        }

        @Nested
        @DisplayName("Templates.render FirstUpper function")
        class RenderFirstUpperFunctionTests {
            @Test
            @DisplayName("Templates.render(String, null)")
            void renderWithNullScopeTest1()
                throws Exception {
                assertEquals("ABCDE", Templates.render("{{#firstUpper}}abcde 12345 FGHIJ{{/firstUpper}}", null));
            }
        }

        @Nested
        @DisplayName("Templates.render Last function")
        class RenderLastFunctionTests {
            @Test
            @DisplayName("Templates.render(String, null)")
            void renderWithNullScopeTest1()
                throws Exception {
                assertEquals("FGHIJ", Templates.render("{{#last}}ABCDE 12345 FGHIJ{{/last}}", null));
            }
        }

        @Nested
        @DisplayName("Templates.render LastLower function")
        class RenderLastLowerFunctionTests {
            @Test
            @DisplayName("Templates.render(String, null)")
            void renderWithNullScopeTest1()
                throws Exception {
                assertEquals("fghij", Templates.render("{{#lastLower}}ABCDE 12345 FGHIJ{{/lastLower}}", null));
            }
        }

        @Nested
        @DisplayName("Templates.render LastUpper function")
        class RenderLastUpperFunctionTests {
            @Test
            @DisplayName("Templates.render(String, null)")
            void renderWithNullScopeTest1()
                throws Exception {
                assertEquals("FGHIJ", Templates.render("{{#lastUpper}}abcde 12345 FGHIJ{{/lastUpper}}", null));
            }
        }

        @Nested
        @DisplayName("Templates.render Sanitize function")
        class RenderSanitizeFunctionTests {
            @Test
            @DisplayName("Templates.render(String, null)")
            void renderWithNullScopeTest1()
                throws Exception {
                assertEquals("aBc123defgHI", Templates.render("{{#sanitize}} a!B£c$ 123 d%e&f/g(H)I  {{/sanitize}}", null));
            }
        }

        @Nested
        @DisplayName("Templates.render SanitizeLower function")
        class RenderSanitizeLowerFunctionTests {
            @Test
            @DisplayName("Templates.render(String, null)")
            void renderWithNullScopeTest1()
                throws Exception {
                assertEquals("abc123defghi", Templates.render("{{#sanitizeLower}} a!B£c$ 123 d%e&f/g(H)I  {{/sanitizeLower}}", null));
            }
        }

        @Nested
        @DisplayName("Templates.render SanitizeUpper function")
        class RenderSanitizeUpperFunctionTests {
            @Test
            @DisplayName("Templates.render(String, null)")
            void renderWithNullScopeTest1()
                throws Exception {
                assertEquals("ABC123DEFGHI", Templates.render("{{#sanitizeUpper}} a!B£c$ 123 d%e&f/g(H)I  {{/sanitizeUpper}}", null));
            }
        }

        @Nested
        @DisplayName("Templates.render Short5 function")
        class RenderShort5FunctionTests {
            @Test
            @DisplayName("Templates.render(String, null)")
            void renderWithNullScopeTest1()
                throws Exception {
                assertEquals("12345", Templates.render("{{#short5}}1234567890{{/short5}}", null));
            }
        }

        @Nested
        @DisplayName("Templates.render Short6 function")
        class RenderShort6FunctionTests {
            @Test
            @DisplayName("Templates.render(String, null)")
            void renderWithNullScopeTest1()
                throws Exception {
                assertEquals("123456", Templates.render("{{#short6}}1234567890{{/short6}}", null));
            }
        }

        @Nested
        @DisplayName("Templates.render Short7 function")
        class RenderShort7FunctionTests {
            @Test
            @DisplayName("Templates.render(String, null)")
            void renderWithNullScopeTest1()
                throws Exception {
                assertEquals("1234567", Templates.render("{{#short7}}1234567890{{/short7}}", null));
            }
        }

        @Nested
        @DisplayName("Templates.render TimestampISO8601 function")
        class RenderTimestampISO8601FunctionTests {
            @Test
            @DisplayName("Templates.render(String, null)")
            void renderWithNullScopeTest1()
                throws Exception {
                assertEquals("1970-01-01T00:00:00", Templates.render("{{#timestampISO8601}}0{{/timestampISO8601}}", null));
            }
        }

        @Nested
        @DisplayName("Templates.render TimestampYYYYMMDDHHMMSS function")
        class RenderTimestampYYYYMMDDHHMMSSFunctionTests {
            @Test
            @DisplayName("Templates.render(String, null)")
            void renderWithNullScopeTest1()
                throws Exception {
                assertEquals("19700101000000", Templates.render("{{#timestampYYYYMMDDHHMMSS}}0{{/timestampYYYYMMDDHHMMSS}}", null));
            }
        }

        @Nested
        @DisplayName("Templates.render Environment.Variable function")
        class RenderEnvironmentVariableFunctionTests {
            @Test
            @DisplayName("Templates.render(String, null)")
            void renderWithNullScopeTest1()
                throws Exception {
                // just test it's not empty as it changes depending on the runtime environment
                assertFalse(Templates.render("{{#environment.variable}}PATH{{/environment.variable}}", null).isBlank());
            }
        }

        @Nested
        @DisplayName("Templates.render Environment.User function")
        class RenderEnvironmentUserFunctionTests {
            @Test
            @DisplayName("Templates.render(String, null)")
            void renderWithNullScopeTest1()
                throws Exception {
                // just test it's not empty as it changes depending on the runtime environment
                assertFalse(Templates.render("{{environment.user}}", null).isBlank());
                assertFalse(Templates.render("{{#environment.user}}{{/environment.user}}", null).isBlank());
            }
        }

        @Nested
        @DisplayName("Templates.render File.Content function")
        class RenderFileContentFunctionTests {
            @Test
            @DisplayName("Templates.render(String, null)")
            void renderWithNullScopeTest1()
                throws Exception {
                String hashCode = Integer.toString(this.hashCode());
                File savedFile = new File(System.getProperty("java.io.tmpdir"), "templates1"+hashCode+".txt");
                FileWriter savedFileWriter = new FileWriter(savedFile);
                savedFileWriter.write("test12345");
                savedFileWriter.flush();
                savedFileWriter.close();
                savedFile.deleteOnExit();
                assertEquals("test12345", Templates.render("{{#file.content}}"+savedFile.getAbsolutePath()+"{{/file.content}}", null));
            }
        }

        @Nested
        @DisplayName("Templates.render File.Exists function")
        class RenderFileExistsFunctionTests {
            @Test
            @DisplayName("Templates.render(String, null)")
            void renderWithNullScopeTest1()
                throws Exception {
                String hashCode = Integer.toString(this.hashCode());
                File savedFile = new File(System.getProperty("java.io.tmpdir"), "templates1"+hashCode+".txt");
                assertEquals("false", Templates.render("{{#file.exists}}"+savedFile.getAbsolutePath()+"{{/file.exists}}", null));
                FileWriter savedFileWriter = new FileWriter(savedFile);
                savedFileWriter.write("test12345");
                savedFileWriter.flush();
                savedFileWriter.close();
                savedFile.deleteOnExit();
                assertEquals("true", Templates.render("{{#file.exists}}"+savedFile.getAbsolutePath()+"{{/file.exists}}", null));
            }
        }
    }

    @Nested
    @DisplayName("Templates.render with null scope")
    class RenderWithNullScopeTests {
        /**
         * A test template
         */
        public static final String TEMPLATE = "{{#items}}\nName: {{name}}\n\nPrice: {{price}}\n  {{#features}}\n  Feature: {{description}}\n  {{/features}}\n{{/items}}";

        /**
         * The expected output for the template
         */
        public static final String TEMPLATE_OUTPUT = "";

        @Test
        @DisplayName("Templates.render(String, null)")
        void renderWithNullScopeTest1()
            throws Exception {
            assertEquals(TEMPLATE_OUTPUT, Templates.render(TEMPLATE, null));
        }

        @Test
        @DisplayName("Templates.render(Reader, null)")
        void renderWithNullScopeTest2()
            throws Exception {
            assertEquals(TEMPLATE_OUTPUT, Templates.render(new StringReader(TEMPLATE), null));
        }

        @Test
        @DisplayName("Templates.render(Reader, null, Writer)")
        void renderWithNullScopeTest3()
            throws Exception {
            StringWriter output = new StringWriter();
            Templates.render(new StringReader(TEMPLATE), null, output);
            assertEquals(TEMPLATE_OUTPUT, output.toString());
        }
    }

    @Nested
    @DisplayName("Templates.render with mock")
    class RenderWithMockTests {
        /**
         * A test template
         */
        public static final String TEMPLATE = "{{#items}}\nName: {{name}}\n\nPrice: {{price}}\n  {{#features}}\n  Feature: {{description}}\n  {{/features}}\n{{/items}}";

        /**
         * The expected output for the template
         */
        public static final String TEMPLATE_OUTPUT = "Name: Item 1\n\nPrice: $19.99\n  Feature: New!\n  Feature: Awesome!\nName: Item 2\n\nPrice: $29.99\n  Feature: Old.\n  Feature: Ugly.\n";

        @Test
        @DisplayName("Templates.render(String, Object)")
        void renderWithMockTest1()
            throws Exception {
            assertEquals(TEMPLATE_OUTPUT, Templates.render(TEMPLATE, new TestScope()));
        }

        @Test
        @DisplayName("Templates.render(Reader, Object)")
        void renderWithMockTest2()
            throws Exception {
            assertEquals(TEMPLATE_OUTPUT, Templates.render(new StringReader(TEMPLATE), new TestScope()));
        }

        @Test
        @DisplayName("Templates.render(Reader, Object, Writer)")
        void renderWithMockTest3()
            throws Exception {
            StringWriter output = new StringWriter();
            Templates.render(new StringReader(TEMPLATE), new TestScope(), output);
            assertEquals(TEMPLATE_OUTPUT, output.toString());
        }
    }

    @Nested
    @DisplayName("Templates.render with state")
    class RenderWithStateTests {
        /**
         * A test template
         */
        public static final String TEMPLATE = "Version: {{version}} (bumping '{{bump}}' on {{configuration.initialVersion}} using lenient ({{configuration.releaseLenient}}))\nScheme: {{scheme}}\nTimestamp: {{timestamp}}\nPrevious Version: {{releaseScope.previousVersion}} at {{#short5}}{{releaseScope.previousVersionCommit}}{{/short5}}\n\nCommits:\n{{#releaseScope.commits}}\n  {{.}}\n{{/releaseScope.commits}}\n";

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
