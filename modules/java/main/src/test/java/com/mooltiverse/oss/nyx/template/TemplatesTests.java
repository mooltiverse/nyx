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
import java.util.stream.Stream;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import com.mooltiverse.oss.nyx.template.mock.TestScope;

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
        void isTemplateWithValidStringTest(String template)
            throws Exception {
            assertTrue(Templates.isTemplate(template));
        }

        @ParameterizedTest(name = "Templates.isTemplate(''{0}'') == false")
        @MethodSource("com.mooltiverse.oss.nyx.template.TemplatesTests#wellKnownInvalidTemplates")
        void isTemplateWithInvalidStringTest(String template)
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
            assertFalse(Templates.toBoolean("11"));
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
            void renderLower()
                throws Exception {
                assertEquals("abcde 12345 fghij", Templates.render("{{#lower}}abcde 12345 FGHIJ{{/lower}}", null));
            }
        }

        @Nested
        @DisplayName("Templates.render Trim function")
        class RenderTrimFunctionTests {
            @Test
            @DisplayName("Templates.render(String, null)")
            void renderTrim()
                throws Exception {
                assertEquals("abcde 12345 FGHIJ", Templates.render("{{#trim}} abcde 12345 FGHIJ  {{/trim}}", null));
            }
        }

        @Nested
        @DisplayName("Templates.render Upper function")
        class RenderUpperFunctionTests {
            @Test
            @DisplayName("Templates.render(String, null)")
            void renderUpper()
                throws Exception {
                assertEquals("ABCDE 12345 FGHIJ", Templates.render("{{#upper}}abcde 12345 FGHIJ{{/upper}}", null));
            }
        }

        @Nested
        @DisplayName("Templates.render First function")
        class RenderFirstFunctionTests {
            @Test
            @DisplayName("Templates.render(String, null)")
            void renderFirst()
                throws Exception {
                assertEquals("ABCDE", Templates.render("{{#first}}ABCDE 12345 FGHIJ{{/first}}", null));
            }
        }

        @Nested
        @DisplayName("Templates.render FirstLower function")
        class RenderFirstLowerFunctionTests {
            @Test
            @DisplayName("Templates.render(String, null)")
            void renderFirstLower()
                throws Exception {
                assertEquals("abcde", Templates.render("{{#firstLower}}ABCDE 12345 FGHIJ{{/firstLower}}", null));
            }
        }

        @Nested
        @DisplayName("Templates.render FirstUpper function")
        class RenderFirstUpperFunctionTests {
            @Test
            @DisplayName("Templates.render(String, null)")
            void renderFirstUpper()
                throws Exception {
                assertEquals("ABCDE", Templates.render("{{#firstUpper}}abcde 12345 FGHIJ{{/firstUpper}}", null));
            }
        }

        @Nested
        @DisplayName("Templates.render Last function")
        class RenderLastFunctionTests {
            @Test
            @DisplayName("Templates.render(String, null)")
            void renderLast()
                throws Exception {
                assertEquals("FGHIJ", Templates.render("{{#last}}ABCDE 12345 FGHIJ{{/last}}", null));
            }
        }

        @Nested
        @DisplayName("Templates.render LastLower function")
        class RenderLastLowerFunctionTests {
            @Test
            @DisplayName("Templates.render(String, null)")
            void renderLastLower()
                throws Exception {
                assertEquals("fghij", Templates.render("{{#lastLower}}ABCDE 12345 FGHIJ{{/lastLower}}", null));
            }
        }

        @Nested
        @DisplayName("Templates.render LastUpper function")
        class RenderLastUpperFunctionTests {
            @Test
            @DisplayName("Templates.render(String, null)")
            void renderLastUpper()
                throws Exception {
                assertEquals("FGHIJ", Templates.render("{{#lastUpper}}abcde 12345 FGHIJ{{/lastUpper}}", null));
            }
        }

        @Nested
        @DisplayName("Templates.render Sanitize function")
        class RenderSanitizeFunctionTests {
            @Test
            @DisplayName("Templates.render(String, null)")
            void renderSanitize()
                throws Exception {
                assertEquals("aBc123defgHI", Templates.render("{{#sanitize}} a!B£c$ 123 d%e&f/g(H)I  {{/sanitize}}", null));
            }
        }

        @Nested
        @DisplayName("Templates.render SanitizeLower function")
        class RenderSanitizeLowerFunctionTests {
            @Test
            @DisplayName("Templates.render(String, null)")
            void renderSanitizeLower()
                throws Exception {
                assertEquals("abc123defghi", Templates.render("{{#sanitizeLower}} a!B£c$ 123 d%e&f/g(H)I  {{/sanitizeLower}}", null));
            }
        }

        @Nested
        @DisplayName("Templates.render SanitizeUpper function")
        class RenderSanitizeUpperFunctionTests {
            @Test
            @DisplayName("Templates.render(String, null)")
            void renderSanitizeUpper()
                throws Exception {
                assertEquals("ABC123DEFGHI", Templates.render("{{#sanitizeUpper}} a!B£c$ 123 d%e&f/g(H)I  {{/sanitizeUpper}}", null));
            }
        }

        @Nested
        @DisplayName("Templates.render Short5 function")
        class RenderShort5FunctionTests {
            @Test
            @DisplayName("Templates.render(String, null)")
            void renderShort5()
                throws Exception {
                assertEquals("12345", Templates.render("{{#short5}}1234567890{{/short5}}", null));
            }
        }

        @Nested
        @DisplayName("Templates.render Short6 function")
        class RenderShort6FunctionTests {
            @Test
            @DisplayName("Templates.render(String, null)")
            void renderShort6()
                throws Exception {
                assertEquals("123456", Templates.render("{{#short6}}1234567890{{/short6}}", null));
            }
        }

        @Nested
        @DisplayName("Templates.render Short7 function")
        class RenderShort7FunctionTests {
            @Test
            @DisplayName("Templates.render(String, null)")
            void renderShort7()
                throws Exception {
                assertEquals("1234567", Templates.render("{{#short7}}1234567890{{/short7}}", null));
            }
        }

        @Nested
        @DisplayName("Templates.render TimestampISO8601 function")
        class RenderTimestampISO8601FunctionTests {
            @Test
            @DisplayName("Templates.render(String, null)")
            void renderTimestampISO8601()
                throws Exception {
                assertEquals("1970-01-01T00:00:00", Templates.render("{{#timestampISO8601}}0{{/timestampISO8601}}", null));
            }
        }

        @Nested
        @DisplayName("Templates.render TimestampYYYYMMDDHHMMSS function")
        class RenderTimestampYYYYMMDDHHMMSSFunctionTests {
            @Test
            @DisplayName("Templates.render(String, null)")
            void renderTimestampYYYYMMDDHHMMSS()
                throws Exception {
                assertEquals("19700101000000", Templates.render("{{#timestampYYYYMMDDHHMMSS}}0{{/timestampYYYYMMDDHHMMSS}}", null));
            }
        }

        @Nested
        @DisplayName("Templates.render EnvironmentVariable function")
        class RenderEnvironmentVariableFunctionTests {
            @Test
            @DisplayName("Templates.render(String, null)")
            void renderEnvironmentVariable()
                throws Exception {
                // just test it's not empty as it changes depending on the runtime environment
                assertFalse(Templates.render("{{#environmentVariable}}PATH{{/environmentVariable}}", null).isBlank());
            }
        }

        // TODO: remove this method as per release 2.0.0 or more
        @Nested
        @DisplayName("Templates.render Environment.Variable function (deprecated dotted name)")
        @Deprecated(since="1.1.0", forRemoval=true) //The dotted name is deprecated and will be removed in future releases. Use the camel case name instead. See the user manual for more."
        class RenderEnvironmentVariableFunctionDeprecatedDottedNameTests {
            @Test
            @DisplayName("Templates.render(String, null) (deprecated dotted name)")
            @Deprecated(since="1.1.0", forRemoval=true) //The dotted name is deprecated and will be removed in future releases. Use the camel case name instead. See the user manual for more."
            void renderEnvironmentVariableDeprecatedDottedName()
                throws Exception {
                // just test it's not empty as it changes depending on the runtime environment
                assertFalse(Templates.render("{{#environment.variable}}PATH{{/environment.variable}}", null).isBlank());
            }
        }

        @Nested
        @DisplayName("Templates.render EnvironmentUser function")
        class RenderEnvironmentUserFunctionTests {
            @Test
            @DisplayName("Templates.render(String, null)")
            void renderEnvironmentUser()
                throws Exception {
                // just test it's not empty as it changes depending on the runtime environment
                assertFalse(Templates.render("{{environmentUser}}", null).isBlank());
                assertFalse(Templates.render("{{#environmentUser}}{{/environmentUser}}", null).isBlank());
            }
        }

        // TODO: remove this method as per release 2.0.0 or more
        @Nested
        @DisplayName("Templates.render Environment.User function (deprecated dotted name)")
        @Deprecated(since="1.1.0", forRemoval=true) //The dotted name is deprecated and will be removed in future releases. Use the camel case name instead. See the user manual for more."
        class RenderEnvironmentUserFunctionDeprecatedDottedNameTests {
            @Test
            @DisplayName("Templates.render(String, null) (deprecated dotted name)")
            @Deprecated(since="1.1.0", forRemoval=true) //The dotted name is deprecated and will be removed in future releases. Use the camel case name instead. See the user manual for more."
            void renderEnvironmentUserDeprecatedDottedName()
                throws Exception {
                // just test it's not empty as it changes depending on the runtime environment
                assertFalse(Templates.render("{{environment.user}}", null).isBlank());
                assertFalse(Templates.render("{{#environment.user}}{{/environment.user}}", null).isBlank());
            }
        }

        @Nested
        @DisplayName("Templates.render FileContent function")
        class RenderFileContentFunctionTests {
            @Test
            @DisplayName("Templates.render(String, null)")
            void renderFileContent()
                throws Exception {
                String hashCode = Integer.toString(this.hashCode());
                File savedFile = new File(System.getProperty("java.io.tmpdir"), "templates1"+hashCode+".txt");
                FileWriter savedFileWriter = new FileWriter(savedFile);
                savedFileWriter.write("test12345");
                savedFileWriter.flush();
                savedFileWriter.close();
                savedFile.deleteOnExit();
                assertEquals("test12345", Templates.render("{{#fileContent}}"+savedFile.getAbsolutePath()+"{{/fileContent}}", null));
            }
        }

        // TODO: remove this method as per release 2.0.0 or more
        @Nested
        @DisplayName("Templates.render File.Content function (deprecated dotted name)")
        @Deprecated(since="1.1.0", forRemoval=true) //The dotted name is deprecated and will be removed in future releases. Use the camel case name instead. See the user manual for more."
        class RenderFileContentFunctionDeprecatedDottedNameTests {
            @Test
            @DisplayName("Templates.render(String, null) (deprecated dotted name)")
            @Deprecated(since="1.1.0", forRemoval=true) //The dotted name is deprecated and will be removed in future releases. Use the camel case name instead. See the user manual for more."
            void renderFileDeprecatedDottedNameContent()
                throws Exception {
                String hashCode = Integer.toString(this.hashCode());
                File savedFile = new File(System.getProperty("java.io.tmpdir"), "templates2"+hashCode+".txt");
                FileWriter savedFileWriter = new FileWriter(savedFile);
                savedFileWriter.write("test12345");
                savedFileWriter.flush();
                savedFileWriter.close();
                savedFile.deleteOnExit();
                assertEquals("test12345", Templates.render("{{#file.content}}"+savedFile.getAbsolutePath()+"{{/file.content}}", null));
            }
        }

        @Nested
        @DisplayName("Templates.render FileExists function")
        class RenderFileExistsFunctionTests {
            @Test
            @DisplayName("Templates.render(String, null)")
            void renderFileExists()
                throws Exception {
                String hashCode = Integer.toString(this.hashCode());
                File savedFile = new File(System.getProperty("java.io.tmpdir"), "templates3"+hashCode+".txt");
                assertEquals("false", Templates.render("{{#fileExists}}"+savedFile.getAbsolutePath()+"{{/fileExists}}", null));
                FileWriter savedFileWriter = new FileWriter(savedFile);
                savedFileWriter.write("test12345");
                savedFileWriter.flush();
                savedFileWriter.close();
                savedFile.deleteOnExit();
                assertEquals("true", Templates.render("{{#fileExists}}"+savedFile.getAbsolutePath()+"{{/fileExists}}", null));
            }
        }

        // TODO: remove this method as per release 2.0.0 or more
        @Nested
        @DisplayName("Templates.render File.Exists function (deprecated dotted name)")
        @Deprecated(since="1.1.0", forRemoval=true) //The dotted name is deprecated and will be removed in future releases. Use the camel case name instead. See the user manual for more."
        class RenderFileExistsFunctionDeprecatedDottedNameTests {
            @Test
            @DisplayName("Templates.render(String, null) (deprecated dotted name)")
            @Deprecated(since="1.1.0", forRemoval=true) //The dotted name is deprecated and will be removed in future releases. Use the camel case name instead. See the user manual for more."
            void renderFileExistsDeprecatedDottedName()
                throws Exception {
                String hashCode = Integer.toString(this.hashCode());
                File savedFile = new File(System.getProperty("java.io.tmpdir"), "templates4"+hashCode+".txt");
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
}
