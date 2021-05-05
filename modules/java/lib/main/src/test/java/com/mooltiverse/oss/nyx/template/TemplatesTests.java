package com.mooltiverse.oss.nyx.template;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.params.provider.Arguments.arguments;

import java.io.StringReader;
import java.io.StringWriter;
import java.util.stream.Stream;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import com.mooltiverse.oss.nyx.configuration.Configuration;
import com.mooltiverse.oss.nyx.configuration.mock.ConfigurationLayerMock;
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
            
            assertTrue(Templates.toBoolean("1"));
            assertTrue(Templates.toBoolean("a"));
            assertTrue(Templates.toBoolean("!"));
            assertTrue(Templates.toBoolean("lkjhewm,òlkàòld wq dsedewdwedw"));
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
        public static final String TEMPLATE = "Version: {{version}} (bumping '{{ bump }}' on {{ configuration.initialVersion }} using lenient ({{ configuration.releaseLenient }}))\nScheme: {{ scheme }}\nTimestamp: {{ timestamp }}\nPrevious Version: {{releaseScope.previousVersion}} at {{#short5}}{{releaseScope.previousVersionCommit}}{{/short5}}\n\nCommits:\n{{#releaseScope.commits}}\n  {{.}}\n{{/releaseScope.commits}}\n";

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
            ConfigurationLayerMock configurationLayerMock = new ConfigurationLayerMock();
            configurationLayerMock.bump = "theta";
            configurationLayerMock.releaseLenient = Boolean.TRUE;
            configurationLayerMock.initialVersion = "1.2.3";

            configuration.withCommandLineConfiguration(configurationLayerMock);
            State state = new State(configuration);

            state.setVersion("9.8.7");
            state.setTimestamp(Long.MAX_VALUE);

            state.getReleaseScope().setPreviousVersion("4.5.6");
            state.getReleaseScope().setPreviousVersionCommit("05cbfd58fadbec3d96b220a0054d96875aa37011");
            state.getReleaseScope().getCommits().add("d40fcded9e516158a2901f5657794931528af106");
            state.getReleaseScope().getCommits().add("9bed70fac8a27a4b14b6b12307d034bc59da85c3");
            state.getReleaseScope().getCommits().add("ef6a6481adb2df26bc7eebfde465e5c2f3e93539");

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
