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

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import com.github.jknack.handlebars.Context;
import com.github.jknack.handlebars.context.FieldValueResolver;
import com.github.jknack.handlebars.context.JavaBeanValueResolver;
import com.github.jknack.handlebars.context.MapValueResolver;
import com.github.jknack.handlebars.context.MethodValueResolver;
import com.github.jknack.handlebars.Handlebars;

/**
 * The base class used for template management and rendering, encapsulating the
 * <a href="https://jknack.github.io/handlebars.java/">Handlebars.java</a> library.
 */
public class Templates {
    /**
     * The opening delimiter of a template.
     */
    private static final String OPENING_DELIMITER = Handlebars.DELIM_START;

    /**
     * The closing delimiter of a template.
     */
    private static final String CLOSING_DELIMITER = Handlebars.DELIM_END;

    /**
     * Default constructor is private on purpose.
     */
    private Templates() {
        super();
    }

    /**
     * Returns {@code true} if the given buffer is a template, {@code false} otherwise.
     * The buffer is a template if it has the opening <code>{{</code> and closing <code>}}</code>
     * delimiters.
     * This method does not check for syntactic correctness of the template.
     * 
     * @param buffer the buffer to test
     * 
     * @return {@code true} if the given buffer is a template, {@code false} otherwise.
     */
    public static boolean isTemplate(String buffer) {
        int startDelimiterIndex = buffer.indexOf(OPENING_DELIMITER);
        return startDelimiterIndex >=0 ? buffer.indexOf(CLOSING_DELIMITER, startDelimiterIndex) >= 0 : false;
    }

    /**
     * Renders the given template using the given scope to fetch the values.
     * Standard functions are available in the rendering engine.
     * 
     * @param template the template
     * @param scope the object representing the value to use in rendering. If {@code null} it won't be used.
     * 
     * @return a string with the rendered output
     * 
     * @throws IOException in case of I/O issues
     */
    public static String render(String template, Object scope)
        throws IOException {
        // prepare the context with the value object (scope) and all the available resolvers
        Context handlebarsContext = Context.newBuilder(scope)
            .resolver(
                JavaBeanValueResolver.INSTANCE,
                MethodValueResolver.INSTANCE,
                FieldValueResolver.INSTANCE,
                MapValueResolver.INSTANCE
            ).build();

        Handlebars handlebars = new Handlebars();
        // register the helper functions
        Functions.registerHelpers(handlebars);
        // this avoids handlebar to add extra new lines and, instead,
        // stick with Mustache's rules about spaces and new lines
        handlebars.prettyPrint(true);
        return handlebars.compileInline(template).apply(handlebarsContext);
    }

    /**
     * Renders the given template using the given scope to fetch the values.
     * Standard functions are available in the rendering engine.
     * 
     * @param template the template
     * @param scope the object representing the value to use in rendering. If {@code null} it won't be used.
     * 
     * @return a string with the rendered output
     * 
     * @throws IOException in case of I/O issues
     */
    public static String render(Reader template, Object scope)
        throws IOException {
        StringWriter templateString = new StringWriter();
        template.transferTo(templateString);
        return render(templateString.toString(), scope);
    }

    /**
     * Renders the given template using the given scope to fetch the values and writing the output to the given writer.
     * Standard functions are available in the rendering engine.
     * 
     * @param template the template
     * @param scope the object representing the value to use in rendering. If {@code null} it won't be used.
     * @param writer the object to write the output to
     * 
     * @throws IOException in case of I/O issues
     */
    public static void render(Reader template, Object scope, Writer writer)
        throws IOException {
        writer.write(render(template, scope));
    }

    /**
     * Converts the given value to a boolean.
     * 
     * @param value the text to convert
     * 
     * @return {@code true} if the given value is not {@code null} and not blank and it is a string representation of a {@code true},
     * {@code false} in all other cases.
     */
    public static Boolean toBoolean(String value) {
        return Boolean.valueOf(value);
    }

    /**
     * Converts the given value to an integer.
     * 
     * @param value the text to convert
     * 
     * @return the integer representation of the given string, when the given string contains a valid integer
     * representation, or 0 otherwise.
     */
    public static Integer toInteger(String value) {
        try {
            return Integer.valueOf(value);
        }
        catch (NumberFormatException nfe) {
            return Integer.valueOf(0);
        }
    }
}
