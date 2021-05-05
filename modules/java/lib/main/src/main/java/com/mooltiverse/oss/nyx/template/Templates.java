package com.mooltiverse.oss.nyx.template;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import com.github.mustachejava.DefaultMustacheFactory;
import com.github.mustachejava.Mustache;
import com.github.mustachejava.MustacheFactory;

/**
 * The base class used for template management and rendering, encapsulating the
 * <a href="https://github.com/spullara/mustache.java">Mustache.java</a> library.
 */
public class Templates {
    /**
     * The opening delimiter of a template.
     */
    private static final String OPENING_DELIMITER = "{{";

    /**
     * The closing delimiter of a template.
     */
    private static final String CLOSING_DELIMITER = "}}";

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
     * @param scope the object representing the value to use in rendering
     * 
     * @return a string with the rendered output
     * 
     * @throws IOException in case of I/O issues
     */
    public static String render(String template, Object scope)
        throws IOException {
        StringWriter writer = new StringWriter();
        render(new StringReader(template), scope, writer);
        return writer.toString();
    }

    /**
     * Renders the given template using the given scope to fetch the values.
     * Standard functions are available in the rendering engine.
     * 
     * @param template the template
     * @param scope the object representing the value to use in rendering
     * 
     * @return a string with the rendered output
     * 
     * @throws IOException in case of I/O issues
     */
    public static String render(Reader template, Object scope)
        throws IOException {
        StringWriter writer = new StringWriter();
        render(template, scope, writer);
        return writer.toString();
    }

    /**
     * Renders the given template using the given scope to fetch the values and writing the output to the given writer.
     * Standard functions are available in the rendering engine.
     * 
     * @param template the template
     * @param scope the object representing the value to use in rendering
     * @param writer the object to write the output to
     * 
     * @throws IOException in case of I/O issues
     */
    public static void render(Reader template, Object scope, Writer writer)
        throws IOException {
        MustacheFactory mf = new DefaultMustacheFactory();
        Mustache mustache = mf.compile(template, "template");

        // prepare the scopes with the object passed by the used plust the map of all standard functions
        List<Object> scopes = new ArrayList<Object>(2);
        scopes.add(scope);
        scopes.add(Functions.FUNCTIONS);
        mustache.execute(writer, scopes).flush();
    }

    /**
     * Converts the given value to a boolean.
     * 
     * @param value the text to convert
     * 
     * @return {@code true} if the given value is not {@code null} and not blank, {@code false} in all other cases.
     */
    public static Boolean toBoolean(String value) {
        return Objects.isNull(value) || value.isBlank() ? Boolean.FALSE : Boolean.TRUE;
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
