package com.mooltiverse.oss.nyx.template;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;

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
        mustache.execute(writer, scope).flush();
    }
}
