package com.mooltiverse.oss.nyx.template;

import static com.mooltiverse.oss.nyx.log.Markers.TEMPLATE;

import java.io.FileReader;
import java.io.IOException;
import java.io.StringWriter;
import java.text.SimpleDateFormat;
import java.time.ZoneOffset;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.github.jknack.handlebars.HelperRegistry;
import com.github.jknack.handlebars.Helper;
import com.github.jknack.handlebars.Options;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The class modelling functions to use in dynamic configurations.
 */
class Functions {
    /**
     * The private logger instance
     */
    private static final Logger logger = LoggerFactory.getLogger(Functions.class);

    /**
     * The map of all available functions, where keys are function names to be used in various contexts,
     * and values are function implementations.
     */
    public static Map<String,Function<String,String>> FUNCTIONS = new HashMap<String,Function<String,String>>(){
        {
            put(Lower.NAME,                   new Lower());
            put(Upper.NAME,                   new Upper());
            put(Trim.NAME,                    new Trim());
            put(First.NAME,                   new First());
            put(FirstLower.NAME,              new FirstLower());
            put(FirstUpper.NAME,              new FirstUpper());
            put(Last.NAME,                    new Last());
            put(LastLower.NAME,               new LastLower());
            put(LastUpper.NAME,               new LastUpper());
            put(Sanitize.NAME,                new Sanitize());
            put(SanitizeLower.NAME,           new SanitizeLower());
            put(SanitizeUpper.NAME,           new SanitizeUpper());
            put(Short5.NAME,                  new Short5());
            put(Short6.NAME,                  new Short6());
            put(Short7.NAME,                  new Short7());
            put(TimestampISO8601.NAME,        new TimestampISO8601());
            put(TimestampYYYYMMDDHHMMSS.NAME, new TimestampYYYYMMDDHHMMSS());

            put(EnvironmentUser.NAME,         new EnvironmentUser());
            put(EnvironmentVariable.NAME,     new EnvironmentVariable());

            put(FileContent.NAME,             new FileContent());
            put(FileExists.NAME,              new FileExists());
        }
    };

    /**
     * Default constructor is private on purpose.
     */
    private Functions() {
        super();
    }

    /**
     * Registers all the available functions as a helpers with their names for the given registry.
     * 
     * @param registry the registry to register the helpers to
     */
    public static void registerHelpers(HelperRegistry registry) {
        registry.registerHelper(Lower.NAME,                   new Lower());
        registry.registerHelper(Upper.NAME,                   new Upper());
        registry.registerHelper(Trim.NAME,                    new Trim());
        registry.registerHelper(First.NAME,                   new First());
        registry.registerHelper(FirstLower.NAME,              new FirstLower());
        registry.registerHelper(FirstUpper.NAME,              new FirstUpper());
        registry.registerHelper(Last.NAME,                    new Last());
        registry.registerHelper(LastLower.NAME,               new LastLower());
        registry.registerHelper(LastUpper.NAME,               new LastUpper());
        registry.registerHelper(Sanitize.NAME,                new Sanitize());
        registry.registerHelper(SanitizeLower.NAME,           new SanitizeLower());
        registry.registerHelper(SanitizeUpper.NAME,           new SanitizeUpper());
        registry.registerHelper(Short5.NAME,                  new Short5());
        registry.registerHelper(Short6.NAME,                  new Short6());
        registry.registerHelper(Short7.NAME,                  new Short7());
        registry.registerHelper(TimestampISO8601.NAME,        new TimestampISO8601());
        registry.registerHelper(TimestampYYYYMMDDHHMMSS.NAME, new TimestampYYYYMMDDHHMMSS());

        registry.registerHelper(EnvironmentUser.NAME,         new EnvironmentUser());
        registry.registerHelper(EnvironmentVariable.NAME,     new EnvironmentVariable());

        registry.registerHelper(FileContent.NAME,             new FileContent());
        registry.registerHelper(FileExists.NAME,              new FileExists());
    }

    /**
     * This class provides basic features for functions.
     * 
     * In particular this class offers the {@link Helper} implementation as an adapter method
     * used by Handlebars to invoke the actual business method of the function.
     */
    static abstract class AbstractFunction implements Function<String,String>, Helper<Object> {
        /**
         * {@inheritDoc}}
         */
        @Override
        public Object apply(Object context, Options options)
            throws IOException {
            // The docs are poorly documented by all examples show that options.fn(this) is the only way to have the block resolved
            return (Objects.isNull(options) ? "" : apply(options.fn(this).toString()));
        }
    }

    /**
     * This function returns the lower case representation of the input string.
     */
    public static class Lower extends AbstractFunction {
        /**
         * The name to use for this function.
         */
        public static final String NAME = "lower";

        /**
         * This method returns the lower case representation of the input string.
         * 
         * @param input the input string. If {@code null} an empty string is returned.
         * 
         * @return the lower case representation of the input string.
         */ 
        public String apply(String input) {
            if (Objects.isNull(input))
                return "";

            return input.toLowerCase();
        }
    }

    /**
     * This function returns the trimmed case representation of the input string.
     */
    public static class Trim extends AbstractFunction {
        /**
         * The name to use for this function.
         */
        public static final String NAME = "trim";

        /**
         * This method returns the trimmed case representation of the input string.
         * 
         * @param input the input string. If {@code null} an empty string is returned.
         * 
         * @return the trimmed case representation of the input string.
         */ 
        public String apply(String input) {
            if (Objects.isNull(input))
                return "";

            return input.trim();
        }
    }

    /**
     * This function returns the upper case representation of the input string.
     */
    public static class Upper extends AbstractFunction {
        /**
         * The name to use for this function.
         */
        public static final String NAME = "upper";

        /**
         * This method returns the upper case representation of the input string.
         * 
         * @param input the input string. If {@code null} an empty string is returned.
         * 
         * @return the upper case representation of the input string.
         */ 
        public String apply(String input) {
            if (Objects.isNull(input))
                return "";

            return input.toUpperCase();
        }
    }

    /**
     * This function returns the input string with everything from the first occurrence of a character other
     * than letters and positive digits discarded.
     */
    public static class First extends AbstractFunction {
        /**
         * The name to use for this function.
         */
        public static final String NAME = "first";

        /**
         * This method returns the input string with everything from the first occurrence of a character other
         * than letters and positive digits discarded.
         * 
         * @param input the input string. If {@code null} an empty string is returned.
         * 
         * @return the input string with everything from the first occurrence of a character other
         * than letters and positive digits discarded.
         */ 
        public String apply(String input) {
            if (Objects.isNull(input))
                return "";

            Matcher matcher = Pattern.compile("^([a-zA-Z0-9]*)").matcher(input);
            return matcher.find() ? matcher.group() : "";
        }
    }

    /**
     * This function returns the input string with everything from the first occurrence of a character other
     * than letters and positive digits discarded and the remainder transformed to lower case.
     */
    public static class FirstLower extends AbstractFunction {
        /**
         * The name to use for this function.
         */
        public static final String NAME = "firstLower";

        /**
         * This method returns the input string with everything other than letters and positive digits discarded
         * and the remainder transformed to lower case.
         * 
         * @param input the input string. If {@code null} an empty string is returned.
         * 
         * @return the input string with everything other than letters and positive digits discarded
         * and the remainder transformed to lower case.
         */ 
        public String apply(String input) {
            if (Objects.isNull(input))
                return "";

            return new Lower().apply((new First().apply(input)));
        }
    }

    /**
     * This function returns the input string with everything from the first occurrence of a character other
     * than letters and positive digits discarded and the remainder transformed to upper case.
     */
    public static class FirstUpper extends AbstractFunction {
        /**
         * The name to use for this function.
         */
        public static final String NAME = "firstUpper";

        /**
         * This method returns the input string with everything other than letters and positive digits discarded
         * and the remainder transformed to upper case.
         * 
         * @param input the input string. If {@code null} an empty string is returned.
         * 
         * @return the input string with everything other than letters and positive digits discarded
         * and the remainder transformed to upper case.
         */ 
        public String apply(String input) {
            if (Objects.isNull(input))
                return "";

            return new Upper().apply((new First().apply(input)));
        }
    }

    /**
     * This function returns the last part of the input string that does not contains characters other than
     * letters and positive digits.
     */
    public static class Last extends AbstractFunction {
        /**
         * The name to use for this function.
         */
        public static final String NAME = "last";

        /**
         * This method returns the last part of the input string that does not contains characters other than.
         * letters and positive digits.
         * 
         * @param input the input string. If {@code null} an empty string is returned.
         * 
         * @return the last part of the input string that does not contains characters other than.
         * letters and positive digits.
         */ 
        public String apply(String input) {
            if (Objects.isNull(input))
                return "";

            Matcher matcher = Pattern.compile("(^[a-zA-Z0-9]*)?([a-zA-Z0-9]*)$").matcher(input);
            return matcher.find() ? matcher.group() : "";
        }
    }

    /**
     * This function returns the last part of the input string that does not contains characters other than
     * letters and positive digits and the remainder transformed to lower case.
     */
    public static class LastLower extends AbstractFunction {
        /**
         * The name to use for this function.
         */
        public static final String NAME = "lastLower";

        /**
         * This method returns the last part of the input string that does not contains characters other than
         * letters and positive digits and the remainder transformed to lower case.
         * 
         * @param input the input string. If {@code null} an empty string is returned.
         * 
         * @return the last part of the input string that does not contains characters other than
         * letters and positive digits and the remainder transformed to lower case.
         */ 
        public String apply(String input) {
            if (Objects.isNull(input))
                return "";

            return new Lower().apply((new Last().apply(input)));
        }
    }

    /**
     * This function returns the last part of the input string that does not contains characters other than
     * letters and positive digits and the remainder transformed to upper case.
     */
    public static class LastUpper extends AbstractFunction {
        /**
         * The name to use for this function.
         */
        public static final String NAME = "lastUpper";

        /**
         * This method returns the last part of the input string that does not contains characters other than
         * letters and positive digits and the remainder transformed to upper case.
         * 
         * @param input the input string. If {@code null} an empty string is returned.
         * 
         * @return the last part of the input string that does not contains characters other than
         * letters and positive digits and the remainder transformed to upper case.
         */ 
        public String apply(String input) {
            if (Objects.isNull(input))
                return "";

            return new Upper().apply((new Last().apply(input)));
        }
    }

    /**
     * This function returns the input string with everything other than letters and positive digits discarded.
     */
    public static class Sanitize extends AbstractFunction {
        /**
         * The name to use for this function.
         */
        public static final String NAME = "sanitize";

        /**
         * This method returns the input string with everything other than letters and positive digits discarded.
         * 
         * @param input the input string. If {@code null} an empty string is returned.
         * 
         * @return the input string with everything other than letters and positive digits discarded.
         */ 
        public String apply(String input) {
            if (Objects.isNull(input))
                return "";

            return input.replaceAll("[^a-zA-Z0-9]", "");
        }
    }

    /**
     * This function returns the input string with everything other than letters and positive digits discarded
     * and the remainder transformed to lower case.
     */
    public static class SanitizeLower extends AbstractFunction {
        /**
         * The name to use for this function.
         */
        public static final String NAME = "sanitizeLower";

        /**
         * This method returns the input string with everything other than letters and positive digits discarded
         * and the remainder transformed to lower case.
         * 
         * @param input the input string. If {@code null} an empty string is returned.
         * 
         * @return the input string with everything other than letters and positive digits discarded
         * and the remainder transformed to lower case.
         */ 
        public String apply(String input) {
            if (Objects.isNull(input))
                return "";

            return new Lower().apply(new Sanitize().apply(input));
        }
    }

    /**
     * This function returns the input string with everything other than letters and positive digits discarded
     * and the remainder transformed to upper case.
     */
    public static class SanitizeUpper extends AbstractFunction {
        /**
         * The name to use for this function.
         */
        public static final String NAME = "sanitizeUpper";

        /**
         * This method returns the input string with everything other than letters and positive digits discarded
         * and the remainder transformed to upper case.
         * 
         * @param input the input string. If {@code null} an empty string is returned.
         * 
         * @return the input string with everything other than letters and positive digits discarded
         * and the remainder transformed to upper case.
         */ 
        public String apply(String input) {
            if (Objects.isNull(input))
                return "";

            return new Upper().apply(new Sanitize().apply(input));
        }
    }
    
    /**
     * This function returns the first 5 characters of the input string, if it's longer than 5 characters,
     * otherwise returns the input string.
     */
    public static class Short5 extends AbstractFunction {
        /**
         * The name to use for this function.
         */
        public static final String NAME = "short5";

        /**
         * This method returns the first 5 characters of the input string, if it's longer than 5 characters,
         * otherwise returns the input string or the empty string if the input is {code null}.
         * 
         * @param input the input string. If {@code null} an empty string is returned.
         * 
         * @return the first 5 characters of the input string, if it's longer than 5 characters,
         * otherwise returns the input string.
         */ 
        public String apply(String input) {
            if (Objects.isNull(input))
                return "";

            return input.length() > 5 ? input.substring(0, 5) : input;
        }
    }

    /**
     * This function returns the first 6 characters of the input string, if it's longer than 6 characters,
     * otherwise returns the input string.
     */
    public static class Short6 extends AbstractFunction {
        /**
         * The name to use for this function.
         */
        public static final String NAME = "short6";

        /**
         * This method returns the first 6 characters of the input string, if it's longer than 6 characters,
         * otherwise returns the input string or the empty string if the input is {code null}.
         * 
         * @param input the input string. If {@code null} an empty string is returned.
         * 
         * @return the first 6 characters of the input string, if it's longer than 6 characters,
         * otherwise returns the input string.
         */ 
        public String apply(String input) {
            if (Objects.isNull(input))
                return "";

            return input.length() > 6 ? input.substring(0, 6) : input;
        }
    }

    /**
     * This function returns the first 7 characters of the input string, if it's longer than 7 characters,
     * otherwise returns the input string.
     */
    public static class Short7 extends AbstractFunction {
        /**
         * The name to use for this function.
         */
        public static final String NAME = "short7";

        /**
         * This method returns the first 7 characters of the input string, if it's longer than 7 characters,
         * otherwise returns the input string or the empty string if the input is {code null}.
         * 
         * @param input the input string. If {@code null} an empty string is returned.
         * 
         * @return the first 7 characters of the input string, if it's longer than 7 characters,
         * otherwise returns the input string.
         */ 
        public String apply(String input) {
            if (Objects.isNull(input))
                return "";

            return input.length() > 7 ? input.substring(0, 7) : input;
        }
    }

    /**
     * This function parses the input string as a {@link Long} representing a timestamp in the
     * <a href="https://www.unixtimestamp.com/">unix format</a> and returns it formatted as
     * <a href="https://en.wikipedia.org/wiki/ISO_8601">ISO 8601</a> UTC timestamp.
     */
    public static class TimestampISO8601 extends AbstractFunction {
        /**
         * The name to use for this function.
         */
        public static final String NAME = "timestampISO8601";

        /**
         * This method parses the input string as a {@link Long} representing a timestamp in the
         * <a href="https://www.unixtimestamp.com/">unix format</a> and returns it formatted as
         * <a href="https://en.wikipedia.org/wiki/ISO_8601">ISO 8601</a> UTC timestamp.
         * If the input value fails to parse for whatever reason then the emty string is returned
         * (and an error is logged).
         * 
         * @param input the input string. If {@code null} an empty string is returned.
         * 
         * @return the ISO 8601 representation of the input timestamp, or an empty string if parsing the input
         * fails for any reson.
         */ 
        public String apply(String input) {
            if (Objects.isNull(input))
                return "";
            
            try {
                Date date = new Date(Long.valueOf(input).longValue());
                SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
                dateFormat.setTimeZone(TimeZone.getTimeZone(ZoneOffset.UTC));
                return dateFormat.format(date);
            }
            catch (NumberFormatException nfe) {
                logger.error(TEMPLATE, "String '{}' does not represent a valid Long timestamp", input);
                return "";
            }
        }
    }

    /**
     * This function parses the input string as a {@link Long} representing a timestamp in the
     * <a href="https://www.unixtimestamp.com/">unix format</a> and returns it formatted as
     * {@code YYYYMMDDHHMMSS} UTC.
     */
    public static class TimestampYYYYMMDDHHMMSS extends AbstractFunction {
        /**
         * The name to use for this function.
         */
        public static final String NAME = "timestampYYYYMMDDHHMMSS";

        /**
         * This method parses the input string as a {@link Long} representing a timestamp in the
         * <a href="https://www.unixtimestamp.com/">unix format</a> and returns it formatted as
         * {@code YYYYMMDDHHMMSS} UTC.
         * If the input value fails to parse for whatever reason then the emty string is returned
         * (and an error is logged).
         * 
         * @param input the input string. If {@code null} an empty string is returned.
         * 
         * @return the ISO 8601 representation of the input timestamp, or an empty string if parsing the input
         * fails for any reson.
         */ 
        public String apply(String input) {
            if (Objects.isNull(input))
                return "";
            
            try {
                Date date = new Date(Long.valueOf(input).longValue());
                SimpleDateFormat dateFormat = new SimpleDateFormat("yyyyMMddHHmmss");
                dateFormat.setTimeZone(TimeZone.getTimeZone(ZoneOffset.UTC));
                return dateFormat.format(date);
            }
            catch (NumberFormatException nfe) {
                logger.error(TEMPLATE, "String '{}' does not represent a valid Long timestamp", input);
                return "";
            }
        }
    }

    /**
     * This function returns the value of the environment variable with the given name, if any.
     */
    public static class EnvironmentVariable extends AbstractFunction {
        /**
         * The name to use for this function.
         */
        public static final String NAME = "environmentVariable";

        /**
         * This method returns the value of the environment variable with the given name, if any.
         * 
         * @param name the name of the requested variable. If {@code null} an empty string is returned.
         * 
         * @return the value of the environment variable with the given name, if any.
         */ 
        public String apply(String name) {
            if (Objects.isNull(name))
                return "";

            String res = System.getenv(name);
            return Objects.isNull(res) ? "" : res;
        }
    }

    /**
     * This function returns the current user name. The input parameter is ignored.
     */
    public static class EnvironmentUser extends AbstractFunction {
        /**
         * The name to use for this function.
         */
        public static final String NAME = "environmentUser";

        /**
         * This method returns the current user name.
         * 
         * @param name ignored.
         * 
         * @return the current user name.
         */ 
        public String apply(String name) {
            return System.getProperty("user.name");
        }
    }

    /**
     * This function returns the content of the given file, if any.
     */
    public static class FileContent extends AbstractFunction {
        /**
         * The name to use for this function.
         */
        public static final String NAME = "fileContent";

        /**
         * This method returns the content of the given file, if any.
         * 
         * @param path the path of file to get the content from. When a relative path is used it's resolved
         * against the system current directory (configured directories are ignored here).
         * 
         * @return the content of the given file, if any.
         */ 
        public String apply(String path) {
            if (Objects.isNull(path))
                return "";
            
            try {
                java.io.File f = new java.io.File(path);
                logger.trace(TEMPLATE, "Trying to read contents from file '{}' ('{}')", path, f.getAbsolutePath());
                FileReader reader = new FileReader(f);
                StringWriter writer = new StringWriter();
                reader.transferTo(writer);
                reader.close();
                writer.flush();
                return writer.toString();
            }
            catch (IOException ioe) {
                logger.error(TEMPLATE, "Unable to read from file '{}': '{}'", path, ioe.getMessage());
                logger.trace(TEMPLATE, "Unable to read from file", ioe);
                return "";
            }
        }
    }

    /**
     * This function returns {@code true} if the file with the given name exists, {@code false} otherwise.
     */
    public static class FileExists extends AbstractFunction {
        /**
         * The name to use for this function.
         */
        public static final String NAME = "fileExists";

        /**
         * This method returns {@code true} if the file with the given name exists, {@code false} otherwise.
         * 
         * @param path the path of file to check. When a relative path is used it's resolved against the system
         * current directory (configured directories are ignored here).
         * 
         * @return {@code true} if the file with the given name exists, {@code false} otherwise.
         */ 
        public String apply(String path) {
            if (Objects.isNull(path))
                return Boolean.FALSE.toString();

            java.io.File f = new java.io.File(path);
            logger.trace(TEMPLATE, "Checking whether file '{}' ('{}') exists", path, f.getAbsolutePath());
            return Boolean.toString(f.exists() && f.isFile());
        }
    }
}
