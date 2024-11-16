package com.mooltiverse.oss.nyx.template;

import static com.mooltiverse.oss.nyx.log.Markers.TEMPLATE;

import java.io.FileReader;
import java.io.IOException;
import java.io.StringWriter;
import java.text.SimpleDateFormat;
import java.time.ZoneOffset;
import java.util.Date;
import java.util.Map;
import java.util.Objects;
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

        registry.registerHelper(Capture.NAME,                 new Capture());
        registry.registerHelper(CutLeft.NAME,                 new CutLeft());
        registry.registerHelper(CutRight.NAME,                new CutRight());
        registry.registerHelper(Replace.NAME,                 new Replace());
        registry.registerHelper(TimeFormat.NAME,              new TimeFormat());
    }

    /**
     * This is the superclass for all template functions.
     * 
     * In particular this class offers the {@link Helper} implementation as an adapter method
     * used by Handlebars to invoke the actual business method of the function.
     */
    static abstract class AbstractFunction implements Helper<Object> {
    }

    /**
     * This class provides basic features for functions with options (parameters).
     * This kind of function has a name (the function name), one input which is the text from the template,
     * and one or more parameters.
     */
    static abstract class AbstractParametricFunction extends AbstractFunction {
        /**
         * {@inheritDoc}
         */
        @Override
        public Object apply(Object context, Options options)
            throws IOException {
            // The docs are poorly documented by all examples show that options.fn(this)
            // is the only way to have the block resolved
            return (Objects.isNull(options) ? "" : apply(options.fn(this).toString(), options.hash));
        }

        /**
         * This method must be implemented by subclasses and will return the function specific output,
         * provided the input and options.
         * 
         * @param input the function input
         * @param options the function options
         * 
         * @return the function output
         */
        public abstract String apply(String input, Map<String,Object> options);
    }

    /**
     * This class provides basic features for functions without options (parameters).
     * This kind of function only has a name (the function name) and one input which is the text from the template.
     */
    static abstract class AbstractSimpleFunction extends AbstractFunction {
        /**
         * {@inheritDoc}
         */
        @Override
        public Object apply(Object context, Options options)
            throws IOException {
            // The docs are poorly documented by all examples show that options.fn(this)
            // is the only way to have the block resolved
            return (Objects.isNull(options) ? "" : apply(options.fn(this).toString()));
        }

        /**
         * This method must be implemented by subclasses and will return the function specific output,
         * provided the input.
         * 
         * @param input the function input
         * 
         * @return the function output
         */
        public abstract String apply(String input);
    }

    /**
     * This function returns the lower case representation of the input string.
     */
    public static class Lower extends AbstractSimpleFunction {
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
        @Override
        public String apply(String input) {
            if (Objects.isNull(input))
                return "";

            return input.toLowerCase();
        }
    }

    /**
     * This function returns the trimmed case representation of the input string.
     */
    public static class Trim extends AbstractSimpleFunction {
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
        @Override
        public String apply(String input) {
            if (Objects.isNull(input))
                return "";

            return input.trim();
        }
    }

    /**
     * This function returns the upper case representation of the input string.
     */
    public static class Upper extends AbstractSimpleFunction {
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
        @Override
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
    public static class First extends AbstractSimpleFunction {
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
        @Override
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
    public static class FirstLower extends AbstractSimpleFunction {
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
        @Override
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
    public static class FirstUpper extends AbstractSimpleFunction {
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
        @Override
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
    public static class Last extends AbstractSimpleFunction {
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
        @Override
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
    public static class LastLower extends AbstractSimpleFunction {
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
        @Override
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
    public static class LastUpper extends AbstractSimpleFunction {
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
        @Override
        public String apply(String input) {
            if (Objects.isNull(input))
                return "";

            return new Upper().apply((new Last().apply(input)));
        }
    }

    /**
     * This function returns the input string with everything other than letters and positive digits discarded.
     */
    public static class Sanitize extends AbstractSimpleFunction {
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
        @Override
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
    public static class SanitizeLower extends AbstractSimpleFunction {
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
        @Override
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
    public static class SanitizeUpper extends AbstractSimpleFunction {
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
        @Override
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
    public static class Short5 extends AbstractSimpleFunction {
        /**
         * The name to use for this function.
         */
        public static final String NAME = "short5";

        /**
         * This method returns the first 5 characters of the input string, if it's longer than 5 characters,
         * otherwise returns the input string or the empty string if the input is {@code null}.
         * 
         * @param input the input string. If {@code null} an empty string is returned.
         * 
         * @return the first 5 characters of the input string, if it's longer than 5 characters,
         * otherwise returns the input string.
         */
        @Override
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
    public static class Short6 extends AbstractSimpleFunction {
        /**
         * The name to use for this function.
         */
        public static final String NAME = "short6";

        /**
         * This method returns the first 6 characters of the input string, if it's longer than 6 characters,
         * otherwise returns the input string or the empty string if the input is {@code null}.
         * 
         * @param input the input string. If {@code null} an empty string is returned.
         * 
         * @return the first 6 characters of the input string, if it's longer than 6 characters,
         * otherwise returns the input string.
         */
        @Override
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
    public static class Short7 extends AbstractSimpleFunction {
        /**
         * The name to use for this function.
         */
        public static final String NAME = "short7";

        /**
         * This method returns the first 7 characters of the input string, if it's longer than 7 characters,
         * otherwise returns the input string or the empty string if the input is {@code null}.
         * 
         * @param input the input string. If {@code null} an empty string is returned.
         * 
         * @return the first 7 characters of the input string, if it's longer than 7 characters,
         * otherwise returns the input string.
         */
        @Override
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
    public static class TimestampISO8601 extends AbstractSimpleFunction {
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
        @Override
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
    public static class TimestampYYYYMMDDHHMMSS extends AbstractSimpleFunction {
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
        @Override
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
    public static class EnvironmentVariable extends AbstractSimpleFunction {
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
        @Override
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
    public static class EnvironmentUser extends AbstractSimpleFunction {
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
        @Override
        public String apply(String name) {
            return System.getProperty("user.name");
        }
    }

    /**
     * This function returns the content of the given file, if any.
     */
    public static class FileContent extends AbstractSimpleFunction {
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
        @Override
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
    public static class FileExists extends AbstractSimpleFunction {
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
        @Override
        public String apply(String path) {
            if (Objects.isNull(path))
                return Boolean.FALSE.toString();

            java.io.File f = new java.io.File(path);
            logger.trace(TEMPLATE, "Checking whether file '{}' ('{}') exists", path, f.getAbsolutePath());
            return Boolean.toString(f.exists() && f.isFile());
        }
    }

    /**
     * This function uses a given regular expression to match the string input and uses the given (named) group
     * value as a return value.
     */
    public static class Capture extends AbstractParametricFunction {
        /**
         * The name to use for this function.
         */
        public static final String NAME = "capture";

        /**
         * This method uses a given regular expression to match the string input and uses the given (named) group
         * value as a return value. The {@code expression} option gives the regular expression to use, while the
         * {@code group} option tells which group to return (if it's an integer it will be interpreded as a group index,
         * otherwise a group name). Remember that group {@code 0} returns the whole string. If the regular expression
         * doesn't even match the input string then an empty string is returned.
         * 
         * @param input the function input
         * @param options the function options, where the option named {@code expression} gives the regular expression to use,
         * while the {@code group} option tells which group to return (if it's an integer it will be interpreded as a group index,
         * otherwise a group name)
         * 
         * @return the function output
         */
        @Override
        public String apply(String input, Map<String,Object> options) {
            if (Objects.isNull(input))
                return "";
            if (options.containsKey("expression")) {
                String expression = options.get("expression").toString();
                Matcher m = Pattern.compile(expression).matcher(input);
                m.find();

                if (options.containsKey("group")) {
                    if (options.containsKey("group")) {
                        String groupName = options.get("group").toString();
                        try {
                            int groupIndex = Integer.parseUnsignedInt(groupName);
                            // The 'group' is an index
                            try {
                                return m.group(groupIndex);
                            }
                            catch (IllegalStateException ise) {
                                logger.debug(TEMPLATE, "The '{}' expression doesn't match '{}'", expression, input);
                                return "";
                            }
                            catch (IndexOutOfBoundsException ioobe) {
                                logger.debug(TEMPLATE, "The '{}' expression doesn't return any group with index '{}' from string '{}'", expression, groupIndex, input);
                                return "";
                            }
                        }
                        catch (NumberFormatException nfe) {
                            // The 'group' is a name
                            try {
                                return m.group(groupName);
                            }
                            catch (IllegalStateException ise) {
                                logger.debug(TEMPLATE, "The '{}' expression doesn't match '{}'", expression, input);
                                return "";
                            }
                            catch (IllegalArgumentException iae) {
                                logger.debug(TEMPLATE, "The '{}' expression doesn't return any group with name '{}' from string '{}'", expression, groupName, input);
                                return "";
                            }
                        }
                    }
                    else return input;
                }
                else return input;
            }
            else return input;
        }
    }

    /**
     * This function returns the last {@code N} characters of the input string, where {@code N} is the {@code length} option.
     * If the input string is shorter or the same length than the parameter, the whole input string is returned unchanged.
     * If the input is {@code null} an empty string is returned.
     */
    public static class CutLeft extends AbstractParametricFunction {
        /**
         * The name to use for this function.
         */
        public static final String NAME = "cutLeft";

        /**
         * This method returns the last {@code N} characters of the input string, where {@code N} is the {@code length} option.
         * If the input string is shorter or the same length than the parameter, the whole input string is returned unchanged.
         * If the input is {@code null} an empty string is returned.
         * 
         * @param input the function input
         * @param options the function options, where the option named {@code length} determines the maximum length of the
         * returned string. If there is no such option the same string given as input is returned
         * 
         * @return the function output
         */
        @Override
        public String apply(String input, Map<String,Object> options) {
            if (Objects.isNull(input))
                return "";
            if (options.containsKey("length")) {
                String optionString = options.get("length").toString();
                int length = 0;
                try {
                    length = Integer.valueOf(optionString).intValue();
                }
                catch (NumberFormatException nfe) {
                    logger.error(TEMPLATE, "The '{}' option value '{}' for the '{}' function is not a valid integer", "length", optionString, NAME);
                    return input;
                }
                if (length < 0) {
                    logger.error(TEMPLATE, "The '{}' option value '{}' for the '{}' function cannot be negative", "length", optionString, NAME);
                    return input;
                }
                return input.length() > length ? input.substring(input.length()-length, input.length()) : input;
            }
            else return input;
        }
    }

    /**
     * This function returns the first {@code N} characters of the input string, where {@code N} is the {@code length} option.
     * If the input string is shorter or the same length than the parameter, the whole input string is returned unchanged.
     * If the input is {@code null} an empty string is returned.
     */
    public static class CutRight extends AbstractParametricFunction {
        /**
         * The name to use for this function.
         */
        public static final String NAME = "cutRight";

        /**
         * This method returns the first {@code N} characters of the input string, where {@code N} is the {@code length} option.
         * If the input string is shorter or the same length than the parameter, the whole input string is returned unchanged.
         * If the input is {@code null} an empty string is returned.
         * 
         * @param input the function input
         * @param options the function options, where the option named {@code length} determines the maximum length of the
         * returned string. If there is no such option the same string given as input is returned
         * 
         * @return the function output
         */
        @Override
        public String apply(String input, Map<String,Object> options) {
            if (Objects.isNull(input))
                return "";
            if (options.containsKey("length")) {
                String optionString = options.get("length").toString();
                int length = 0;
                try {
                    length = Integer.valueOf(optionString).intValue();
                }
                catch (NumberFormatException nfe) {
                    logger.error(TEMPLATE, "The '{}' option value '{}' for the '{}' function is not a valid integer", "length", optionString, NAME);
                    return input;
                }
                if (length < 0) {
                    logger.error(TEMPLATE, "The '{}' option value '{}' for the '{}' function cannot be negative", "length", optionString, NAME);
                    return input;
                }
                return input.length() > length ? input.substring(0, length) : input;
            }
            else return input;
        }
    }

    /**
     * This function replaces the given character sequences from the input string with the given replacement.
     */
    public static class Replace extends AbstractParametricFunction {
        /**
         * The name to use for this function.
         */
        public static final String NAME = "replace";

        /**
         * This method replaces the given character sequences from the input string with the given replacement.
         * 
         * @param input the function input
         * @param options the function options, where the option named {@code from} determines the character sequence to be replaced,
         * {@code to} determines the character sequence to be used for replacement
         * 
         * @return the function output
         */
        @Override
        public String apply(String input, Map<String,Object> options) {
            if (Objects.isNull(input))
                return "";
            if (options.containsKey("from")) {
                String fromString = options.get("from").toString();
                String toString = "";
                if (options.containsKey("to")) {
                    toString = options.get("to").toString();
                }
                return input.replace(fromString, toString);
            }
            else return input;
        }
    }

    /**
     * This function returns a time value (expressed in milliseconds) and is also able to format it according to an optional
     * format string.
     */
    public static class TimeFormat extends AbstractParametricFunction {
        /**
         * The name to use for this function.
         */
        public static final String NAME = "timeFormat";

        /**
         * This method returns returns a time value (expressed in milliseconds) and is also able to format it according to an optional
         * format string.
         * 
         * @param input the function input. This must be a long value representing a timestamp in milliseconds
         * since January 1, 1970, 00:00:00 GMT. If this value is not provided, the system current timestamp is used.
         * @param options the function options, where the option named {@code format} determines the string representation of the timestamp
         * 
         * @return the function output
         */
        @Override
        public String apply(String input, Map<String,Object> options) {
            long currentTime = System.currentTimeMillis();
            if ((!Objects.isNull(input)) && !input.isBlank()) {
                try {
                    currentTime = Long.valueOf(input).longValue();
                }
                catch (NumberFormatException nfe) {
                    logger.error(TEMPLATE, "The value '{}' for the '{}' function is not a valid long", input, NAME);
                    return "";
                }
                if (currentTime < 0) {
                    logger.error(TEMPLATE, "The value '{}' for the '{}' function cannot be negative", input, NAME);
                    return "";
                }
            }

            if (options.containsKey("format")) {
                String formatString = options.get("format").toString();
                try {
                    Date date = new Date(Long.valueOf(currentTime).longValue());
                    SimpleDateFormat dateFormat = new SimpleDateFormat(formatString);
                    dateFormat.setTimeZone(TimeZone.getTimeZone(ZoneOffset.UTC));
                    return dateFormat.format(date);
                }
                catch (IllegalArgumentException iae) {
                    logger.error(TEMPLATE, "String '{}' does not represent a valid date format string", formatString);
                    return "";
                }
            }
            else return Long.valueOf(currentTime).toString();
        }
    }
}
