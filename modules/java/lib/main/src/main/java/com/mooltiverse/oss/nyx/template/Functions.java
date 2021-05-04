package com.mooltiverse.oss.nyx.template;

import static com.mooltiverse.oss.nyx.log.Markers.TEMPLATE;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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
        }
    };

    /**
     * Default constructor is private on purpose.
     */
    private Functions() {
        super();
    }

    /**
     * This function returns the lower case representation of the input string.
     */
    public static class Lower implements Function<String,String> {
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
    public static class Trim implements Function<String,String> {
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
    public static class Upper implements Function<String,String> {
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
    public static class First implements Function<String,String> {
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
    public static class FirstLower implements Function<String,String> {
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
    public static class FirstUpper implements Function<String,String> {
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
    public static class Last implements Function<String,String> {
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
    public static class LastLower implements Function<String,String> {
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
    public static class LastUpper implements Function<String,String> {
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
    public static class Sanitize implements Function<String,String> {
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
    public static class SanitizeLower implements Function<String,String> {
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
    public static class SanitizeUpper implements Function<String,String> {
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
    public static class Short5 implements Function<String,String> {
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
    public static class Short6 implements Function<String,String> {
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
    public static class Short7 implements Function<String,String> {
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
     * <a href="https://en.wikipedia.org/wiki/ISO_8601">ISO 8601</a> timestamp.
     */
    public static class TimestampISO8601 implements Function<String,String> {
        /**
         * The name to use for this function.
         */
        public static final String NAME = "timestampISO8601";

        /**
         * This method parses the input string as a {@link Long} representing a timestamp in the
         * <a href="https://www.unixtimestamp.com/">unix format</a> and returns it formatted as
         * <a href="https://en.wikipedia.org/wiki/ISO_8601">ISO 8601</a> timestamp.
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
                return new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss").format(date);
            }
            catch (NumberFormatException nfe) {
                logger.error(TEMPLATE, "String {} does not represent a valid Long timestamp", input);
                return "";
            }
        }
    }

    /**
     * This function parses the input string as a {@link Long} representing a timestamp in the
     * <a href="https://www.unixtimestamp.com/">unix format</a> and returns it formatted as
     * {@code YYYYMMDDHHMMSS}.
     */
    public static class TimestampYYYYMMDDHHMMSS implements Function<String,String> {
        /**
         * The name to use for this function.
         */
        public static final String NAME = "timestampYYYYMMDDHHMMSS";

        /**
         * This method parses the input string as a {@link Long} representing a timestamp in the
         * <a href="https://www.unixtimestamp.com/">unix format</a> and returns it formatted as
         * {@code YYYYMMDDHHMMSS}.
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
                return new SimpleDateFormat("yyyyMMddHHmmss").format(date);
            }
            catch (NumberFormatException nfe) {
                logger.error(TEMPLATE, "String {} does not represent a valid Long timestamp", input);
                return "";
            }
        }
    }
}
