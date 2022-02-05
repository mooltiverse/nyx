package com.mooltiverse.oss.nyx.template;

import static org.junit.jupiter.api.Assertions.*;

import java.io.File;
import java.io.FileWriter;
import java.util.Objects;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

@DisplayName("Functions")
public class FunctionsTests {
    @Nested
    @DisplayName("Functions.lower")
    class LowerTests {
        @Test
        @DisplayName("Functions.lower.apply()")
        void applyTest()
            throws Exception {
            assertEquals("", new Functions.Lower().apply(null));
            assertEquals("", new Functions.Lower().apply(""));
            assertEquals("0", new Functions.Lower().apply("0"));
            assertEquals("01234", new Functions.Lower().apply("01234"));
            assertEquals("lower", new Functions.Lower().apply("lower"));
            assertEquals("upper", new Functions.Lower().apply("upper"));
            assertEquals("mixed", new Functions.Lower().apply("mIxEd"));

            // also run the same tests by selecting the function by name, as the template engine does
            assertEquals("", Functions.FUNCTIONS.get("lower").apply(null));
            assertEquals("", Functions.FUNCTIONS.get("lower").apply(""));
            assertEquals("0", Functions.FUNCTIONS.get("lower").apply("0"));
            assertEquals("01234", Functions.FUNCTIONS.get("lower").apply("01234"));
            assertEquals("lower", Functions.FUNCTIONS.get("lower").apply("lower"));
            assertEquals("upper", Functions.FUNCTIONS.get("lower").apply("upper"));
            assertEquals("mixed", Functions.FUNCTIONS.get("lower").apply("mIxEd"));
        }
    }

    @Nested
    @DisplayName("Functions.upper")
    class UpperTests {
        @Test
        @DisplayName("Functions.upper.apply()")
        void applyTest()
            throws Exception {
            assertEquals("", new Functions.Upper().apply(null));
            assertEquals("", new Functions.Upper().apply(""));
            assertEquals("0", new Functions.Upper().apply("0"));
            assertEquals("01234", new Functions.Upper().apply("01234"));
            assertEquals("LOWER", new Functions.Upper().apply("lower"));
            assertEquals("UPPER", new Functions.Upper().apply("upper"));
            assertEquals("MIXED", new Functions.Upper().apply("mIxEd"));

            // also run the same tests by selecting the function by name, as the template engine does
            assertEquals("", Functions.FUNCTIONS.get("upper").apply(null));
            assertEquals("", Functions.FUNCTIONS.get("upper").apply(""));
            assertEquals("0", Functions.FUNCTIONS.get("upper").apply("0"));
            assertEquals("01234", Functions.FUNCTIONS.get("upper").apply("01234"));
            assertEquals("LOWER", Functions.FUNCTIONS.get("upper").apply("lower"));
            assertEquals("UPPER", Functions.FUNCTIONS.get("upper").apply("upper"));
            assertEquals("MIXED", Functions.FUNCTIONS.get("upper").apply("mIxEd"));
        }
    }

    @Nested
    @DisplayName("Functions.trim")
    class TrimTests {
        @Test
        @DisplayName("Functions.trim.apply()")
        void applyTest()
            throws Exception {
            assertEquals("", new Functions.Trim().apply(null));
            assertEquals("", new Functions.Trim().apply(""));
            assertEquals("0", new Functions.Trim().apply("0"));
            assertEquals("01  234", new Functions.Trim().apply("   01  234   "));

            // also run the same tests by selecting the function by name, as the template engine does
            assertEquals("", Functions.FUNCTIONS.get("trim").apply(null));
            assertEquals("", Functions.FUNCTIONS.get("trim").apply(""));
            assertEquals("0", Functions.FUNCTIONS.get("trim").apply("0"));
            assertEquals("01  234", Functions.FUNCTIONS.get("trim").apply("   01  234   "));
        }
    }

    @Nested
    @DisplayName("Functions.first")
    class FirstTests {
        @Test
        @DisplayName("Functions.first.apply()")
        void applyTest()
            throws Exception {
            assertEquals("", new Functions.First().apply(null));
            assertEquals("", new Functions.First().apply(""));
            assertEquals("aB1", new Functions.First().apply("aB1"));
            assertEquals("", new Functions.First().apply(" aB1 "));
            assertEquals("", new Functions.First().apply("!£$%&b2/()=?'^ì*+§°#@.;c3"));
            assertEquals("a1", new Functions.First().apply("a1!£$%&b2/()=?'^ì*+§°#@.;c3"));

            // also run the same tests by selecting the function by name, as the template engine does
            assertEquals("", Functions.FUNCTIONS.get("first").apply(null));
            assertEquals("", Functions.FUNCTIONS.get("first").apply(""));
            assertEquals("aB1", Functions.FUNCTIONS.get("first").apply("aB1"));
            assertEquals("", Functions.FUNCTIONS.get("first").apply(" aB1 "));
            assertEquals("", Functions.FUNCTIONS.get("first").apply("!£$%&b2/()=?'^ì*+§°#@.;c3"));
            assertEquals("a1", Functions.FUNCTIONS.get("first").apply("a1!£$%&b2/()=?'^ì*+§°#@.;c3"));
        }
    }

    @Nested
    @DisplayName("Functions.firstLower")
    class FirstLowerTests {
        @Test
        @DisplayName("Functions.firstLower.apply()")
        void applyTest()
            throws Exception {
            assertEquals("", new Functions.FirstLower().apply(null));
            assertEquals("", new Functions.FirstLower().apply(""));
            assertEquals("ab1", new Functions.FirstLower().apply("aB1"));
            assertEquals("", new Functions.FirstLower().apply(" aB1 "));
            assertEquals("", new Functions.FirstLower().apply("!£$%&b2/()=?'^ì*+§°#@.;c3"));
            assertEquals("ab1", new Functions.FirstLower().apply("aB1!£$%&b2/()=?'^ì*+§°#@.;c3"));

            // also run the same tests by selecting the function by name, as the template engine does
            assertEquals("", Functions.FUNCTIONS.get("firstLower").apply(null));
            assertEquals("", Functions.FUNCTIONS.get("firstLower").apply(""));
            assertEquals("ab1", Functions.FUNCTIONS.get("firstLower").apply("aB1"));
            assertEquals("", Functions.FUNCTIONS.get("firstLower").apply(" aB1 "));
            assertEquals("", Functions.FUNCTIONS.get("firstLower").apply("!£$%&b2/()=?'^ì*+§°#@.;c3"));
            assertEquals("ab1", Functions.FUNCTIONS.get("firstLower").apply("aB1!£$%&b2/()=?'^ì*+§°#@.;c3"));
        }
    }

    @Nested
    @DisplayName("Functions.firstUpper")
    class FirstUpperTests {
        @Test
        @DisplayName("Functions.firstUpper.apply()")
        void applyTest()
            throws Exception {
            assertEquals("", new Functions.FirstUpper().apply(null));
            assertEquals("", new Functions.FirstUpper().apply(""));
            assertEquals("AB1", new Functions.FirstUpper().apply("aB1"));
            assertEquals("", new Functions.FirstUpper().apply(" aB1 "));
            assertEquals("", new Functions.FirstUpper().apply("!£$%&b2/()=?'^ì*+§°#@.;c3"));
            assertEquals("AB1", new Functions.FirstUpper().apply("aB1!£$%&b2/()=?'^ì*+§°#@.;c3"));

            // also run the same tests by selecting the function by name, as the template engine does
            assertEquals("", Functions.FUNCTIONS.get("firstUpper").apply(null));
            assertEquals("", Functions.FUNCTIONS.get("firstUpper").apply(""));
            assertEquals("AB1", Functions.FUNCTIONS.get("firstUpper").apply("aB1"));
            assertEquals("", Functions.FUNCTIONS.get("firstUpper").apply(" aB1 "));
            assertEquals("", Functions.FUNCTIONS.get("firstUpper").apply("!£$%&b2/()=?'^ì*+§°#@.;c3"));
            assertEquals("AB1", Functions.FUNCTIONS.get("firstUpper").apply("aB1!£$%&b2/()=?'^ì*+§°#@.;c3"));
        }
    }

    @Nested
    @DisplayName("Functions.last")
    class LastTests {
        @Test
        @DisplayName("Functions.last.apply()")
        void applyTest()
            throws Exception {
            assertEquals("", new Functions.Last().apply(null));
            assertEquals("", new Functions.Last().apply(""));
            assertEquals("aB1", new Functions.Last().apply("aB1"));
            assertEquals("", new Functions.Last().apply(" aB1 "));
            assertEquals("", new Functions.Last().apply("a1!£$%&b2/()=?'^ì*+§°#@.;"));
            assertEquals("c3", new Functions.Last().apply("a1!£$%&b2/()=?'^ì*+§°#@.;c3"));

            // also run the same tests by selecting the function by name, as the template engine does
            assertEquals("", Functions.FUNCTIONS.get("last").apply(null));
            assertEquals("", Functions.FUNCTIONS.get("last").apply(""));
            assertEquals("aB1", Functions.FUNCTIONS.get("last").apply("aB1"));
            assertEquals("", Functions.FUNCTIONS.get("last").apply(" aB1 "));
            assertEquals("", Functions.FUNCTIONS.get("last").apply("a1!£$%&b2/()=?'^ì*+§°#@.;"));
            assertEquals("c3", Functions.FUNCTIONS.get("last").apply("a1!£$%&b2/()=?'^ì*+§°#@.;c3"));
        }
    }

    @Nested
    @DisplayName("Functions.lastLower")
    class LastLowerTests {
        @Test
        @DisplayName("Functions.lastLower.apply()")
        void applyTest()
            throws Exception {
            assertEquals("", new Functions.LastLower().apply(null));
            assertEquals("", new Functions.LastLower().apply(""));
            assertEquals("ab1", new Functions.LastLower().apply("aB1"));
            assertEquals("", new Functions.LastLower().apply(" aB1 "));
            assertEquals("", new Functions.LastLower().apply("a1!£$%&b2/()=?'^ì*+§°#@.;"));
            assertEquals("cd3", new Functions.LastLower().apply("a1!£$%&b2/()=?'^ì*+§°#@.;cD3"));

            // also run the same tests by selecting the function by name, as the template engine does
            assertEquals("", Functions.FUNCTIONS.get("lastLower").apply(null));
            assertEquals("", Functions.FUNCTIONS.get("lastLower").apply(""));
            assertEquals("ab1", Functions.FUNCTIONS.get("lastLower").apply("aB1"));
            assertEquals("", Functions.FUNCTIONS.get("lastLower").apply(" aB1 "));
            assertEquals("", Functions.FUNCTIONS.get("lastLower").apply("a1!£$%&b2/()=?'^ì*+§°#@.;"));
            assertEquals("cd3", Functions.FUNCTIONS.get("lastLower").apply("a1!£$%&b2/()=?'^ì*+§°#@.;cD3"));
        }
    }

    @Nested
    @DisplayName("Functions.lastUpper")
    class LastUpperTests {
        @Test
        @DisplayName("Functions.lastUpper.apply()")
        void applyTest()
            throws Exception {
            assertEquals("", new Functions.LastUpper().apply(null));
            assertEquals("", new Functions.LastUpper().apply(""));
            assertEquals("AB1", new Functions.LastUpper().apply("aB1"));
            assertEquals("", new Functions.LastUpper().apply(" aB1 "));
            assertEquals("", new Functions.LastUpper().apply("a1!£$%&b2/()=?'^ì*+§°#@.;"));
            assertEquals("CD3", new Functions.LastUpper().apply("a1!£$%&b2/()=?'^ì*+§°#@.;cD3"));

            // also run the same tests by selecting the function by name, as the template engine does
            assertEquals("", Functions.FUNCTIONS.get("lastUpper").apply(null));
            assertEquals("", Functions.FUNCTIONS.get("lastUpper").apply(""));
            assertEquals("AB1", Functions.FUNCTIONS.get("lastUpper").apply("aB1"));
            assertEquals("", Functions.FUNCTIONS.get("lastUpper").apply(" aB1 "));
            assertEquals("", Functions.FUNCTIONS.get("lastUpper").apply("a1!£$%&b2/()=?'^ì*+§°#@.;"));
            assertEquals("CD3", Functions.FUNCTIONS.get("lastUpper").apply("a1!£$%&b2/()=?'^ì*+§°#@.;cD3"));
        }
    }

    @Nested
    @DisplayName("Functions.sanitize")
    class SanitizeTests {
        @Test
        @DisplayName("Functions.sanitize.apply()")
        void applyTest()
            throws Exception {
            assertEquals("", new Functions.Sanitize().apply(null));
            assertEquals("", new Functions.Sanitize().apply(""));
            assertEquals("aB1", new Functions.Sanitize().apply("aB1"));
            assertEquals("aB1", new Functions.Sanitize().apply(" aB1 "));
            assertEquals("abc123", new Functions.Sanitize().apply("\\!£$%&abc123/()=?'^ì*+§°#@.;"));

            // also run the same tests by selecting the function by name, as the template engine does
            assertEquals("", Functions.FUNCTIONS.get("sanitize").apply(null));
            assertEquals("", Functions.FUNCTIONS.get("sanitize").apply(""));
            assertEquals("aB1", Functions.FUNCTIONS.get("sanitize").apply("aB1"));
            assertEquals("aB1", Functions.FUNCTIONS.get("sanitize").apply(" aB1 "));
            assertEquals("abc123", Functions.FUNCTIONS.get("sanitize").apply("\\!£$%&abc123/()=?'^ì*+§°#@.;"));
        }
    }

    @Nested
    @DisplayName("Functions.sanitizeLower")
    class SanitizeLowerTests {
        @Test
        @DisplayName("Functions.sanitizeLower.apply()")
        void applyTest()
            throws Exception {
            assertEquals("", new Functions.SanitizeLower().apply(null));
            assertEquals("", new Functions.SanitizeLower().apply(""));
            assertEquals("ab1", new Functions.SanitizeLower().apply("aB1"));
            assertEquals("ab1", new Functions.SanitizeLower().apply(" aB1 "));
            assertEquals("abc123", new Functions.SanitizeLower().apply("\\!£$%&aBc123/()=?'^ì*+§°#@.;"));

            // also run the same tests by selecting the function by name, as the template engine does
            assertEquals("", Functions.FUNCTIONS.get("sanitizeLower").apply(null));
            assertEquals("", Functions.FUNCTIONS.get("sanitizeLower").apply(""));
            assertEquals("ab1", Functions.FUNCTIONS.get("sanitizeLower").apply("aB1"));
            assertEquals("ab1", Functions.FUNCTIONS.get("sanitizeLower").apply(" aB1 "));
            assertEquals("abc123", Functions.FUNCTIONS.get("sanitizeLower").apply("\\!£$%&aBc123/()=?'^ì*+§°#@.;"));
        }
    }

    @Nested
    @DisplayName("Functions.sanitizeUpper")
    class SanitizeUpperTests {
        @Test
        @DisplayName("Functions.sanitizeUpper.apply()")
        void applyTest()
            throws Exception {
            assertEquals("", new Functions.SanitizeUpper().apply(null));
            assertEquals("", new Functions.SanitizeUpper().apply(""));
            assertEquals("AB1", new Functions.SanitizeUpper().apply("aB1"));
            assertEquals("AB1", new Functions.SanitizeUpper().apply(" aB1 "));
            assertEquals("ABC123", new Functions.SanitizeUpper().apply("\\!£$%&aBc123/()=?'^ì*+§°#@.;"));

            // also run the same tests by selecting the function by name, as the template engine does
            assertEquals("", Functions.FUNCTIONS.get("sanitizeUpper").apply(null));
            assertEquals("", Functions.FUNCTIONS.get("sanitizeUpper").apply(""));
            assertEquals("AB1", Functions.FUNCTIONS.get("sanitizeUpper").apply("aB1"));
            assertEquals("AB1", Functions.FUNCTIONS.get("sanitizeUpper").apply(" aB1 "));
            assertEquals("ABC123", Functions.FUNCTIONS.get("sanitizeUpper").apply("\\!£$%&aBc123/()=?'^ì*+§°#@.;"));
        }
    }

    @Nested
    @DisplayName("Functions.short5")
    class Short5Tests {
        @Test
        @DisplayName("Functions.short5.apply()")
        void applyTest()
            throws Exception {
            assertEquals("", new Functions.Short5().apply(null));
            assertEquals("", new Functions.Short5().apply(""));
            assertEquals("0", new Functions.Short5().apply("0"));
            assertEquals("01234", new Functions.Short5().apply("01234"));
            assertEquals("01234", new Functions.Short5().apply("012345"));
            assertEquals("01234", new Functions.Short5().apply("0123456789"));

            // also run the same tests by selecting the function by name, as the template engine does
            assertEquals("", Functions.FUNCTIONS.get("short5").apply(null));
            assertEquals("", Functions.FUNCTIONS.get("short5").apply(""));
            assertEquals("0", Functions.FUNCTIONS.get("short5").apply("0"));
            assertEquals("01234", Functions.FUNCTIONS.get("short5").apply("01234"));
            assertEquals("01234", Functions.FUNCTIONS.get("short5").apply("012345"));
            assertEquals("01234", Functions.FUNCTIONS.get("short5").apply("0123456789"));
        }
    }

    @Nested
    @DisplayName("Functions.short6")
    class Short6Tests {
        @Test
        @DisplayName("Functions.short6.apply()")
        void applyTest()
            throws Exception {
            assertEquals("", new Functions.Short6().apply(null));
            assertEquals("", new Functions.Short6().apply(""));
            assertEquals("0", new Functions.Short6().apply("0"));
            assertEquals("01234", new Functions.Short6().apply("01234"));
            assertEquals("012345", new Functions.Short6().apply("012345"));
            assertEquals("012345", new Functions.Short6().apply("0123456"));
            assertEquals("012345", new Functions.Short6().apply("01234567"));
            assertEquals("012345", new Functions.Short6().apply("0123456789"));

            // also run the same tests by selecting the function by name, as the template engine does
            assertEquals("", Functions.FUNCTIONS.get("short6").apply(null));
            assertEquals("", Functions.FUNCTIONS.get("short6").apply(""));
            assertEquals("0", Functions.FUNCTIONS.get("short6").apply("0"));
            assertEquals("01234", Functions.FUNCTIONS.get("short6").apply("01234"));
            assertEquals("012345", Functions.FUNCTIONS.get("short6").apply("012345"));
            assertEquals("012345", Functions.FUNCTIONS.get("short6").apply("0123456"));
            assertEquals("012345", Functions.FUNCTIONS.get("short6").apply("01234567"));
            assertEquals("012345", Functions.FUNCTIONS.get("short6").apply("0123456789"));
        }
    }

    @Nested
    @DisplayName("Functions.short7")
    class Short7Tests {
        @Test
        @DisplayName("Functions.short7.apply()")
        void applyTest()
            throws Exception {
            assertEquals("", new Functions.Short7().apply(null));
            assertEquals("", new Functions.Short7().apply(""));
            assertEquals("0", new Functions.Short7().apply("0"));
            assertEquals("01234", new Functions.Short7().apply("01234"));
            assertEquals("012345", new Functions.Short7().apply("012345"));
            assertEquals("0123456", new Functions.Short7().apply("0123456"));
            assertEquals("0123456", new Functions.Short7().apply("01234567"));
            assertEquals("0123456", new Functions.Short7().apply("0123456789"));

            // also run the same tests by selecting the function by name, as the template engine does
            assertEquals("", Functions.FUNCTIONS.get("short7").apply(null));
            assertEquals("", Functions.FUNCTIONS.get("short7").apply(""));
            assertEquals("0", Functions.FUNCTIONS.get("short7").apply("0"));
            assertEquals("01234", Functions.FUNCTIONS.get("short7").apply("01234"));
            assertEquals("012345", Functions.FUNCTIONS.get("short7").apply("012345"));
            assertEquals("0123456", Functions.FUNCTIONS.get("short7").apply("0123456"));
            assertEquals("0123456", Functions.FUNCTIONS.get("short7").apply("01234567"));
            assertEquals("0123456", Functions.FUNCTIONS.get("short7").apply("0123456789"));
        }
    }

    @Nested
    @DisplayName("Functions.timestampISO8601")
    class TimestampISO8601Tests {
        @Test
        @DisplayName("Functions.timestampISO8601.apply()")
        void applyTest()
            throws Exception {
            assertEquals("", new Functions.TimestampISO8601().apply(null));
            assertEquals("1970-01-01T00:00:00", new Functions.TimestampISO8601().apply("0"));
            assertEquals("2020-01-01T12:00:00", new Functions.TimestampISO8601().apply("1577880000000"));

            // also run the same tests by selecting the function by name, as the template engine does
            assertEquals("", Functions.FUNCTIONS.get("timestampISO8601").apply(null));
            assertEquals("1970-01-01T00:00:00", Functions.FUNCTIONS.get("timestampISO8601").apply("0"));
            assertEquals("2020-01-01T12:00:00", Functions.FUNCTIONS.get("timestampISO8601").apply("1577880000000"));
        }
    }

    @Nested
    @DisplayName("Functions.timestampYYYYMMDDHHMMSS")
    class TimestampYYYYMMDDHHMMSSTests {
        @Test
        @DisplayName("Functions.timestampYYYYMMDDHHMMSS.apply()")
        void applyTest()
            throws Exception {
            assertEquals("", new Functions.TimestampISO8601().apply(null));
            assertEquals("19700101000000", new Functions.TimestampYYYYMMDDHHMMSS().apply("0"));
            assertEquals("20200101120000", new Functions.TimestampYYYYMMDDHHMMSS().apply("1577880000000"));

            // also run the same tests by selecting the function by name, as the template engine does
            assertEquals("", Functions.FUNCTIONS.get("timestampYYYYMMDDHHMMSS").apply(null));
            assertEquals("19700101000000", Functions.FUNCTIONS.get("timestampYYYYMMDDHHMMSS").apply("0"));
            assertEquals("20200101120000", Functions.FUNCTIONS.get("timestampYYYYMMDDHHMMSS").apply("1577880000000"));
        }
    }

    @Nested
    @DisplayName("Functions.environment.variable")
    class EnvironmentVariableTests {
        @Test
        @DisplayName("Functions.environment.variable.apply()")
        void applyTest()
            throws Exception {
            // on CI platforms the OS variable may not be defined, causing this test to fail, so let's make it conditional
            if (Objects.isNull(System.getenv("OS"))) {
                assertEquals("", new Functions.Environment.Variable().apply(null));
                assertEquals("", new Functions.Environment.Variable().apply("OS"));

                // also run the same tests by selecting the function by name, as the template engine does
                assertEquals("", Functions.FUNCTIONS.get("environment.variable").apply(null));
                assertEquals("", Functions.FUNCTIONS.get("environment.variable").apply("OS"));
            }
            else {
                assertEquals("", new Functions.Environment.Variable().apply(null));
                assertEquals(System.getenv("OS"), new Functions.Environment.Variable().apply("OS"));

                // also run the same tests by selecting the function by name, as the template engine does
                assertEquals("", Functions.FUNCTIONS.get("environment.variable").apply(null));
                assertEquals(System.getenv("OS"), Functions.FUNCTIONS.get("environment.variable").apply("OS"));
            }
        }
    }

    @Nested
    @DisplayName("Functions.environment.user")
    class EnvironmentUserTests {
        @Test
        @DisplayName("Functions.environment.user.apply()")
        void applyTest()
            throws Exception {
            // the input value is ignored by this function, it always returns the system user name
            assertEquals(System.getProperty("user.name"), new Functions.Environment.User().apply(null));
            assertEquals(System.getProperty("user.name"), new Functions.Environment.User().apply("any"));

            // also run the same tests by selecting the function by name, as the template engine does
            assertEquals(System.getProperty("user.name"), Functions.FUNCTIONS.get("environment.user").apply(null));
            assertEquals(System.getProperty("user.name"), Functions.FUNCTIONS.get("environment.user").apply("any"));
        }
    }

    @Nested
    @DisplayName("Functions.file.content")
    class FileContentTests {
        @Test
        @DisplayName("Functions.file.content.apply()")
        void applyTest()
            throws Exception {
            final String FILE_CONTENT = "file content to test";

            File f = File.createTempFile("filecontenttest", "file", new File(System.getProperty("java.io.tmpdir")));
            f.deleteOnExit();
            FileWriter w = new FileWriter(f);
            w.write(FILE_CONTENT);
            w.flush();
            w.close();

            assertEquals("", new Functions.File.Content().apply(null));
            assertEquals("", new Functions.File.Content().apply("afilethatdoesnotexists"));
            assertEquals(FILE_CONTENT, new Functions.File.Content().apply(f.getAbsolutePath()));

            // also run the same tests by selecting the function by name, as the template engine does
            assertEquals("", Functions.FUNCTIONS.get("file.content").apply(null));
            assertEquals("", Functions.FUNCTIONS.get("file.content").apply("afilethatdoesnotexists"));
            assertEquals(FILE_CONTENT, Functions.FUNCTIONS.get("file.content").apply(f.getAbsolutePath()));
        }
    }

    @Nested
    @DisplayName("Functions.file.exists")
    class FileExistsTests {
        @Test
        @DisplayName("Functions.file.exists.apply()")
        void applyTest()
            throws Exception {
            File f = File.createTempFile("fileexiststest", "file", new File(System.getProperty("java.io.tmpdir")));
            f.deleteOnExit();

            assertEquals("false", new Functions.File.Exists().apply(null));
            assertEquals("false", new Functions.File.Exists().apply("afilethatdoesnotexists"));
            assertEquals("true", new Functions.File.Exists().apply(f.getAbsolutePath()));

            // also run the same tests by selecting the function by name, as the template engine does
            assertEquals("false", Functions.FUNCTIONS.get("file.exists").apply(null));
            assertEquals("false", Functions.FUNCTIONS.get("file.exists").apply("afilethatdoesnotexists"));
            assertEquals("true", Functions.FUNCTIONS.get("file.exists").apply(f.getAbsolutePath()));
        }
    }
}
