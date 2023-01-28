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

import java.io.File;
import java.io.FileWriter;
import java.text.SimpleDateFormat;
import java.time.ZoneOffset;
import java.util.Date;
import java.util.Map;
import java.util.Objects;
import java.util.TimeZone;

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
        }
    }

    @Nested
    @DisplayName("Functions.timestampYYYYMMDDHHMMSS")
    class TimestampYYYYMMDDHHMMSSTests {
        @Test
        @DisplayName("Functions.timestampYYYYMMDDHHMMSS.apply()")
        void applyTest()
            throws Exception {
            assertEquals("", new Functions.TimestampYYYYMMDDHHMMSS().apply(null));
            assertEquals("19700101000000", new Functions.TimestampYYYYMMDDHHMMSS().apply("0"));
            assertEquals("20200101120000", new Functions.TimestampYYYYMMDDHHMMSS().apply("1577880000000"));
        }
    }

    @Nested
    @DisplayName("Functions.environmentVariable")
    class EnvironmentVariableTests {
        @Test
        @DisplayName("Functions.environmentVariable.apply()")
        void applyTest()
            throws Exception {
            // on CI platforms the OS variable may not be defined, causing this test to fail, so let's make it conditional
            if (Objects.isNull(System.getenv("OS"))) {
                assertEquals("", new Functions.EnvironmentVariable().apply(null));
                assertEquals("", new Functions.EnvironmentVariable().apply("OS"));
            }
            else {
                assertEquals("", new Functions.EnvironmentVariable().apply(null));
                assertEquals(System.getenv("OS"), new Functions.EnvironmentVariable().apply("OS"));
            }
        }
    }

    @Nested
    @DisplayName("Functions.environmentUser")
    class EnvironmentUserTests {
        @Test
        @DisplayName("Functions.environmentUser.apply()")
        void applyTest()
            throws Exception {
            // the input value is ignored by this function, it always returns the system user name
            assertEquals(System.getProperty("user.name"), new Functions.EnvironmentUser().apply(null));
            assertEquals(System.getProperty("user.name"), new Functions.EnvironmentUser().apply("any"));
        }
    }

    @Nested
    @DisplayName("Functions.fileContent")
    class FileContentTests {
        @Test
        @DisplayName("Functions.fileContent.apply()")
        void applyTest()
            throws Exception {
            final String FILE_CONTENT = "file content to test";

            File f = File.createTempFile("filecontenttest", "file", new File(System.getProperty("java.io.tmpdir")));
            f.deleteOnExit();
            FileWriter w = new FileWriter(f);
            w.write(FILE_CONTENT);
            w.flush();
            w.close();

            assertEquals("", new Functions.FileContent().apply(null));
            assertEquals("", new Functions.FileContent().apply("afilethatdoesnotexists"));
            assertEquals(FILE_CONTENT, new Functions.FileContent().apply(f.getAbsolutePath()));
        }
    }

    @Nested
    @DisplayName("Functions.fileExists")
    class FileExistsTests {
        @Test
        @DisplayName("Functions.fileExists.apply()")
        void applyTest()
            throws Exception {
            File f = File.createTempFile("fileexiststest", "file", new File(System.getProperty("java.io.tmpdir")));
            f.deleteOnExit();

            assertEquals("false", new Functions.FileExists().apply(null));
            assertEquals("false", new Functions.FileExists().apply("afilethatdoesnotexists"));
            assertEquals("true", new Functions.FileExists().apply(f.getAbsolutePath()));
        }
    }

    @Nested
    @DisplayName("Functions.capture")
    class CaptureTests {
        @Test
        @DisplayName("Functions.capture.apply()")
        void applyTest()
            throws Exception {
            assertEquals("", new Functions.Capture().apply(null, Map.<String,Object>of()));
            assertEquals("", new Functions.Capture().apply("", Map.<String,Object>of()));
            assertEquals("  ", new Functions.Capture().apply("  ", Map.<String,Object>of()));
            assertEquals("mytype(myscope): mytitle", new Functions.Capture().apply("mytype(myscope): mytitle", Map.<String,Object>of("expression", "(?<type>[a-zA-Z0-9_]+)(\\((?<scope>[a-z ]+)\\))?:( (?<title>.+))")));
            assertEquals("mytype", new Functions.Capture().apply("mytype(myscope): mytitle", Map.<String,Object>of("expression", "(?<type>[a-zA-Z0-9_]+)(\\((?<scope>[a-z ]+)\\))?:( (?<title>.+))", "group", "type")));
            assertEquals("myscope", new Functions.Capture().apply("mytype(myscope): mytitle", Map.<String,Object>of("expression", "(?<type>[a-zA-Z0-9_]+)(\\((?<scope>[a-z ]+)\\))?:( (?<title>.+))", "group", "scope")));
            assertEquals("mytitle", new Functions.Capture().apply("mytype(myscope): mytitle", Map.<String,Object>of("expression", "(?<type>[a-zA-Z0-9_]+)(\\((?<scope>[a-z ]+)\\))?:( (?<title>.+))", "group", "title")));
            assertEquals("", new Functions.Capture().apply("mytype(myscope): mytitle", Map.<String,Object>of("expression", "(?<type>[a-zA-Z0-9_]+)(\\((?<scope>[a-z ]+)\\))?:( (?<title>.+))", "group", "unknowngroup")));
            assertEquals("mytype(myscope): mytitle", new Functions.Capture().apply("mytype(myscope): mytitle", Map.<String,Object>of("expression", "(?<type>[a-zA-Z0-9_]+)(\\((?<scope>[a-z ]+)\\))?:( (?<title>.+))", "group", "0")));
            assertEquals("mytype", new Functions.Capture().apply("mytype(myscope): mytitle", Map.<String,Object>of("expression", "(?<type>[a-zA-Z0-9_]+)(\\((?<scope>[a-z ]+)\\))?:( (?<title>.+))", "group", "1")));
            assertEquals("(myscope)", new Functions.Capture().apply("mytype(myscope): mytitle", Map.<String,Object>of("expression", "(?<type>[a-zA-Z0-9_]+)(\\((?<scope>[a-z ]+)\\))?:( (?<title>.+))", "group", "2")));
            assertEquals(" mytitle", new Functions.Capture().apply("mytype(myscope): mytitle", Map.<String,Object>of("expression", "(?<type>[a-zA-Z0-9_]+)(\\((?<scope>[a-z ]+)\\))?:( (?<title>.+))", "group", "4")));
            assertEquals("", new Functions.Capture().apply("mytype(myscope): mytitle", Map.<String,Object>of("expression", "(?<type>[a-zA-Z0-9_]+)(\\((?<scope>[a-z ]+)\\))?:( (?<title>.+))", "group", "9")));
        }
    }

    @Nested
    @DisplayName("Functions.cutLeft")
    class CutLeftTests {
        @Test
        @DisplayName("Functions.cutLeft.apply()")
        void applyTest()
            throws Exception {
            assertEquals("", new Functions.CutLeft().apply(null, Map.<String,Object>of()));
            assertEquals("", new Functions.CutLeft().apply("", Map.<String,Object>of()));
            assertEquals("", new Functions.CutLeft().apply("", Map.<String,Object>of("length", "3")));
            assertEquals("", new Functions.CutLeft().apply("", Map.<String,Object>of("length", "0")));
            assertEquals("", new Functions.CutLeft().apply("", Map.<String,Object>of("length", "-3")));
            assertEquals("", new Functions.CutLeft().apply("", Map.<String,Object>of("length", "not a number")));
            assertEquals("0", new Functions.CutLeft().apply("0", Map.<String,Object>of()));
            assertEquals("0", new Functions.CutLeft().apply("0", Map.<String,Object>of("length", "3")));
            assertEquals("", new Functions.CutLeft().apply("0", Map.<String,Object>of("length", "0")));
            assertEquals("0", new Functions.CutLeft().apply("0", Map.<String,Object>of("length", "-3")));
            assertEquals("0", new Functions.CutLeft().apply("0", Map.<String,Object>of("length", "not a number")));
            assertEquals("01234", new Functions.CutLeft().apply("01234", Map.<String,Object>of()));
            assertEquals("234", new Functions.CutLeft().apply("01234", Map.<String,Object>of("length", "3")));
            assertEquals("", new Functions.CutLeft().apply("01234", Map.<String,Object>of("length", "0")));
            assertEquals("01234", new Functions.CutLeft().apply("01234", Map.<String,Object>of("length", "-3")));
            assertEquals("01234", new Functions.CutLeft().apply("01234", Map.<String,Object>of("length", "not a number")));
            assertEquals("012345", new Functions.CutLeft().apply("012345", Map.<String,Object>of()));
            assertEquals("345", new Functions.CutLeft().apply("012345", Map.<String,Object>of("length", "3")));
            assertEquals("", new Functions.CutLeft().apply("012345", Map.<String,Object>of("length", "0")));
            assertEquals("012345", new Functions.CutLeft().apply("012345", Map.<String,Object>of("length", "-3")));
            assertEquals("012345", new Functions.CutLeft().apply("012345", Map.<String,Object>of("length", "not a number")));
            assertEquals("0123456789", new Functions.CutLeft().apply("0123456789", Map.<String,Object>of()));
            assertEquals("789", new Functions.CutLeft().apply("0123456789", Map.<String,Object>of("length", "3")));
            assertEquals("", new Functions.CutLeft().apply("0123456789", Map.<String,Object>of("length", "0")));
            assertEquals("0123456789", new Functions.CutLeft().apply("0123456789", Map.<String,Object>of("length", "-3")));
            assertEquals("0123456789", new Functions.CutLeft().apply("0123456789", Map.<String,Object>of("length", "not a number")));
        }
    }

    @Nested
    @DisplayName("Functions.cutRight")
    class CutRightTests {
        @Test
        @DisplayName("Functions.cutRight.apply()")
        void applyTest()
            throws Exception {
            assertEquals("", new Functions.CutRight().apply(null, Map.<String,Object>of()));
            assertEquals("", new Functions.CutRight().apply("", Map.<String,Object>of()));
            assertEquals("", new Functions.CutRight().apply("", Map.<String,Object>of("length", "3")));
            assertEquals("", new Functions.CutRight().apply("", Map.<String,Object>of("length", "0")));
            assertEquals("", new Functions.CutRight().apply("", Map.<String,Object>of("length", "-3")));
            assertEquals("", new Functions.CutRight().apply("", Map.<String,Object>of("length", "not a number")));
            assertEquals("0", new Functions.CutRight().apply("0", Map.<String,Object>of()));
            assertEquals("0", new Functions.CutRight().apply("0", Map.<String,Object>of("length", "3")));
            assertEquals("", new Functions.CutRight().apply("0", Map.<String,Object>of("length", "0")));
            assertEquals("0", new Functions.CutRight().apply("0", Map.<String,Object>of("length", "-3")));
            assertEquals("0", new Functions.CutRight().apply("0", Map.<String,Object>of("length", "not a number")));
            assertEquals("012", new Functions.CutRight().apply("012", Map.<String,Object>of()));
            assertEquals("012", new Functions.CutRight().apply("012", Map.<String,Object>of("length", "3")));
            assertEquals("", new Functions.CutRight().apply("012", Map.<String,Object>of("length", "0")));
            assertEquals("012", new Functions.CutRight().apply("012", Map.<String,Object>of("length", "-3")));
            assertEquals("012", new Functions.CutRight().apply("012", Map.<String,Object>of("length", "not a number")));
            assertEquals("012345", new Functions.CutRight().apply("012345", Map.<String,Object>of()));
            assertEquals("012", new Functions.CutRight().apply("012345", Map.<String,Object>of("length", "3")));
            assertEquals("", new Functions.CutRight().apply("012345", Map.<String,Object>of("length", "0")));
            assertEquals("012345", new Functions.CutRight().apply("012345", Map.<String,Object>of("length", "-3")));
            assertEquals("012345", new Functions.CutRight().apply("012345", Map.<String,Object>of("length", "not a number")));
            assertEquals("0123456789", new Functions.CutRight().apply("0123456789", Map.<String,Object>of()));
            assertEquals("012", new Functions.CutRight().apply("0123456789", Map.<String,Object>of("length", "3")));
            assertEquals("", new Functions.CutRight().apply("0123456789", Map.<String,Object>of("length", "0")));
            assertEquals("0123456789", new Functions.CutRight().apply("0123456789", Map.<String,Object>of("length", "-3")));
            assertEquals("0123456789", new Functions.CutRight().apply("0123456789", Map.<String,Object>of("length", "not a number")));
        }
    }

    @Nested
    @DisplayName("Functions.replace")
    class ReplaceTests {
        @Test
        @DisplayName("Functions.replace.apply()")
        void applyTest()
            throws Exception {
            assertEquals("", new Functions.Replace().apply(null, Map.<String,Object>of()));
            assertEquals("", new Functions.Replace().apply("", Map.<String,Object>of()));
            assertEquals("  ", new Functions.Replace().apply("  ", Map.<String,Object>of()));
            assertEquals("001002003004005", new Functions.Replace().apply("001002003004005", Map.<String,Object>of("from", "", "to", "")));
            assertEquals("12345", new Functions.Replace().apply("001002003004005", Map.<String,Object>of("from", "0", "to", "")));
            assertEquals("  1  2  3  4  5", new Functions.Replace().apply("001002003004005", Map.<String,Object>of("from", "0", "to", " ")));
            assertEquals("erased1erased2erased3erased4erased5", new Functions.Replace().apply("001002003004005", Map.<String,Object>of("from", "00", "to", "erased")));
        }
    }

    @Nested
    @DisplayName("Functions.timeFormat")
    class TimeTests {
        @Test
        @DisplayName("Functions.timeFormat.apply()")
        void applyTest()
            throws Exception {
            // these are the baseline values to test against
            long currentTime = System.currentTimeMillis();
            String timeString = Long.valueOf(currentTime).toString();
            SimpleDateFormat dateFormat = new SimpleDateFormat("yyyyMMdd");
            dateFormat.setTimeZone(TimeZone.getTimeZone(ZoneOffset.UTC));
            String dateToCompare = dateFormat.format(new Date(currentTime));

            // for timestamps in long format (unformatted) we just test the first 10 characters to avoid false positives in these tests
            assertTrue(new Functions.TimeFormat().apply(null, Map.<String,Object>of()).startsWith(timeString.substring(0, 10)));
            assertTrue(new Functions.TimeFormat().apply("", Map.<String,Object>of()).startsWith(timeString.substring(0, 10)));
            assertEquals("", new Functions.TimeFormat().apply("", Map.<String,Object>of("format", "")));
            assertEquals(dateToCompare, new Functions.TimeFormat().apply("", Map.<String,Object>of("format", "yyyyMMdd")));
            assertEquals("", new Functions.TimeFormat().apply("", Map.<String,Object>of("format", "not a format")));

            assertTrue(new Functions.TimeFormat().apply("  ", Map.<String,Object>of()).startsWith(timeString.substring(0, 10)));
            assertEquals("", new Functions.TimeFormat().apply("  ", Map.<String,Object>of("format", "")));
            assertEquals(dateToCompare, new Functions.TimeFormat().apply("  ", Map.<String,Object>of("format", "yyyyMMdd")));
            assertEquals("", new Functions.TimeFormat().apply("  ", Map.<String,Object>of("format", "not a format")));

            assertEquals("", new Functions.TimeFormat().apply("not a number", Map.<String,Object>of()));
            assertEquals("", new Functions.TimeFormat().apply("not a number", Map.<String,Object>of("format", "")));
            assertEquals("", new Functions.TimeFormat().apply("not a number", Map.<String,Object>of("format", "yyyyMMdd")));
            assertEquals("", new Functions.TimeFormat().apply("not a number", Map.<String,Object>of("format", "not a format")));

            assertEquals("0", new Functions.TimeFormat().apply("0", Map.<String,Object>of()));
            assertEquals("", new Functions.TimeFormat().apply("0", Map.<String,Object>of("format", "")));
            assertEquals("19700101", new Functions.TimeFormat().apply("0", Map.<String,Object>of("format", "yyyyMMdd")));
            assertEquals("", new Functions.TimeFormat().apply("0", Map.<String,Object>of("format", "not a format")));

            assertEquals("1577880000000", new Functions.TimeFormat().apply("1577880000000", Map.<String,Object>of()));
            assertEquals("", new Functions.TimeFormat().apply("1577880000000", Map.<String,Object>of("format", "")));
            assertEquals("20200101", new Functions.TimeFormat().apply("1577880000000", Map.<String,Object>of("format", "yyyyMMdd")));
            assertEquals("", new Functions.TimeFormat().apply("1577880000000", Map.<String,Object>of("format", "not a format")));
        }
    }
}
