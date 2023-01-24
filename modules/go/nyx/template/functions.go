/*
 * Copyright 2020 Mooltiverse
 *
 * Licensed under the Apache License, Version 2.0 (the "License")
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

package templates

import (
	"fmt"           // https://pkg.go.dev/fmt
	"os"            // https://pkg.go.dev/os
	"path/filepath" // https://pkg.go.dev/path/filepath
	"strconv"       // https://pkg.go.dev/strconv
	"strings"       // https://pkg.go.dev/strings
	"time"          // https://pkg.go.dev/time

	raymond "github.com/aymerick/raymond" // https://pkg.go.dev/github.com/aymerick/raymond
	regexp2 "github.com/dlclark/regexp2"  // https://pkg.go.dev/github.com/dlclark/regexp2, we need to use this instead of the standard 'regexp' to have support for lookarounds (look ahead), even if this implementation is a little slower
	log "github.com/sirupsen/logrus"      // https://pkg.go.dev/github.com/sirupsen/logrus
)

var (
	// the flag to know if helpers were already registered, as if we do it twice Raymond will panic
	helpersRegistered = false
)

/*
*
Registers all the available functions as a helpers with their names for the given registry.

Registered helpers are function adapters to the core business functions in the library
*/
func registerHelpers() {
	// avoid registering helpers twice or Raymond will panic
	// this could be done in an init() function, but this way we have more control
	if !helpersRegistered {
		raymond.RegisterHelper("lower", func(options *raymond.Options) raymond.SafeString {
			return raymond.SafeString(lower(options.Fn()))
		})
		raymond.RegisterHelper("upper", func(options *raymond.Options) raymond.SafeString {
			return raymond.SafeString(upper(options.Fn()))
		})
		raymond.RegisterHelper("trim", func(options *raymond.Options) raymond.SafeString {
			return raymond.SafeString(trim(options.Fn()))
		})
		raymond.RegisterHelper("first", func(options *raymond.Options) raymond.SafeString {
			return raymond.SafeString(first(options.Fn()))
		})
		raymond.RegisterHelper("firstLower", func(options *raymond.Options) raymond.SafeString {
			return raymond.SafeString(firstLower(options.Fn()))
		})
		raymond.RegisterHelper("firstUpper", func(options *raymond.Options) raymond.SafeString {
			return raymond.SafeString(firstUpper(options.Fn()))
		})
		raymond.RegisterHelper("last", func(options *raymond.Options) raymond.SafeString {
			return raymond.SafeString(last(options.Fn()))
		})
		raymond.RegisterHelper("lastLower", func(options *raymond.Options) raymond.SafeString {
			return raymond.SafeString(lastLower(options.Fn()))
		})
		raymond.RegisterHelper("lastUpper", func(options *raymond.Options) raymond.SafeString {
			return raymond.SafeString(lastUpper(options.Fn()))
		})
		raymond.RegisterHelper("sanitize", func(options *raymond.Options) raymond.SafeString {
			return raymond.SafeString(sanitize(options.Fn()))
		})
		raymond.RegisterHelper("sanitizeLower", func(options *raymond.Options) raymond.SafeString {
			return raymond.SafeString(sanitizeLower(options.Fn()))
		})
		raymond.RegisterHelper("sanitizeUpper", func(options *raymond.Options) raymond.SafeString {
			return raymond.SafeString(sanitizeUpper(options.Fn()))
		})
		raymond.RegisterHelper("short5", func(options *raymond.Options) raymond.SafeString {
			return raymond.SafeString(short5(options.Fn()))
		})
		raymond.RegisterHelper("short6", func(options *raymond.Options) raymond.SafeString {
			return raymond.SafeString(short6(options.Fn()))
		})
		raymond.RegisterHelper("short7", func(options *raymond.Options) raymond.SafeString {
			return raymond.SafeString(short7(options.Fn()))
		})
		raymond.RegisterHelper("timestampISO8601", func(options *raymond.Options) raymond.SafeString {
			return raymond.SafeString(timestampISO8601(options.Fn()))
		})
		raymond.RegisterHelper("timestampYYYYMMDDHHMMSS", func(options *raymond.Options) raymond.SafeString {
			return raymond.SafeString(timestampYYYYMMDDHHMMSS(options.Fn()))
		})

		raymond.RegisterHelper("environmentUser", func(options *raymond.Options) raymond.SafeString {
			return raymond.SafeString(environmentUser(options.Fn()))
		})
		raymond.RegisterHelper("environmentVariable", func(options *raymond.Options) raymond.SafeString {
			return raymond.SafeString(environmentVariable(options.Fn()))
		})

		raymond.RegisterHelper("fileContent", func(options *raymond.Options) raymond.SafeString {
			return raymond.SafeString(fileContent(options.Fn()))
		})
		raymond.RegisterHelper("fileExists", func(options *raymond.Options) raymond.SafeString {
			return raymond.SafeString(fileExists(options.Fn()))
		})

		raymond.RegisterHelper("cutLeft", func(options *raymond.Options) raymond.SafeString {
			return raymond.SafeString(cutLeft(options.Fn(), options.Hash()))
		})
		raymond.RegisterHelper("cutRight", func(options *raymond.Options) raymond.SafeString {
			return raymond.SafeString(cutRight(options.Fn(), options.Hash()))
		})
		raymond.RegisterHelper("timestamp", func(options *raymond.Options) raymond.SafeString {
			return raymond.SafeString(timestamp(options.Fn(), options.Hash()))
		})
	}
	helpersRegistered = true
}

/*
This method returns the lower case representation of the input string.
*/
func lower(input string) string {
	return strings.ToLower(input)
}

/*
This method returns the trimmed case representation of the input string.
*/
func trim(input string) string {
	return strings.Trim(input, " ")
}

/*
This method returns the upper case representation of the input string.
*/
func upper(input string) string {
	return strings.ToUpper(input)
}

/*
This method returns the input string with everything from the first occurrence of a character other
than letters and positive digits discarded.
*/
func first(input string) string {
	regex := "^([a-zA-Z0-9]*)"
	re, err := regexp2.Compile(regex, 0)
	if err != nil {
		panic(fmt.Sprintf("regular expression '%s' can't be compiled: %v", regex, err))
	}
	m, err := re.FindStringMatch(input)
	if err != nil {
		panic(fmt.Sprintf("regular expression '%s' can't be matched: %v", regex, err))
	}
	return m.String()
}

/*
This method returns the input string with everything other than letters and positive digits discarded
and the remainder transformed to lower case.
*/
func firstLower(input string) string {
	return lower(first(input))
}

/*
This method returns the input string with everything other than letters and positive digits discarded
and the remainder transformed to upper case.
*/
func firstUpper(input string) string {
	return upper(first(input))
}

/*
This method returns the last part of the input string that does not contains characters other than.
letters and positive digits.
*/
func last(input string) string {
	regex := "(^[a-zA-Z0-9]*)?([a-zA-Z0-9]*)$"
	re, err := regexp2.Compile(regex, 0)
	if err != nil {
		log.Errorf("regular expression '%s' can't be compiled: %v", regex, err)
		return ""
	}
	m, err := re.FindStringMatch(input)
	if err != nil {
		log.Errorf("regular expression '%s' can't be matched: %v", regex, err)
		return ""
	}
	return m.String()
}

/*
This method returns the last part of the input string that does not contains characters other than
letters and positive digits and the remainder transformed to lower case.
*/
func lastLower(input string) string {
	return lower(last(input))
}

/*
This method returns the last part of the input string that does not contains characters other than
letters and positive digits and the remainder transformed to upper case.
*/
func lastUpper(input string) string {
	return upper(last(input))
}

/*
This method returns the input string with everything other than letters and positive digits discarded.
*/
func sanitize(input string) string {
	regex := "[^a-zA-Z0-9]"
	re, err := regexp2.Compile(regex, 0)
	if err != nil {
		log.Errorf("regular expression '%s' can't be compiled: %v", regex, err)
		return ""
	}
	output, err := re.Replace(input, "", -1, -1)
	if err != nil {
		log.Errorf("characters matched by regular expression '%s' can't be replaced: %v", regex, err)
		return ""
	}
	return output
}

/*
This method returns the input string with everything other than letters and positive digits discarded
and the remainder transformed to lower case.
*/
func sanitizeLower(input string) string {
	return lower(sanitize(input))
}

/*
This method returns the input string with everything other than letters and positive digits discarded
and the remainder transformed to upper case.
*/
func sanitizeUpper(input string) string {
	return upper(sanitize(input))
}

/*
This method returns the first 5 characters of the input string, if it's longer than 5 characters,
otherwise returns the input string or the empty string if the input is nil.
*/
func short5(input string) string {
	if len(input) > 5 {
		return input[0:5]
	} else {
		return input
	}
}

/*
This method returns the first 6 characters of the input string, if it's longer than 6 characters,
otherwise returns the input string or the empty string if the input is nil.
*/
func short6(input string) string {
	if len(input) > 6 {
		return input[0:6]
	} else {
		return input
	}
}

/*
This method returns the first 7 characters of the input string, if it's longer than 7 characters,
otherwise returns the input string or the empty string if the input is nil.
*/
func short7(input string) string {
	if len(input) > 7 {
		return input[0:7]
	} else {
		return input
	}
}

/*
This method parses the input string as a long representing a timestamp in the
https://www.unixtimestamp.com/ unix format and returns it formatted as
https://en.wikipedia.org/wiki/ISO_8601 ISO 8601 UTC timestamp.
If the input value fails to parse for whatever reason then the emty string is returned
(and an error is logged).
*/
func timestampISO8601(input string) string {
	i, err := strconv.ParseInt(input, 10, 64)
	if err != nil {
		log.Errorf("the string '%d' can't be parsed as integer: %v", i, err)
		return ""
	}
	t := time.UnixMilli(i).UTC()
	return t.Format("2006-01-02T15:04:05" /*time.RFC3339*/)
}

/*
This method parses the input string as a long representing a timestamp in the
https://www.unixtimestamp.com/ unix format and returns it formatted as
YYYYMMDDHHMMSS UTC.
If the input value fails to parse for whatever reason then the emty string is returned
(and an error is logged).
*/
func timestampYYYYMMDDHHMMSS(input string) string {
	i, err := strconv.ParseInt(input, 10, 64)
	if err != nil {
		log.Errorf("the string '%d' can't be parsed as integer: %v", i, err)
		return ""
	}
	t := time.UnixMilli(i).UTC()
	return t.Format("20060102150405" /*"yyyyMMddHHmmss"*/)
}

/*
This method returns the value of the environment variable with the given name, if any.
*/
func environmentVariable(input string) string {
	return os.Getenv(input)
}

/*
This method returns the current user name.
*/
func environmentUser(input string) string {
	// in order to spot whether we run on Windows (where the variable is 'USERNAME') we use this trick
	// as Getuid returns -1
	if os.Getuid() == -1 {
		return os.Getenv("USERNAME")
	} else {
		return os.Getenv("USER")
	}
}

/*
This method returns the content of the given file, if any.
*/
func fileContent(input string) string {
	path, err := filepath.Abs(input)
	if err != nil {
		log.Errorf("unable to get the absolute path for file '%s': %v", input, err)
		return ""
	}
	content, err := os.ReadFile(path)
	if err != nil {
		log.Errorf("unable to read from file '%s': %v", input, err)
		return ""
	}
	return string(content)
}

/*
This method returns true if the file with the given name exists, false otherwise.
*/
func fileExists(input string) string {
	path, err := filepath.Abs(input)
	if err != nil {
		log.Errorf("unable to get the absolute path for file '%s': %v", input, err)
		return ""
	}
	_, err = os.Stat(path)
	if err == nil {
		return "true"
	} else {
		return "false"
	}
}

/*
This method returns the last N characters of the input string, where N is the length option.
If the input string is shorter or the same length than the parameter, the whole input string is returned unchanged.
If the input is nil an empty string is returned.
*/
func cutLeft(input string, options map[string]interface{}) string {
	optionString, found := options["length"]
	if found {
		length, err := strconv.Atoi(fmt.Sprintf("%v", optionString))
		if err != nil {
			log.Errorf("the '%s' option value '%s' for the '%s' function is not a valid integer", "length", optionString, "cutLeft")
			return input
		}
		if length < 0 {
			log.Errorf("the '%s' option value '%s' for the '%s' function cannot be negative", "length", optionString, "cutLeft")
			return input
		}
		if len(input) > length {
			return input[len(input)-length : len(input)]
		} else {
			return input
		}
	} else {
		return input
	}
}

/*
This method returns the first N characters of the input string, where N is the length option.
If the input string is shorter or the same length than the parameter, the whole input string is returned unchanged.
If the input is nil an empty string is returned.
*/
func cutRight(input string, options map[string]interface{}) string {
	optionString, found := options["length"]
	if found {
		length, err := strconv.Atoi(fmt.Sprintf("%v", optionString))
		if err != nil {
			log.Errorf("the '%s' option value '%s' for the '%s' function is not a valid integer", "length", optionString, "cutRight")
			return input
		}
		if length < 0 {
			log.Errorf("the '%s' option value '%s' for the '%s' function cannot be negative", "length", optionString, "cutRight")
			return input
		}
		if len(input) > length {
			return input[0:length]
		} else {
			return input
		}
	} else {
		return input
	}
}

/*
This method returns a time value (expressed in milliseconds) and is also able to format it according to an optional
format string.
*/
func timestamp(input string, options map[string]interface{}) string {
	currentTime := time.Now().UnixMilli()
	var err error
	if "" != strings.TrimSpace(input) {
		currentTime, err = strconv.ParseInt(input, 10, 64)
		if err != nil {
			log.Errorf("the value '%s' for the '%s' function is not a valid integer", input, "timestamp")
			return ""
		}
		if currentTime < 0 {
			log.Errorf("the value '%s' for the '%s' function cannot be negative", input, "timestamp")
			return input
		}
	}

	formatString, found := options["format"]
	if found {
		t := time.UnixMilli(currentTime).UTC()
		return t.Format(fmt.Sprintf("%v", formatString))
	} else {
		return strconv.FormatInt(currentTime, 10)
	}
}
