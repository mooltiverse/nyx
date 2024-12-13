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

package util

import (
	"math/rand" // https://pkg.go.dev/math/rand
	"time"      // https://pkg.go.dev/time
)

/*
Generates a random string containing only alphabetic characters.

Arguments are as follows:

- length the length of the string to generate.
- seed is used to disambiguate between multiple invocations.
*/
func RandomAlphabeticString(length int, seed int) string {
	charset := "abcdefghijklmnopqrstuvwxyz"

	res := make([]byte, length)
	rand.Seed(time.Now().UnixNano() * int64(seed))

	for i := 0; i < length; i++ {
		// Getting random character
		res[i] = charset[rand.Intn(len(charset))]
	}
	return string(res)
}

/*
Generates a random byte array.

Arguments are as follows:

- length the number of bytes to generate.
- seed is used to disambiguate between multiple invocations.
*/
func RandomBytes(length int, seed int) []byte {
	return []byte(RandomAlphabeticString(length, seed))
}
