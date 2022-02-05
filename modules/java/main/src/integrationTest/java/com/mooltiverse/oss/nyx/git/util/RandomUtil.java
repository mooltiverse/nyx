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
package com.mooltiverse.oss.nyx.git.util;

import java.util.Random;

/**
 * Random utilities
 */
public class RandomUtil {
    /**
     * Default constructor is private on purpose.
     */
    private RandomUtil() {
        super();
    }

    /**
     * Generates a random string containing only alphabetic characters.
     * 
     * @param length the length of the string to generate.
     * 
     * @return the generated string.
     */
    public static String randomAlphabeticString(int length) {
        int leftLimit = 97; // letter 'a'
        int rightLimit = 122; // letter 'z'
     
        return new Random().ints(leftLimit, rightLimit + 1).limit(length).collect(StringBuilder::new, StringBuilder::appendCodePoint, StringBuilder::append).toString();
    }

    /**
     * Generates a random byte array.
     * 
     * @param length the number of bytes to generate.
     * 
     * @return the generated byte array.
     */
    public static byte[] randomBytes(int length) {
        byte[] res = new byte[length];
        new Random().nextBytes(res);
        return res;
    }
}