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
package com.mooltiverse.oss.nyx.entities;

import static org.junit.jupiter.api.Assertions.*;

import java.util.Map;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("CommitMessageConvention")
public class CommitMessageConventionTests {
    @Test
    @DisplayName("CommitMessageConvention()")
    void constructorTest()
        throws Exception {
        assertEquals("regex1", new CommitMessageConvention("regex1", Map.<String,String>of("k1", "v1")).getExpression());
        assertTrue(new CommitMessageConvention("regex1", Map.<String,String>of("k1", "v1")).getBumpExpressions().containsKey("k1"));
        assertEquals("v1", new CommitMessageConvention("regex1", Map.<String,String>of("k1", "v1")).getBumpExpressions().get("k1"));
    }
}