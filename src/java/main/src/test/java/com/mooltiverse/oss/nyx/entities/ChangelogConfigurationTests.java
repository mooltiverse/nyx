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

@DisplayName("ChangelogConfiguration")
public class ChangelogConfigurationTests {
    @Test
    @DisplayName("ChangelogConfiguration()")
    void constructorTest()
        throws Exception {
        assertEquals("tail", new ChangelogConfiguration("tail", "CHANGELOG.md", Map.<String,String>of("Section1", "regex1", "Section2", "regex2"), "changelog.tpl", Map.<String,String>of("Expression1", "string1")).getAppend());
        assertEquals("CHANGELOG.md", new ChangelogConfiguration("tail", "CHANGELOG.md", Map.<String,String>of("Section1", "regex1", "Section2", "regex2"), "changelog.tpl", Map.<String,String>of("Expression1", "string1")).getPath());
        assertEquals("regex1", new ChangelogConfiguration("tail", "CHANGELOG.md", Map.<String,String>of("Section1", "regex1", "Section2", "regex2"), "changelog.tpl", Map.<String,String>of("Expression1", "string1")).getSections().get("Section1"));
        assertEquals("regex2", new ChangelogConfiguration("tail", "CHANGELOG.md", Map.<String,String>of("Section1", "regex1", "Section2", "regex2"), "changelog.tpl", Map.<String,String>of("Expression1", "string1")).getSections().get("Section2"));
        assertEquals("string1", new ChangelogConfiguration("tail", "CHANGELOG.md", Map.<String,String>of("Section1", "regex1", "Section2", "regex2"), "changelog.tpl", Map.<String,String>of("Expression1", "string1")).getSubstitutions().get("Expression1"));
        assertEquals("changelog.tpl", new ChangelogConfiguration("tail", "CHANGELOG.md", Map.<String,String>of("Section1", "regex1", "Section2", "regex2"), "changelog.tpl", Map.<String,String>of("Expression1", "string1")).getTemplate());
    }
}