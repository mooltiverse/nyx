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

import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("ReleaseType")
public class ReleaseTypeTests {
    @Test
    @DisplayName("ReleaseType()")
    void constructorTest()
        throws Exception {
        assertNotNull(
            new ReleaseType(
                true,
                "{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}",
                "Release description",
                "^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$", Boolean.TRUE.toString(),
                "Committing {{version}}",
                Boolean.TRUE.toString(),
                Boolean.TRUE.toString(),
                "Tagging {{version}}",
                List.<Identifier>of(new Identifier("build", "12", Identifier.Position.BUILD)),
                "",
                Map.<String,String>of("PATH",".*"),
                null,
                Boolean.TRUE.toString(),
                "",
                Boolean.FALSE));
        assertNull(new ReleaseType().getMatchEnvironmentVariables());
    }
}