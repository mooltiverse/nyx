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

@DisplayName("GitConfiguration")
public class GitConfigurationTests {
    @Test
    @DisplayName("GitConfiguration()")
    void constructorTest()
        throws Exception {
        assertEquals(1, new GitConfiguration(Map.<String,GitRemoteConfiguration>of("r1", new GitRemoteConfiguration(AuthenticationMethod.USER_PASSWORD, "u1", "p1", "k1", "p2"))).getRemotes().size());
        assertTrue(new GitConfiguration(Map.<String,GitRemoteConfiguration>of("r1", new GitRemoteConfiguration(AuthenticationMethod.USER_PASSWORD, "u1", "p1", "k1", "p2"))).getRemotes().containsKey("r1"));
        assertEquals(AuthenticationMethod.USER_PASSWORD, new GitConfiguration(Map.<String,GitRemoteConfiguration>of("r1", new GitRemoteConfiguration(AuthenticationMethod.USER_PASSWORD, "u1", "p1", "k1", "p2"))).getRemotes().get("r1").getAuthenticationMethod());
        assertEquals("p1", new GitConfiguration(Map.<String,GitRemoteConfiguration>of("r1", new GitRemoteConfiguration(AuthenticationMethod.USER_PASSWORD, "u1", "p1", "k1", "p2"))).getRemotes().get("r1").getPassword());
        assertEquals("u1", new GitConfiguration(Map.<String,GitRemoteConfiguration>of("r1", new GitRemoteConfiguration(AuthenticationMethod.USER_PASSWORD, "u1", "p1", "k1", "p2"))).getRemotes().get("r1").getUser());
        assertEquals("k1", new GitConfiguration(Map.<String,GitRemoteConfiguration>of("r1", new GitRemoteConfiguration(AuthenticationMethod.USER_PASSWORD, "u1", "p1", "k1", "p2"))).getRemotes().get("r1").getPrivateKey());
        assertEquals("p2", new GitConfiguration(Map.<String,GitRemoteConfiguration>of("r1", new GitRemoteConfiguration(AuthenticationMethod.USER_PASSWORD, "u1", "p1", "k1", "p2"))).getRemotes().get("r1").getPassphrase());
    }
}